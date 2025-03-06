"""
Blackjack simulation and reinforcement learning algorithms
Based on Chapter 5 of Reinforcement Learning: An Introduction
"""
# Actions: hit or stand
@enum BlackjackAction ACTION_HIT=0 ACTION_STAND=1

# State representation
# the player's sum, the dealer's visible card, and a boolean indicating if the player has a usable ace
mutable struct BlackjackState
    player_sum::Int
    dealer_card::Int
    usable_ace::Bool
end

# Environment
# the state, the dealer's second card, a boolean indicating if the dealer has a usable ace, a boolean indicating if the episode is terminated, and the reward
mutable struct BlackjackEnv
    state::BlackjackState
    dealer_second_card::Int
    dealer_usable_ace::Bool
    terminated::Bool
    reward::Float64
    
    function BlackjackEnv(initial_state=nothing)
        if initial_state === nothing
            # Initialize with random state
            env = new(
                BlackjackState(0, 0, false),
                0,
                false,
                false,
                0.0
            )
            reset!(env)
            return env
        else
            # Use specified initial state
            env = new(
                initial_state,
                get_card(),
                false,
                false,
                0.0
            )
            
            # Initialize dealer's status
            dealer_sum = card_value(initial_state.dealer_card) + card_value(env.dealer_second_card)
            env.dealer_usable_ace = (initial_state.dealer_card == 1 || env.dealer_second_card == 1)
            
            # If dealer's sum is over 21, must have two aces
            if dealer_sum > 21
                @assert dealer_sum == 22 "Dealer sum exceeded 21 without two aces"
                dealer_sum -= 10
                env.dealer_usable_ace = true
            end
            
            return env
        end
    end
end

# Get a new card (1-10, face cards are 10)
function get_card()::Int
    card = rand(1:13)
    return min(card, 10)
end

# Get the value of a card (11 for ace)
function card_value(card::Int)::Int
    return card == 1 ? 11 : card
end

# Default policy for the player (based on current sum)
function player_policy(state::BlackjackState)::BlackjackAction
    return state.player_sum >= 20 ? ACTION_STAND : ACTION_HIT
end

# Behavior policy for off-policy learning (random 50/50 policy)
function behavior_policy(state::BlackjackState)::BlackjackAction
    return rand() < 0.5 ? ACTION_STAND : ACTION_HIT
end

# Policy for the dealer (hit on 16 or less, stand on 17+)
function dealer_policy(dealer_sum::Int)::BlackjackAction
    return dealer_sum >= 17 ? ACTION_STAND : ACTION_HIT
end

# Reset the environment to a new initial state
function reset!(env::BlackjackEnv)
    # Reset player status
    env.state.player_sum = 0
    env.state.usable_ace = false
    env.terminated = false
    env.reward = 0.0
    
    # Deal cards until player has 12 or more
    while env.state.player_sum < 12
        card = get_card()
        env.state.player_sum += card_value(card)
        
        # If player's sum is over 21, must use ace as 1 instead of 11
        if env.state.player_sum > 21
            if card == 1  # If the last card was an ace
                env.state.player_sum -= 10
            elseif env.state.usable_ace
                env.state.player_sum -= 10
                env.state.usable_ace = false
            else
                # This should rarely happen, but let's handle it gracefully
                # Start over with initialization to avoid assertion errors
                env.state.player_sum = 0
                env.state.usable_ace = false
                continue
            end
        elseif card == 1
            env.state.usable_ace = true
        end
    end
    
    # Initialize dealer's cards
    env.state.dealer_card = get_card()
    env.dealer_second_card = get_card()
    
    # Calculate dealer's initial sum and usable ace status
    dealer_sum = card_value(env.state.dealer_card) + card_value(env.dealer_second_card)
    env.dealer_usable_ace = (env.state.dealer_card == 1 || env.dealer_second_card == 1)
    
    # If dealer's sum is over 21, must have two aces
    if dealer_sum > 21
        if env.state.dealer_card == 1 && env.dealer_second_card == 1  # Two aces
            dealer_sum -= 10
            env.dealer_usable_ace = true
        else
            # This should not happen based on the rules, but let's handle it
            # by regenerating dealer cards
            while true
                env.state.dealer_card = get_card()
                env.dealer_second_card = get_card()
                dealer_sum = card_value(env.state.dealer_card) + card_value(env.dealer_second_card)
                env.dealer_usable_ace = (env.state.dealer_card == 1 || env.dealer_second_card == 1)
                
                if dealer_sum <= 21
                    break
                elseif env.state.dealer_card == 1 && env.dealer_second_card == 1
                    dealer_sum -= 10
                    env.dealer_usable_ace = true
                    break
                end
            end
        end
    end
    
    return env.state
end

# Player takes an action
function step!(env::BlackjackEnv, action::BlackjackAction)
    if env.terminated
        return env.state, env.reward, env.terminated
    end
    
    # Player's turn
    if action == ACTION_HIT
        card = get_card()
        ace_count = Int(env.state.usable_ace)
        
        # Add new card to player's sum
        if card == 1
            ace_count += 1
        end
        env.state.player_sum += card_value(card)
        
        # Use aces as 1 to avoid busting
        while env.state.player_sum > 21 && ace_count > 0
            env.state.player_sum -= 10
            ace_count -= 1
        end
        
        # Check if player busts
        if env.state.player_sum > 21
            env.terminated = true
            env.reward = -1.0
            return env.state, env.reward, env.terminated
        end
        
        env.state.usable_ace = (ace_count == 1)
        return env.state, 0.0, false
    else  # ACTION_STAND
        # Dealer's turn
        dealer_sum = card_value(env.state.dealer_card) + card_value(env.dealer_second_card)
        usable_ace_dealer = env.dealer_usable_ace
        
        # Dealer hits until sum >= 17
        while true
            action = dealer_policy(dealer_sum)
            if action == ACTION_STAND
                break
            end
            
            # Dealer hits
            new_card = get_card()
            ace_count = Int(usable_ace_dealer)
            
            if new_card == 1
                ace_count += 1
            end
            
            dealer_sum += card_value(new_card)
            
            # Use aces as 1 to avoid busting
            while dealer_sum > 21 && ace_count > 0
                dealer_sum -= 10
                ace_count -= 1
            end
            
            # Check if dealer busts
            if dealer_sum > 21
                env.terminated = true
                env.reward = 1.0
                return env.state, env.reward, env.terminated
            end
            
            usable_ace_dealer = (ace_count == 1)
        end
        
        # Compare player and dealer
        if dealer_sum > 21 || env.state.player_sum > 21
            # This should never happen due to the bust checks above
            # But just in case, let's handle it properly
            if dealer_sum > 21
                env.terminated = true
                env.reward = 1.0  # Player wins
            else
                env.terminated = true
                env.reward = -1.0  # Dealer wins
            end
        else
            # Normal comparison
            env.terminated = true
            if env.state.player_sum > dealer_sum
                env.reward = 1.0
            elseif env.state.player_sum == dealer_sum
                env.reward = 0.0
            else
                env.reward = -1.0
            end
        end
        
        return env.state, env.reward, env.terminated
    end
end

# Play a full episode
function play_episode(policy_fn, initial_state=nothing, initial_action=nothing)
    env = BlackjackEnv(initial_state)
    state = env.state
    rewards = 0.0
    trajectory = Vector{Tuple{BlackjackState, BlackjackAction}}()
    
    # First action
    action = initial_action === nothing ? policy_fn(state) : initial_action
    
    # Play until termination
    terminated = false
    while !terminated
        # Record state-action pair
        trajectory_state = BlackjackState(state.player_sum, state.dealer_card, state.usable_ace)
        push!(trajectory, (trajectory_state, action))
        
        # Take action
        state, reward, terminated = step!(env, action)
        rewards += reward
        
        # Get next action if not terminated
        if !terminated
            action = policy_fn(state)
        end
    end
    
    return env.state, rewards, trajectory
end

# Monte Carlo with On-Policy
function monte_carlo_on_policy(episodes::Int)
    # Initialize value functions
    states_usable_ace = zeros(Float64, 10, 10)  # player_sum (12-21), dealer_card (1-10)
    states_usable_ace_count = ones(Float64, 10, 10)  # Initialize to 1 to avoid division by zero
    states_no_usable_ace = zeros(Float64, 10, 10)
    states_no_usable_ace_count = ones(Float64, 10, 10)
    
    for _ in 1:episodes
        _, reward, trajectory = play_episode(player_policy)
        
        # Update counts and rewards for each state in trajectory
        for (state, _) in trajectory
            # Skip invalid states
            if state.player_sum < 12 || state.player_sum > 21 || state.dealer_card < 1 || state.dealer_card > 10
                continue
            end
            
            player_idx = state.player_sum - 11
            dealer_idx = state.dealer_card
            
            # Ensure indices are within range
            if player_idx < 1 || player_idx > 10 || dealer_idx < 1 || dealer_idx > 10
                continue
            end
            
            if state.usable_ace
                states_usable_ace_count[player_idx, dealer_idx] += 1
                states_usable_ace[player_idx, dealer_idx] += reward
            else
                states_no_usable_ace_count[player_idx, dealer_idx] += 1
                states_no_usable_ace[player_idx, dealer_idx] += reward
            end
        end
    end
    
    return states_usable_ace ./ states_usable_ace_count, states_no_usable_ace ./ states_no_usable_ace_count
end

# Monte Carlo with Exploring Starts
function monte_carlo_es(episodes::Int)
    # Initialize state-action values
    # [player_sum (12-21), dealer_card (1-10), usable_ace (false=1, true=2), action (hit=1, stand=2)]
    state_action_values = zeros(Float64, 10, 10, 2, 2)
    state_action_counts = ones(Float64, 10, 10, 2, 2)
    
    # Define greedy policy based on current values
    function greedy_policy(state::BlackjackState)
        # Skip invalid states
        if state.player_sum < 12 || state.player_sum > 21 || state.dealer_card < 1 || state.dealer_card > 10
            return ACTION_STAND
        end
        
        player_idx = state.player_sum - 11
        dealer_idx = state.dealer_card
        usable_ace_idx = Int(state.usable_ace) + 1
        
        # Ensure indices are within range
        if player_idx < 1 || player_idx > 10 || dealer_idx < 1 || dealer_idx > 10
            return ACTION_STAND
        end
        
        values = state_action_values[player_idx, dealer_idx, usable_ace_idx, :]
        counts = state_action_counts[player_idx, dealer_idx, usable_ace_idx, :]
        action_values = values ./ counts
        
        # Choose the action with highest value (with random tie-breaking)
        best_actions = findall(action_values .== maximum(action_values))
        best_action_idx = rand(best_actions) - 1
    
        # Properly convert to enum
        if best_action_idx == 0
            return ACTION_HIT
        else
            return ACTION_STAND
        end
    end
    
    for episode in 1:episodes
        # Generate random initial state and action
        player_sum = rand(12:21)
        dealer_card = rand(1:10)
        usable_ace = rand(Bool)
        initial_state = BlackjackState(player_sum, dealer_card, usable_ace)
        initial_action = rand([ACTION_HIT, ACTION_STAND])
        
        # For first episode, use fixed policy to match Python code
        policy = episode == 1 ? player_policy : greedy_policy
        
        # Run episode
        _, reward, trajectory = play_episode(policy, initial_state, initial_action)
        
        # Track visited state-action pairs
        visited = Set{Tuple{Int, Int, Int, Int}}()
        
        # Update values for first visits only
        for (state, action) in trajectory
            # Skip invalid states
            if state.player_sum < 12 || state.player_sum > 21 || state.dealer_card < 1 || state.dealer_card > 10
                continue
            end
            
            player_idx = state.player_sum - 11
            dealer_idx = state.dealer_card
            usable_ace_idx = Int(state.usable_ace) + 1
            action_idx = Int(action) + 1
            
            # Ensure indices are within range
            if player_idx < 1 || player_idx > 10 || dealer_idx < 1 || dealer_idx > 10
                continue
            end
            
            state_action = (player_idx, dealer_idx, usable_ace_idx, action_idx)
            
            if state_action ∉ visited
                push!(visited, state_action)
                state_action_values[state_action...] += reward
                state_action_counts[state_action...] += 1
            end
        end
    end
    
    return state_action_values ./ state_action_counts
end

# Monte Carlo with Off-Policy
function monte_carlo_off_policy(episodes::Int)
    # Define initial state
    initial_state = BlackjackState(13, 2, true)
    
    rhos = Vector{Float64}(undef, episodes)
    returns = Vector{Float64}(undef, episodes)
    
    for episode in 1:episodes
        # Generate episode using behavior policy
        _, reward, trajectory = play_episode(behavior_policy, initial_state)
        
        # Calculate importance sampling ratio
        numerator = 1.0
        denominator = 1.0
        
        for (state, action) in trajectory
            target_action = player_policy(state)
            
            if action == target_action
                # Probability of selecting this action under target policy is 1
                # Probability of selecting this action under behavior policy is 0.5
                denominator *= 0.5
            else
                # Target policy would not select this action, ratio becomes 0
                numerator = 0.0
                break
            end
        end
        
        rho = numerator / denominator
        rhos[episode] = rho
        returns[episode] = reward
    end
    
    # Calculate estimations
    weighted_returns = rhos .* returns
    
    cumulative_weighted_returns = cumsum(weighted_returns)
    cumulative_rhos = cumsum(rhos)
    
    ordinary_sampling = cumulative_weighted_returns ./ (1:episodes)
    weighted_sampling = zeros(Float64, episodes)
    for i in 1:episodes
        if cumulative_rhos[i] != 0
            weighted_sampling[i] = cumulative_weighted_returns[i] / cumulative_rhos[i]
        end
    end
    
    return ordinary_sampling, weighted_sampling
end