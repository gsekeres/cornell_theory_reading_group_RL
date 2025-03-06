#= 
Consider a blackjack game with a large, but not infinite, shuffled deck. (of size N * 52)
We take the position of a single player with n other players at the table, playing 
against the dealer. 

We assume that the other players are following a fixed policy, as is the dealer. They will 
hit on 16 or less, and stand on 17 or more.

Moreover, we will initialize the player with a number of chips, Y, and they can bet some amount.

The player can double down and can split. The player can hit or stand on any point except for 21.

After each round, the number of cards that have been removed from the deck are recorded, and the 
cards are randomized and then placed on the bottom of the deck.

We want to model the value of the player's strategy as a function of Y, the number of other players,
and the number of decks in the game.
=# 

# BROKEN!! I"M FIXING - gabe
using Random, Statistics, Plots, ProgressMeter

"""
    Card

A playing card with a rank and suit.
"""
struct Card
    rank::Int  # 1 (Ace) through 13 (King)
    suit::Int  # 1-4 for clubs, diamonds, hearts, spades
end

"""
    card_value(card::Card)

Return the blackjack value of a card.
"""
function card_value(card::Card)
    if card.rank == 1
        return 11  # Ace (will be handled separately for soft hands)
    elseif card.rank > 10
        return 10  # Face cards
    else
        return card.rank
    end
end

"""
    hand_value(hand::Vector{Card})

Calculate the value of a blackjack hand, handling aces optimally.
"""
function hand_value(hand::Vector{Card})
    # Count aces separately
    non_aces = filter(card -> card.rank != 1, hand)
    aces = filter(card -> card.rank == 1, hand)
    
    # Sum non-ace cards
    value = sum(card_value.(non_aces))
    
    # Handle aces optimally
    for _ in aces
        if value + 11 <= 21
            value += 11
        else
            value += 1
        end
    end
    
    return value
end

"""
    is_soft_hand(hand::Vector{Card})

Determine if a hand is a "soft" hand (contains an ace counted as 11).
"""
function is_soft_hand(hand::Vector{Card})
    # Count aces and non-ace values
    non_aces = filter(card -> card.rank != 1, hand)
    aces = filter(card -> card.rank == 1, hand)
    
    non_ace_value = sum(card_value.(non_aces))
    
    # If we have at least one ace and counting one as 11 doesn't bust, it's a soft hand
    return !isempty(aces) && (non_ace_value + 11 <= 21)
end

"""
    is_pair(hand::Vector{Card})

Determine if a hand is a pair (two cards of the same rank).
"""
function is_pair(hand::Vector{Card})
    return length(hand) == 2 && hand[1].rank == hand[2].rank
end

"""
    BlackjackState

Represents the state of a blackjack game.
"""
mutable struct BlackjackState
    player_hands::Vector{Vector{Card}}  # Multiple hands in case of splits
    dealer_visible::Card
    dealer_hand::Vector{Card}
    current_bets::Vector{Int}  # Bet amount for each player hand
    chips::Int                 # Player's remaining chips
    deck::Vector{Card}         # The deck of cards
    cards_seen::Vector{Card}   # Cards that have been seen
    num_players::Int           # Number of other players at the table
    other_player_hands::Vector{Vector{Card}}  # Hands of other players
end

"""
    create_deck(num_decks::Int)

Create a shuffled deck with the specified number of standard decks.
"""
function create_deck(num_decks::Int)
    deck = Card[]
    for _ in 1:num_decks
        for rank in 1:13
            for suit in 1:4
                push!(deck, Card(rank, suit))
            end
        end
    end
    shuffle!(deck)
    return deck
end

"""
    deal_card!(state::BlackjackState)

Deal a card from the deck and add it to cards_seen.
"""
function deal_card!(state::BlackjackState)
    if isempty(state.deck)
        error("Deck is empty!")
    end
    card = pop!(state.deck)
    push!(state.cards_seen, card)
    return card
end

"""
    initialize_game(num_decks::Int, initial_chips::Int, num_players::Int)

Initialize a new blackjack game.
"""
function initialize_game(num_decks::Int, initial_chips::Int, num_players::Int)
    deck = create_deck(num_decks)
    
    return BlackjackState(
        [Card[]],           # Player starts with no cards
        Card(0, 0),         # No dealer card visible yet
        Card[],             # Dealer has no cards yet
        [0],                # No bets placed yet
        initial_chips,      # Starting chips
        deck,               # The shuffled deck
        Card[],             # No cards seen yet
        num_players,        # Number of other players
        [Card[] for _ in 1:num_players]  # Other players' hands
    )
end

"""
    deal_initial_cards!(state::BlackjackState)

Deal the initial cards to all players and the dealer.
"""
function deal_initial_cards!(state::BlackjackState)
    # First card to each player
    for i in 1:length(state.player_hands)
        push!(state.player_hands[i], deal_card!(state))
    end
    
    # First card to other players
    for i in 1:state.num_players
        push!(state.other_player_hands[i], deal_card!(state))
    end
    
    # First card to dealer (face down)
    push!(state.dealer_hand, deal_card!(state))
    
    # Second card to each player
    for i in 1:length(state.player_hands)
        push!(state.player_hands[i], deal_card!(state))
    end
    
    # Second card to other players
    for i in 1:state.num_players
        push!(state.other_player_hands[i], deal_card!(state))
    end
    
    # Second card to dealer (face up)
    state.dealer_visible = deal_card!(state)
    push!(state.dealer_hand, state.dealer_visible)
end

"""
    fixed_policy_action(hand::Vector{Card}, dealer_card::Card)

Determine action based on a fixed policy (for other players and dealer).
Returns :hit or :stand.
"""
function fixed_policy_action(hand::Vector{Card}, dealer_card::Card)
    value = hand_value(hand)
    if value <= 16
        return :hit
    else
        return :stand
    end
end

"""
    play_dealer_hand!(state::BlackjackState)

Play out the dealer's hand according to fixed rules (hit on 16 or less, stand on 17 or more).
"""
function play_dealer_hand!(state::BlackjackState)
    while hand_value(state.dealer_hand) <= 16
        push!(state.dealer_hand, deal_card!(state))
    end
end

"""
    play_other_players_hands!(state::BlackjackState)

Play out the hands of other players using the fixed policy.
"""
function play_other_players_hands!(state::BlackjackState)
    for i in 1:state.num_players
        hand = state.other_player_hands[i]
        while fixed_policy_action(hand, state.dealer_visible) == :hit
            push!(hand, deal_card!(state))
            if hand_value(hand) > 21
                break  # Bust
            end
        end
    end
end

"""
    determine_outcome(player_value::Int, dealer_value::Int, is_blackjack::Bool, dealer_hand::Vector{Card})

Determine the outcome of a hand. Returns :win, :lose, :push, or :blackjack.
"""
function determine_outcome(player_value::Int, dealer_value::Int, is_blackjack::Bool, dealer_hand::Vector{Card})
    if player_value > 21
        return :lose  # Player busts
    elseif is_blackjack && length(dealer_hand) == 2 && dealer_value == 21
        return :push  # Both have blackjack
    elseif is_blackjack
        return :blackjack  # Player has blackjack, dealer doesn't
    elseif dealer_value > 21
        return :win  # Dealer busts
    elseif player_value > dealer_value
        return :win  # Player has higher value
    elseif player_value < dealer_value
        return :lose  # Dealer has higher value
    else
        return :push  # Equal values
    end
end

"""
    calculate_reward(outcome::Symbol, bet::Int, doubled::Bool)

Calculate the reward based on the outcome and bet.
Returns an integer value.
"""
function calculate_reward(outcome::Symbol, bet::Int, doubled::Bool)
    actual_bet = doubled ? 2 * bet : bet
    
    if outcome == :win
        return actual_bet
    elseif outcome == :blackjack
        # Convert the floating point result to integer to avoid conversion issues
        return Int(floor(1.5 * bet))
    elseif outcome == :lose
        return -actual_bet
    else  # Push
        return 0
    end
end

# State representation for RL
"""
    state_to_features(state::BlackjackState, hand_idx::Int)

Convert the current state to a feature vector for the RL agent.
"""
function state_to_features(state::BlackjackState, hand_idx::Int)
    # Basic features
    hand = state.player_hands[hand_idx]
    player_value = hand_value(hand)
    dealer_visible_value = card_value(state.dealer_visible)
    is_soft = is_soft_hand(hand)
    is_pair_hand = is_pair(hand)
    
    # Improved card counting - Hi-Lo system
    seen_cards = state.cards_seen
    total_cards = length(state.deck) + length(seen_cards)
    
    # Count high, mid, and low cards seen (Hi-Lo system)
    high_cards_seen = count(c -> c.rank >= 10 || c.rank == 1, seen_cards)  # 10s, face cards, aces
    low_cards_seen = count(c -> c.rank >= 2 && c.rank <= 6, seen_cards)    # 2-6
    # mid_cards_seen = count(c -> c.rank >= 7 && c.rank <= 9, seen_cards)  # 7-9 (neutral)
    
    # Calculate running count and true count
    running_count = low_cards_seen - high_cards_seen  # Positive when low cards removed (deck rich in high cards)
    decks_remaining = max(1.0, length(state.deck) / 52)
    true_count = running_count / decks_remaining
    
    # Features for betting and playing strategy
    return [
        player_value,         # Current hand value
        dealer_visible_value, # Dealer's visible card value
        is_soft ? 1 : 0,      # Whether the hand is soft
        is_pair_hand ? 1 : 0, # Whether we have a pair
        length(hand),         # Number of cards in hand
        true_count,           # Card counting metric
        state.chips,          # Available chips
        state.current_bets[hand_idx]  # Current bet for this hand
    ]
end

# Q-learning agent
"""
    QLearningAgent

A Q-learning agent for blackjack.
"""
mutable struct QLearningAgent
    Q_play::Dict{Tuple, Dict{Symbol, Float64}}  # Q-values for playing
    Q_bet::Dict{Tuple{Int, Int}, Dict{Int, Float64}}  # Q-values for betting (keys are (true_count, chips_bucket))
    alpha::Float64       # Learning rate
    gamma::Float64       # Discount factor
    epsilon::Float64     # Exploration rate
    min_bet::Int         # Minimum bet
    max_bet::Int         # Maximum bet
    betting_options::Vector{Int}  # Possible bet amounts
end

function create_agent(min_bet::Int, max_bet::Int, betting_increments::Int; use_basic_strategy::Bool=true)
    # Initialize empty Q-value dictionaries
    Q_play = Dict{Tuple, Dict{Symbol, Float64}}()
    Q_bet = Dict{Tuple{Int, Int}, Dict{Int, Float64}}()
    
    # Define betting options
    betting_options = collect(min_bet:betting_increments:max_bet)
    
    agent = QLearningAgent(
        Q_play,
        Q_bet,
        0.1,    # alpha - learning rate
        0.9,    # gamma - discount factor
        0.1,    # epsilon - exploration rate
        min_bet,
        max_bet,
        betting_options
    )
    
    # Initialize with basic strategy if requested
    if use_basic_strategy
        initialize_basic_strategy(agent)
    end
    
    return agent
end

"""
    get_play_state_key(features)

Convert state features to a key for the play Q-table.
"""
function get_play_state_key(features)
    # Discretize continuous features like true_count
    player_value = features[1]
    dealer_value = features[2]
    is_soft = features[3] > 0
    is_pair = features[4] > 0
    true_count_bucket = round(features[6])  # Simplify by rounding
    
    return (player_value, dealer_value, is_soft, is_pair, true_count_bucket)
end

"""
    get_bet_state_key(features)

Convert state features to a key for the betting Q-table.
"""
function get_bet_state_key(features)
    # For betting, we mainly care about the card counting and our chip stack
    true_count_bucket = Int(round(features[6]))
    chips_bucket = min(1000, Int(features[7] ÷ 100) * 100)  # Discretize chips into buckets of 100, max 1000
    
    return (true_count_bucket, chips_bucket)
end

"""
    sample(collection, weights)

Sample an element from a collection according to the given weights.
This is a simplified version for use in the improved betting strategy.
"""
function sample(collection, weights)
    # Compute cumulative weights
    cum_weights = cumsum(weights.values)
    
    # Generate a random number between 0 and the sum of weights
    r = rand() * cum_weights[end]
    
    # Find the index of the first element whose cumulative weight is >= r
    for i in 1:length(collection)
        if cum_weights[i] >= r
            return collection[i]
        end
    end
    
    # Fallback
    return collection[end]
end

"""
    Weights

A simple struct to hold weights for sampling.
"""
struct Weights
    values::Vector{Float64}
    
    Weights(values) = new(values ./ sum(values))
end

"""
    choose_bet(agent::QLearningAgent, features)

Choose a bet amount based on the current state.
"""
function choose_bet(agent::QLearningAgent, features)
    key = get_bet_state_key(features)
    chips = features[7]
    true_count = features[6]
    
    # Initialize Q-values if not seen this state before
    if !haskey(agent.Q_bet, key)
        agent.Q_bet[key] = Dict{Int, Float64}()
        for bet in agent.betting_options
            if bet <= chips  # Can't bet more than we have
                # Initialize with a basic card counting heuristic
                if true_count > 2
                    # Higher true count favors the player - initialize with higher bets
                    agent.Q_bet[key][bet] = 0.1 * bet
                elseif true_count < -1
                    # Negative true count favors the dealer - initialize with lower bets
                    agent.Q_bet[key][bet] = -0.1 * bet
                else
                    agent.Q_bet[key][bet] = 0.0
                end
            end
        end
    end
    
    # Filter bets that we can afford
    affordable_bets = filter(bet -> bet <= chips, collect(keys(agent.Q_bet[key])))
    
    if isempty(affordable_bets)
        return min(agent.min_bet, chips)  # Default to minimum bet if nothing is affordable
    end
    
    # Epsilon-greedy selection with smart initialization based on true count
    if rand() < agent.epsilon
        # Smart exploration - more likely to try higher bets with positive true count
        if true_count > 2 && length(affordable_bets) > 1
            # More likely to explore higher bets when count is favorable
            weights = [bet/sum(affordable_bets) for bet in affordable_bets]
            return sample(affordable_bets, Weights(weights))
        else
            return rand(affordable_bets)  # Uniform exploration
        end
    else
        # Exploit - choose bet with highest Q-value
        return argmax(bet -> agent.Q_bet[key][bet], affordable_bets)
    end
end
"""
    get_valid_actions(features)

Determine valid actions based on the current state.
"""
function get_valid_actions(features)
    player_value = features[1]
    is_pair = features[4] > 0
    num_cards = features[5]
    
    actions = [:hit, :stand]
    
    # Can only double down on first two cards
    if num_cards == 2
        push!(actions, :double)
    end
    
    # Can only split pairs with 2 cards
    if is_pair && num_cards == 2
        push!(actions, :split)
    end
    
    # If 21, can only stand
    if player_value == 21
        return [:stand]
    end
    
    return actions
end
"""
    choose_action(agent::QLearningAgent, features)

Choose an action based on the current state.
"""
function choose_action(agent::QLearningAgent, features)
    key = get_play_state_key(features)
    valid_actions = get_valid_actions(features)
    
    # Initialize Q-values if not seen this state before
    if !haskey(agent.Q_play, key)
        agent.Q_play[key] = Dict{Symbol, Float64}()
        for action in valid_actions
            agent.Q_play[key][action] = 0.0
        end
    end
    
    # Ensure all valid actions have Q-values
    for action in valid_actions
        if !haskey(agent.Q_play[key], action)
            agent.Q_play[key][action] = 0.0
        end
    end
    
    # Epsilon-greedy selection
    if rand() < agent.epsilon
        return rand(valid_actions)  # Explore
    else
        # Exploit - choose action with highest Q-value among valid actions
        best_action = valid_actions[1]
        best_value = agent.Q_play[key][best_action]
        
        for action in valid_actions[2:end]
            if agent.Q_play[key][action] > best_value
                best_action = action
                best_value = agent.Q_play[key][action]
            end
        end
        
        return best_action
    end
end


"""
    update_Q_value!(agent::QLearningAgent, state_key, action, reward, next_state_key, valid_next_actions)

Update Q-value based on the observed reward and next state.
"""
function update_Q_play!(agent::QLearningAgent, state_key, action, reward, next_state_key, valid_next_actions)
    # Initialize next state if needed
    if !haskey(agent.Q_play, next_state_key)
        agent.Q_play[next_state_key] = Dict{Symbol, Float64}()
        for next_action in valid_next_actions
            agent.Q_play[next_state_key][next_action] = 0.0
        end
    end
    
    # Get max Q-value for next state
    next_max_q = 0.0
    if !isempty(valid_next_actions)
        next_max_q = maximum([agent.Q_play[next_state_key][a] for a in valid_next_actions])
    end
    
    # Update Q-value
    current_q = agent.Q_play[state_key][action]
    agent.Q_play[state_key][action] = current_q + agent.alpha * (reward + agent.gamma * next_max_q - current_q)
end

"""
    update_Q_bet!(agent::QLearningAgent, state_key, bet, reward, next_state_key, affordable_bets)

Update Q-value for betting based on the observed reward and next state.
"""
function update_Q_bet!(agent::QLearningAgent, state_key, bet, reward, next_state_key, affordable_bets)
    # Initialize current state if needed
    if !haskey(agent.Q_bet, state_key)
        agent.Q_bet[state_key] = Dict{Int, Float64}()
        for bet_option in agent.betting_options
            if bet_option <= affordable_bets[end]  # Using the largest affordable bet as a proxy for chips
                agent.Q_bet[state_key][bet_option] = 0.0
            end
        end
    end
    
    # Ensure the bet is in the Q-table for this state
    if !haskey(agent.Q_bet[state_key], bet)
        agent.Q_bet[state_key][bet] = 0.0
    end
    
    # Initialize next state if needed
    if !haskey(agent.Q_bet, next_state_key)
        agent.Q_bet[next_state_key] = Dict{Int, Float64}()
        for next_bet in affordable_bets
            agent.Q_bet[next_state_key][next_bet] = 0.0
        end
    end
    
    # Get max Q-value for next state
    next_max_q = 0.0
    if !isempty(affordable_bets)
        # Ensure all affordable bets have Q-values in the next state
        for next_bet in affordable_bets
            if !haskey(agent.Q_bet[next_state_key], next_bet)
                agent.Q_bet[next_state_key][next_bet] = 0.0
            end
        end
        
        next_max_q = maximum([agent.Q_bet[next_state_key][b] for b in affordable_bets])
    end
    
    # Update Q-value
    current_q = agent.Q_bet[state_key][bet]
    agent.Q_bet[state_key][bet] = current_q + agent.alpha * (reward + agent.gamma * next_max_q - current_q)
end

"""
    play_hand!(state::BlackjackState, hand_idx::Int, agent::QLearningAgent)

Play a single hand using the agent's policy. Returns the final state and actions taken.
"""
function play_hand!(state::BlackjackState, hand_idx::Int, agent::QLearningAgent)
    actions_taken = Symbol[]
    doubled = false
    hand = state.player_hands[hand_idx]
    
    while true
        features = state_to_features(state, hand_idx)
        action = choose_action(agent, features)
        push!(actions_taken, action)
        
        if action == :hit
            push!(hand, deal_card!(state))
            if hand_value(hand) > 21
                break  # Bust
            end
        elseif action == :stand
            break
        elseif action == :double
            # Double down - double bet, take one card, and end turn
            state.current_bets[hand_idx] *= 2
            push!(hand, deal_card!(state))
            doubled = true
            break
        elseif action == :split
            # Split hand - create a new hand with the second card
            # This is a simplified implementation - a full one would recursively play both hands
            new_hand = [hand[2]]
            state.player_hands[hand_idx] = [hand[1]]  # Update the current hand
            
            # Add the new hand and corresponding bet
            push!(state.player_hands, new_hand)
            push!(state.current_bets, state.current_bets[hand_idx])
            
            # Deal one more card to each hand
            push!(state.player_hands[hand_idx], deal_card!(state))
            push!(new_hand, deal_card!(state))
            
            # Continue playing the current hand (which now only has the first card + new card)
        else
            # Unknown action - just stand
            break
        end
    end
    
    return doubled, actions_taken
end

"""
    play_game!(state::BlackjackState, agent::QLearningAgent, training::Bool=true)

Play a complete game of blackjack and update the agent's Q-values.
Returns the net change in chips.
"""
function play_game!(state::BlackjackState, agent::QLearningAgent, training::Bool=true)
    # Reset game state but keep the deck and chips
    initial_chips = state.chips
    state.player_hands = [Card[]]
    state.dealer_hand = Card[]
    state.dealer_visible = Card(0, 0)
    state.current_bets = [0]
    state.cards_seen = Card[]
    state.other_player_hands = [Card[] for _ in 1:state.num_players]
    
    # Choose bet
    features = [0, 0, 0, 0, 0, 0, state.chips, 0]  # Initial features for betting
    bet = choose_bet(agent, features)
    
    # Ensure bet is affordable
    bet = min(bet, state.chips)
    state.current_bets[1] = bet
    state.chips -= bet
    
    # Deal initial cards
    deal_initial_cards!(state)
    
    # Store initial state for Q-learning
    initial_features = state_to_features(state, 1)
    bet_state_key = get_bet_state_key(initial_features)
    
    # Track outcomes for all hands
    doubled_hands = Bool[]
    hand_outcomes = Symbol[]
    hand_rewards = Int[]  # Changed from Float64 to Int to ensure integer rewards
    
    # Play each of the player's hands
    # Note: We need to use a while loop because play_hand! might add more hands through splitting
    hand_idx = 1
    while hand_idx <= length(state.player_hands)
        doubled, actions = play_hand!(state, hand_idx, agent)
        push!(doubled_hands, doubled)
        
        # If training, save state transitions for Q-learning
        if training && !isempty(actions)
            # We'll update Q-values after seeing all outcomes
        end
        
        hand_idx += 1
    end
    
    # Play other players' hands
    play_other_players_hands!(state)
    
    # Play dealer's hand
    play_dealer_hand!(state)
    
    # Determine outcomes and update chips
    total_reward = 0
    for hand_idx in 1:length(state.player_hands)
        hand = state.player_hands[hand_idx]
        player_value = hand_value(hand)
        dealer_value = hand_value(state.dealer_hand)
        is_blackjack = (length(hand) == 2 && player_value == 21)
        
        outcome = determine_outcome(player_value, dealer_value, is_blackjack, state.dealer_hand)
        push!(hand_outcomes, outcome)
        
        # Make sure we have enough doubled_hands flags for all hands
        doubled = hand_idx <= length(doubled_hands) ? doubled_hands[hand_idx] : false
        
        reward = calculate_reward(outcome, state.current_bets[hand_idx], doubled)
        push!(hand_rewards, reward)
        
        # Since reward is now guaranteed to be an integer, this addition is safe
        state.chips += state.current_bets[hand_idx] + reward
        total_reward += reward
    end
    
    # Update Q-values for betting if training
    if training
        # Determine next state features
        next_features = [0, 0, 0, 0, 0, 0, state.chips, 0]
        next_bet_state_key = get_bet_state_key(next_features)
        
        # Make sure affordable_bets is never empty
        affordable_bets = filter(b -> b <= state.chips, agent.betting_options)
        if isempty(affordable_bets)
            affordable_bets = [agent.min_bet]  # Fallback to minimum bet
        end
        
        # Update betting Q-value
        update_Q_bet!(agent, bet_state_key, bet, total_reward, next_bet_state_key, affordable_bets)
    end
    
    return state.chips - initial_chips
end

"""
    evaluate_agent(agent::QLearningAgent, num_games::Int, num_decks::Int, initial_chips::Int, num_players::Int)

Evaluate the agent's performance over multiple games without updating its policy.
"""
function evaluate_agent(agent::QLearningAgent, num_games::Int, num_decks::Int, initial_chips::Int, num_players::Int)
    state = initialize_game(num_decks, initial_chips, num_players)
    
    # Track performance
    game_rewards = zeros(num_games)
    chip_history = zeros(num_games + 1)
    chip_history[1] = initial_chips
    
    # Disable exploration for evaluation
    original_epsilon = agent.epsilon
    agent.epsilon = 0.0
    
    for game in 1:num_games
        # Reset if almost out of chips
        if state.chips < agent.min_bet
            state.chips = initial_chips
        end
        
        # Shuffle deck if it's getting low
        if length(state.deck) < 52
            append!(state.deck, state.cards_seen)
            state.cards_seen = Card[]
            shuffle!(state.deck)
        end
        
        # Play one game
        reward = play_game!(state, agent, false)
        game_rewards[game] = reward
        chip_history[game + 1] = state.chips
    end
    
    # Restore exploration rate
    agent.epsilon = original_epsilon
    
    return game_rewards, chip_history
end

"""
    analyze_policy(agent::QLearningAgent)

Analyze and visualize the agent's policy.
Returns both plots for further handling.
"""
function analyze_policy(agent::QLearningAgent)
    # Analyze betting strategy
    println("Analyzing betting strategy...")
    
    # Create a better visualization for hard hands strategy
    # Create matrices to store average Q-values for different actions
    hard_hit_values = zeros(Float64, 18, 10)    # Player 4-21 vs Dealer 2-11
    hard_stand_values = zeros(Float64, 18, 10)
    hard_double_values = zeros(Float64, 18, 10)
    
    # Gather Q-values for hard hands (averaging over different true counts)
    for player_value in 4:21
        for dealer_value in 2:11
            hit_vals = Float64[]
            stand_vals = Float64[]
            double_vals = Float64[]
            
            for true_count in -2:2
                key = (player_value, dealer_value, false, false, true_count)
                if haskey(agent.Q_play, key)
                    qvals = agent.Q_play[key]
                    if haskey(qvals, :hit)
                        push!(hit_vals, qvals[:hit])
                    end
                    if haskey(qvals, :stand)
                        push!(stand_vals, qvals[:stand])
                    end
                    if haskey(qvals, :double)
                        push!(double_vals, qvals[:double])
                    end
                end
            end
            
            # Store average Q-values
            if !isempty(hit_vals)
                hard_hit_values[player_value-3, dealer_value-1] = mean(hit_vals)
            end
            if !isempty(stand_vals)
                hard_stand_values[player_value-3, dealer_value-1] = mean(stand_vals)
            end
            if !isempty(double_vals)
                hard_double_values[player_value-3, dealer_value-1] = mean(double_vals)
            end
        end
    end
    
    # Determine optimal action for each state
    hard_strategy = zeros(Int, 18, 10)
    for i in 1:18
        for j in 1:10
            hit_q = hard_hit_values[i, j]
            stand_q = hard_stand_values[i, j]
            double_q = hard_double_values[i, j]
            
            # Encode action as integer: 1=hit, 2=stand, 3=double
            if i == 18 && j == 10  # Special case for debugging
                println("Player $(i+3) vs Dealer $(j+1): Hit=$(hit_q), Stand=$(stand_q), Double=$(double_q)")
            end
            
            # Only consider double on first two cards
            if double_q > hit_q && double_q > stand_q && i+3 <= 11
                hard_strategy[i, j] = 3  # Double
            elseif stand_q > hit_q
                hard_strategy[i, j] = 2  # Stand
            else
                hard_strategy[i, j] = 1  # Hit
            end
        end
    end
    
    # Create an enhanced Hard Hands Strategy heatmap
    p1 = heatmap(2:11, 4:21, hard_strategy, 
                title="Hard Hands Strategy", 
                xlabel="Dealer Upcard", 
                ylabel="Player Total",
                color=[:red, :green, :blue],
                colorbar = false,
                annotations=[(j, i, text(["H", "S", "D"][hard_strategy[i-3, j-1]], 7, :white)) 
                           for i in 4:21 for j in 2:11])
    
    # Analyze betting strategy with true count
    betting_data = zeros(11, 3)  # True count -5:5 vs Chips [100,500,1000]
    chip_values = [100, 500, 1000]
    true_counts = collect(-5:5)
    
    # Determine optimal bet size for each true count and chip level
    for (i, true_count) in enumerate(true_counts)
        for (j, chips) in enumerate(chip_values)
            key = (true_count, chips)
            if haskey(agent.Q_bet, key)
                best_bet = argmax(bet -> agent.Q_bet[key][bet], keys(agent.Q_bet[key]))
                betting_data[i, j] = best_bet
                println("True count: $true_count, Chips: $chips, Best bet: $best_bet")
            else
                betting_data[i, j] = agent.min_bet
            end
        end
    end
    
    # Create the betting strategy heatmap with annotations
    p2 = heatmap(true_counts, ["100", "500", "1000"], betting_data', 
                title="Betting Strategy by True Count", 
                xlabel="True Count", 
                ylabel="Chips",
                color=:thermal,
                annotations=[(i, j, text(Int(round(betting_data[i, j])), 7, :white)) 
                           for i in 1:11 for j in 1:3])
    
    # Calculate average bet by true count (across chip levels)
    avg_bets = [mean(betting_data[i, :]) for i in 1:11]
    
    # Create a line plot of average bet vs true count
    p3 = plot(true_counts, avg_bets, 
              title="Average Bet vs True Count", 
              xlabel="True Count", 
              ylabel="Average Bet",
              legend=false, 
              marker=:circle,
              line=:solid)
    
    return p1, p2, p3
end

# Main function to run the simulation
"""
    run_blackjack_simulation(; save_plots=false, plots_dir="plots")

Run the blackjack simulation, including training, evaluation, and visualization.
Returns the trained agent and all generated plots.

Parameters:
- save_plots: Boolean indicating whether to save plots to disk
- plots_dir: Directory where plots should be saved (will be created if it doesn't exist)

Returns:
- agent: The trained QLearningAgent
- plots: A dictionary containing all generated plots
"""
function run_blackjack_simulation(; save_plots=false, plots_dir="plots", use_basic_strategy=true)
    # Parameters
    num_episodes = 1_000_000  # Training episodes
    num_decks = 8
    initial_chips = 1000
    num_players = 6
    
    # Train the agent
    println("Training agent with basic strategy initialization: $(use_basic_strategy)")
    agent = create_agent(5, 100, 5, use_basic_strategy=use_basic_strategy)
    state = initialize_game(num_decks, initial_chips, num_players)
    
    # Track performance
    episode_rewards = zeros(num_episodes)
    chip_history = zeros(num_episodes + 1)
    chip_history[1] = initial_chips
    
    # Progress meter
    p = Progress(num_episodes, dt=1.0, desc="Training agent: ", barglyphs=BarGlyphs("[=> ]"))
    
    for episode in 1:num_episodes
        # Reset if almost out of chips
        if state.chips < agent.min_bet
            state.chips = initial_chips
        end
        
        # Shuffle deck if it's getting low
        if length(state.deck) < 52
            # Put seen cards back in deck and shuffle
            append!(state.deck, state.cards_seen)
            state.cards_seen = Card[]
            shuffle!(state.deck)
        end
        
        # Play one game
        reward = play_game!(state, agent, true)
        episode_rewards[episode] = reward
        chip_history[episode + 1] = state.chips
        
        # Decay exploration rate
        if episode % 1000 == 0
            agent.epsilon = max(0.01, agent.epsilon * 0.95)
        end
        
        next!(p)
    end
    
    # Evaluate the agent
    println("Evaluating agent...")
    eval_rewards, eval_chip_history = evaluate_agent(agent, 1000, num_decks, initial_chips, num_players)
    
    # Create plots
    plots = Dict()
    
    # Enhanced training reward plot with smoother moving average
    window_size = 1000
    ma_rewards = moving_average(episode_rewards, window_size)
    plot_interval = max(1, num_episodes ÷ 1000)
    
    plots[:training_rewards] = plot(1:plot_interval:num_episodes, ma_rewards[1:plot_interval:num_episodes], 
        title="Training Reward Moving Average (window=$window_size)", 
        xlabel="Episode", 
        ylabel="Average Reward",
        legend=false,
        linewidth=2)
    
    # Chip history with clearer visualization
    plots[:chip_history] = plot(0:plot_interval:num_episodes, chip_history[1:plot_interval:(num_episodes+1)], 
        title="Chip History During Training", 
        xlabel="Episode", 
        ylabel="Chips",
        legend=false,
        linewidth=2,
        ylims=(0, max(2000, maximum(chip_history))))
    
    # Cumulative reward during evaluation
    plots[:cumulative_rewards] = plot(1:1000, cumsum(eval_rewards), 
        title="Cumulative Reward During Evaluation", 
        xlabel="Game", 
        ylabel="Cumulative Reward",
        legend=false,
        linewidth=2)
    
    # Add win rate per 100 games
    win_rates = [count(eval_rewards[i:min(i+99, length(eval_rewards))] .> 0) / 
                min(100, length(eval_rewards) - i + 1) for i in 1:100:length(eval_rewards)]
    plots[:win_rates] = plot(1:100:1000, win_rates, 
        title="Win Rate During Evaluation (per 100 games)", 
        xlabel="Game", 
        ylabel="Win Rate",
        legend=false,
        linewidth=2,
        marker=:circle)
    
    # Combined training plots in a 2x2 layout
    plots[:training_combined] = plot(
        plots[:training_rewards], 
        plots[:chip_history], 
        plots[:cumulative_rewards], 
        plots[:win_rates],
        layout=(2,2), 
        size=(1000,800)
    )
    
    # Analyze policy
    hard_strategy_plot, betting_strategy_plot, avg_bet_plot = analyze_policy(agent)
    plots[:hard_strategy] = hard_strategy_plot
    plots[:betting_strategy] = betting_strategy_plot
    plots[:avg_bet_plot] = avg_bet_plot
    
    # Combined policy plots in a better layout
    plots[:policy_combined] = plot(
        hard_strategy_plot, 
        plot(betting_strategy_plot, avg_bet_plot, layout=(2,1)),
        layout=(1,2), 
        size=(1000,600)
    )
    
    # Display plots
    display(plots[:training_combined])
    display(plots[:policy_combined])
    
    # Print final statistics
    println("\nFinal Statistics:")
    println("Average reward per game during evaluation: ", round(mean(eval_rewards), digits=2))
    println("Total profit after 1000 evaluation games: ", round(sum(eval_rewards), digits=2))
    println("Win rate: ", round(count(r -> r > 0, eval_rewards) / 1000 * 100, digits=2), "%")
    println("Win/Loss/Push: $(count(r -> r > 0, eval_rewards))/$(count(r -> r < 0, eval_rewards))/$(count(r -> r == 0, eval_rewards))")
    
    # Save plots if requested
    if save_plots
        # Ensure directory exists
        mkpath(plots_dir)
        
        # Save individual plots
        savefig(plots[:training_combined], joinpath(plots_dir, "training_combined.png"))
        savefig(plots[:policy_combined], joinpath(plots_dir, "policy_combined.png"))
        savefig(plots[:hard_strategy], joinpath(plots_dir, "hard_strategy.png"))
        savefig(plots[:betting_strategy], joinpath(plots_dir, "betting_strategy.png"))
        
        println("Plots saved to directory: $plots_dir")
    end
    
    return agent, plots
end

"""
    moving_average(A, n)

Calculate the moving average of array A with window size n.
"""
function moving_average(A, n)
    result = similar(A)
    for i in 1:length(A)
        result[i] = mean(A[max(1, i-n+1):i])
    end
    return result
end

"""
    simulate_different_parameters(; save_plots=false, plots_dir="plots", 
                                  deck_counts=[1, 2, 6, 8], 
                                  player_counts=[0, 1, 3, 6])

Run simulations with different parameters and compare results.
This version ensures consistent matrix dimensions for visualization.
"""
function simulate_different_parameters(; save_plots=false, plots_dir="plots", 
                                        deck_counts=[1, 2, 6, 8], 
                                        player_counts=[0, 1, 3, 6])
    # Results storage
    results = Dict()
    plots = Dict()
    
    # Track win rates and profits for heatmaps
    win_rates = zeros(length(deck_counts), length(player_counts))
    profits = zeros(length(deck_counts), length(player_counts))
    
    # Run simulations for each combination
    for (d_idx, decks) in enumerate(deck_counts)
        for (p_idx, players) in enumerate(player_counts)
            key = (decks, players)
            
            # Train a smaller agent for quicker comparison
            agent = create_agent(5, 100, 5)
            state = initialize_game(decks, 1000, players)
            
            # Quick training
            for episode in 1:10000
                if state.chips < agent.min_bet
                    state.chips = 1000
                end
                if length(state.deck) < 52
                    append!(state.deck, state.cards_seen)
                    state.cards_seen = Card[]
                    shuffle!(state.deck)
                end
                play_game!(state, agent, true)
            end
            
            # Evaluate
            eval_rewards, _ = evaluate_agent(agent, 1000, decks, 1000, players)
            
            # Store results
            win_rate = count(r -> r > 0, eval_rewards) / 1000
            total_profit = sum(eval_rewards)
            
            results[key] = (
                mean_reward = mean(eval_rewards),
                total_profit = total_profit,
                win_rate = win_rate
            )
            
            # Store for heatmap
            win_rates[d_idx, p_idx] = win_rate
            profits[d_idx, p_idx] = total_profit
            
            println("Decks: $decks, Players: $players, Win Rate: $(win_rate), Profit: $(total_profit)")
        end
    end
    
    # Create string labels for plotting
    deck_labels = ["$d Deck" * (d > 1 ? "s" : "") for d in deck_counts]
    player_labels = ["$p" for p in player_counts]
    
    # Win rate heatmap with consistent dimensions
    plots[:win_rates] = heatmap(
        deck_labels, player_labels, win_rates',
        title="Win Rate by Deck Count and Number of Players",
        xlabel="Number of Decks",
        ylabel="Number of Other Players",
        color=:thermal,
        clim=(0.3, 0.5),  # Set consistent color limits
        annotations=[(i, j, text(round(win_rates[i,j], digits=2), 8, :white)) 
                   for i in 1:length(deck_counts), j in 1:length(player_counts)]
    )
    
    # Profit heatmap with consistent dimensions
    plots[:profits] = heatmap(
        deck_labels, player_labels, profits',
        title="Total Profit by Deck Count and Number of Players",
        xlabel="Number of Decks",
        ylabel="Number of Other Players",
        color=:thermal,
        annotations=[(i, j, text(Int(round(profits[i,j])), 8, :white)) 
                   for i in 1:length(deck_counts), j in 1:length(player_counts)]
    )
    
    # Combined parameters plot
    plots[:parameters_combined] = plot(
        plots[:win_rates], 
        plots[:profits], 
        layout=(1,2), 
        size=(900,400)
    )
    
    # Display plots
    display(plots[:parameters_combined])
    
    # Save plots if requested
    if save_plots
        # Ensure directory exists
        mkpath(plots_dir)
        
        # Save plots
        savefig(plots[:parameters_combined], joinpath(plots_dir, "parameters_combined.png"))
        savefig(plots[:win_rates], joinpath(plots_dir, "win_rates.png"))
        savefig(plots[:profits], joinpath(plots_dir, "profits.png"))
        
        println("Parameter plots saved to directory: $plots_dir")
    end
    
    return results, plots
end
"""
    initialize_basic_strategy(agent::QLearningAgent)

Initialize the agent with a basic blackjack strategy.

Parameters:
- agent: The QLearningAgent to initialize

Returns:
- agent: The initialized QLearningAgent
"""
function initialize_basic_strategy(agent::QLearningAgent)
    println("Initializing agent with basic blackjack strategy...")
    
    # Hard totals (non-soft, non-pair hands)
    for player_value in 4:21
        for dealer_value in 2:11
            for true_count in -5:5
                key = (player_value, dealer_value, false, false, true_count)
                agent.Q_play[key] = Dict{Symbol, Float64}()
                
                # Basic strategy for hard hands
                if player_value >= 17
                    # Always stand on hard 17 or higher
                    agent.Q_play[key][:stand] = 1.0
                    agent.Q_play[key][:hit] = 0.0
                    agent.Q_play[key][:double] = -1.0
                    agent.Q_play[key][:split] = -1.0
                elseif player_value >= 13 && dealer_value <= 6
                    # Stand on 13-16 vs dealer 2-6
                    agent.Q_play[key][:stand] = 1.0
                    agent.Q_play[key][:hit] = 0.0
                    agent.Q_play[key][:double] = -1.0
                    agent.Q_play[key][:split] = -1.0
                elseif player_value >= 12 && dealer_value <= 3
                    # Stand on 12 vs dealer 2-3
                    agent.Q_play[key][:stand] = 0.5
                    agent.Q_play[key][:hit] = 0.0
                    agent.Q_play[key][:double] = -1.0
                    agent.Q_play[key][:split] = -1.0
                elseif player_value == 11
                    # Double on 11 vs any dealer
                    agent.Q_play[key][:double] = 1.0
                    agent.Q_play[key][:hit] = 0.5
                    agent.Q_play[key][:stand] = -0.5
                    agent.Q_play[key][:split] = -1.0
                elseif player_value == 10 && dealer_value <= 9
                    # Double on 10 vs dealer 2-9
                    agent.Q_play[key][:double] = 1.0
                    agent.Q_play[key][:hit] = 0.5
                    agent.Q_play[key][:stand] = -0.5
                    agent.Q_play[key][:split] = -1.0
                elseif player_value == 9 && dealer_value >= 3 && dealer_value <= 6
                    # Double on 9 vs dealer 3-6
                    agent.Q_play[key][:double] = 0.5
                    agent.Q_play[key][:hit] = 0.0
                    agent.Q_play[key][:stand] = -0.5
                    agent.Q_play[key][:split] = -1.0
                else
                    # Otherwise hit
                    agent.Q_play[key][:hit] = 0.5
                    agent.Q_play[key][:stand] = -0.5
                    agent.Q_play[key][:double] = -1.0
                    agent.Q_play[key][:split] = -1.0
                end
            end
        end
    end
    
    # Soft hands (includes an Ace counted as 11)
    for player_value in 12:21
        for dealer_value in 2:11
            for true_count in -5:5
                key = (player_value, dealer_value, true, false, true_count)
                agent.Q_play[key] = Dict{Symbol, Float64}()
                
                # Basic strategy for soft hands
                if player_value >= 19
                    # Always stand on soft 19+
                    agent.Q_play[key][:stand] = 1.0
                    agent.Q_play[key][:hit] = -0.5
                    agent.Q_play[key][:double] = -1.0
                    agent.Q_play[key][:split] = -1.0
                elseif player_value == 18 && dealer_value >= 2 && dealer_value <= 8
                    # Stand on soft 18 vs dealer 2-8
                    agent.Q_play[key][:stand] = 0.5
                    agent.Q_play[key][:hit] = -0.2
                    agent.Q_play[key][:double] = -0.5
                    agent.Q_play[key][:split] = -1.0
                elseif player_value == 18
                    # Hit on soft 18 vs dealer 9-A
                    agent.Q_play[key][:hit] = 0.5
                    agent.Q_play[key][:stand] = 0.0
                    agent.Q_play[key][:double] = -0.5
                    agent.Q_play[key][:split] = -1.0
                elseif player_value == 17 && dealer_value >= 3 && dealer_value <= 6
                    # Double on soft 17 vs dealer 3-6, otherwise hit
                    agent.Q_play[key][:double] = 0.5
                    agent.Q_play[key][:hit] = 0.3
                    agent.Q_play[key][:stand] = -0.5
                    agent.Q_play[key][:split] = -1.0
                elseif player_value >= 15 && dealer_value >= 4 && dealer_value <= 6
                    # Double on soft 15-16 vs dealer 4-6, otherwise hit
                    agent.Q_play[key][:double] = 0.5
                    agent.Q_play[key][:hit] = 0.3
                    agent.Q_play[key][:stand] = -0.5
                    agent.Q_play[key][:split] = -1.0
                elseif player_value >= 13 && dealer_value >= 5 && dealer_value <= 6
                    # Double on soft 13-14 vs dealer 5-6, otherwise hit
                    agent.Q_play[key][:double] = 0.5
                    agent.Q_play[key][:hit] = 0.3
                    agent.Q_play[key][:stand] = -0.5
                    agent.Q_play[key][:split] = -1.0
                else
                    # Otherwise hit
                    agent.Q_play[key][:hit] = 0.5
                    agent.Q_play[key][:stand] = -0.5
                    agent.Q_play[key][:double] = -0.3
                    agent.Q_play[key][:split] = -1.0
                end
            end
        end
    end
    
    # Pairs
    for card_value in 1:10
        player_value = card_value == 1 ? 12 : card_value * 2  # Ace pair is 12, others are double
        for dealer_value in 2:11
            for true_count in -5:5
                key = (player_value, dealer_value, card_value == 1, true, true_count)
                agent.Q_play[key] = Dict{Symbol, Float64}()
                
                # Basic strategy for pairs
                if card_value == 1 || card_value == 8
                    # Always split As and 8s
                    agent.Q_play[key][:split] = 1.0
                    agent.Q_play[key][:hit] = -0.5
                    agent.Q_play[key][:stand] = -1.0
                    agent.Q_play[key][:double] = -1.0
                elseif card_value == 10
                    # Never split 10s
                    agent.Q_play[key][:stand] = 1.0
                    agent.Q_play[key][:hit] = -0.5
                    agent.Q_play[key][:double] = -0.5
                    agent.Q_play[key][:split] = -1.0
                elseif card_value == 9 && dealer_value != 7 && dealer_value != 10 && dealer_value != 11
                    # Split 9s except vs 7, 10, A
                    agent.Q_play[key][:split] = 0.5
                    agent.Q_play[key][:stand] = 0.0
                    agent.Q_play[key][:hit] = -0.5
                    agent.Q_play[key][:double] = -1.0
                elseif card_value == 7 && dealer_value <= 7
                    # Split 7s vs 2-7
                    agent.Q_play[key][:split] = 0.5
                    agent.Q_play[key][:hit] = 0.0
                    agent.Q_play[key][:stand] = -0.5
                    agent.Q_play[key][:double] = -1.0
                elseif card_value == 6 && dealer_value <= 6
                    # Split 6s vs 2-6
                    agent.Q_play[key][:split] = 0.5
                    agent.Q_play[key][:hit] = 0.0
                    agent.Q_play[key][:stand] = -0.5
                    agent.Q_play[key][:double] = -1.0
                elseif card_value == 4 && (dealer_value == 5 || dealer_value == 6)
                    # Split 4s vs 5-6
                    agent.Q_play[key][:split] = 0.3
                    agent.Q_play[key][:hit] = 0.0
                    agent.Q_play[key][:stand] = -0.5
                    agent.Q_play[key][:double] = -1.0
                elseif card_value == 3 && dealer_value <= 7
                    # Split 3s vs 2-7
                    agent.Q_play[key][:split] = 0.3
                    agent.Q_play[key][:hit] = 0.0
                    agent.Q_play[key][:stand] = -0.5
                    agent.Q_play[key][:double] = -1.0
                elseif card_value == 2 && dealer_value <= 7
                    # Split 2s vs 2-7
                    agent.Q_play[key][:split] = 0.3
                    agent.Q_play[key][:hit] = 0.0
                    agent.Q_play[key][:stand] = -0.5
                    agent.Q_play[key][:double] = -1.0
                else
                    # Otherwise follow normal hard total strategy
                    if player_value >= 17
                        agent.Q_play[key][:stand] = 0.5
                        agent.Q_play[key][:hit] = -0.3
                        agent.Q_play[key][:split] = -0.5
                        agent.Q_play[key][:double] = -1.0
                    else
                        agent.Q_play[key][:hit] = 0.5
                        agent.Q_play[key][:stand] = -0.3
                        agent.Q_play[key][:split] = -0.5
                        agent.Q_play[key][:double] = -1.0
                    end
                end
            end
        end
    end
    
    # Initialize betting strategy based on true count
    for true_count in -5:5
        for chips in [100, 500, 1000]
            key = (true_count, chips)
            agent.Q_bet[key] = Dict{Int, Float64}()
            
            for bet in agent.betting_options
                if bet <= chips
                    # Card counting betting strategy
                    if true_count >= 3
                        # Highly favorable - bet more
                        value = 0.1 * (bet / agent.min_bet)
                    elseif true_count >= 1
                        # Slightly favorable - bet moderately
                        value = 0.05 * (bet / agent.min_bet)
                    elseif true_count <= -2
                        # Unfavorable - bet minimum
                        value = -0.1 * (bet / agent.min_bet)
                    else
                        # Neutral - bet conservatively
                        value = 0.0
                    end
                    agent.Q_bet[key][bet] = value
                end
            end
        end
    end
    
    println("Basic strategy initialization complete")
end

"""
    analyze_parameter_results(results, deck_counts, player_counts)

Analyze and summarize the results of parameter simulation.
"""
function analyze_parameter_results(results, deck_counts, player_counts)
    # Initialize summary
    summary = Dict()
    
    # Find best win rate and profit configurations
    best_win_rate = 0.0
    best_win_rate_config = nothing
    
    best_profit = -Inf
    best_profit_config = nothing
    
    worst_profit = Inf
    worst_profit_config = nothing
    
    # Analyze by deck count
    deck_win_rates = Dict()
    deck_profits = Dict()
    
    for decks in deck_counts
        win_rates = [results[(decks, players)].win_rate for players in player_counts]
        profits = [results[(decks, players)].total_profit for players in player_counts]
        
        deck_win_rates[decks] = mean(win_rates)
        deck_profits[decks] = mean(profits)
        
        for players in player_counts
            result = results[(decks, players)]
            
            if result.win_rate > best_win_rate
                best_win_rate = result.win_rate
                best_win_rate_config = (decks, players)
            end
            
            if result.total_profit > best_profit
                best_profit = result.total_profit
                best_profit_config = (decks, players)
            end
            
            if result.total_profit < worst_profit
                worst_profit = result.total_profit
                worst_profit_config = (decks, players)
            end
        end
    end
    
    # Analyze by player count
    player_win_rates = Dict()
    player_profits = Dict()
    
    for players in player_counts
        win_rates = [results[(decks, players)].win_rate for decks in deck_counts]
        profits = [results[(decks, players)].total_profit for decks in deck_counts]
        
        player_win_rates[players] = mean(win_rates)
        player_profits[players] = mean(profits)
    end
    
    # Overall statistics
    all_win_rates = [results[key].win_rate for key in keys(results)]
    all_profits = [results[key].total_profit for key in keys(results)]
    
    summary[:best_win_rate] = (config = best_win_rate_config, value = best_win_rate)
    summary[:best_profit] = (config = best_profit_config, value = best_profit)
    summary[:worst_profit] = (config = worst_profit_config, value = worst_profit)
    summary[:average_win_rate] = mean(all_win_rates)
    summary[:average_profit] = mean(all_profits)
    summary[:deck_win_rates] = deck_win_rates
    summary[:deck_profits] = deck_profits
    summary[:player_win_rates] = player_win_rates
    summary[:player_profits] = player_profits
    
    return summary
end

"""
    print_parameter_summary(summary)

Print a readable summary of parameter analysis results.
"""
function print_parameter_summary(summary)
    println("\n=== BLACKJACK PARAMETER ANALYSIS SUMMARY ===")
    
    println("\nBest Configurations:")
    
    best_wr_decks, best_wr_players = summary[:best_win_rate].config
    println("  Highest Win Rate: $(round(summary[:best_win_rate].value * 100, digits=1))% with $best_wr_decks deck(s) and $best_wr_players player(s)")
    
    best_profit_decks, best_profit_players = summary[:best_profit].config
    println("  Highest Profit: $(Int(round(summary[:best_profit].value))) with $best_profit_decks deck(s) and $best_profit_players player(s)")
    
    worst_profit_decks, worst_profit_players = summary[:worst_profit].config
    println("  Lowest Profit: $(Int(round(summary[:worst_profit].value))) with $worst_profit_decks deck(s) and $worst_profit_players player(s)")
    
    println("\nWin Rate by Deck Count:")
    for (decks, win_rate) in sort(collect(summary[:deck_win_rates]))
        println("  $decks deck(s): $(round(win_rate * 100, digits=1))%")
    end
    
    println("\nProfit by Deck Count:")
    for (decks, profit) in sort(collect(summary[:deck_profits]))
        println("  $decks deck(s): $(Int(round(profit)))")
    end
    
    println("\nWin Rate by Player Count:")
    for (players, win_rate) in sort(collect(summary[:player_win_rates]))
        println("  $players player(s): $(round(win_rate * 100, digits=1))%")
    end
    
    println("\nProfit by Player Count:")
    for (players, profit) in sort(collect(summary[:player_profits]))
        println("  $players player(s): $(Int(round(profit)))")
    end
    
    println("\nOverall Statistics:")
    println("  Average Win Rate: $(round(summary[:average_win_rate] * 100, digits=1))%")
    println("  Average Profit: $(Int(round(summary[:average_profit])))")
    
    println("\nRecommendation:")
    if summary[:best_win_rate].config == summary[:best_profit].config
        println("  Best overall configuration: $(summary[:best_profit].config[1]) deck(s) with $(summary[:best_profit].config[2]) player(s)")
    else
        println("  For highest win rate: $(summary[:best_win_rate].config[1]) deck(s) with $(summary[:best_win_rate].config[2]) player(s)")
        println("  For highest profit: $(summary[:best_profit].config[1]) deck(s) with $(summary[:best_profit].config[2]) player(s)")
    end
end

"""
    run_focused_parameter_analysis(; save_plots=true)

Run a focused parameter simulation with analysis and visualization.
"""
function run_focused_parameter_analysis(; save_plots=true, plots_dir="plots")
    # Use a focused set of parameters
    deck_counts = [1, 2, 4, 6, 8]
    player_counts = [0, 1, 3, 6]
    
    # Run simulation
    println("Running parameter simulation...")
    results, plots = simulate_different_parameters(
        save_plots=save_plots,
        plots_dir=plots_dir,
        deck_counts=deck_counts,
        player_counts=player_counts
    )
    
    # Analyze results
    println("Analyzing results...")
    summary = analyze_parameter_results(results, deck_counts, player_counts)
    
    # Print summary
    print_parameter_summary(summary)
    
    return results, plots, summary
end


# Uncomment the following lines to run the simulation

# Run standard simulation
agent, plots1 = run_blackjack_simulation(save_plots=true, plots_dir="cornell_theory_reading_group_RL/chapter05/")

# Run focused parameter analysis for clearer visualization and insights
results, plots2, summary = run_focused_parameter_analysis(save_plots=true, plots_dir="cornell_theory_reading_group_RL/chapter05/")
