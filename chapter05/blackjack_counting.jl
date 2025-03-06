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

# NOTE IS BROKEN, WILL FIX WHEN I GET A CHANCE - GABE
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
    
    # Card counting features
    seen_cards = state.cards_seen
    total_cards = length(state.deck) + length(seen_cards)
    
    # Count high and low cards seen
    high_cards_seen = count(c -> c.rank >= 10, seen_cards)
    low_cards_seen = count(c -> c.rank <= 6, seen_cards)
    
    # Calculate running count and true count
    running_count = low_cards_seen - high_cards_seen
    decks_remaining = (length(state.deck) / 52)
    true_count = decks_remaining > 0 ? running_count / decks_remaining : 0
    
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

"""
    create_agent(min_bet::Int, max_bet::Int, betting_increments::Int)

Create a new QLearningAgent with initialized Q-values.
"""
function create_agent(min_bet::Int, max_bet::Int, betting_increments::Int)
    # Initialize empty Q-value dictionaries
    Q_play = Dict{Tuple, Dict{Symbol, Float64}}()
    Q_bet = Dict{Tuple{Int, Int}, Dict{Int, Float64}}()
    
    # Define betting options
    betting_options = collect(min_bet:betting_increments:max_bet)
    
    return QLearningAgent(
        Q_play,
        Q_bet,
        0.1,    # alpha
        0.9,    # gamma
        0.1,    # epsilon
        min_bet,
        max_bet,
        betting_options
    )
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
    choose_bet(agent::QLearningAgent, features)

Choose a bet amount based on the current state.
"""
function choose_bet(agent::QLearningAgent, features)
    key = get_bet_state_key(features)
    chips = features[7]
    
    # Initialize Q-values if not seen this state before
    if !haskey(agent.Q_bet, key)
        agent.Q_bet[key] = Dict{Int, Float64}()
        for bet in agent.betting_options
            if bet <= chips  # Can't bet more than we have
                agent.Q_bet[key][bet] = 0.0
            end
        end
    end
    
    # Filter bets that we can afford
    affordable_bets = filter(bet -> bet <= chips, collect(keys(agent.Q_bet[key])))
    
    if isempty(affordable_bets)
        return agent.min_bet  # Default to minimum bet if nothing is affordable
    end
    
    # Epsilon-greedy selection
    if rand() < agent.epsilon
        return rand(affordable_bets)  # Explore
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
    for true_count in -5:5
        for chips in [100, 500, 1000]
            key = (true_count, chips)
            if haskey(agent.Q_bet, key)
                best_bet = argmax(bet -> agent.Q_bet[key][bet], keys(agent.Q_bet[key]))
                println("True count: $true_count, Chips: $chips, Best bet: $best_bet")
            end
        end
    end
    
    # Analyze playing strategy for hard hands
    hard_strategy = zeros(Int, 18, 10)  # Player's 4-21 vs Dealer's 2-11 (Ace)
    
    for player_value in 4:21
        for dealer_value in 2:11
            key = (player_value, dealer_value, false, false, 0)  # Hard hand, not pair, neutral count
            if haskey(agent.Q_play, key)
                action = argmax(a -> agent.Q_play[key][a], keys(agent.Q_play[key]))
                # Encode action as integer: 1=hit, 2=stand, 3=double, 4=split
                action_code = action == :hit ? 1 : action == :stand ? 2 : action == :double ? 3 : 4
                
                # Adjust index to fit within array bounds
                if player_value >= 4 && player_value <= 21
                    hard_strategy[player_value-3, dealer_value-1] = action_code
                end
            end
        end
    end
    
    # Visualize hard hands strategy
    p1 = heatmap(2:11, 4:21, hard_strategy, 
                title="Hard Hands Strategy", 
                xlabel="Dealer Upcard", 
                ylabel="Player Total",
                color=[:red, :green, :blue, :purple],
                colorbar_ticks=([1, 2, 3, 4], ["Hit", "Stand", "Double", "Split"]))
    
    # Visualize betting strategy
    # Create a properly sized matrix for the betting data
    betting_data = zeros(11, 3)  # True count -5:5 vs Chips [100,500,1000]
    chip_values = [100, 500, 1000]
    true_counts = collect(-5:5)
    
    for (i, true_count) in enumerate(true_counts)
        for (j, chips) in enumerate(chip_values)
            key = (true_count, chips)
            if haskey(agent.Q_bet, key)
                best_bet = argmax(bet -> agent.Q_bet[key][bet], keys(agent.Q_bet[key]))
                betting_data[i, j] = best_bet
            end
        end
    end
    
    # Create the betting strategy heatmap with correct dimensions
    p2 = heatmap(true_counts, ["100", "500", "1000"], betting_data', 
                title="Betting Strategy", 
                xlabel="True Count", 
                ylabel="Chips",
                color=:thermal)
    
    return p1, p2
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
function run_blackjack_simulation(; save_plots=false, plots_dir="plots")
    # Parameters
    num_episodes = 100000  # Training episodes
    num_decks = 6
    initial_chips = 1000
    num_players = 3
    
    # Train the agent
    println("Training agent...")
    agent, episode_rewards, chip_history = train_agent(num_episodes, num_decks, initial_chips, num_players)
    
    # Evaluate the agent
    println("Evaluating agent...")
    eval_rewards, eval_chip_history = evaluate_agent(agent, 1000, num_decks, initial_chips, num_players)
    
    # Create plots
    plots = Dict()
    
    # Plot training results
    plots[:training_rewards] = plot(1:100:num_episodes, moving_average(episode_rewards, 100)[1:100:num_episodes], 
        title="Training Reward Moving Average", 
        xlabel="Episode", 
        ylabel="Average Reward",
        legend=false)
    
    plots[:chip_history] = plot(0:num_episodes, chip_history, 
        title="Chip History During Training", 
        xlabel="Episode", 
        ylabel="Chips",
        legend=false)
    
    plots[:cumulative_rewards] = plot(1:1000, cumsum(eval_rewards), 
        title="Cumulative Reward During Evaluation", 
        xlabel="Game", 
        ylabel="Cumulative Reward",
        legend=false)
    
    # Combined training plots
    plots[:training_combined] = plot(
        plots[:training_rewards], 
        plots[:chip_history], 
        plots[:cumulative_rewards], 
        layout=(3,1), 
        size=(800,600)
    )
    
    # Analyze policy
    hard_strategy_plot, betting_strategy_plot = analyze_policy(agent)
    plots[:hard_strategy] = hard_strategy_plot
    plots[:betting_strategy] = betting_strategy_plot
    
    # Combined policy plots
    plots[:policy_combined] = plot(
        hard_strategy_plot, 
        betting_strategy_plot, 
        layout=(1,2), 
        size=(900,400)
    )
    
    # Display plots
    display(plots[:training_combined])
    display(plots[:policy_combined])
    
    # Print final statistics
    println("Average reward per game during evaluation: ", mean(eval_rewards))
    println("Profit after 1000 evaluation games: ", sum(eval_rewards))
    println("Win rate: ", count(r -> r > 0, eval_rewards) / 1000)
    
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
    simulate_different_parameters(; save_plots=false, plots_dir="plots")

Run simulations with different parameters and compare results.
Returns the results dictionary and generated plots.

Parameters:
- save_plots: Boolean indicating whether to save plots to disk
- plots_dir: Directory where plots should be saved (will be created if it doesn't exist)

Returns:
- results: Dictionary containing simulation results for different parameters
- plots: Dictionary containing generated plots
"""
function simulate_different_parameters(; save_plots=false, plots_dir="plots")
    # Parameters to vary
    deck_counts = [1, 2, 6, 8]
    player_counts = [0, 1, 3, 6]
    
    # Results storage
    results = Dict()
    plots = Dict()
    
    # Track win rates for heatmap
    win_rates = zeros(length(deck_counts), length(player_counts))
    profits = zeros(length(deck_counts), length(player_counts))
    
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
    
    # Plot comparison
    deck_labels = ["1 Deck", "2 Decks", "6 Decks", "8 Decks"]
    player_labels = ["0", "1", "3", "6"]
    
    # Win rate heatmap
    plots[:win_rates] = heatmap(
        deck_labels, player_labels, win_rates',
        title="Win Rate by Deck Count and Number of Players",
        xlabel="Number of Decks",
        ylabel="Number of Other Players",
        color=:thermal,
        annotations=[(i, j, text(round(win_rates[i,j], digits=2), 8, :white)) 
                   for i in 1:length(deck_counts), j in 1:length(player_counts)]
    )
    
    # Profit heatmap
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

# Uncomment the following lines to run the simulation
#=
agent, plots1 = run_blackjack_simulation(save_plots=true, plots_dir="cornell_theory_reading_group_RL/chapter05/")
results, plots2 = simulate_different_parameters(save_plots=true, plots_dir="cornell_theory_reading_group_RL/chapter05/")
=# 