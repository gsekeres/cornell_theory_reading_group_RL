# Define structures for the game:

# Game state
mutable struct State
    # Board information
    data::Matrix{Int}
    # Unique hash of the state
    hash::Union{Int,Nothing}
    # Winner of the game
    winner::Union{Int,Nothing}
    # Whether the game is over
    is_end::Union{Bool,Nothing}
    # Best move(s) for the state
    best_move::Union{Tuple{Int,Int},Nothing}
    # Best score for the state
    best_score::Union{Float64,Nothing}

    function State()
        new(zeros(Int, BOARD_ROWS, BOARD_COLS), nothing, nothing, nothing, nothing, nothing)
    end
end

# Player types:
abstract type Player end

mutable struct RLPlayer <: Player
    # Initial estimations
    estimations::Dict{Int,Float64}
    # Step size for the RLPlayer
    step_size::Float64
    # Epsilon for the RLPlayer (for random moves)
    epsilon::Float64
    # States attained
    states::Vector{State}
    # Greedy in the state?
    greedy::Vector{Bool}
    # Symbol
    symbol::Int
    # Type of the RLPlayer (rlBase, rlNoTies, rlMinimax)
    type::String

    function RLPlayer(;estimations::Dict{Int,Float64}=Dict{Int,Float64}(), symbol::Int, step_size::Float64, epsilon::Float64, type::String="rlBase")
        new(estimations, step_size, epsilon, [], [], symbol, type)
    end
end

mutable struct HumanPlayer <: Player
    # Humans just need symbols and states
    symbol::Int
    states::Vector{State}
    greedy::Vector{Bool}
    function HumanPlayer(; symbol::Int)
        new(symbol, [], [])
    end
end

# Set judger who coordinates the game
mutable struct Judger
    p1::Player
    p2::Player
    current_player::Union{Player,Nothing}
    p1_symbol::Int
    p2_symbol::Int
    current_state::State

    function Judger(player1::Player, player2::Player)
        new(player1, player2, nothing, player1.symbol, player2.symbol, State())
    end
end







# State functions:
"""
    hash_state(state::State) -> Int

Generate a unique hash for the state.
"""
function hash_state(state::State)
    if isnothing(state.hash)
        state.hash = 0
        for i in state.data
            state.hash = state.hash * 3 + i + 1
        end
    end
    return state.hash
end

"""
    check_end(state::State) -> Bool

Check if the game is over, and set the winner if so.
"""
function check_end(state::State)
    # if is_end already known, just return
    if !isnothing(state.is_end)
        return state.is_end
    end

    # gather row sums
    results = []
    for i in 1:BOARD_ROWS
        push!(results, sum(state.data[i, :]))
    end
    # gather column sums
    for j in 1:BOARD_COLS
        push!(results, sum(state.data[:, j]))
    end
    # gather diag sums
    diag1 = 0
    diag2 = 0
    for i in 1:BOARD_ROWS
        diag1 += state.data[i, i]
        diag2 += state.data[i, BOARD_ROWS - i + 1]
    end
    push!(results, diag1)
    push!(results, diag2)

    # check winners
    for r in results
        if r == 3
            state.winner = 1
            state.is_end = true
            return true
        elseif r == -3
            state.winner = -1
            state.is_end = true
            return true
        end
    end

    # check tie
    if sum(abs.(state.data)) == BOARD_SIZE
        state.winner = 0
        state.is_end = true
        return true
    end

    # otherwise
    state.is_end = false
    return false
end

"""
    next_state(state::State, i::Int, j::Int, symbol::Int) -> State

Generate a new state with a move made.
"""
function next_state(state::State, i::Int, j::Int, symbol::Int)
    new_state = State()
    new_state.data = copy(state.data)
    new_state.data[i, j] = symbol
    return new_state
end

"""
    print_board_state(state::State)

Print the board state.
"""
function print_board_state(state::State)
    for i in 1:BOARD_ROWS
        println("-------------")
        out = "| "
        for j in 1:BOARD_COLS
            if state.data[i, j] == 1
                token = "x"
            elseif state.data[i, j] == -1
                token = "o"
            else
                token = " "
            end
            out *= token * " | "
        end
        println(out)
    end
    println("-------------")
end

"""
    build_all_states() -> Dict{Int, Tuple{State, Bool, Vector{Tuple{Int,Int}}, Float64}}

Build all possible states, where the key is the hash of the state.
"""
function build_all_states()
    all_states = Dict{Int, Tuple{State, Bool, Vector{Tuple{Int,Int}}, Float64}}()

    function dfs(current_state::State, symbol::Int)
        h = hash_state(current_state)
        # If we've already seen this state, don't re‐explore
        if haskey(all_states, h)
            return
        end

        # Check if terminal
        is_end = check_end(current_state)
        # best_moves, best_score can just be placeholders initially
        all_states[h] = (current_state, is_end, Vector{Tuple{Int,Int}}(), 0.0)

        # If not terminal, explore children by placing `symbol` on every empty cell
        if !is_end
            for i in 1:BOARD_ROWS, j in 1:BOARD_COLS
                if current_state.data[i, j] == 0
                    child_state = next_state(current_state, i, j, symbol)
                    dfs(child_state, -symbol)  # flip the symbol
                end
            end
        end
    end

    # Start from the empty board with X (symbol=+1) as the first mover
    start_state = State()
    dfs(start_state, 1)

    return all_states
end

"""
    fill_minimax!(all_states::Dict{Int, Tuple{State,Bool,Vector{Tuple{Int,Int}},Float64}})

Fill the minimax cache (of values) for the given states.
"""
function fill_minimax!(all_states::Dict{Int, Tuple{State,Bool,Vector{Tuple{Int,Int}},Float64}})
    # A local cache for storing each state's minimax value
    minimax_cache = Dict{Int, Float64}()

    function get_minimax_value(hash_val::Int)
        # If we have it cached, just return
        if haskey(minimax_cache, hash_val)
            return minimax_cache[hash_val]
        end

        # Otherwise compute it
        st, is_end, _, _ = all_states[hash_val]
        if is_end
            # Terminal state's score from X's perspective
            if st.winner == 1
                minimax_cache[hash_val] = 1.0
            elseif st.winner == -1
                minimax_cache[hash_val] = -1.0
            else
                minimax_cache[hash_val] = 0.0
            end
            return minimax_cache[hash_val]
        else
            # Not terminal
            # Determine whose turn it is: X if total moves so far is even
            current_symbol = sum(abs.(st.data)) % 2 == 0 ? 1 : -1

            # Collect child states
            child_hashes = Int[]
            for i in 1:BOARD_ROWS, j in 1:BOARD_COLS
                if st.data[i, j] == 0
                    child = next_state(st, i, j, current_symbol)
                    push!(child_hashes, hash_state(child))
                end
            end
            # If X's turn, we want to maximize the final outcome for X
            # If O's turn, we want to minimize that outcome (which is the same as
            # "maximize negative").
            child_values = [ -get_minimax_value(ch) for ch in child_hashes ]
            # Always choose maximum
            best_val = maximum(child_values)

            minimax_cache[hash_val] = best_val
            return best_val
        end
    end

    # Now fill in everything
    for (hash_val, (st, is_end, _, _)) in all_states
        val = get_minimax_value(hash_val)
        # We store the "best_score" into all_states
        old_tuple = all_states[hash_val]  # (st, is_end, old_best_moves, old_best_score)
        # e.g. set the "best_score" field to val
        all_states[hash_val] = (st, is_end, old_tuple[3], val)
    end
end
"""
    fill_minimax_bestmoves!(all_states::Dict{Int, Tuple{State,Bool,Vector{Tuple{Int,Int}},Float64}})

Fill the minimax cache (of best moves) for the given states.
"""
function fill_minimax_bestmoves!(all_states::Dict{Int, Tuple{State,Bool,Vector{Tuple{Int,Int}},Float64}})
    # first get the minimax values as above
    fill_minimax!(all_states)

    # Now compute "best_moves" for each non‐terminal state
    for (h, (st, is_end, _, best_score)) in all_states
        if !is_end
            # current player's symbol
            symbol = sum(abs.(st.data)) % 2 == 0 ? 1 : -1

            # gather children
            child_data = []
            for i in 1:BOARD_ROWS, j in 1:BOARD_COLS
                if st.data[i, j] == 0
                    c = next_state(st, i, j, symbol)
                    c_hash = hash_state(c)
                    # The child's minimax value is all_states[c_hash][4], 
                    # but from the child's perspective. The parent's perspective 
                    # is the negative of that, so parent's value if it picks 
                    # child c is `- all_states[c_hash][4]`.
                    c_val = - all_states[c_hash][4]
                    push!(child_data, (i, j, c_val))
                end
            end

            best_moves = [(i, j) for (i, j, val) in child_data if val == best_score]
            all_states[h] = (st, is_end, best_moves, best_score)
        end
    end
end

# Player functions:
"""
    reset!(player::RLPlayer)

Reset the player's states and greedy vectors.
"""
function reset!(player::RLPlayer)
    player.states = []
    player.greedy = []
end

"""
    set_state!(player::RLPlayer, state::State)

Push a new move to the player's states and greedy vectors.
"""
function set_state!(player::RLPlayer, state::State)
    push!(player.states, state)
    push!(player.greedy, true)
end

"""
    set_symbol!(player::RLPlayer, symbol::Int, type::String; estimations::Dict{Int,Float64}=Dict{Int,Float64}())

Set the symbol of the player, and initialize the estimations from the type.
 - rlBase: Use 1 for win, 0.5 for tie, 0 for loss.
 - rlNoTies: Use 1 for win, 0 for loss or tie.
 - rlMinimax: Use the minimax value.
 - rlRandom: Use a random value.
"""
function set_symbol!(player::RLPlayer, symbol::Int, type::String; estimations::Dict{Int,Float64}=Dict{Int,Float64}())
    player.symbol = symbol
    if !isempty(estimations)
        player.estimations = estimations
    else
        if type == "rlBase"
            for hash_val in keys(ALL_STATES)
                state, is_end, _, _ = ALL_STATES[hash_val]
                if is_end
                    if state.winner == player.symbol
                        player.estimations[hash_val] = 1.0
                    elseif state.winner == 0
                    player.estimations[hash_val] = 0.5
                    else
                    player.estimations[hash_val] = 0.0
                    end
                else
                    player.estimations[hash_val] = 0.5
                end
            end
        elseif type == "rlNoTies"
            for hash_val in keys(ALL_STATES)
            state, is_end, _, _ = ALL_STATES[hash_val]
                if is_end
                    if state.winner == player.symbol
                        player.estimations[hash_val] = 1.0
                    elseif state.winner == 0
                        player.estimations[hash_val] = 0.0
                    else
                        player.estimations[hash_val] = 0.0
                    end
                else
                    player.estimations[hash_val] = 0.5
                end
            end
        elseif type == "rlMinimax"
            for hash_val in keys(ALL_STATES)
                _, _, _, best_score = ALL_STATES[hash_val]
                if symbol == 1
                    if best_score == 1.0
                        player.estimations[hash_val] = 1.0
                    elseif best_score == -1.0
                        player.estimations[hash_val] = 0.0
                    else
                        player.estimations[hash_val] = 0.5
                    end
                elseif symbol == -1
                    if best_score == 1.0
                        player.estimations[hash_val] = 0.0
                    elseif best_score == -1.0
                        player.estimations[hash_val] = 1.0
                    else
                        player.estimations[hash_val] = 0.5
                    end
                end
            end
        elseif type == "rlRandom"
            for hash_val in keys(ALL_STATES)
                player.estimations[hash_val] = rand()
            end
        end
    end
end


"""
    backup!(player::RLPlayer)

Update the player's estimations using the TD(0) algorithm.
"""
function backup!(player::RLPlayer)
    states = [hash_state(state) for state in player.states]

    for i in length(states)-1:-1:1
        state = states[i]
        td_error = player.greedy[i] * (player.estimations[states[i+1]] - player.estimations[state])
        player.estimations[state] += player.step_size * td_error
    end
end
"""
    act!(player::RLPlayer, state::State) -> Tuple{Int, Int, Int, Bool}

Call RL player to act, returning the action, the next symbol, and whether it is greedy.
 - if rand() < epsilon, the action is random.
 - otherwise, the action is the greedy action.
"""
function act!(player::RLPlayer, state::State)
    next_sym = sum(abs.(state.data)) % 2 == 0 ? 1 : -1
    next_states = Int[]
    next_positions = Vector{Tuple{Int,Int}}()
    for i in 1:BOARD_ROWS
        for j in 1:BOARD_COLS
            if state.data[i, j] == 0
                push!(next_positions, (i, j))
                push!(next_states, hash_state(next_state(state, i, j, next_sym)))
            end
        end
    end

    # If not greedy, choose a random action
    if rand() < player.epsilon
        action_idx = rand(1:length(next_positions))
        action = next_positions[action_idx]
        is_greedy = false
    else
        # Greedy action
    values = []
    for (k, v) in enumerate(next_states)
        push!(values, (player.estimations[v], next_positions[k]))
    end
        # Break ties randomly
        shuffle!(values)
        sort!(values, by=x -> x[1], rev=true)
        action = values[1][2]
        is_greedy = true
    end
    return (action[1], action[2], next_sym, is_greedy)
end

"""
    reset!(player::HumanPlayer)

Reset the human player (does nothing).
"""
function reset!(player::HumanPlayer)
    # No state to reset for human player
end

"""
    set_state!(player::HumanPlayer, state::State)

Set the state of the human player.
"""
function set_state!(player::HumanPlayer, state::State)
    player.state = state
end

"""
    set_symbol!(player::HumanPlayer, symbol::Int)

Set the symbol of the human player.
"""
function set_symbol!(player::HumanPlayer, symbol::Int)
    player.symbol = symbol
end


"""
    act!(player::HumanPlayer, state::State) -> Tuple{Int, Int, Int, Bool}

Call human player to act. Takes in a number between 1 and 9, 
and moves to the corresponding position.
Human interface:
| 1 | 2 | 3 |
| 4 | 5 | 6 |
| 7 | 8 | 9 |
"""
function act!(player::HumanPlayer, state::State)
    while true
        print_board_state(state)
        print("Input your position (1-9): ")
        Base.flush(stdout)
        key = readline()
        if isempty(key)
            println("Invalid input. Please enter a number between 1 and 9.")
            continue
        end
        data = tryparse(Int, key)
        if data === nothing || data < 1 || data > 9
            println("Invalid input. Please enter a number between 1 and 9.")
            continue
        end
        data -= 1
        i = floor(Int, data / BOARD_COLS)
        j = data % BOARD_COLS
        if state.data[i+1, j+1] != 0
            println("That position is already occupied. Please choose another position.")
            continue
        end
        return (i + 1, j + 1, player.symbol, false)
    end
end

# Judger functions:
"""
    reset!(judger::Judger)

Reset the judger, resetting the players and the current state to empty.
"""
function reset!(judger::Judger)
    reset!(judger.p1)
    reset!(judger.p2)
    judger.current_player = nothing
    judger.current_state = State()
end

"""
    play(judger::Judger) -> Int

Play a game, returning the winner.
"""
function play(judger::Judger)
    reset!(judger)
    current_state = State()
    judger.current_state = current_state
    judger.p1.states = [current_state]
    judger.p2.states = [current_state]
    while true
        # p1 plays
        player = judger.p1
        i, j, next_sym, is_greedy = act!(player, current_state) 
        next_state_hash = hash_state(next_state(current_state, i, j, next_sym))
        current_state, is_end = ALL_STATES[next_state_hash]
        judger.current_state = current_state
        push!(judger.p1.states, current_state)
        push!(judger.p1.greedy, is_greedy)
        push!(judger.p2.states, current_state)
        push!(judger.p2.greedy, true)
        if is_end
            return current_state.winner
        end
        # p2 plays
        player = judger.p2
        i, j, next_sym, is_greedy = act!(player, current_state)
        next_state_hash = hash_state(next_state(current_state, i, j, next_sym))
        if !haskey(ALL_STATES, next_state_hash)
            println("Missing board with hash $next_state_hash")
            println("Current board:")
            print_board_state(current_state)
            println("Attempted move: ($i, $j) with symbol $next_sym")
            error("State not found!")
        end
        current_state, is_end = ALL_STATES[next_state_hash]
        judger.current_state = current_state
        push!(judger.p2.states, current_state)
        push!(judger.p2.greedy, is_greedy)
        push!(judger.p1.states, current_state)
        push!(judger.p1.greedy, true)
        if is_end
            return current_state.winner
        end
    end
end

# Universal functions:
"""
    train(epochs::Int, p1::RLPlayer, p2::RLPlayer; print_every_n=500) -> Tuple{Dict{Int,Float64}, Dict{Int,Float64}}

Train the RLPlayer, over a number of epochs, printing the winrate every n epochs. Return the trained estimations.
"""
function train(epochs::Int, p1::RLPlayer, p2::RLPlayer; print_every_n=500)
    player1 = p1
    player2 = p2
    judger = Judger(player1, player2)
    player1_win = 0.0
    player2_win = 0.0
    for i in 1:epochs
        winner = play(judger)
        if winner == 1
            player1_win += 1
        end
        if winner == -1
            player2_win += 1
        end
        if i % print_every_n == 0
            println("Epoch $i, player 1 total winrate: $(round(player1_win / i, digits=4)), player 2 total winrate: $(round(player2_win / i, digits=4))")
        end

        # Update estimations
        backup!(player1)
        backup!(player2)
        reset!(judger)
    end
    return player1.estimations, player2.estimations
end

"""
    compete(turns::Int, p1_estimations::Dict{Int,Float64}, p2_estimations::Dict{Int,Float64}; print::Bool=true) -> Tuple{Float64, Float64}

Make two RLPlayers compete over a number of turns, never exploring. Return the winrates.
"""
function compete(turns::Int, p1_estimations::Dict{Int,Float64}, p2_estimations::Dict{Int,Float64}; print::Bool=true)
    player1 = RLPlayer(symbol=1, step_size=0.0, epsilon=0.0)
    player1.estimations = p1_estimations
    player2 = RLPlayer(symbol=-1, step_size=0.0, epsilon=0.0)
    player2.estimations = p2_estimations
    judger = Judger(player1, player2)
    player1_win = 0.0
    player2_win = 0.0
    for _ in 1:turns
        winner = play(judger)
        if winner == 1
            player1_win += 1
        elseif winner == -1
            player2_win += 1
        end
        reset!(judger)
    end
    if print
        println("$turns turns, player 1 win $(round((player1_win / turns)*100, digits=4))%, player 2 win $(round((player2_win / turns)*100, digits=4))%")
    end
    return player1_win, player2_win
end

"""
    play_human(estimations::Dict{Int,Float64}, symbol::Int)

Play games against the human player. Begin with the human player as player 1, then switch roles once the human player chooses to play again.
"""
function play_human(estimations::Dict{Int,Float64}, symbol::Int)
    if symbol == -1
        while true
            player2 = HumanPlayer(symbol=symbol)
            player1 = RLPlayer(symbol=-symbol, step_size=0.0, epsilon=0.0)
            player1.estimations = estimations
            judger = Judger(player1, player2)
            winner = play(judger)
            if winner == player1.symbol
                print_board_state(judger.current_state)
                println("You lose!")
            elseif winner == player2.symbol
                println("You win!")
            else
                print_board_state(judger.current_state)
                println("It is a tie!")
            end
            reset!(judger)
            # Loop until the human player wants to quit
            println("Do you want to play again? (y/n)")
            Base.flush(stdout)
            key = readline()
            if key == "n"
                break
            end
        end
    else
        while true
            player1 = HumanPlayer(symbol=symbol)
            player2 = RLPlayer(symbol=-symbol, step_size=0.0, epsilon=0.0)
            player2.estimations = estimations
            judger = Judger(player1, player2)
            winner = play(judger)
            if winner == player2.symbol
                print_board_state(judger.current_state)
                println("You lose!")
            elseif winner == player1.symbol
                println("You win!")
            else
                print_board_state(judger.current_state)
                println("It is a tie!")
            end
            reset!(judger)
            # Loop until the human player wants to quit
            println("Do you want to play again? (y/n)")
            Base.flush(stdout)
            key = readline()
            if key == "n"
                break
            end
        end
    end
end