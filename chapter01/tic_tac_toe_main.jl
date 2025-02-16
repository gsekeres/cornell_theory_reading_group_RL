using Random

# Board parameters
const BOARD_ROWS = 3
const BOARD_COLS = 3
const BOARD_SIZE = BOARD_ROWS * BOARD_COLS

include("tic_tac_toe_functions.jl")

# Get all possible board configurations
const ALL_STATES = build_all_states()
# Get the (static) best moves and value for each state
fill_minimax_bestmoves!(ALL_STATES)

# Initialize the players
player1_initial = RLPlayer(symbol=1, step_size=0.1, epsilon=0.01, type="rlBase")
player2_initial = RLPlayer(symbol=-1, step_size=0.1, epsilon=0.01, type="rlBase")
set_symbol!(player1_initial, 1, "rlBase")
set_symbol!(player2_initial, -1, "rlBase")

# Train the players (python replication)
p1_estimations_initial, p2_estimations_initial = train(100000, player1_initial, player2_initial, print_every_n=500)
compete(1000, p1_estimations_initial, p2_estimations_initial)

# Play against the human player (leave commented, cursor cannot handle inputs)
#play_human(p1_estimations_initial, -1)
#play_human(p2_estimations_initial, 1)


# Python replication complete, now time for some fun experiments.
# Want to test the following: 

# 1. Train for a long time, with epsilon decreasing (done)
# 2. Train for short / long time, starting with minimax evaluations
# 3. Train for short / long time, starting with random evaluations (not implemented)
# 4. Train for short / long time, consider a draw a loss

# Then play all of the above against each other, as well as initial random players,
# and minimax players. Compare winrates over 10,000 games, make a matrix of results.

# If have time, implement a wrapper to play against a human in a nice way.
# If have time, figure out some (principled) way to compare evaluations of different players.


# Full training run (shrinking epsilon, 3m epochs)
full_training_run_epsilon = 0.1
updated_p1_estimations = Dict{Int,Float64}()
updated_p2_estimations = Dict{Int,Float64}()
total_epochs = 0

while full_training_run_epsilon > 1e-10  # Avoid floating-point underflow issue
    local player1 = RLPlayer(symbol=1, step_size=0.1, epsilon=full_training_run_epsilon, type="rlBase")
    local player2 = RLPlayer(symbol=-1, step_size=0.1, epsilon=full_training_run_epsilon, type="rlBase")

    set_symbol!(player1, 1, "rlBase"; estimations=updated_p1_estimations)
    set_symbol!(player2, -1, "rlBase"; estimations=updated_p2_estimations)

    # Train players
    new_p1_estimations, new_p2_estimations = train(100000, player1, player2, print_every_n=100000)

    # Update estimations (no need for global)
    global updated_p1_estimations = copy(new_p1_estimations)
    global updated_p2_estimations = copy(new_p2_estimations)

    # Update epsilon (needs global)
    global full_training_run_epsilon /= 2  
    global total_epochs += 100000
end

# Store final results
full_training_run_p1_estimations = copy(updated_p1_estimations)
full_training_run_p2_estimations = copy(updated_p2_estimations)

println("Total epochs: ", total_epochs)
compete(1000000, full_training_run_p1_estimations, full_training_run_p2_estimations)




# Plot some results
using Plots, PyPlot, LaTeXStrings
# Make the plots look pretty
pyplot()
PyPlot.rc("text", usetex=true)
PyPlot.rc("font", family="serif")
PyPlot.matplotlib.rcParams["mathtext.fontset"] = "cm"








