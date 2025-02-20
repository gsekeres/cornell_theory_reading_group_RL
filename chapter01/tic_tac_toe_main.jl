using Random

# Board parameters
const BOARD_ROWS = 3
const BOARD_COLS = 3
const BOARD_SIZE = BOARD_ROWS * BOARD_COLS

# A small annoyance with the logic here:
if BOARD_ROWS != BOARD_COLS
    throw(ArgumentError("BOARD_ROWS and BOARD_COLS must be equal, because Tic Tac Toe is not well-defined for non-square boards."))
end

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
println("Baseline short training:")
p1_estimations_initial, p2_estimations_initial = train(100_000, player1_initial, player2_initial, print_every_n=10_000)
p1_win_initial, p2_win_initial = compete(10_000, p1_estimations_initial, p2_estimations_initial)

# Play against the human player (leave commented, cursor cannot handle inputs)
#play_human(p1_estimations_initial, -1)
#play_human(p2_estimations_initial, 1)


# Python replication complete, now time for some fun experiments.

# Short training runs (100k epochs, eps = 0.01)

# Start from minimax evaluations
player1_short_minimax = RLPlayer(symbol=1, step_size=0.1, epsilon=0.01, type="rlMinimax")
player2_short_minimax = RLPlayer(symbol=-1, step_size=0.1, epsilon=0.01, type="rlMinimax")
set_symbol!(player1_short_minimax, 1, "rlMinimax")
set_symbol!(player2_short_minimax, -1, "rlMinimax")
println("Minimax short training:")
p1_estimations_short_minimax, p2_estimations_short_minimax = train(100_000, player1_short_minimax, player2_short_minimax, print_every_n=10_000)
p1_win_short_minimax, p2_win_short_minimax = compete(10_000, p1_estimations_short_minimax, p2_estimations_short_minimax)

# Start from random evaluations
player1_short_random = RLPlayer(symbol=1, step_size=0.1, epsilon=0.01, type="rlRandom")
player2_short_random = RLPlayer(symbol=-1, step_size=0.1, epsilon=0.01, type="rlRandom")
set_symbol!(player1_short_random, 1, "rlRandom")
set_symbol!(player2_short_random, -1, "rlRandom")
println("Random short training:")
p1_estimations_short_random, p2_estimations_short_random = train(100_000, player1_short_random, player2_short_random, print_every_n=10_000)
p1_win_short_random, p2_win_short_random = compete(10_000, p1_estimations_short_random, p2_estimations_short_random)

# Consider a draw a loss
player1_short_noties = RLPlayer(symbol=1, step_size=0.1, epsilon=0.01, type="rlNoTies")
player2_short_noties = RLPlayer(symbol=-1, step_size=0.1, epsilon=0.01, type="rlNoTies")
set_symbol!(player1_short_noties, 1, "rlNoTies")
set_symbol!(player2_short_noties, -1, "rlNoTies")
println("No ties short training:")
p1_estimations_short_noties, p2_estimations_short_noties = train(100_000, player1_short_noties, player2_short_noties, print_every_n=10_000)
p1_win_short_noties, p2_win_short_noties = compete(10_000, p1_estimations_short_noties, p2_estimations_short_noties)

# Train player 1 against random player
player1_short_against_random = RLPlayer(symbol=1, step_size=0.1, epsilon=0.01, type="rlBase")
player2_true_random = RLPlayer(symbol=-1, step_size=0.0, epsilon=1.0, type="rlBase")
set_symbol!(player1_short_against_random, 1, "rlBase")
set_symbol!(player2_true_random, -1, "rlBase")
println("Short training against random (player 1):")
p1_estimations_short_against_random, p2_estimations_true_random = train(100_000, player1_short_against_random, player2_true_random, print_every_n=10_000)
p1_win_short_against_random, _ = compete(10_000, p1_estimations_short_against_random, p2_estimations_true_random)

# Train player 2 against random player
player1_true_random = RLPlayer(symbol=1, step_size=0.0, epsilon=1.0, type="rlBase")
player2_short_against_random = RLPlayer(symbol=-1, step_size=0.1, epsilon=0.01, type="rlBase")
set_symbol!(player1_true_random, 1, "rlBase")
set_symbol!(player2_short_against_random, -1, "rlBase")
println("Short training against random (player 2):")
p1_estimations_true_random, p2_estimations_short_against_random = train(100_000, player1_true_random, player2_short_against_random, print_every_n=10_000)
_, p2_win_short_against_random = compete(10_000, p1_estimations_true_random, p2_estimations_short_against_random)






# Full training runs (3m epochs)

# Base type
player1_full_baseline = RLPlayer(symbol=1, step_size=0.1, epsilon=0.1, type="rlBase")
player2_full_baseline = RLPlayer(symbol=-1, step_size=0.1, epsilon=0.1, type="rlBase")
set_symbol!(player1_full_baseline, 1, "rlBase")
set_symbol!(player2_full_baseline, -1, "rlBase")
p1_estimations_full_baseline, p2_estimations_full_baseline = train(3_000_000, player1_full_baseline, player2_full_baseline, print_every_n=100_000)
p1_win_full_baseline, p2_win_full_baseline = compete(10_000, p1_estimations_full_baseline, p2_estimations_full_baseline)

# Start from minimax evaluations
player1_full_minimax = RLPlayer(symbol=1, step_size=0.1, epsilon=0.1, type="rlMinimax")
player2_full_minimax = RLPlayer(symbol=-1, step_size=0.1, epsilon=0.1, type="rlMinimax")
set_symbol!(player1_full_minimax, 1, "rlMinimax")
set_symbol!(player2_full_minimax, -1, "rlMinimax")
println("Minimax full training:")
p1_estimations_full_minimax, p2_estimations_full_minimax = train(3_000_000, player1_full_minimax, player2_full_minimax, print_every_n=100_000)
p1_win_full_minimax, p2_win_full_minimax = compete(10_000, p1_estimations_full_minimax, p2_estimations_full_minimax)

# Start from random evaluations
player1_full_random = RLPlayer(symbol=1, step_size=0.1, epsilon=0.1, type="rlRandom")
player2_full_random = RLPlayer(symbol=-1, step_size=0.1, epsilon=0.1, type="rlRandom")
set_symbol!(player1_full_random, 1, "rlRandom")
set_symbol!(player2_full_random, -1, "rlRandom")
println("Random full training:")
p1_estimations_full_random, p2_estimations_full_random = train(3_000_000, player1_full_random, player2_full_random, print_every_n=100_000)
p1_win_full_random, p2_win_full_random = compete(10_000, p1_estimations_full_random, p2_estimations_full_random)

# Consider a draw a loss
player1_full_noties = RLPlayer(symbol=1, step_size=0.1, epsilon=0.1, type="rlNoTies")
player2_full_noties = RLPlayer(symbol=-1, step_size=0.1, epsilon=0.1, type="rlNoTies")
set_symbol!(player1_full_noties, 1, "rlNoTies")
set_symbol!(player2_full_noties, -1, "rlNoTies")
println("No ties full training:")
p1_estimations_full_noties, p2_estimations_full_noties = train(3_000_000, player1_full_noties, player2_full_noties, print_every_n=100_000)
p1_win_full_noties, p2_win_full_noties = compete(10_000, p1_estimations_full_noties, p2_estimations_full_noties)

# Train player 1 against random player
player1_full_against_random = RLPlayer(symbol=1, step_size=0.1, epsilon=0.1, type="rlBase")
player2_true_random = RLPlayer(symbol=-1, step_size=0.0, epsilon=1.0, type="rlBase")
set_symbol!(player1_full_against_random, 1, "rlBase")
set_symbol!(player2_true_random, -1, "rlBase")
println("Full training against random (player 1):")
p1_estimations_full_against_random, p2_estimations_true_random = train(3_000_000, player1_full_against_random, player2_true_random, print_every_n=100_000)
p1_win_full_against_random, _ = compete(10_000, p1_estimations_full_against_random, p2_estimations_true_random)

# Train player 2 against random player
player1_true_random = RLPlayer(symbol=1, step_size=0.0, epsilon=1.0, type="rlBase")
player2_full_against_random = RLPlayer(symbol=-1, step_size=0.1, epsilon=0.1, type="rlBase")
set_symbol!(player1_true_random, 1, "rlBase")
set_symbol!(player2_full_against_random, -1, "rlBase")
println("Full training against random (player 2):")
p1_estimations_true_random, p2_estimations_full_against_random = train(3_000_000, player1_true_random, player2_full_against_random, print_every_n=100_000)
_, p2_win_full_against_random = compete(10_000, p1_estimations_true_random, p2_estimations_full_against_random)

# Quickly get true minimax evaluations
player1_true_minimax = RLPlayer(symbol=1, step_size=0.0, epsilon=1.0, type="rlMinimax")
player2_true_minimax = RLPlayer(symbol=-1, step_size=0.0, epsilon=1.0, type="rlMinimax")
set_symbol!(player1_true_minimax, 1, "rlMinimax")
set_symbol!(player2_true_minimax, -1, "rlMinimax")
p1_estimations_true_minimax, p2_estimations_true_minimax = train(10_000, player1_true_minimax, player2_true_minimax, print_every_n=1000)






# Results of different training regimes:

# 1. Baseline
# 2. Minimax
# 3. Random
# 4. No ties
# 5. Against random

println("In general, baseline, minimax, and no ties all get ties against each other:")
_, _ = compete(10_000, p1_estimations_full_baseline, p2_estimations_full_minimax)
_, _ = compete(10_000, p1_estimations_full_minimax, p2_estimations_full_baseline)
_, _ = compete(10_000, p1_estimations_full_baseline, p2_estimations_full_noties)
_, _ = compete(10_000, p1_estimations_full_noties, p2_estimations_full_baseline)
_, _ = compete(10_000, p1_estimations_full_noties, p2_estimations_full_minimax)

println("The exception is minimax p1 vs no ties p2, where minimax always wins:")
_, _ = compete(10_000, p1_estimations_full_minimax, p2_estimations_full_noties)
println("This is, of course, because noties sees the draw as the same as a loss.")

println("Nothing changes, interestingly, when we train against a random player:")
_,_ = compete(10_000, p1_estimations_full_baseline, p2_estimations_full_against_random)
_,_ = compete(10_000, p1_estimations_full_against_random, p2_estimations_full_baseline)
_,_ = compete(10_000, p1_estimations_full_minimax, p2_estimations_full_against_random)
_,_ = compete(10_000, p1_estimations_full_against_random, p2_estimations_full_minimax)
_,_ = compete(10_000, p1_estimations_full_noties, p2_estimations_full_against_random)
_,_ = compete(10_000, p1_estimations_full_against_random, p2_estimations_full_noties)

println("When we start with random evaluations, however, all of the other models win 100% of the time:")
_,_ = compete(10_000, p1_estimations_full_baseline, p2_estimations_full_random)
_,_ = compete(10_000, p1_estimations_full_random, p2_estimations_full_baseline)
_,_ = compete(10_000, p1_estimations_full_minimax, p2_estimations_full_random)
_,_ = compete(10_000, p1_estimations_full_random, p2_estimations_full_minimax)
_,_ = compete(10_000, p1_estimations_full_noties, p2_estimations_full_random)
_,_ = compete(10_000, p1_estimations_full_random, p2_estimations_full_noties)
_,_ = compete(10_000, p1_estimations_full_against_random, p2_estimations_full_random)
_,_ = compete(10_000, p1_estimations_full_random, p2_estimations_full_against_random)



# Plot the fitted estimations of the players:
using Plots
# Set transparent background
default(background_color=:transparent)

# Sort by the baseline full fitted estimations:
p1_all_hashes = collect(keys(p1_estimations_full_baseline))
sorted_p1_hashes = sort(p1_all_hashes, by = hash -> p1_estimations_full_baseline[hash])

p1_initial_estimations = [p1_estimations_true_random[hash] for hash in sorted_p1_hashes]
p1_true_minimax_estimations = [p1_estimations_true_minimax[hash] for hash in sorted_p1_hashes]
p1_baseline_estimations = [p1_estimations_full_baseline[hash] for hash in sorted_p1_hashes]
p1_minimax_estimations = [p1_estimations_full_minimax[hash] for hash in sorted_p1_hashes]
p1_random_estimations = [p1_estimations_full_random[hash] for hash in sorted_p1_hashes]
p1_noties_estimations = [p1_estimations_full_noties[hash] for hash in sorted_p1_hashes]
p1_against_random_estimations = [p1_estimations_full_against_random[hash] for hash in sorted_p1_hashes]

xvals = 1:length(sorted_p1_hashes)

p1_plot = plot(legend=:outerright)
scatter!(p1_plot, xvals, p1_initial_estimations, label="Initial", color=:blue, alpha=0.2, markerstrokewidth=0, markersize=2)
savefig(p1_plot, "cornell_theory_reading_group_RL/chapter01/p1_estimations_1.png")
scatter!(p1_plot, xvals, p1_true_minimax_estimations, label="True minimax", color=:red, alpha=0.2, markerstrokewidth=0, markersize=2)
savefig(p1_plot, "cornell_theory_reading_group_RL/chapter01/p1_estimations_2.png")
scatter!(p1_plot, xvals, p1_baseline_estimations, label="Baseline", color=:green, alpha=0.2, markerstrokewidth=0, markersize=2)
savefig(p1_plot, "cornell_theory_reading_group_RL/chapter01/p1_estimations_3.png")
scatter!(p1_plot, xvals, p1_minimax_estimations, label="Minimax", color=:purple, alpha=0.2, markerstrokewidth=0, markersize=2)
savefig(p1_plot, "cornell_theory_reading_group_RL/chapter01/p1_estimations_4.png")
scatter!(p1_plot, xvals, p1_noties_estimations, label="No ties", color=:brown, alpha=0.2, markerstrokewidth=0, markersize=2)
savefig(p1_plot, "cornell_theory_reading_group_RL/chapter01/p1_estimations_5.png")
scatter!(p1_plot, xvals, p1_against_random_estimations, label="Against random", color=:pink, alpha=0.2, markerstrokewidth=0, markersize=2)
savefig(p1_plot, "cornell_theory_reading_group_RL/chapter01/p1_estimations_6.png")
scatter!(p1_plot, xvals, p1_random_estimations, label="Random", color=:orange, alpha=0.2, markerstrokewidth=0, markersize=2)
savefig(p1_plot, "cornell_theory_reading_group_RL/chapter01/p1_estimations_7.png")


p2_all_hashes = collect(keys(p2_estimations_full_baseline))
sorted_p2_hashes = sort(p2_all_hashes, by = hash -> p2_estimations_full_baseline[hash])

p2_initial_estimations = [p2_estimations_true_random[hash] for hash in sorted_p2_hashes]
p2_true_minimax_estimations = [p2_estimations_true_minimax[hash] for hash in sorted_p2_hashes]
p2_baseline_estimations = [p2_estimations_full_baseline[hash] for hash in sorted_p2_hashes]
p2_minimax_estimations = [p2_estimations_full_minimax[hash] for hash in sorted_p2_hashes]
p2_random_estimations = [p2_estimations_full_random[hash] for hash in sorted_p2_hashes]
p2_noties_estimations = [p2_estimations_full_noties[hash] for hash in sorted_p2_hashes]
p2_against_random_estimations = [p2_estimations_full_against_random[hash] for hash in sorted_p2_hashes]

xvals = 1:length(sorted_p2_hashes)

p2_plot = plot(legend=:outerright)
scatter!(p2_plot, xvals, p2_initial_estimations, label="Initial", color=:blue, alpha=0.2, markerstrokewidth=0, markersize=2)
savefig(p2_plot, "cornell_theory_reading_group_RL/chapter01/p2_estimations_1.png")
scatter!(p2_plot, xvals, p2_true_minimax_estimations, label="True minimax", color=:red, alpha=0.2, markerstrokewidth=0, markersize=2)
savefig(p2_plot, "cornell_theory_reading_group_RL/chapter01/p2_estimations_2.png")
scatter!(p2_plot, xvals, p2_baseline_estimations, label="Baseline", color=:green, alpha=0.2, markerstrokewidth=0, markersize=2)
savefig(p2_plot, "cornell_theory_reading_group_RL/chapter01/p2_estimations_3.png")
scatter!(p2_plot, xvals, p2_minimax_estimations, label="Minimax", color=:purple, alpha=0.2, markerstrokewidth=0, markersize=2)
savefig(p2_plot, "cornell_theory_reading_group_RL/chapter01/p2_estimations_4.png")
scatter!(p2_plot, xvals, p2_noties_estimations, label="No ties", color=:brown, alpha=0.2, markerstrokewidth=0, markersize=2)
savefig(p2_plot, "cornell_theory_reading_group_RL/chapter01/p2_estimations_5.png")
scatter!(p2_plot, xvals, p2_against_random_estimations, label="Against random", color=:pink, alpha=0.2, markerstrokewidth=0, markersize=2)
savefig(p2_plot, "cornell_theory_reading_group_RL/chapter01/p2_estimations_6.png")
scatter!(p2_plot, xvals, p2_random_estimations, label="Random", color=:orange, alpha=0.2, markerstrokewidth=0, markersize=2)
savefig(p2_plot, "cornell_theory_reading_group_RL/chapter01/p2_estimations_7.png")


