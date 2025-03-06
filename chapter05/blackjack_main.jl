# Packages
using Plots, ProgressMeter, Statistics, Random, Statistics

# Include the blackjack functions
include("blackjack_functions.jl")

# =========================================================
# Figure 5.1: State-Value function from On-Policy Monte Carlo
# =========================================================
# Run Monte Carlo simulations with 10,000 episodes
@time states_usable_ace_10k, states_no_usable_ace_10k = monte_carlo_on_policy(10_000)

# Run Monte Carlo simulations with 500,000 episodes
@time states_usable_ace_500k, states_no_usable_ace_500k = monte_carlo_on_policy(500_000)

# Create the figures
p1 = plot(layout = (2, 2), size = (1000, 800), fontfamily="Computer Modern", background=:transparent)

dealer_labels = ["A", "2", "3", "4", "5", "6", "7", "8", "9", "10"]
player_labels = string.(12:21)

# Plot 1: Usable Ace, 10k episodes
heatmap!(
    p1[1],
    1:10, 12:21, states_usable_ace_10k,
    title = "Usable Ace, 10,000 Episodes",
    xlabel = "Dealer Showing",
    ylabel = "Player Sum",
    color = :thermal,
    xticks = (1:10, dealer_labels),
    yticks = (12:21, player_labels),
    yflip = true
)

# Plot 2: Usable Ace, 500k episodes
heatmap!(
    p1[2],
    1:10, 12:21, states_usable_ace_500k,
    title = "Usable Ace, 500,000 Episodes",
    xlabel = "Dealer Showing",
    ylabel = "Player Sum",
    color = :thermal,
    xticks = (1:10, dealer_labels),
    yticks = (12:21, player_labels),
    yflip = true
)

# Plot 3: No Usable Ace, 10k episodes
heatmap!(
    p1[3],
    1:10, 12:21, states_no_usable_ace_10k,
    title = "No Usable Ace, 10,000 Episodes",
    xlabel = "Dealer Showing",
    ylabel = "Player Sum",
    color = :thermal,
    xticks = (1:10, dealer_labels),
    yticks = (12:21, player_labels),
    yflip = true
)

# Plot 4: No Usable Ace, 500k episodes
heatmap!(
    p1[4],
    1:10, 12:21, states_no_usable_ace_500k,
    title = "No Usable Ace, 500,000 Episodes",
    xlabel = "Dealer Showing",
    ylabel = "Player Sum",
    color = :thermal,
    xticks = (1:10, dealer_labels),
    yticks = (12:21, player_labels),
    yflip = true
)
savefig(p1, "cornell_theory_reading_group_RL/chapter05/figure_5_1.png")

# =========================================================
# Figure 5.2: Optimal policy and state-value function from Monte Carlo ES
# =========================================================
# Run Monte Carlo ES with 500,000 episodes
@time state_action_values = monte_carlo_es(500_000)

# Extract state values and optimal policy
state_value_usable_ace = zeros(Float64, 10, 10)
state_value_no_usable_ace = zeros(Float64, 10, 10)
action_usable_ace = zeros(Int, 10, 10)
action_no_usable_ace = zeros(Int, 10, 10)

for player_sum in 12:21
    for dealer_card in 1:10
        player_idx = player_sum - 11
        dealer_idx = dealer_card
        
        # Usable ace
        usable_ace_values = state_action_values[player_idx, dealer_idx, 2, :]
        best_action_usable = argmax(usable_ace_values) - 1
        state_value_usable_ace[player_idx, dealer_idx] = maximum(usable_ace_values)
        action_usable_ace[player_idx, dealer_idx] = best_action_usable
        
        # No usable ace
        no_usable_ace_values = state_action_values[player_idx, dealer_idx, 1, :]
        best_action_no_usable = argmax(no_usable_ace_values) - 1
        state_value_no_usable_ace[player_idx, dealer_idx] = maximum(no_usable_ace_values)
        action_no_usable_ace[player_idx, dealer_idx] = best_action_no_usable
    end
end


# Create the figure
p2 = plot(layout = (2, 2), size = (1000, 800), fontfamily="Computer Modern", background=:transparent)

# Plot 1: Optimal Policy with Usable Ace
heatmap!(
    p2[1],
    1:10, 12:21, action_usable_ace,
    title = "Optimal Policy with Usable Ace",
    xlabel = "Dealer Showing",
    ylabel = "Player Sum",
    color = :grays,  # Different colormap for policy
    xticks = (1:10, dealer_labels),
    yticks = (12:21, player_labels),
    colorbar_ticks = [0, 1],
    colorbar_labels = ["Hit", "Stand"],
    clims = (0, 1),  # Force the color limits
)

# Plot 2: Optimal Value with Usable Ace
heatmap!(
    p2[2],
    1:10, 12:21, state_value_usable_ace,
    title = "Optimal Value with Usable Ace",
    xlabel = "Dealer Showing",
    ylabel = "Player Sum",
    color = :thermal,  # Match your figure 5.1 colormap
    xticks = (1:10, dealer_labels),
    yticks = (12:21, player_labels),
    yflip = true
)

# Plot 3: Optimal Policy without Usable Ace
heatmap!(
    p2[3],
    1:10, 12:21, action_no_usable_ace,
    title = "Optimal Policy without Usable Ace",
    xlabel = "Dealer Showing",
    ylabel = "Player Sum",
    color = :grays,  # Different colormap for policy
    xticks = (1:10, dealer_labels),
    yticks = (12:21, player_labels),
    colorbar_ticks = [0, 1],
    colorbar_labels = ["Hit", "Stand"],
    clims = (0, 1),  # Force the color limits
)

# Plot 4: Optimal Value without Usable Ace
heatmap!(
    p2[4],
    1:10, 12:21, state_value_no_usable_ace,
    title = "Optimal Value without Usable Ace",
    xlabel = "Dealer Showing",
    ylabel = "Player Sum",
    color = :thermal,  # Match your figure 5.1 colormap
    xticks = (1:10, dealer_labels),
    yticks = (12:21, player_labels),
    yflip = true
)

savefig(p2, "cornell_theory_reading_group_RL/chapter05/figure_5_2.png")

# =========================================================
# Figure 5.3: Ordinary vs Weighted Importance Sampling
# =========================================================
# Parameters
true_value = -0.27726
episodes = 10_000
runs = 100

error_ordinary = zeros(Float64, episodes)
error_weighted = zeros(Float64, episodes)

# Run simulations
p = Progress(runs, desc="Running simulations: ", dt=1.0)
for run in 1:runs
    ordinary_sampling, weighted_sampling = monte_carlo_off_policy(episodes)
    
    # Calculate squared errors
    error_ordinary .+= (ordinary_sampling .- true_value).^2
    error_weighted .+= (weighted_sampling .- true_value).^2
    
    next!(p)
end

# Average over runs
error_ordinary ./= runs
error_weighted ./= runs

# Create plot
p3 = plot(
    1:episodes, error_ordinary, 
    label = "Ordinary Importance Sampling",
    color = :green, 
    xlabel = "Episodes (log scale)",
    ylabel = "Mean Square Error\n(average over $runs runs)",
    xscale = :log10,
    ylim = (-0.1, 5),
    legend = :topright,
    size = (800, 600),
    fontfamily = "Computer Modern",
    background = :transparent,
    linewidth = 2
)

plot!(
    p3, 
    1:episodes, error_weighted, 
    label = "Weighted Importance Sampling",
    color = :red,
    linewidth = 2
)

savefig(p3, "cornell_theory_reading_group_RL/chapter05/figure_5_3.png")