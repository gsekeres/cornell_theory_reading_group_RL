using Random, Distributions, Plots, Statistics, StatsPlots
# Make plots transparent
default(background_color=:transparent)

include("ten_armed_testbed_functions.jl")

# ==================== FIGURE 2.1: Violin Plot of Reward Distributions ==================== #
const K_ARMS = 10
# Generate a true reward offset for each arm
const true_rewards = randn(K_ARMS)

# Create data for a 200×K_ARMS matrix where each column is shifted by the corresponding true reward.
data = randn(200, K_ARMS) .+ true_rewards'
xvalues = 1:K_ARMS

# Create a violin plot
fig1 = violin(data,
    xlabel = "Action",
    ylabel = "Reward Distribution",
    color = :gray,
    legend = false,
    xticks = (xvalues, string.(xvalues)),
    leftmargin = 5Plots.mm,
    rightmargin = 5Plots.mm,
    topmargin = 5Plots.mm,
    bottommargin = 5Plots.mm,
)

# Draw horizontal lines for each arm's true reward
for i in 1:K_ARMS
    plot!(fig1, [xvalues[i]-0.5, xvalues[i]+0.5], [true_rewards[i], true_rewards[i]], color = :black, linewidth = 2)
end
# Draw a dashed horizontal line at 0
plot!(fig1, [0.5, K_ARMS+0.5], [0, 0], color = :black, linewidth = 2, linestyle = :dash)
savefig(fig1, "cornell_theory_reading_group_RL/chapter02/ten_armed_testbed_violin.png")

# ------------------ Simulation: Epsilon-Greedy Bandits ------------------ #
# Define simulation parameters
const RUNS = 2000
const TIME = 1000

# Define epsilon values for the ε-greedy algorithm
epsilon_values = [0.1, 0.01, 0.0]

# Create bandits (using sample averages for updating)
bandits = [Bandit(k_arm = K_ARMS, epsilon = eps, sample_averages = true, step_size = 0.1) for eps in epsilon_values]

# Run simulation: returns (best_action_counts, rewards)
best_action_counts, rewards_arr = simulate(RUNS, TIME, bandits)

# ==================== FIGURE 2.2(1): Average Rewards ==================== #
fig21 = plot(xlabel = "Steps", ylabel = "Average Reward", size = (800, 400), leftmargin = 5Plots.mm, rightmargin = 5Plots.mm, topmargin = 5Plots.mm, bottommargin = 5Plots.mm)
# Use eachrow to iterate over the rows of rewards_arr
for (eps, rwd) in zip(epsilon_values, eachrow(rewards_arr))
    plot!(fig21, 1:TIME, rwd, label = "ε = $(eps)")
end
savefig(fig21, "cornell_theory_reading_group_RL/chapter02/ten_armed_testbed_epsilon_greedy.png")

# ==================== FIGURE 2.2(2): Optimal Action Percentage ==================== #
fig22 = plot(xlabel = "Steps", ylabel = "% Optimal Action", ylim = (0, 100), size = (800, 400), leftmargin = 5Plots.mm, rightmargin = 5Plots.mm, topmargin = 5Plots.mm, bottommargin = 5Plots.mm)
for (eps, optimal) in zip(epsilon_values, eachrow(best_action_counts))
    plot!(fig22, 1:TIME, optimal .* 100, label = "ε = $(eps)")
end
savefig(fig22, "cornell_theory_reading_group_RL/chapter02/ten_armed_testbed_epsilon_optimal.png")

# ==================== FIGURE 2.3: Optimistic Initialization vs. ε-Greedy ==================== #
# Bandit 1: Optimistic initialization (q₀ = 5, ε = 0).
bandit_opt = Bandit(k_arm = K_ARMS, epsilon = 0.0, initial = 5.0, step_size = 0.1)
# Bandit 2: ε-greedy with ε = 0.1, initial = 0.
bandit_std = Bandit(k_arm = K_ARMS, epsilon = 0.1, initial = 0.0, step_size = 0.1)
bandits_3 = [bandit_opt, bandit_std]
best_action_counts_3, _ = simulate(RUNS, TIME, bandits_3)

fig3 = plot(xlabel = "Steps", ylabel = "% Optimal Action", size = (800, 400), ylim = (0, 100), leftmargin = 5Plots.mm, rightmargin = 5Plots.mm, topmargin = 5Plots.mm, bottommargin = 5Plots.mm)
plot!(fig3, 1:TIME, best_action_counts_3[1, :] .* 100, label = "ε = 0, q₀ = 5")
plot!(fig3, 1:TIME, best_action_counts_3[2, :] .* 100, label = "ε = 0.1, q₀ = 0")
savefig(fig3, "cornell_theory_reading_group_RL/chapter02/ten_armed_testbed_optimistic.png")

# ==================== FIGURE 2.4: UCB vs. ε-Greedy ==================== #
# Bandit 1: UCB with c = 2.
bandit_ucb = Bandit(k_arm = K_ARMS, epsilon = 0.0, UCB_param = 2.0, sample_averages = true)
# Bandit 2: ε-greedy with ε = 0.1.
bandit_eps2 = Bandit(k_arm = K_ARMS, epsilon = 0.1, sample_averages = true)
bandits_4 = [bandit_ucb, bandit_eps2]
_, rewards_4 = simulate(RUNS, TIME, bandits_4)

fig4 = plot(xlabel = "Steps", ylabel = "Average Reward", size = (800, 400), leftmargin = 5Plots.mm, rightmargin = 5Plots.mm, topmargin = 5Plots.mm, bottommargin = 5Plots.mm)
plot!(fig4, 1:TIME, rewards_4[1, :], label = "UCB, c = 2")
plot!(fig4, 1:TIME, rewards_4[2, :], label = "ε-greedy, ε = 0.1")
savefig(fig4, "cornell_theory_reading_group_RL/chapter02/ten_armed_testbed_ucb.png")

# ==================== FIGURE 2.5: Gradient Bandit Comparison ==================== #
# Four gradient bandits with different step sizes and with/without baseline.
bandit_grad1 = Bandit(k_arm = K_ARMS, gradient = true, step_size = 0.1, gradient_baseline = true,  true_reward = 4.0)
bandit_grad2 = Bandit(k_arm = K_ARMS, gradient = true, step_size = 0.1, gradient_baseline = false, true_reward = 4.0)
bandit_grad3 = Bandit(k_arm = K_ARMS, gradient = true, step_size = 0.4, gradient_baseline = true,  true_reward = 4.0)
bandit_grad4 = Bandit(k_arm = K_ARMS, gradient = true, step_size = 0.4, gradient_baseline = false, true_reward = 4.0)
bandits_5 = [bandit_grad1, bandit_grad2, bandit_grad3, bandit_grad4]
best_action_counts_5, _ = simulate(RUNS, TIME, bandits_5)

fig5 = plot(xlabel = "Steps", ylabel = "% Optimal Action", size = (800, 400), ylim = (0, 100), leftmargin = 5Plots.mm, rightmargin = 5Plots.mm, topmargin = 5Plots.mm, bottommargin = 5Plots.mm)
plot!(fig5, 1:TIME, best_action_counts_5[1, :] .* 100, label = "α = 0.1, with baseline")
plot!(fig5, 1:TIME, best_action_counts_5[2, :] .* 100, label = "α = 0.1, without baseline")
plot!(fig5, 1:TIME, best_action_counts_5[3, :] .* 100, label = "α = 0.4, with baseline")
plot!(fig5, 1:TIME, best_action_counts_5[4, :] .* 100, label = "α = 0.4, without baseline")
savefig(fig5, "cornell_theory_reading_group_RL/chapter02/ten_armed_testbed_gradient.png")


# ==================== FIGURE 2.6: Comparison of Four Methods ==================== #
# Define parameter ranges (exponents for 2^x).
eps_exponents   = -7:-2       # For ε-greedy
grad_exponents  = -5:1        # For gradient bandit
ucb_exponents   = -4:2        # For UCB
optim_exponents = -2:2        # For optimistic initialization

bandits_6 = Bandit[]
# ε-greedy bandits: use epsilon = 2^p.
for p in eps_exponents
    push!(bandits_6, Bandit(k_arm = K_ARMS, epsilon = 2.0^p, sample_averages = true))
end
# Gradient bandits: use step_size = 2^p, with baseline.
for p in grad_exponents
    push!(bandits_6, Bandit(k_arm = K_ARMS, gradient = true, step_size = 2.0^p, gradient_baseline = true))
end
# UCB bandits: use UCB_param = 2^p.
for p in ucb_exponents
    push!(bandits_6, Bandit(k_arm = K_ARMS, epsilon = 0.0, UCB_param = 2.0^p, sample_averages = true))
end
# Optimistic initialization: use initial = 2^p.
for p in optim_exponents
    push!(bandits_6, Bandit(k_arm = K_ARMS, epsilon = 0.0, initial = 2.0^p, step_size = 0.1))
end

# Simulate all bandits.
_, rewards_6 = simulate(RUNS, TIME, bandits_6)
# Compute the average reward (averaged over time) for each bandit.
avg_rewards = [mean(rewards_6[i, :]) for i in 1:length(bandits_6)]

# Create the comparison plot.
fig6 = plot(xlabel = "Parameter (2^x)", ylabel = "Average Reward", size = (800, 400), leftmargin = 5Plots.mm, rightmargin = 5Plots.mm, topmargin = 5Plots.mm, bottommargin = 5Plots.mm)
let m = 1
    for (label, exponents) in zip(["ε-greedy", "gradient bandit", "UCB", "optimistic initialization"],
                                [eps_exponents, grad_exponents, ucb_exponents, optim_exponents])
        n_params = length(exponents)
        method_rewards = avg_rewards[m:(m+n_params-1)]
        plot!(fig6, collect(exponents), method_rewards, label = label, marker = :circle)
        m += n_params
    end
end
savefig(fig6, "cornell_theory_reading_group_RL/chapter02/ten_armed_testbed_comparison.png")


