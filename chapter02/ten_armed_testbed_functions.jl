# Define the Bandit type
mutable struct Bandit
    # Number of arms
    k::Int
    # Exploration probability
    epsilon::Float64
    # Initial estimation for each action
    initial::Float64
    # Step size (or learning rate)
    step_size::Float64
    # If true, update using sample averages
    sample_averages::Bool
    # If not nothing, use UCB with the given parameter
    UCB_param::Union{Float64, Nothing}
    # If true, use the gradient-based bandit method
    gradient::Bool
    # If true, use the average reward as baseline for the gradient method
    gradient_baseline::Bool
    # True reward offset
    true_reward::Float64
    # Indices of the actions (1:k)
    indices::Vector{Int}
    # Running average reward (for baseline)
    average_reward::Float64
    # Number of time steps so far
    time::Int
    # The true (but unknown) reward for each action
    q_true::Vector{Float64}
    # Current estimation (or preference) for each action
    q_estimation::Vector{Float64}
    # Number of times each action has been chosen
    action_count::Vector{Int}
    # The best (optimal) action index
    best_action::Int
    # For gradient bandit: current action probabilities
    action_prob::Vector{Float64}
    
    function Bandit(; k_arm::Int=10, 
                    epsilon::Float64=0.0, 
                    initial::Float64=0.0, 
                    step_size::Float64=0.1, 
                    sample_averages::Bool=false, 
                    UCB_param::Union{Float64, Nothing}=nothing,
                    gradient::Bool=false, 
                    gradient_baseline::Bool=false, 
                    true_reward::Float64=0.0)
        indices = collect(1:k_arm)
        average_reward = 0.0
        time = 0
        q_true = zeros(k_arm)
        q_estimation = fill(initial, k_arm)
        action_count = zeros(Int, k_arm)
        best_action = 1
        action_prob = zeros(k_arm)
        new(k_arm, epsilon, initial, step_size, sample_averages, UCB_param,
            gradient, gradient_baseline, true_reward, indices, average_reward, time,
            q_true, q_estimation, action_count, best_action, action_prob)
    end
end

"""
    reset!(bandit::Bandit)

Reset the bandit to its initial state.
- Generate new true rewards for each arm.
- Reset estimations, counts, average reward, and time.
"""
function reset!(bandit::Bandit)
    bandit.q_true .= randn(bandit.k) .+ bandit.true_reward
    bandit.q_estimation .= fill(bandit.initial, bandit.k)
    bandit.action_count .= 0
    bandit.best_action = findmax(bandit.q_true)[2]
    bandit.time = 0
    bandit.average_reward = 0.0
    return nothing
end

"""
    act!(bandit::Bandit) -> Int

Select an action using one of the algorithms:
- With probability epsilon, choose randomly.
- If UCB_param is set, use UCB.
- If gradient is true, compute softmax probabilities.
- Otherwise, choose greedily.
"""
function act!(bandit::Bandit)
    if rand() < bandit.epsilon
        return rand(bandit.indices)
    end

    if bandit.UCB_param !== nothing
        # Avoid division by zero by adding a small constant
        UCB_estimation = bandit.q_estimation .+ bandit.UCB_param * sqrt.(log(bandit.time + 1) ./ (bandit.action_count .+ 1e-5))
        q_best = maximum(UCB_estimation)
        best_actions = findall(x -> x == q_best, UCB_estimation)
        return rand(best_actions)
    end

    if bandit.gradient
        exp_est = exp.(bandit.q_estimation)
        bandit.action_prob .= exp_est ./ sum(exp_est)
        # Use a Categorical distribution for weighted random choice.
        d = Categorical(bandit.action_prob)
        return rand(d)
    end

    # Default: greedy selection (break ties randomly)
    q_best = maximum(bandit.q_estimation)
    best_actions = findall(x -> x == q_best, bandit.q_estimation)
    return rand(best_actions)
end
"""
    step!(bandit::Bandit, action::Int) -> Float64

Take the specified action, generate a reward, and update the estimates.
- The reward is sampled from N(q_true[action], 1).
- Update time, action count, average reward, and estimations.
"""
function step!(bandit::Bandit, action::Int)
    reward = randn() + bandit.q_true[action]
    bandit.time += 1
    bandit.action_count[action] += 1
    bandit.average_reward += (reward - bandit.average_reward) / bandit.time

    if bandit.sample_averages
        # Update estimation using sample averages.
        bandit.q_estimation[action] += (reward - bandit.q_estimation[action]) / bandit.action_count[action]
    elseif bandit.gradient
        one_hot = zeros(length(bandit.q_estimation))
        one_hot[action] = 1.0
        baseline = bandit.gradient_baseline ? bandit.average_reward : 0.0
        # Gradient ascent update on the preferences.
        bandit.q_estimation .+= bandit.step_size * (reward - baseline) * (one_hot .- bandit.action_prob)
    else
        # Update with a constant step size.
        bandit.q_estimation[action] += bandit.step_size * (reward - bandit.q_estimation[action])
    end
    return reward
end
"""
    simulate(runs, time, bandits) -> (mean_best_action_counts, mean_rewards)

Simulate multiple runs for a set of bandits.
- `runs`: Number of independent runs.
- `time`: Number of time steps per run.
- `bandits`: A vector of Bandit objects.

Returns:
- mean_best_action_counts: Array of optimal action percentages (bandit x time).
- mean_rewards: Array of average rewards (bandit x time).
"""
function simulate(runs::Int, time::Int, bandits::Vector{Bandit})
    n = length(bandits)
    rewards = zeros(n, runs, time)
    best_action_counts = zeros(n, runs, time)
    for (i, bandit) in enumerate(bandits)
        for r in 1:runs
            reset!(bandit)
            for t in 1:time
                a = act!(bandit)
                rwd = step!(bandit, a)
                rewards[i, r, t] = rwd
                best_action_counts[i, r, t] = (a == bandit.best_action) ? 1.0 : 0.0
            end
        end
    end
    # Average over runs (dimension 2)
    mean_best_action_counts = dropdims(mean(best_action_counts, dims=2), dims=2)
    mean_rewards = dropdims(mean(rewards, dims=2), dims=2)
    return mean_best_action_counts, mean_rewards
end