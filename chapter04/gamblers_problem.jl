#=
Gambler's Problem
- Goal is to reach a certain amount of money, n
- Can bet any amount between 0 and current amount
- If a coin comes up heads, win that amount, if tails, lose that amount
- If you reach n, you win
- If you go broke, you lose
- Need to find optimal policy (starting at any state)
- Solve for any probability p of heads
=#
using Plots

"""
    Gambler's Problem
"""
mutable struct GamblersProblem
    n::Int              # Goal amount of money 
    p::Float64          # Probability of heads
    states::Vector{Int} # All possible states (amount of money)

    """
    Constructor
    """
    function GamblersProblem(n::Int, p::Float64)
        return new(n, p, collect(0:n))
    end
end

"""
    iterate(problem::GamblersProblem) -> value, policy, value_history, policy_history

Iterate for the value and policy functions
"""
function iterate(problem::GamblersProblem)
    value = zeros(problem.n + 1)
    policy = zeros(problem.n + 1)
    value[problem.n+1] = 1.0
    value_history = []
    policy_history = []

    # Value function iteration
    while true
        old_value = copy(value)
        push!(value_history, old_value)
        
        for state in problem.states[2:problem.n]  # Skip state 0 and goal state
            # Get possible actions for current state
            actions = collect(0:min(state, problem.n - state))
            action_returns = []
            
            for action in actions
                # Calculate expected return for each action
                push!(action_returns, 
                      problem.p * value[state + action + 1] + 
                      (1 - problem.p) * value[state - action + 1])
            end
            
            # Update value with maximum return
            value[state+1] = maximum(action_returns)
        end
        
        # Check for convergence
        delta = maximum(abs.(value - old_value))
        if delta < 1e-9
            push!(value_history, value)
            break
        end
    end
    
    # Compute the optimal policy
    for state in problem.states[2:problem.n]  # Skip state 0 and goal state
        actions = collect(0:min(state, problem.n - state))
        action_returns = []
        
        for action in actions
            push!(action_returns, 
                  problem.p * value[state + action + 1] + 
                  (1 - problem.p) * value[state - action + 1])
        end
        
        # Find the best action
        if length(actions) > 1
            # Skip the first action (which is 0) when finding the best non-zero action
            rounded_returns = round.(action_returns[2:end], digits=5)
            best_idx = argmax(rounded_returns) + 1  # +1 because we skipped the first action
            policy[state+1] = actions[best_idx]
        else
            policy[state+1] = 0
        end
    end
    
    return value, policy, value_history, policy_history
end




# Uncomment to run examples
#=
# Book example (p = 0.4)
problem04 = GamblersProblem(100, 0.4)
value04, policy04, value_history04, policy_history04 = iterate(problem04)

# Value function iterations
p04value = plot(title="Value Function Iterations for p = 0.4", xlabel="Capital", ylabel="Value", legend=:outertopright, background=:transparent)

max_iter = length(value_history04)
iterations_to_plot = [1, 2, 3, 10]

for i in iterations_to_plot
    plot!(p04value, problem04.states, value_history04[i], label="Iteration $i")
end
plot!(p04value, problem04.states, value_history04[end], label="Final Iteration $max_iter")

savefig(p04value, "cornell_theory_reading_group_RL/chapter04/gamblers_problem_value_p04.png")

# Final policy function
p04policy = plot(title="Policy Function for p = 0.4", xlabel="Capital", ylabel="Policy", legend=false, size=(800, 500), background=:transparent)
plot!(p04policy, problem04.states, policy04, marker=:circle, markersize=3)

savefig(p04policy, "cornell_theory_reading_group_RL/chapter04/gamblers_problem_policy_p04.png")

# p = 0.25
problem025 = GamblersProblem(100, 0.25)
value025, policy025, value_history025, policy_history025 = iterate(problem025)

# Value function iterations
p025value = plot(title="Value Function Iterations for p = 0.25", xlabel="Capital", ylabel="Value", legend=:outertopright, background=:transparent)

max_iter = length(value_history025)
iterations_to_plot = [1, 2, 3, 10]

for i in iterations_to_plot
    plot!(p025value, problem025.states, value_history025[i], label="Iteration $i")
end
plot!(p025value, problem025.states, value_history025[end], label="Final Iteration $max_iter")

savefig(p025value, "cornell_theory_reading_group_RL/chapter04/gamblers_problem_value_p025.png")

# Final policy function
p025policy = plot(title="Policy Function for p = 0.25", xlabel="Capital", ylabel="Policy", legend=false, size=(800, 500), background=:transparent)
plot!(p025policy, problem025.states, policy025, marker=:circle, markersize=3)

savefig(p025policy, "cornell_theory_reading_group_RL/chapter04/gamblers_problem_policy_p025.png")

# p = 0.5
problem05 = GamblersProblem(100, 0.5)
value05, policy05, value_history05, policy_history05 = iterate(problem05)

# Value function iterations
p05value = plot(title="Value Function Iterations for p = 0.5", xlabel="Capital", ylabel="Value", legend=:outertopright, background=:transparent)

max_iter = length(value_history05)
iterations_to_plot = [1, 2, 3, 10]

for i in iterations_to_plot
    plot!(p05value, problem05.states, value_history05[i], label="Iteration $i")
end
plot!(p05value, problem05.states, value_history05[end], label="Final Iteration $max_iter")

savefig(p05value, "cornell_theory_reading_group_RL/chapter04/gamblers_problem_value_p05.png")

# Final policy function
p05policy = plot(title="Policy Function for p = 0.5", xlabel="Capital", ylabel="Policy", legend=false, size=(800, 500), background=:transparent)
plot!(p05policy, problem05.states, policy05, marker=:circle, markersize=3)

savefig(p05policy, "cornell_theory_reading_group_RL/chapter04/gamblers_problem_policy_p05.png")

# p = 0.75
problem075 = GamblersProblem(100, 0.75)
value075, policy075, value_history075, policy_history075 = iterate(problem075)

# Value function iterations
p075value = plot(title="Value Function Iterations for p = 0.75", xlabel="Capital", ylabel="Value", legend=:outertopright, background=:transparent)

max_iter = length(value_history075)
iterations_to_plot = [1, 2, 3, 10]

for i in iterations_to_plot
    plot!(p075value, problem075.states, value_history075[i], label="Iteration $i")
end
plot!(p075value, problem075.states, value_history075[end], label="Final Iteration $max_iter")

savefig(p075value, "cornell_theory_reading_group_RL/chapter04/gamblers_problem_value_p075.png")

# Final policy function
p075policy = plot(title="Policy Function for p = 0.75", xlabel="Capital", ylabel="Policy", legend=false, size=(800, 500), background=:transparent)
plot!(p075policy, problem075.states, policy075, marker=:circle, markersize=3)

savefig(p075policy, "cornell_theory_reading_group_RL/chapter04/gamblers_problem_policy_p075.png")
=#