using LinearAlgebra
using Random
using Statistics

"""
    compute_profits(p1, p2, mu, a0)

Compute profits for a given pair of prices using sigmoid demand.
Returns a tuple of profits for both players.
"""
function compute_profits(p1, p2, mu, a0)
    # Calculate all exponential terms once
    exp1 = exp((2-p1)/mu)
    exp2 = exp((2-p2)/mu)
    exp0 = exp(a0/mu)
    denominator = exp1 + exp2 + exp0
    
    # Calculate demands for both players
    d1 = exp1 / denominator
    d2 = exp2 / denominator
    
    # Return profits for both players
    return ((p1-1)*d1, (p2-1)*d2)
end

"""
    initialize_q_values(action_space, mu, a0)

Initialize Q-values for both players.
Returns a tuple of two 3D arrays for player 1 and player 2.
"""
function initialize_q_values(action_space, mu, a0; delta=0.95)
    m = length(action_space)
    q_value_1 = zeros(Float64, m, m, m)
    q_value_2 = zeros(Float64, m, m, m)
    
    # Precompute profits for all action pairs to avoid redundant calculations
    profit_matrix = [compute_profits(action_space[a1], action_space[a2], mu, a0) for a1 in eachindex(action_space), a2 in eachindex(action_space)]
    
    # Vectorized initialization for player 1
    for s1 in axes(q_value_1, 1), s2 in axes(q_value_1, 2), a1 in axes(q_value_1, 3)
        q_value_1[s1, s2, a1] = mean([profit_matrix[a1, i][1] for i in eachindex(action_space)]) / (1-delta)
    end
    
    # Vectorized initialization for player 2
    for s1 in axes(q_value_2, 1), s2 in axes(q_value_2, 2), a2 in axes(q_value_2, 3)
        q_value_2[s1, s2, a2] = mean([profit_matrix[i, a2][2] for i in eachindex(action_space)]) / (1-delta)
    end
    
    return q_value_1, q_value_2
end

"""
    choose_action(state_idx, q_value, beta, time)

Choose action based on Q-values and time (epsilon-greedy).
Returns the index of the chosen action.
"""
function choose_action(state_idx, q_value, beta, time)
    epsilon = exp(-beta * time)
    actions_range = axes(q_value, 3)
    
    if rand() < epsilon
        # Random action
        return rand(actions_range)
    else
        # Greedy action - find actions with maximum Q-value
        values = @view q_value[state_idx[1], state_idx[2], :]
        max_val = maximum(values)
        max_indices = findall(x -> x == max_val, values)
        return rand(max_indices)
    end
end

"""
    q_learning(action_space, step_size, beta, mu, delta, a0; max_iterations=10_000_000, stability_threshold=100_000)

Run Q-learning algorithm with two agents playing against each other.
Returns the final state (prices), time to learn, and success flag.
"""
function q_learning(action_space, step_size, beta, mu, delta, a0; max_iterations=10_000_000, stability_threshold=100_000)
    # Get action space dimensions
    action_range = eachindex(action_space)
    
    # Initialize Q-values
    q_value_1, q_value_2 = initialize_q_values(action_space, mu, a0; delta=delta)
    
    # Initialize state randomly
    state_idx = [rand(action_range), rand(action_range)]
    state = [action_space[state_idx[1]], action_space[state_idx[2]]]
    
    time = 0
    action_idx = [0, 0]
    stay = 0
    
    # Pre-allocate arrays for performance
    next_state = similar(state)
    next_state_idx = similar(state_idx)
    last_state = similar(state)
    state_minus_two = similar(state)
    last_state .= zeros(2)
    state_minus_two .= zeros(2)
    reward = (0.0, 0.0)
    
    # Main learning loop
    while stay < stability_threshold && time < max_iterations
        time += 1
        
        # Choose actions for both players
        action_idx[1] = choose_action(state_idx, q_value_1, beta, time)
        action_idx[2] = choose_action(state_idx, q_value_2, beta, time)
        
        # Next state is determined by the actions
        next_state[1] = action_space[action_idx[1]]
        next_state[2] = action_space[action_idx[2]]
        
        # Check for stability
        if next_state == state || (next_state == last_state && state == state_minus_two)
            stay += 1
        else
            stay = 0
        end
        
        # Calculate rewards
        reward = compute_profits(next_state[1], next_state[2], mu, a0)
        
        # Map states to indices
        next_state_idx[1] = action_idx[1]
        next_state_idx[2] = action_idx[2]
        
        # Q-Learning update for player 1
        q_value_1[state_idx[1], state_idx[2], action_idx[1]] += step_size * (
            reward[1] + delta * maximum(q_value_1[next_state_idx[1], next_state_idx[2], :]) -
            q_value_1[state_idx[1], state_idx[2], action_idx[1]])
        
        # Q-Learning update for player 2
        q_value_2[state_idx[1], state_idx[2], action_idx[2]] += step_size * (
            reward[2] + delta * maximum(q_value_2[next_state_idx[1], next_state_idx[2], :]) -
            q_value_2[state_idx[1], state_idx[2], action_idx[2]])
        
        # Update state
        state_minus_two .= last_state
        last_state .= state
        state .= next_state
        state_idx .= next_state_idx
    end
    
    success = time < max_iterations
    return state, time, success
end

"""
    run_parameter_sweep(alphas, betas, action_space, mu, delta, a0, pn, pm; num_runs=5)

Run parameter sweep over alphas and betas to find optimal Q-learning parameters.
Performs multiple runs for each parameter combination and averages the results.
Returns matrices of prices, average profits, and profit gains.
"""
function run_parameter_sweep(alphas, betas, action_space, mu, delta, a0, pn, pm; num_runs=5)
    # Get dimensions for result arrays
    alpha_range = eachindex(alphas)
    beta_range = eachindex(betas)
    
    # Initialize result arrays
    prices = zeros(Float64, length(alphas), length(betas), 2)
    avg_profit = zeros(Float64, length(alphas), length(betas))
    profit_gain = zeros(Float64, length(alphas), length(betas))
    convergence_counts = zeros(Int, length(alphas), length(betas))
    
    # Nash and monopoly profits for reference
    nash_profit = compute_profits(pn, pn, mu, a0)[1]
    monopoly_profit = compute_profits(pm, pm, mu, a0)[1]
    profit_diff = monopoly_profit - nash_profit
    
    # Run parameter sweep
    Threads.@threads for i in alpha_range
        for j in beta_range
            # Arrays to store results from multiple runs
            run_prices = zeros(Float64, num_runs, 2)
            run_profits = zeros(Float64, num_runs)
            run_success = zeros(Bool, num_runs)
            
            # Run multiple times for each parameter combination
            for run in 1:num_runs
                # Run Q-learning with current parameters
                p_optimal, time_to_learn, success = q_learning(action_space, alphas[i], betas[j], mu, delta, a0)
                
                # Store results
                run_prices[run, 1] = p_optimal[1]
                run_prices[run, 2] = p_optimal[2]
                profits = compute_profits(p_optimal[1], p_optimal[2], mu, a0)
                run_profits[run] = (profits[1] + profits[2]) / 2
                run_success[run] = success
            end
            
            # Count convergences
            convergence_counts[i, j] = count(run_success)
            
            # Calculate averages based on convergence
            if convergence_counts[i, j] > 0
                # Use only converged runs
                converged_indices = findall(run_success)
                prices[i, j, 1] = mean(run_prices[converged_indices, 1])
                prices[i, j, 2] = mean(run_prices[converged_indices, 2])
                avg_profit[i, j] = mean(run_profits[converged_indices])
            else
                # Use all runs if none converged
                prices[i, j, 1] = mean(run_prices[:, 1])
                prices[i, j, 2] = mean(run_prices[:, 2])
                avg_profit[i, j] = mean(run_profits)
            end
            
            # Calculate profit gain
            profit_gain[i, j] = (avg_profit[i, j] - nash_profit) / profit_diff
            
            # Report results
            println("alpha: $(alphas[i]), beta: $(betas[j]), per-firm profit: $(avg_profit[i, j]), " *
                   "converged: $(convergence_counts[i, j])/$(num_runs)")
        end
    end
    
    return prices, avg_profit, profit_gain, convergence_counts
end

"""
    save_results(prices, avg_profit, profit_gain, convergence_counts, alphas, betas, output_dir)

Save results to CSV files.
"""
function save_results(prices, avg_profit, profit_gain, convergence_counts, alphas, betas, output_dir)
    # Create output directory if it doesn't exist
    mkpath(output_dir)
    
    # Save matrices
    open(joinpath(output_dir, "profit_gain.csv"), "w") do io
        for i in axes(profit_gain, 1)
            println(io, join(profit_gain[i, :], ","))
        end
    end
    
    open(joinpath(output_dir, "avg_profit.csv"), "w") do io
        for i in axes(avg_profit, 1)
            println(io, join(avg_profit[i, :], ","))
        end
    end
    
    open(joinpath(output_dir, "prices_0.csv"), "w") do io
        for i in axes(prices, 1)
            println(io, join(view(prices, i, :, 1), ","))
        end
    end
    
    open(joinpath(output_dir, "prices_1.csv"), "w") do io
        for i in axes(prices, 1)
            println(io, join(view(prices, i, :, 2), ","))
        end
    end
    
    # Save convergence counts
    open(joinpath(output_dir, "convergence_counts.csv"), "w") do io
        for i in axes(convergence_counts, 1)
            println(io, join(convergence_counts[i, :], ","))
        end
    end
    
    # Save parameter vectors
    open(joinpath(output_dir, "alphas.csv"), "w") do io
        println(io, "alphas")
        for alpha in alphas
            println(io, alpha)
        end
    end
    
    open(joinpath(output_dir, "betas.csv"), "w") do io
        println(io, "betas")
        for beta in betas
            println(io, beta)
        end
    end
end