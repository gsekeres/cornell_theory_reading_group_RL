#=
This problem:
- Two locations
- Each car rental nets y
- Cars can be moved overnight for c
- The cars requested and returned are Poisson distributed
- There is a maximum number of cars per location
- There is a discount rate

We formulate this as a Markov Decision Process (MDP), where:
- The state is the number of cars at each location at the end of each day
- The action is the number of cars to move from one location to another
- The time step is one day
- The reward is profit
=#

# NOTE LOGIC CURRENTLY BROKEN, FIXING WHEN TIME - Gabe

using Plots, Distributions, Printf

"""
Car Rental Problem struct with all configurable parameters
"""
mutable struct CarRentalProblem
    # Reward for each car rental
    rental_credit::Float64
    
    # Cost per car moved
    move_car_cost::Float64
    
    # Maximum cars per location
    max_cars::Int
    
    # Discount factor
    gamma::Float64
    
    # Expected rental requests at locations
    rental_request_first_loc::Float64
    rental_request_second_loc::Float64
    
    # Expected returns at locations
    returns_first_loc::Float64
    returns_second_loc::Float64

    function CarRentalProblem(;
        rental_credit::Float64=10.0,
        move_car_cost::Float64=2.0,
        max_cars::Int=20,
        gamma::Float64=0.9,
        rental_request_first_loc::Float64=3.0,
        rental_request_second_loc::Float64=4.0,
        returns_first_loc::Float64=3.0,
        returns_second_loc::Float64=2.0)
        
        new(rental_credit, move_car_cost, max_cars, gamma, 
            rental_request_first_loc, rental_request_second_loc, 
            returns_first_loc, returns_second_loc)
    end
end

"""
    create_poisson_cache(problem::CarRentalProblem, poisson_upper_bound::Int=11)

Create a cache for Poisson probabilities to avoid recalculating them
"""
function create_poisson_cache(problem::CarRentalProblem, poisson_upper_bound::Int=11)
    # Create distributions
    rental_first_dist = Poisson(problem.rental_request_first_loc)
    rental_second_dist = Poisson(problem.rental_request_second_loc)
    returns_first_dist = Poisson(problem.returns_first_loc)
    returns_second_dist = Poisson(problem.returns_second_loc)
    
    # Create caches
    rental_first_cache = [pdf(rental_first_dist, n) for n in 0:poisson_upper_bound-1]
    rental_second_cache = [pdf(rental_second_dist, n) for n in 0:poisson_upper_bound-1]
    returns_first_cache = [pdf(returns_first_dist, n) for n in 0:poisson_upper_bound-1]
    returns_second_cache = [pdf(returns_second_dist, n) for n in 0:poisson_upper_bound-1]
    
    return (
        rental_first_cache=rental_first_cache, 
        rental_second_cache=rental_second_cache,
        returns_first_cache=returns_first_cache, 
        returns_second_cache=returns_second_cache,
        poisson_upper_bound=poisson_upper_bound
    )
end

"""
    expected_return(
        problem::CarRentalProblem, 
        state::Tuple{Int, Int}, 
        action::Int, 
        state_value::Matrix{Float64},
        poisson_cache::NamedTuple,
        constant_returned_cars::Bool=true
    )

Calculate expected return for a given state and action
"""
function expected_return(
    problem::CarRentalProblem, 
    state::Tuple{Int, Int}, 
    action::Int, 
    state_value::Matrix{Float64},
    poisson_cache::NamedTuple,
    constant_returned_cars::Bool=true
)
    # Initialize total return
    returns = 0.0
    
    # Cost for moving cars
    returns -= problem.move_car_cost * abs(action)
    
    # Moving cars (after action)
    num_cars_first_loc = min(state[1] - action, problem.max_cars)
    num_cars_second_loc = min(state[2] + action, problem.max_cars)
    
    # Extract the Poisson upper bound from the cache
    poisson_upper_bound = poisson_cache.poisson_upper_bound
    
    # Go through all possible rental requests
    for rental_request_first_loc in 0:(poisson_upper_bound-1)
        for rental_request_second_loc in 0:(poisson_upper_bound-1)
            # Probability for current combination of rental requests
            prob = poisson_cache.rental_first_cache[rental_request_first_loc+1] * 
                   poisson_cache.rental_second_cache[rental_request_second_loc+1]
            
            # Current car counts
            cars_first = num_cars_first_loc
            cars_second = num_cars_second_loc
            
            # Valid rental requests should be less than actual number of cars
            valid_rental_first_loc = min(cars_first, rental_request_first_loc)
            valid_rental_second_loc = min(cars_second, rental_request_second_loc)
            
            # Get credits for renting
            reward = (valid_rental_first_loc + valid_rental_second_loc) * problem.rental_credit
            cars_first -= valid_rental_first_loc
            cars_second -= valid_rental_second_loc
            
            if constant_returned_cars
                # Get returned cars (constant case)
                returned_cars_first_loc = problem.returns_first_loc
                returned_cars_second_loc = problem.returns_second_loc
                
                # Update car counts with returns (bounded by max_cars)
                new_cars_first = min(Int(round(cars_first + returned_cars_first_loc)), problem.max_cars)
                new_cars_second = min(Int(round(cars_second + returned_cars_second_loc)), problem.max_cars)
                
                # Calculate expected future reward
                future_value = state_value[new_cars_first+1, new_cars_second+1]
                returns += prob * (reward + problem.gamma * future_value)
            else
                # Handle stochastic returns
                for returned_cars_first_loc in 0:(poisson_upper_bound-1)
                    for returned_cars_second_loc in 0:(poisson_upper_bound-1)
                        # Probability of this return combination
                        prob_return = poisson_cache.returns_first_cache[returned_cars_first_loc+1] * 
                                      poisson_cache.returns_second_cache[returned_cars_second_loc+1]
                        
                        # Update car counts with returns (bounded by max_cars)
                        new_cars_first = min(cars_first + returned_cars_first_loc, problem.max_cars)
                        new_cars_second = min(cars_second + returned_cars_second_loc, problem.max_cars)
                        
                        # Combined probability
                        joint_prob = prob_return * prob
                        
                        # Calculate expected future reward
                        future_value = state_value[new_cars_first+1, new_cars_second+1]
                        returns += joint_prob * (reward + problem.gamma * future_value)
                    end
                end
            end
        end
    end
    
    return returns
end

"""
    policy_iteration(problem::CarRentalProblem; 
                    constant_returned_cars::Bool=true, 
                    verbose::Bool=true,
                    poisson_upper_bound::Int=11)

Policy iteration for the car rental problem
"""
function policy_iteration(problem::CarRentalProblem; 
                          constant_returned_cars::Bool=true, 
                          verbose::Bool=true,
                          poisson_upper_bound::Int=11)
    # Initialize state value and policy matrices
    value = zeros(problem.max_cars + 1, problem.max_cars + 1)
    policy = zeros(Int, problem.max_cars + 1, problem.max_cars + 1)
    
    # All possible actions
    actions = collect(-problem.max_cars:problem.max_cars)
    
    # Create Poisson probability cache
    poisson_cache = create_poisson_cache(problem, poisson_upper_bound)
    
    # Store policies for visualization
    policy_history = [copy(policy)]
    
    # Policy iteration
    iterations = 0
    while true
        iterations += 1
        if verbose
            println("Iteration $iterations")
        end
        
        # Policy evaluation (in-place)
        while true
            old_value = copy(value)
            
            for i in 0:problem.max_cars
                for j in 0:problem.max_cars
                    value[i+1, j+1] = expected_return(
                        problem, (i, j), policy[i+1, j+1], value, poisson_cache, constant_returned_cars
                    )
                end
            end
            
            max_value_change = maximum(abs.(old_value .- value))
            if verbose
                println("  Max value change: $max_value_change")
            end
            
            if max_value_change < 1e-4
                break
            end
        end
        
        # Policy improvement
        policy_stable = true
        
        for i in 0:problem.max_cars
            for j in 0:problem.max_cars
                old_action = policy[i+1, j+1]
                action_returns = Float64[]
                
                for action in actions
                    if (0 <= action <= i) || (-j <= action <= 0)
                        push!(action_returns, expected_return(
                            problem, (i, j), action, value, poisson_cache, constant_returned_cars
                        ))
                    else
                        push!(action_returns, -Inf)
                    end
                end
                
                best_action_idx = argmax(action_returns)
                new_action = actions[best_action_idx]
                policy[i+1, j+1] = new_action
                
                if policy_stable && old_action != new_action
                    policy_stable = false
                end
            end
        end
        
        if verbose
            println("  Policy stable: $policy_stable")
        end
        
        # Store the current policy for visualization
        push!(policy_history, copy(policy))
        
        if policy_stable
            break
        end
    end
    
    return value, policy, policy_history
end

"""
    visualize_policy_iteration(problem::CarRentalProblem, policy_history, value)

Generate heatmap visualizations for the policy iteration process
"""
function visualize_policy_iteration(problem::CarRentalProblem, policy_history, value)
    n_policies = length(policy_history)
    n_plots = n_policies + 1  # +1 for the final value function
    
    # Determine grid layout
    n_rows = min(2, n_plots)
    n_cols = ceil(Int, n_plots / n_rows)
    
    plt = plot(layout=(n_rows, n_cols), size=(300*n_cols, 250*n_rows), legend=false)
    
    # Plot policies
    for (i, policy) in enumerate(policy_history)
        # Flip policy matrix for visualization (to match the Python version)
        policy_display = reverse(policy, dims=1)
        
        # Create heatmap
        heatmap!(plt[i], policy_display, 
                 title="Policy $(i-1)",
                 xlabel="# cars at second location", 
                 ylabel="# cars at first location",
                 color=:YlGnBu,
                 colorbar=true,
                 xticks=0:5:problem.max_cars,
                 yticks=0:5:problem.max_cars,
                 yflip=false)
    end
    
    # Plot optimal value function
    value_display = reverse(value, dims=1)
    heatmap!(plt[n_plots], value_display, 
             title="Optimal Value",
             xlabel="# cars at second location", 
             ylabel="# cars at first location",
             color=:YlGnBu,
             colorbar=true,
             xticks=0:5:problem.max_cars,
             yticks=0:5:problem.max_cars,
             yflip=false)
    
    return plt
end

"""
Run the car rental problem with visualization
"""
function run_car_rental_problem(; 
    problem=CarRentalProblem(), 
    constant_returned_cars=true, 
    verbose=true,
    poisson_upper_bound=11,
    save_plot=true,
    filename="figure_4_2.png"
)
    value, policy, policy_history = policy_iteration(
        problem, 
        constant_returned_cars=constant_returned_cars, 
        verbose=verbose,
        poisson_upper_bound=poisson_upper_bound
    )
    
    plt = visualize_policy_iteration(problem, policy_history, value)
    
    if save_plot
        savefig(plt, filename)
    end
    
    return plt, value, policy, policy_history
end




# Uncomment to run examples

# Create problem with default parameters (matching the textbook example)
problem = CarRentalProblem()
    
# Run policy iteration with visualization
plt, value, policy, policy_history = run_car_rental_problem(
    problem=problem, 
    verbose=true,
    poisson_upper_bound=11
)
    
# Display some statistics about the optimal policy
println("\nOptimal policy statistics:")
println("  Number of iterations until convergence: $(length(policy_history) - 1)")
println("  Maximum movement of cars in optimal policy: $(maximum(abs.(policy)))")
    
# Save the plot
savefig(plt, "cornell_theory_reading_group_RL/chapter04/car_rental_policy_iteration.png")
