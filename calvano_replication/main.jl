using LinearAlgebra
using BenchmarkTools
using Statistics

# Include functions
include("functions.jl")

function main(delta, num_alphas, num_betas, alphamin, alphamax, betamin, betamax, output_dir, num_runs, specification)
    # Set number of threads for parallel processing
    println("Running with $(Threads.nthreads()) threads")
    
    # Parameters
    a0 = 0.0
    mu = 0.25
    xi = 0.1
    pn = 1.4729      # Nash price
    pm = 1.92498     # Monopoly price
    m = 15
    
    # Actions (prices)
    action_space = collect(LinRange(pn-xi*(pm-pn), pm+xi*(pm-pn), m))
    
    # Parameter sweep settings
    alphas = collect(LinRange(alphamin, alphamax, num_alphas))
    betas = collect(LinRange(betamin, betamax, num_betas))
    
    println("Running parameter sweep with $num_runs runs per combination...")
    println("Specification: $specification")
    @time prices, avg_profit, profit_gain, convergence_counts, all_prices, all_success = run_parameter_sweep(
        alphas, betas, action_space, mu, delta, a0, pn, pm; num_runs=num_runs, specification
    )
    
    # Calculate and print convergence statistics
    total_combinations = length(alphas) * length(betas)
    fully_converged = count(x -> x == num_runs, convergence_counts)
    partially_converged = count(x -> x > 0 && x < num_runs, convergence_counts)
    not_converged = count(x -> x == 0, convergence_counts)
    
    println("\nConvergence Statistics:")
    println("  Fully converged: $fully_converged/$total_combinations ($(round(fully_converged/total_combinations*100, digits=1))%)")
    println("  Partially converged: $partially_converged/$total_combinations ($(round(partially_converged/total_combinations*100, digits=1))%)")
    println("  Not converged: $not_converged/$total_combinations ($(round(not_converged/total_combinations*100, digits=1))%)")
    
    println("\nSaving results...")
    save_results(prices, avg_profit, profit_gain, convergence_counts, all_prices, all_success, alphas, betas, output_dir)
    
    println("Done!")
end

# Run!

# Q-learning specification, base in Calvano
main(0.95, 100, 100, 0.025, 0.25, 0.0000000000000000001, 0.00002, "output_main_delta_0.95", 100, "calvano")

# Q-learning specification, without delta.
main(0.0, 100, 100, 0.025, 0.25, 0.0000000000000000001, 0.00002, "output_main_delta_0.0", 100, "calvano")

# Full feedback specification
main(0.95, 100, 100, 0.025, 0.25, 0.0000000000000000001, 0.00002, "output_full_feedback", 100, "full_feedback")

# SARSA specification
main(0.95, 100, 100, 0.025, 0.25, 0.0000000000000000001, 0.00002, "output_sarsa", 100, "sarsa")

# Q-learning specification, Q_0 = 0 initially
main(0.95, 100, 100, 0.025, 0.25, 0.0000000000000000001, 0.00002, "output_zeros", 100, "zeros")