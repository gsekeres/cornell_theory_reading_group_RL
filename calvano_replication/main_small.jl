using LinearAlgebra
using BenchmarkTools
using Statistics

# Include functions
include("functions.jl")

function main_small(delta, num_alphas, num_betas, alphamin, alphamax, betamin, betamax, output_dir, num_runs, specification; sens=1)
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
        alphas, betas, action_space, mu, delta, a0, pn, pm; num_runs=num_runs, specification, sens=sens
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
main_small(0.95, 25, 25, 0.025, 0.25, 0.0000008, 0.00002, "output_small_main_delta_0.95", 25, "calvano", sens=1)

# Q-learning specification, without delta.
main_small(0.0, 25, 25, 0.025, 0.25, 0.0000008, 0.00002, "output_small_main_delta_0.0", 25, "calvano", sens=1)

# Full feedback specification
main_small(0.95, 25, 25, 0.025, 0.25, 0.0000008, 0.00002, "output_small_full_feedback", 25, "full_feedback", sens=1)

# SARSA specification
main_small(0.95, 25, 25, 0.025, 0.25, 0.0000008, 0.00002, "output_small_sarsa", 25, "sarsa", sens=1)

# Q-learning specification, Q_0 = 0 initially
main_small(0.95, 25, 25, 0.025, 0.25, 0.0000008, 0.00002, "output_small_zero", 25, "sensitive", sens=0)

# Test Q-learning, 0.8*Q_0
main_small(0.95, 25, 25, 0.025, 0.25, 0.0000008, 0.00002, "output_small_0.8", 25, "sensitive", sens=0.8)

# Test Q-learning, 0.9*Q_0
main_small(0.95, 25, 25, 0.025, 0.25, 0.0000008, 0.00002, "output_small_0.9", 25, "sensitive", sens=0.9)

# Test Q-learning, 1.1*Q_0
main_small(0.95, 25, 25, 0.025, 0.25, 0.0000008, 0.00002, "output_small_1.1", 25, "sensitive", sens=1.1)

# Test Q-learning, 1.2*Q_0
main_small(0.95, 25, 25, 0.025, 0.25, 0.0000008, 0.00002, "output_small_1.2", 25, "sensitive", sens=1.2)