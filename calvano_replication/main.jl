using LinearAlgebra
using BenchmarkTools
using Statistics

# Include functions
include("functions.jl")

function main()
    # Set number of threads for parallel processing
    println("Running with $(Threads.nthreads()) threads")
    
    # Parameters
    a0 = 0.0
    mu = 0.25
    delta = 0.95
    xi = 0.1
    pn = 1.4729      # Nash price
    pm = 1.92498     # Monopoly price
    m = 15
    
    # Actions (prices)
    action_space = collect(LinRange(pn-xi*(pm-pn), pm+xi*(pm-pn), m))
    
    # Parameter sweep settings
    num_alphas = 15
    num_betas = 15
    alphas = collect(LinRange(0.1, 0.2, num_alphas))
    betas = collect(LinRange(0.000005, 0.000015, num_betas))
    
    # Output directory
    output_dir = "cornell_theory_reading_group_RL/calvano_slides/julia_output_delta"
    
    # Number of runs per parameter combination
    num_runs = 25
    
    println("Running parameter sweep with $num_runs runs per combination...")
    @time prices, avg_profit, profit_gain, convergence_counts = run_parameter_sweep(
        alphas, betas, action_space, mu, 0.95, a0, pn, pm; num_runs=num_runs
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
    save_results(prices, avg_profit, profit_gain, convergence_counts, alphas, betas, output_dir)
    
    println("Done!")
end

# Run!
main()