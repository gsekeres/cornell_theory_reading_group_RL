using Plots, LaTeXStrings, Measures, CSV, DataFrames, LinearAlgebra, Statistics, StatsBase

f(x, y) = ((x-1) * exp(8-4x)) / (exp(8-4x) + exp(8-4y) + 1)

# Create a range for x and y values (positive orthant)
x = range(1, stop=2, length=100)
y = range(1, stop=2, length=100)

# Create the 3D plot
p = plot(x, y, (x,y) -> f(x,y), 
    st=:surface, 
    color=:thermal, 
    xlabel=L"p_1", 
    ylabel=L"p_2", 
    zlabel=L"\textrm{Payoff}",
    size=(800, 600),
    legend=false,
    background=:transparent,
    camera=(30, 30))  # Adjust the viewing angle

# Define the specific points
points_x = [1.473, 1.473, 1.925, 1.925]
points_y = [1.473, 1.925, 1.473, 1.925]
points_z = [f(points_x[i], points_y[i]) for i in 1:4]

# Add the points to the plot
scatter!(p, points_x, points_y, points_z, 
    color=:black, 
    markersize=4,
    markerstrokewidth=0)
# Save the plot as a PNG
savefig(p, "cornell_theory_reading_group_RL/calvano_slides/cont_plot.png")

p_min = 1.473 - 0.1 * (1.925 - 1.473)
p_max = 1.925 + 0.1 * (1.925 - 1.473)

# Create 15 equally spaced points in the range
p_values = range(p_min, stop=p_max, length=15)

# Create a grid to store payoffs
payoffs = zeros(15, 15)

# Fill the payoff grid
for i in 1:15
    for j in 1:15
        p1 = p_values[i]  # Player 1's choice (rows)
        p2 = p_values[j]  # Player 2's choice (columns)
        payoffs[i, j] = f(p1, p2)
    end
end
# Define the specific points we want to highlight
key_points = [1.473, 1.925]

# Find the closest indices in our grid to these points
key_indices = []
for point in key_points
    idx = argmin(abs.(p_values .- point))
    push!(key_indices, idx)
end

# Format price values for labeling
formatted_prices = [string(round(p, digits=2)) for p in p_values]

# Create the heatmap with proper ticks and labels
heatmap_plot = heatmap(
    1:15, 1:15, payoffs,  # Use indices instead of values
    color=:thermal,
    xlabel=L"p_2",
    ylabel=L"p_1",
    aspect_ratio=:equal,
    margin=0mm,
    xticks=(1:15, formatted_prices),
    yticks=(1:15, formatted_prices),
    legend=false,
    tickdirection=:none,
    size=(800, 700),
    background=:transparent,
    grid=false,
    yaxis=:flip,
    framestyle=:none  # Keep the frame but not the ticks
)
annotate!(heatmap_plot, 8, -0.8, text(L"p_2", 10))
annotate!(heatmap_plot, -0.8, 8, text(L"p_1", 10, rotation=90))
for i in 1:15
    annotate!(heatmap_plot, i, 0, text(formatted_prices[i], 6, :top, rotation=45))
end

# Add y-axis (left) price labels
for i in 1:15
    annotate!(heatmap_plot, 0, i, text(formatted_prices[i], 6, :right))
end

# Add the four key points as dots
for i in key_indices
    for j in key_indices
        scatter!(heatmap_plot, [j], [i], 
            markersize=6, 
            markercolor=:black,
            markerstrokewidth=0)
    end
end

# Save the heatmap as PNG
savefig(heatmap_plot, "cornell_theory_reading_group_RL/calvano_slides/heatmap_plot.png")


# Plot converged profit gain FROM A SINGLE RUN of PYTHON CODE
# Read the CSV files
profit_gain_df = CSV.read("cornell_theory_reading_group_RL/calvano_slides/profit_gain_gs.csv", DataFrame, header=false)
alphas_df = CSV.read("cornell_theory_reading_group_RL/calvano_slides/alphas.csv", DataFrame, header=true)
betas_df = CSV.read("cornell_theory_reading_group_RL/calvano_slides/betas.csv", DataFrame, header=true)

# Convert DataFrames to matrices
profit_gain = Matrix(profit_gain_df)
alphas = alphas_df[:, 1]
betas = betas_df[:, 1]
alpha_labels = [string(round(a, digits=3)) for a in alphas]
beta_labels = [string(round(b * 10^5, digits=3)) for b in betas]

alphas_reversed = reverse(alphas)  # This will have 0.2 as the first element
alpha_labels_reversed = reverse(alpha_labels)
profit_gain_reversed = reverse(profit_gain, dims=1)  # Reverse rows

# Create the heatmap
heatmap_profit_gain = heatmap(
    1:size(profit_gain_reversed, 2), 1:size(profit_gain_reversed, 1), profit_gain_reversed,
    color=:thermal,
    ylabel=L"\alpha",
    xlabel=L"\beta \times 10^{5}",
    aspectratio=:equal,
    margin=0mm,
    xticks=(1:15, beta_labels),
    yticks=(1:15, alpha_labels),
    tickdirection=:none,
    size=(800, 700),
    background=:transparent,
    grid=false,
    framestyle=:none  # Keep the frame but not the ticks
)
annotate!(heatmap_profit_gain, 8, -0.8, text(L"\beta \times 10^{5}", 10))
annotate!(heatmap_profit_gain, -0.8, 8, text(L"\alpha", 10, rotation=90))
for i in 1:15
    annotate!(heatmap_profit_gain, i, 0, text(beta_labels[i], 6, :top, rotation=45))
    annotate!(heatmap_profit_gain, 0, i, text(alpha_labels[i], 6, :right))
end


savefig(heatmap_profit_gain, "cornell_theory_reading_group_RL/calvano_slides/heatmap_profit_gain_once.png")


# Plot converged profit gain from 25 runs of Julia code (no 1-delta in denom)
profit_gain_df = CSV.read("cornell_theory_reading_group_RL/calvano_slides/julia_output/profit_gain.csv", DataFrame, header=false)
alphas_df = CSV.read("cornell_theory_reading_group_RL/calvano_slides/julia_output/alphas.csv", DataFrame, header=true)
betas_df = CSV.read("cornell_theory_reading_group_RL/calvano_slides/julia_output/betas.csv", DataFrame, header=true)

profit_gain = Matrix(profit_gain_df)
alphas = alphas_df[:, 1]
betas = betas_df[:, 1]

alpha_labels = [string(round(a, digits=3)) for a in alphas]
beta_labels = [string(round(b * 10^5, digits=3)) for b in betas]

alphas_reversed = reverse(alphas)  # This will have 0.2 as the first element
alpha_labels_reversed = reverse(alpha_labels)
profit_gain_reversed = reverse(profit_gain, dims=1)  # Reverse rows


heatmap_profit_gain = heatmap(
    1:size(profit_gain_reversed, 2), 1:size(profit_gain_reversed, 1), profit_gain_reversed,
    color=:blues,
    ylabel=L"\alpha",
    xlabel=L"\beta \times 10^{5}",
    aspectratio=:equal,
    margin=0mm,
    xticks=(1:15, beta_labels),
    yticks=(1:15, alpha_labels),
    tickdirection=:none,
    size=(800, 700),
    background=:transparent,
    grid=false,
    framestyle=:none  # Keep the frame but not the ticks
)
annotate!(heatmap_profit_gain, 8, -0.8, text(L"\beta \times 10^{5}", 10))
annotate!(heatmap_profit_gain, -0.8, 8, text(L"\alpha", 10, rotation=90))
for i in 1:15
    annotate!(heatmap_profit_gain, i, 0, text(beta_labels[i], 6, :top, rotation=45))
    annotate!(heatmap_profit_gain, 0, i, text(alpha_labels[i], 6, :right))
end

savefig(heatmap_profit_gain, "cornell_theory_reading_group_RL/calvano_slides/heatmap_profit_gain_25_nodelta.png")

# Plot converged profit gain from 25 runs of Julia code (with 1-delta in denom)
profit_gain_df = CSV.read("cornell_theory_reading_group_RL/calvano_slides/julia_output_delta/profit_gain.csv", DataFrame, header=false)
alphas_df = CSV.read("cornell_theory_reading_group_RL/calvano_slides/julia_output_delta/alphas.csv", DataFrame, header=true)
betas_df = CSV.read("cornell_theory_reading_group_RL/calvano_slides/julia_output_delta/betas.csv", DataFrame, header=true)

profit_gain = Matrix(profit_gain_df)
alphas = alphas_df[:, 1]
betas = betas_df[:, 1]

alpha_labels = [string(round(a, digits=3)) for a in alphas]
beta_labels = [string(round(b * 10^5, digits=3)) for b in betas]

alphas_reversed = reverse(alphas)  # This will have 0.2 as the first element
alpha_labels_reversed = reverse(alpha_labels)
profit_gain_reversed = reverse(profit_gain, dims=1)  # Reverse rows

heatmap_profit_gain = heatmap(
    1:size(profit_gain_reversed, 2), 1:size(profit_gain_reversed, 1), profit_gain_reversed,
    color=:blues,
    ylabel=L"\alpha",
    xlabel=L"\beta \times 10^{5}",
    aspectratio=:equal,
    margin=0mm,
    xticks=(1:15, beta_labels),
    yticks=(1:15, alpha_labels),
    tickdirection=:none,
    size=(800, 700),
    background=:transparent,
    grid=false,
    framestyle=:none  # Keep the frame but not the ticks
)
annotate!(heatmap_profit_gain, 8, -0.8, text(L"\beta \times 10^{5}", 10))
annotate!(heatmap_profit_gain, -0.8, 8, text(L"\alpha", 10, rotation=90))
for i in 1:15
    annotate!(heatmap_profit_gain, i, 0, text(beta_labels[i], 6, :top, rotation=45))
    annotate!(heatmap_profit_gain, 0, i, text(alpha_labels[i], 6, :right))
end

savefig(heatmap_profit_gain, "cornell_theory_reading_group_RL/calvano_slides/heatmap_profit_gain_25_delta.png")

convergence_counts_df = CSV.read("cornell_theory_reading_group_RL/calvano_slides/julia_output_delta/convergence_counts.csv", DataFrame, header=false)
convergence_counts = Matrix(convergence_counts_df)
alphas_df = CSV.read("cornell_theory_reading_group_RL/calvano_slides/julia_output_delta/alphas.csv", DataFrame, header=true)
betas_df = CSV.read("cornell_theory_reading_group_RL/calvano_slides/julia_output_delta/betas.csv", DataFrame, header=true)

alphas = alphas_df[:, 1]
betas = betas_df[:, 1]

# Format the labels to be more readable
alpha_labels = [string(round(a, digits=3)) for a in alphas]
beta_labels = [string(round(b * 10^5, digits=3)) for b in betas]

alphas_reversed = reverse(alphas)  # This will have 0.2 as the first element
alpha_labels_reversed = reverse(alpha_labels)
convergence_counts_reversed = reverse(convergence_counts, dims=1)  # Reverse rows


heatmap_convergence_counts = heatmap(
    1:size(convergence_counts_reversed, 2), 1:size(convergence_counts_reversed, 1), convergence_counts_reversed,
    color=:thermal,
    ylabel=L"\alpha",
    xlabel=L"\beta \times 10^{5}",
    aspectratio=:equal,
    margin=0mm,
    xticks=(1:15, beta_labels),
    yticks=(1:15, alpha_labels),
    tickdirection=:none,
    size=(800, 700),
    background=:transparent,
    grid=false,
    framestyle=:none  # Keep the frame but not the ticks
)
annotate!(heatmap_convergence_counts, 8, -0.8, text(L"\beta \times 10^{5}", 10))
annotate!(heatmap_convergence_counts, -0.8, 8, text(L"\alpha", 10, rotation=90))
for i in 1:15
    annotate!(heatmap_convergence_counts, i, 0, text(beta_labels[i], 6, :top, rotation=45))
    annotate!(heatmap_convergence_counts, 0, i, text(alpha_labels[i], 6, :right))
end

savefig(heatmap_convergence_counts, "cornell_theory_reading_group_RL/calvano_slides/heatmap_convergence_counts_25_delta.png")



convergence_counts_df = CSV.read("cornell_theory_reading_group_RL/calvano_slides/julia_output/convergence_counts.csv", DataFrame, header=false)
convergence_counts = Matrix(convergence_counts_df)
alphas_df = CSV.read("cornell_theory_reading_group_RL/calvano_slides/julia_output_delta/alphas.csv", DataFrame, header=true)
betas_df = CSV.read("cornell_theory_reading_group_RL/calvano_slides/julia_output_delta/betas.csv", DataFrame, header=true)

alphas = alphas_df[:, 1]
betas = betas_df[:, 1]

# Format the labels to be more readable
alpha_labels = [string(round(a, digits=3)) for a in alphas]
beta_labels = [string(round(b * 10^5, digits=3)) for b in betas]

alphas_reversed = reverse(alphas)  # This will have 0.2 as the first element
alpha_labels_reversed = reverse(alpha_labels)
convergence_counts_reversed = reverse(convergence_counts, dims=1)  # Reverse rows
heatmap_convergence_counts = heatmap(
    1:size(convergence_counts_reversed, 2), 1:size(convergence_counts_reversed, 1), convergence_counts_reversed,
    color=:thermal,
    ylabel=L"\alpha",
    xlabel=L"\beta \times 10^{5}",
    aspectratio=:equal,
    margin=0mm,
    xticks=(1:15, beta_labels),
    yticks=(1:15, alpha_labels),
    tickdirection=:none,
    size=(800, 700),
    background=:transparent,
    grid=false,
    framestyle=:none  # Keep the frame but not the ticks
)
annotate!(heatmap_convergence_counts, 8, -0.8, text(L"\beta \times 10^{5}", 10))
annotate!(heatmap_convergence_counts, -0.8, 8, text(L"\alpha", 10, rotation=90))
for i in 1:15
    annotate!(heatmap_convergence_counts, i, 0, text(beta_labels[i], 6, :top, rotation=45))
    annotate!(heatmap_convergence_counts, 0, i, text(alpha_labels[i], 6, :right))
end

savefig(heatmap_convergence_counts, "cornell_theory_reading_group_RL/calvano_slides/heatmap_convergence_counts_25_nodelta.png")



















# Plot outputs from all small runs
types = ["main_delta_0.95", "main_delta_0.0", "full_feedback", "sarsa", "zero", "0.8", "0.9", "1.1", "1.2"]

alphas_df_new = CSV.read("cornell_theory_reading_group_RL/output_data/output_small_zero/alphas.csv", DataFrame, header=true)
betas_df_new = CSV.read("cornell_theory_reading_group_RL/output_data/output_small_zero/betas.csv", DataFrame, header=true)

alphas_new = alphas_df_new[:, 1]
betas_new = betas_df_new[:, 1]

alpha_labels_new = [string(round(a, digits=3)) for a in alphas_new]
beta_labels_new = [string(round(b * 10^5, digits=3)) for b in betas_new]



for type in types
    local profit_gain_df = CSV.read("cornell_theory_reading_group_RL/output_data/output_small_$(type)/profit_gain.csv", DataFrame, header=false)
    local convergence_counts_df = CSV.read("cornell_theory_reading_group_RL/output_data/output_small_$(type)/convergence_counts.csv", DataFrame, header=false)
    local profit_gain = reverse(Matrix(profit_gain_df), dims=1)
    local convergence_counts = reverse(Matrix(convergence_counts_df), dims=1)    
    convergence_counts = convergence_counts ./ 25
    for i in eachindex(profit_gain)
        if convergence_counts[i] == 0
            profit_gain[i] = NaN
        end
    end

    local heatmap_profit_gain = heatmap(
        1:size(profit_gain, 2), 1:size(profit_gain, 1), profit_gain, 
        color=:thermal, 
        ylabel=L"\alpha", 
        xlabel=L"\beta \times 10^{5}", 
        margin=0mm, 
        xticks=(1:15, beta_labels_new),
        yticks=(1:15, alpha_labels_new),
        tickdirection=:none,
        clim=(-0.023764994861794113, 1),
        size=(1000, 700),
        background=:transparent,
        grid=false, 
        framestyle=:none)
    annotate!(heatmap_profit_gain, size(profit_gain, 2) / 2, -0.8, text(L"\beta \times 10^{5}", 10))
    annotate!(heatmap_profit_gain, -0.8, size(profit_gain, 1) / 2, text(L"\alpha", 10, rotation=90))
    for i in 1:size(profit_gain, 2)
        annotate!(heatmap_profit_gain, i, 0, text(beta_labels_new[i], 6, :top, rotation=45))
        annotate!(heatmap_profit_gain, 0, i, text(alpha_labels_new[i], 6, :right))
    end
    savefig(heatmap_profit_gain, "cornell_theory_reading_group_RL/calvano_slides/heatmap_profit_gain_small_$(type).png")

    local heatmap_convergence_counts = heatmap(
        1:size(convergence_counts, 2), 1:size(convergence_counts, 1), convergence_counts,
        color=:thermal,
        ylabel=L"\alpha",
        xlabel=L"\beta \times 10^{5}",
        margin=0mm, 
        xticks=(1:15, beta_labels_new),
        yticks=(1:15, alpha_labels_new),
        tickdirection=:none,
        clim=(0, 1),
        size=(1000, 700),
        background=:transparent,
        grid=false, 
        framestyle=:none)
    annotate!(heatmap_convergence_counts, size(convergence_counts, 2) / 2, -0.8, text(L"\beta \times 10^{5}", 10))
    annotate!(heatmap_convergence_counts, -0.8, size(convergence_counts, 1) / 2, text(L"\alpha", 10, rotation=90))
    for i in 1:size(convergence_counts, 2)
        annotate!(heatmap_convergence_counts, i, 0, text(beta_labels_new[i], 6, :top, rotation=45))
        annotate!(heatmap_convergence_counts, 0, i, text(alpha_labels_new[i], 6, :right))
    end
    savefig(heatmap_convergence_counts, "cornell_theory_reading_group_RL/calvano_slides/heatmap_convergence_counts_small_$(type).png")
end




# Plot actual converged prices
for type in types
    local all_success_df = CSV.read("cornell_theory_reading_group_RL/output_data/output_small_$(type)/all_success.csv", DataFrame, header=false)
    local all_prices_df = CSV.read("cornell_theory_reading_group_RL/output_data/output_small_$(type)/all_prices.csv", DataFrame, header=false, skipto=2)
    local alphas_df = CSV.read("cornell_theory_reading_group_RL/output_data/output_small_$(type)/alphas.csv", DataFrame, header=true)
    local betas_df = CSV.read("cornell_theory_reading_group_RL/output_data/output_small_$(type)/betas.csv", DataFrame, header=true)

    local all_success = Matrix(all_success_df)
    local all_prices = Matrix(all_prices_df)
    local alphas = alphas_df[:, 1]
    local betas = betas_df[:, 1]

    nalphas = length(alphas)   # 25
    nbetas = length(betas)    # 25
    price_tuples = Vector{NTuple{4,Float64}}()

    for run in 1:25                        # rows
        for col in 1:625                   # columns
            alphaindex = ((col - 1) % nalphas) + 1
            betaindex = ((col - 1) ÷ nalphas) + 1

        if all_success[run, col] == 0          # did not converge
            push!(price_tuples,
                  (NaN, NaN, alphas[alphaindex], betas[betaindex]))
        else                                    # converged
            p1 = all_prices[run,      col]      # firm 1
            p2 = all_prices[run, col + 625]      # firm 2
                push!(price_tuples,
                    (p1, p2, alphas[alphaindex], betas[betaindex]))
            end
        end
    end

    valid_prices = [t for t in price_tuples if !isnan(t[1])]
    price_pairs = [(t[1], t[2]) for t in valid_prices]
    frequency_dict = countmap(price_pairs)
    x_values = [p[1] for p in keys(frequency_dict)]
    y_values = [p[2] for p in keys(frequency_dict)]
    frequencies = collect(values(frequency_dict))

    marker_sizes = sqrt.(frequencies) * 5

    scatter_plot = scatter(
        x_values, y_values, 
        markersize = marker_sizes,
        markerstrokewidth = 0,
        alpha = 0.7,
        legend = false,
        xlabel = L"p_1",
        ylabel = L"p_2",
        size=(1000, 700),
        background=:transparent,
    )
    savefig(scatter_plot, "cornell_theory_reading_group_RL/calvano_slides/scatter_plot_prices_small_$(type).png")
end
    


