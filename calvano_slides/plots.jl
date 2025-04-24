using Plots, LaTeXStrings, Measures, CSV, DataFrames, LinearAlgebra

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
    aspectratio=:equal,
    margin=0mm,
    xticks=(1:15, formatted_prices),
    yticks=(1:15, formatted_prices),
    legend=false,
    tickdirection=:none,
    size=(800, 700),
    background=:transparent,
    grid=false,
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


# Plot converged profit gain
# Read the CSV files
profit_gain_df = CSV.read("cornell_theory_reading_group_RL/calvano_slides/profit_gain.csv", DataFrame, header=false)
avg_profit_df = CSV.read("cornell_theory_reading_group_RL/calvano_slides/avg_profit.csv", DataFrame, header=false)
prices_0_df = CSV.read("cornell_theory_reading_group_RL/calvano_slides/prices_0.csv", DataFrame, header=false)
prices_1_df = CSV.read("cornell_theory_reading_group_RL/calvano_slides/prices_1.csv", DataFrame, header=false)
alphas_df = CSV.read("cornell_theory_reading_group_RL/calvano_slides/alphas.csv", DataFrame, header=true)
betas_df = CSV.read("cornell_theory_reading_group_RL/calvano_slides/betas.csv", DataFrame, header=true)

# Convert DataFrames to matrices
profit_gain = Matrix(profit_gain_df)
avg_profit = Matrix(avg_profit_df)
prices_0 = Matrix(prices_0_df)
prices_1 = Matrix(prices_1_df)
alphas = alphas_df[:, 1]
betas = betas_df[:, 1]

# Create the heatmap
heatmap_profit_gain = heatmap(
    1:size(profit_gain, 2), 1:size(profit_gain, 1), profit_gain,
    color=:reds,
    xlabel=L"\alpha",
    ylabel=L"\beta",
    aspectratio=:equal,
    margin=5mm,
    tickdirection=:out,
    size=(800, 700),
    background=:transparent,
    grid=false,
    framestyle=:box  # Added a box frame for better visibility
)

# Add custom x and y tick labels (fewer ticks for readability)
tick_indices_x = 1:3:length(alphas)
tick_indices_y = 1:3:length(betas)

# Format the labels to be more readable
alpha_labels = [string(round(a, digits=3)) for a in alphas[tick_indices_x]]
beta_labels = [string(round(b, digits=8)) for b in betas[tick_indices_y]]

# Set the tick values and labels
xticks!(heatmap_profit_gain, tick_indices_x, alpha_labels)
yticks!(heatmap_profit_gain, tick_indices_y, beta_labels)

savefig(heatmap_profit_gain, "cornell_theory_reading_group_RL/calvano_slides/heatmap_profit_gain.png")


