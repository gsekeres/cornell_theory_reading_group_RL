# ========================================== Grid World ==========================================
# Changes from the Python version:
# - Made the code more modular, to accept different world sizes, reward states, and reward values.
# - Added functionality for n dimensional and non-square world sizes.
# - n.b. the plotting functions only work for 2D grids.
# - Added functions to return optimal policy and values for any world, and a function to return
#   the value of a random policy.
# - Added test cases at the end (commented out).

using Plots, Colors
# ========================================== Model functions ==========================================
"""
    step(state::Vector{Int}, action::Vector{Int}; WORLD_SIZE::Vector{Int}=[5, 5], REWARD_STATES::Vector{Vector{Int}}=[],
    REWARD_VALUES::Vector{Int}=[], REWARD_NEXT_STATES::Vector{Vector{Int}}=[]) -> (next_state::Vector{Int}, reward::Int)

Given a state (position) and action (direction), return the next state and reward.
If in the reward states, receive a reward and go to the prime of the reward state.
If in the other states, move in the action direction and receive a reward of 0.
If the action moves the agent out of bounds, the agent remains in the same state and receives a reward of -1.
"""
function step(state::Vector{Int}, action::Vector{Int}; WORLD_SIZE::Vector{Int}=[5, 5], REWARD_STATES::Vector{Vector{Int}}=[],
    REWARD_VALUES::Vector{Int}=[], REWARD_NEXT_STATES::Vector{Vector{Int}}=[])
    # Check if in a reward state:
    for (i, rstate) in enumerate(REWARD_STATES)
        if state == rstate
            return REWARD_NEXT_STATES[i], REWARD_VALUES[i]
        end
    end
    # Otherwise, attempt to move.
    next_state = state .+ action
    # Check bounds:
    for i in 1:length(state)
        if next_state[i] < 1 || next_state[i] > WORLD_SIZE[i]
            return state, -1
        end
    end
    return next_state, 0
end
"""
    get_manhattan_path(start::Vector{Int}, goal::Vector{Int}) -> path::Vector{Vector{Int}}

Given a start and goal state, return the shortest path between them, in terms of the number of steps.
"""
function get_manhattan_path(start::Vector{Int}, goal::Vector{Int})
    path = [copy(start)]
    current = copy(start)
    for i in 1:length(start)
        while current[i] < goal[i]
            current[i] += 1
            push!(path, copy(current))
        end
        while current[i] > goal[i]
            current[i] -= 1
            push!(path, copy(current))
        end
    end
    return path
end
"""
    get_optimal_steady_state(WORLD_SIZE::Vector{Int}=[5,5],
         REWARD_STATES::Vector{Vector{Int}}=[], REWARD_VALUES::Vector{Int}=[],
         REWARD_NEXT_STATES::Vector{Vector{Int}}=[]) 
         -> (full_path::Vector{Vector{Int}}, rewards::Vector{Float64}, best_avg::Float64)

For worlds with at least one reward state, find the cycle (i.e. repeating path)
that maximizes the long-run average reward per move. In our formulation each cycle
consists of (a) the reward transition at a reward state and (b) the Manhattan path
from the post-reward state to the next reward state.
"""
function get_optimal_steady_state(WORLD_SIZE::Vector{Int}=[5, 5],
    REWARD_STATES::Vector{Vector{Int}}=[], REWARD_VALUES::Vector{Int}=[],
    REWARD_NEXT_STATES::Vector{Vector{Int}}=[])
    if isempty(REWARD_STATES)
        return [], [], 0.0
    end

    n = length(REWARD_STATES)
    # Compute travel time using teleport (REWARD_NEXT_STATES) as the departure point.
    travel_time = Array{Int}(undef, n, n)
    for i in 1:n
        for j in 1:n
            d = sum(abs.(REWARD_NEXT_STATES[i] .- REWARD_STATES[j]))
            travel_time[i, j] = d + 1  # +1 for the reward action itself
        end
    end

    best_cycle = nothing
    best_avg = -Inf

    # Recursive DFS: We start at a reward state and try to build cycles.
    # total_reward and total_time accumulate the rewards and travel times along the path.
    function dfs(start::Int, current::Int, visited::Vector{Int},
                 total_reward::Int, total_time::Int, path::Vector{Int})
        for next in 1:n
            if next == start
                # Allow a cycle even if the path is of length 1 (a self-cycle)
                cycle_total_reward = total_reward + REWARD_VALUES[current]
                cycle_total_time   = total_time + travel_time[current, next]
                cycle_avg_reward   = cycle_total_reward / cycle_total_time
                if cycle_avg_reward > best_avg
                    best_cycle = vcat(copy(path), start)
                    best_avg = cycle_avg_reward
                end
            elseif !(next in visited)
                # Add the current reward before moving to the next state.
                new_total_reward = total_reward + REWARD_VALUES[current]
                new_total_time   = total_time + travel_time[current, next]
                push!(visited, next)
                push!(path, next)
                dfs(start, next, visited, new_total_reward, new_total_time, path)
                pop!(path)
                deleteat!(visited, findfirst(==(next), visited))
            end
        end
    end

    # Start DFS from every reward state.
    for i in 1:n
        dfs(i, i, [i], 0, 0, [i])
    end

    if best_cycle === nothing
        return [], [], 0.0
    end

    # best_cycle is something like [i, ..., i]; drop the duplicate end.
    cycle_nodes = best_cycle[1:end-1]

    # Reconstruct the full repeating path.
    full_path = Vector{Vector{Int}}()
    rewards   = Float64[]
    for idx in eachindex(cycle_nodes)
        i = cycle_nodes[idx]
        j = cycle_nodes[mod1(idx+1, length(cycle_nodes))]
        # At reward state i, the agent gets the reward.
        push!(full_path, REWARD_STATES[i])
        push!(rewards, REWARD_VALUES[i])
        # Then the agent is teleported, and must travel from REWARD_NEXT_STATES[i] to REWARD_STATES[j].
        segment = get_manhattan_path(REWARD_NEXT_STATES[i], REWARD_STATES[j])
        for s in segment
            push!(full_path, s)
            push!(rewards, 0.0)
        end
    end

    return full_path, rewards, best_avg
end
"""
    get_optimal_policy(gamma::Float64;
         WORLD_SIZE::Vector{Int}=[5,5],
         REWARD_STATES::Vector{Vector{Int}}=[], REWARD_VALUES::Vector{Int}=[],
         REWARD_NEXT_STATES::Vector{Vector{Int}}=[]) -> 
    (V::Dict{NTuple{length(WORLD_SIZE),Int},Float64}, policy::Dict{NTuple{length(WORLD_SIZE),Int},Vector{Int}})

Returns a mapping of every state (as a tuple of coordinates) to its optimal value and
the corresponding optimal move (as a vector). When gamma==1, the policy is defined as the move that minimizes
the Manhattan distance to the start of the best repeating cycle.
"""
function get_optimal_policy(gamma::Float64; WORLD_SIZE::Vector{Int}=[5, 5], REWARD_STATES::Vector{Vector{Int}}=[], REWARD_VALUES::Vector{Int}=[], REWARD_NEXT_STATES::Vector{Vector{Int}}=[])
    
    # Allowed actions: for each dimension, ±1.
    actions = Vector{Vector{Int}}()
    for i in 1:length(WORLD_SIZE)
        a = zeros(Int, length(WORLD_SIZE))
        a[i] = 1
        push!(actions, copy(a))
        a[i] = -1
        push!(actions, copy(a))
    end

    # Generate all states in the grid.
    states = Vector{Vector{Int}}()
    function gen_states(dim::Int, current::Vector{Int})
        if dim > length(WORLD_SIZE)
            push!(states, copy(current))
        else
            for i in 1:WORLD_SIZE[dim]
                push!(current, i)
                gen_states(dim+1, current)
                pop!(current)
            end
        end
    end
    gen_states(1, Int[])

    # Initialize value function and policy dictionaries.
    V = Dict{NTuple{length(WORLD_SIZE),Int},Float64}()
    policy = Dict{NTuple{length(WORLD_SIZE),Int},Vector{Int}}()
    for s in states
        V[Tuple(s)] = 0.0
        policy[Tuple(s)] = zeros(Int, length(WORLD_SIZE))
    end

    # If gamma == 1, then (as you noted) the optimal objective is to get
    # to the best cycle as quickly as possible.
    if gamma == 1.0
        full_path, cycle_rewards, best_avg = get_optimal_steady_state(WORLD_SIZE,
                                                REWARD_STATES, REWARD_VALUES, REWARD_NEXT_STATES)
        if isempty(full_path)
            error("No reward states defined; cannot compute optimal policy for gamma==1.")
        end
        # For this version we choose the cycle’s first reward state as the target.
        target = full_path[1]
        # For every state (except reward states where the move is forced), pick the move that minimizes
        # the Manhattan distance to the target.
        for s in states
            s_key = Tuple(s)
            # If s is a reward state, the move is forced.
            is_reward = any(s == rs for rs in REWARD_STATES)
            if is_reward
                policy[s_key] = [0]  # special marker (forced)
                # Value can be computed from the step function.
                for (i, rs) in enumerate(REWARD_STATES)
                    if s == rs
                        next_state, r = step(s, zeros(Int, length(s)); 
                                               WORLD_SIZE=WORLD_SIZE, 
                                               REWARD_STATES=REWARD_STATES, 
                                               REWARD_VALUES=REWARD_VALUES, 
                                               REWARD_NEXT_STATES=REWARD_NEXT_STATES)
                        V[s_key] = r  # since no discounting penalty for waiting
                        break
                    end
                end
            else
                best_action = nothing
                best_dist = Inf
                for a in actions
                    next_state, _ = step(s, a; 
                        WORLD_SIZE=WORLD_SIZE, 
                        REWARD_STATES=REWARD_STATES, 
                        REWARD_VALUES=REWARD_VALUES, 
                        REWARD_NEXT_STATES=REWARD_NEXT_STATES)
                    d = sum(abs.(next_state .- target))
                    if d < best_dist
                        best_dist = d
                        best_action = a
                    end
                end
                policy[s_key] = best_action
            end
        end
        return V, policy
    end

    # Otherwise, use discounted value iteration.
    delta = Inf
    threshold = 1e-6
    while delta > threshold
        delta = 0.0
        for s in states
            s_key = Tuple(s)
            # If s is a reward state, the transition is forced.
            is_reward = any(s == rs for rs in REWARD_STATES)
            if is_reward
                for (i, rs) in enumerate(REWARD_STATES)
                    if s == rs
                        next_state, r = step(s, zeros(Int, length(s)); 
                                               WORLD_SIZE=WORLD_SIZE, 
                                               REWARD_STATES=REWARD_STATES, 
                                               REWARD_VALUES=REWARD_VALUES, 
                                               REWARD_NEXT_STATES=REWARD_NEXT_STATES)
                        v_new = r + gamma * V[Tuple(next_state)]
                        delta = max(delta, abs(v_new - V[s_key]))
                        V[s_key] = v_new
                        policy[s_key] = [0]  # special marker
                        break
                    end
                end
            else
                best_v = -Inf
                best_a = nothing
                for a in actions
                    next_state, r = step(s, a; 
                        WORLD_SIZE=WORLD_SIZE, 
                        REWARD_STATES=REWARD_STATES, 
                        REWARD_VALUES=REWARD_VALUES, 
                        REWARD_NEXT_STATES=REWARD_NEXT_STATES)
                    v_candidate = r + gamma * V[Tuple(next_state)]
                    if v_candidate > best_v
                        best_v = v_candidate
                        best_a = a
                    end
                end
                delta = max(delta, abs(best_v - V[s_key]))
                V[s_key] = best_v
                policy[s_key] = best_a
            end
        end
    end

    return V, policy
end
"""
    get_value_random_policy(gamma::Float64; WORLD_SIZE::Vector{Int}=[5, 5],
        REWARD_STATES::Vector{Vector{Int}}=[], REWARD_VALUES::Vector{Int}=[],
        REWARD_NEXT_STATES::Vector{Vector{Int}}=[]) -> V::Dict{NTuple{length(WORLD_SIZE),Int},Float64}

Evaluates (via policy evaluation) the value function when the agent chooses uniformly among the four
cardinal actions at every state (with forced transitions at reward states).
Returns V, a dictionary mapping states to values.
"""
function get_value_random_policy(gamma::Float64; WORLD_SIZE::Vector{Int}=[5, 5],REWARD_STATES::Vector{Vector{Int}}=[], REWARD_VALUES::Vector{Int}=[],REWARD_NEXT_STATES::Vector{Vector{Int}}=[])
    # Generate all possible actions (orthonormal basis vectors).
    actions = Vector{Vector{Int}}()
    for i in 1:length(WORLD_SIZE)
        a = zeros(Int, length(WORLD_SIZE))
        a[i] = 1
        push!(actions, copy(a))
        a[i] = -1
        push!(actions, copy(a))
    end

    # Generate all states in the grid.
    states = Vector{Vector{Int}}()
    function gen_states(dim::Int, current::Vector{Int})
        if dim > length(WORLD_SIZE)
            push!(states, copy(current))
        else
            for i in 1:WORLD_SIZE[dim]
                push!(current, i)
                gen_states(dim+1, current)
                pop!(current)
            end
        end
    end
    gen_states(1, Int[])
    
    V = Dict{NTuple{length(WORLD_SIZE),Int},Float64}()
    for s in states
        V[Tuple(s)] = 0.0
    end

    threshold = 1e-6
    delta = Inf
    while delta > threshold
        delta = 0.0
        for s in states
            s_key = Tuple(s)
            if any(s == rs for rs in REWARD_STATES)
                for (i, rs) in enumerate(REWARD_STATES)
                    if s == rs
                        next_state, r = step(s, zeros(Int, length(s));
                                               WORLD_SIZE=WORLD_SIZE, 
                                               REWARD_STATES=REWARD_STATES,
                                               REWARD_VALUES=REWARD_VALUES, 
                                               REWARD_NEXT_STATES=REWARD_NEXT_STATES)
                        v_new = r + gamma * V[Tuple(next_state)]
                        delta = max(delta, abs(v_new - V[s_key]))
                        V[s_key] = v_new
                        break
                    end
                end
            else
                sum_val = 0.0
                for a in actions
                    next_state, r = step(s, a;
                        WORLD_SIZE=WORLD_SIZE, 
                        REWARD_STATES=REWARD_STATES,
                        REWARD_VALUES=REWARD_VALUES, 
                        REWARD_NEXT_STATES=REWARD_NEXT_STATES)
                    sum_val += (r + gamma * V[Tuple(next_state)])
                end
                v_new = sum_val / length(actions)
                delta = max(delta, abs(v_new - V[s_key]))
                V[s_key] = v_new
            end
        end
    end

    return V
end
# ========================================== Plotting Functions ==========================================

"""
    plot_grid_world_matrix_style(WORLD_SIZE, REWARD_STATES, REWARD_NEXT_STATES, full_path)

Plots the 2d grid, marks the reward states (red, labeled A, B, …) and their teleport targets
(blue, labeled A', B', …), then overlays the optimal cycle as black arrows.
Teleport arrows are drawn dashed, with curvature.
"""
function plot_grid_world_matrix_style(WORLD_SIZE, REWARD_STATES, REWARD_NEXT_STATES, full_path;
    curvature=0.2)
    nx, ny = WORLD_SIZE

    # Basic plot: no ticks, no frame, aspect ratio = 1:1
    plt = plot(
        xlim=(0.5, nx+0.5), ylim=(0.5, ny+0.5),
        size=(4000, 4000),
        aspect_ratio=:equal,
        legend=false,
        framestyle=:none,  # remove bounding box
        xticks=[], yticks=[],
        background_color=:white
    )

    # Draw cell borders (vertical and horizontal lines).
    for x in 0:nx
        plot!(plt, [x+0.5, x+0.5], [0.5, ny+0.5], color=:black, lw=2)
    end
    for y in 0:ny
        plot!(plt, [0.5, nx+0.5], [y+0.5, y+0.5], color=:black, lw=2)
    end

    # Plot reward states in red, teleport states in blue.
    # We label them A, B, C,... and A′, B′, C′,...
    labels = 'A':'Z'  # or however many you need
    for (i, rs) in enumerate(REWARD_STATES)
        annotate!(plt, rs[1]-0.1, rs[2], text(string(labels[i]), :black, 12, :bold))
    end
    for (i, rns) in enumerate(REWARD_NEXT_STATES)
        annotate!(plt, rns[1]-0.2, rns[2], text(string(labels[i]) * "'", :black, 12, :bold))
    end

    # A small helper to plot a dashed, curved arrow from (x1,y1) to (x2,y2).
    function plot_curved_arrow!(x1, y1, x2, y2; curvature=0.2, color=:black)
        # We'll do a simple quadratic Bezier curve from (x1,y1) to (x2,y2).
        N = 50
        tvals = range(0, 1, length=N)
        mx = (x1 + x2)/2
        my = (y1 + y2)/2
        dx, dy = x2 - x1, y2 - y1
        dist = sqrt(dx^2 + dy^2)
        # A perpendicular offset for curvature:
        # normal direction = (-dy, dx) or (dy, -dx)
        ndx, ndy = -dy, dx
        nlen = sqrt(ndx^2 + ndy^2)
        ndx /= nlen; ndy /= nlen
        # scale by "curvature" * distance
        ndx *= (curvature * dist)
        ndy *= (curvature * dist)
        # Control point for the Bezier curve:
        cx, cy = mx + ndx, my + ndy

        # Quadratic Bezier formula: B(t) = (1-t)^2 * P0 + 2t(1-t)*Pc + t^2*P1
        X = [(1-t)^2 * x1 + 2t*(1-t)*cx + t^2*x2 for t in tvals]
        Y = [(1-t)^2 * y1 + 2t*(1-t)*cy + t^2*y2 for t in tvals]

        plot!(plt, X, Y, color=color, linestyle=:dash, arrow=arrow(:end), lw=4)
    end

    # Draw dashed, curved arrows from reward states to their teleport destinations.
    for (i, rs) in enumerate(REWARD_STATES)
        rns = REWARD_NEXT_STATES[i]
        plot_curved_arrow!(rs[1], rs[2], rns[1], rns[2];
                           curvature=curvature, color=:black)
    end

    # Now draw the “optimal cycle” (solid arrows). If full_path is empty or length 1, skip.
    if length(full_path) > 1
        for i in 1:length(full_path)-1
            p1 = full_path[i]
            p2 = full_path[i+1]
            plot!(plt, [p1[1], p2[1]], [p1[2], p2[2]],
                  color=:black, arrow=:arrow, lw=4)
        end
    end

    return plt
end

"""
    plot_value_function(V, WORLD_SIZE; text=true)

Constructs a heatmap of the value function V over the grid (with text if text=true).
"""
function plot_value_function(V, WORLD_SIZE; write=true, color_type=:blues)
    nx, ny = WORLD_SIZE
    mat = zeros(nx, ny)
    for x in 1:nx
        for y in 1:ny
            mat[y, x] = V[(x,y)]
        end
    end
    plt = heatmap(1:nx, 1:ny, mat, color=color_type, aspect_ratio=:equal, xticks=[], yticks=[], framestyle=:none, colorbar=false)
    if write
        for x in 1:nx
            for y in 1:ny
                annotate!(plt, x, y, text(round(mat[y, x], digits=1), :white, :center))
            end
        end
    end
    return plt
end

"""
    plot_policy(policy, WORLD_SIZE)

Plots arrows (via a quiver plot) on the grid showing the best move in each state.
Forced states (where the move is forced) are marked with an “X.”
"""
function plot_policy(policy, WORLD_SIZE)
    nx, ny = WORLD_SIZE
    # Basic plot: no ticks, no frame, aspect ratio = 1:1
    plt = plot(
        xlim=(0.5, nx+0.5), ylim=(0.5, ny+0.5),
        size=(400, 400),
        aspect_ratio=:equal,
        legend=false,
        framestyle=:none,  # remove bounding box
        xticks=[], yticks=[],
        background_color=:white
    )
    for x in 1:nx+1
        plot!(plt, [x-0.5, x-0.5], [0.5, ny+0.5], color=:gray, lw=0.5, linestyle=:dot)
    end
    for y in 1:ny+1
        plot!(plt, [0.5, nx+0.5], [y-0.5, y-0.5], color=:gray, lw=0.5, linestyle=:dot)
    end
    xs, ys, dxs, dys = Float64[], Float64[], Float64[], Float64[]
    forced_positions = Tuple[]
    for (s, a) in policy
        if a != [0]
            x, y = s
            ax, ay = a
            push!(xs, x - 0.25 * ax)
            push!(ys, y - 0.25 * ay)
            push!(dxs, 0.4 * ax)
            push!(dys, 0.4 * ay)
        else
            push!(forced_positions, s)
        end
    end
    quiver!(plt, xs, ys, quiver=(dxs, dys), color=:red, arrow=true, linewidth=2)
    for s in forced_positions
        x, y = s
        annotate!(plt, x, y, text("X", :black, 12, :bold))
    end
    return plt
end





# Uncomment to run the test cases

# ========================================== Test cases ==========================================

# Simple 5x5 grid world with two reward states (from textbook example)
WORLD_SIZE = [5, 5]
REWARD_STATES = [[2, 5], [4, 5]]
REWARD_VALUES = [10, 5]
REWARD_NEXT_STATES = [[2, 1], [4, 3]]

full_path, rewards, best_avg = get_optimal_steady_state(WORLD_SIZE, REWARD_STATES, REWARD_VALUES, REWARD_NEXT_STATES)

println("Optimal steady state:")
for i in eachindex(full_path)
    println("Step $i: $(full_path[i]) -> $(rewards[i])")
end
println("Best average reward: $best_avg")

# No discounting case:
V1, policy1 = get_optimal_policy(1.0; WORLD_SIZE, REWARD_STATES, REWARD_VALUES, REWARD_NEXT_STATES)

# Book case
V09, policy09 = get_optimal_policy(0.9; WORLD_SIZE, REWARD_STATES, REWARD_VALUES, REWARD_NEXT_STATES)

# Discounting case:
V05, policy05 = get_optimal_policy(0.5; WORLD_SIZE, REWARD_STATES, REWARD_VALUES, REWARD_NEXT_STATES)

# Move randomly case:
Vrandom = get_value_random_policy(0.9; WORLD_SIZE, REWARD_STATES, REWARD_VALUES, REWARD_NEXT_STATES)

# Plot the results:
plt_cycle = plot_grid_world_matrix_style(WORLD_SIZE, REWARD_STATES, REWARD_NEXT_STATES, full_path)
plot!(plt_cycle, background=:transparent)
savefig(plt_cycle, "cornell_theory_reading_group_RL/chapter03/grid_world_cycle.png")

plt_value = plot_value_function(V1, WORLD_SIZE; write=true)
plot!(plt_value, title="Value Function (gamma=1)", background=:transparent)
savefig(plt_value, "cornell_theory_reading_group_RL/chapter03/grid_world_value_1.png")

plt_policy = plot_policy(policy1, WORLD_SIZE)
plot!(plt_policy, title="Best Policy (gamma=1)", background=:transparent)
savefig(plt_policy, "cornell_theory_reading_group_RL/chapter03/grid_world_policy_1.png")

plt_value09 = plot_value_function(V09, WORLD_SIZE; write=true)
plot!(plt_value09, title="Value Function (gamma=0.9)", background=:transparent)
savefig(plt_value09, "cornell_theory_reading_group_RL/chapter03/grid_world_value_09.png")

plt_policy09 = plot_policy(policy09, WORLD_SIZE)
plot!(plt_policy09, title="Best Policy (gamma=0.9)", background=:transparent)
savefig(plt_policy09, "cornell_theory_reading_group_RL/chapter03/grid_world_policy_09.png")

plt_value05 = plot_value_function(V05, WORLD_SIZE; write=true)
plot!(plt_value05, title="Value Function (gamma=0.5)", background=:transparent)
savefig(plt_value05, "cornell_theory_reading_group_RL/chapter03/grid_world_value_05.png")

plt_policy05 = plot_policy(policy05, WORLD_SIZE)
plot!(plt_policy05, title="Best Policy (gamma=0.5)", background=:transparent)
savefig(plt_policy05, "cornell_theory_reading_group_RL/chapter03/grid_world_policy_05.png")

plt_value_random = plot_value_function(Vrandom, WORLD_SIZE; write=true)
plot!(plt_value_random, title="Value Function (random policy, gamma=0.9)", background=:transparent)
savefig(plt_value_random, "cornell_theory_reading_group_RL/chapter03/grid_world_value_random.png")


using Random
# Large grid to see the basins of attraction
WORLD_SIZE = [100, 100]
random_states_1 = zeros(Int, 10)
random_states_2 = zeros(Int, 10)
random_states_3 = zeros(Int, 10)
random_states_4 = zeros(Int, 10)
rand!(random_states_1, 1:WORLD_SIZE[1])
rand!(random_states_2, 1:WORLD_SIZE[2])
rand!(random_states_3, 1:WORLD_SIZE[1])
rand!(random_states_4, 1:WORLD_SIZE[2])
REWARD_STATES = [[random_states_1[i], random_states_2[i]] for i in 1:10]
REWARD_VALUES = [10, 20, 30, 40, 50, 60, 70, 80, 90, 100]
REWARD_NEXT_STATES = [[random_states_3[i], random_states_4[i]] for i in 1:10]

Vlarge, policylarge = get_optimal_policy(0.995; WORLD_SIZE, REWARD_STATES, REWARD_VALUES, REWARD_NEXT_STATES)

plt_cycle_large = plot_grid_world_matrix_style(WORLD_SIZE, REWARD_STATES, REWARD_NEXT_STATES, full_path)
plot!(plt_cycle_large, background=:transparent)
savefig(plt_cycle_large, "cornell_theory_reading_group_RL/chapter03/grid_world_cycle_large.png")

plt_value_large = plot_value_function(Vlarge, WORLD_SIZE; write=false, color_type=:thermal)
plot!(plt_value_large, title="Value Function (gamma=0.995, 100x100 grid)", background=:transparent)
savefig(plt_value_large, "cornell_theory_reading_group_RL/chapter03/grid_world_value_large.png")

Vlarge_random = get_value_random_policy(0.995; WORLD_SIZE, REWARD_STATES, REWARD_VALUES, REWARD_NEXT_STATES)
plt_value_large_random = plot_value_function(Vlarge_random, WORLD_SIZE; write=false, color_type=:thermal)
plot!(plt_value_large_random, title="Value Function (random policy)", background=:transparent)
savefig(plt_value_large_random, "cornell_theory_reading_group_RL/chapter03/grid_world_value_large_random.png")
