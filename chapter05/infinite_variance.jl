##############################################################################
# Show an example of how Ordinary Importance Sampling has infinite variance. #
# Specifically, we have very unstable estimates on the one-state MDP.        #
##############################################################################

using Random, Plots, Statistics

# Actions
@enum Action ACTION_BACK=0 ACTION_END=1

# Behavior policy (random with 0.5 probability for each action)
function behavior_policy()::Action
    return rand() < 0.5 ? ACTION_BACK : ACTION_END
end

# Target policy (always selects BACK)
function target_policy()::Action
    return ACTION_BACK
end

# One turn of the game
function play()
    # Track the actions for importance ratio
    trajectory = Action[]
    
    while true
        action = behavior_policy()
        push!(trajectory, action)
        
        if action == ACTION_END
            return 0, trajectory
        end
        
        # 0.9 probability of continuing, 0.1 probability of terminating with reward 1
        if rand() < 0.1
            return 1, trajectory
        end
    end
end

runs = 10
episodes = 100_000_000
    
plt = plot(
    xlabel = "Episodes (log scale)",
    ylabel = "Ordinary Importance Sampling",
    xscale = :log10,
    size = (800, 600),
    ylims = (0, 3),
    legend = false,
    fontfamily = "Computer Modern"
)
    
for run in 1:runs
    rewards = Float64[]
        
    for episode in 1:episodes
        reward, trajectory = play()
            
        # Calculate importance sampling ratio
        if trajectory[end] == ACTION_END
            rho = 0.0
        else
            # Target policy always chooses BACK with probability 1
            # Behavior policy chooses BACK with probability 0.5
            # For each BACK in the trajectory, we multiply by (1.0/0.5) = 2.0
            rho = 1.0 / (0.5^length(trajectory))
        end
            
        push!(rewards, rho * reward)
    end
        
    # Calculate cumulative rewards and average estimations
    cumulative_rewards = cumsum(rewards)
    estimations = cumulative_rewards ./ (1:episodes)
        
    # Plot the estimations for this run
    plot!(plt, 1:episodes, estimations, alpha=0.6)
end
    
savefig(plt, "cornell_theory_reading_group_RL/chapter05/figure_5_4.png")
