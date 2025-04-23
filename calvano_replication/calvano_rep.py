import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

a0 = 0
mu = 0.25
delta = 0.95


def demand(p1,p2):
    q = np.exp((2-p1)/mu)/(np.exp((2-p1)/mu)+np.exp((2-p2)/mu)+np.exp(a0/mu))
    return q

def profit(p1,p2):
    profit = (p1-1)*demand(p1,p2)
    return profit

def process(action_0, action_1):
    profit_0 = profit(action_0, action_1)
    profit_1 = profit(action_1, action_0)
    return [profit_0, profit_1]

xi = 0.1
pn = 1.4729      # Nash price
pm = 1.92498    # Monopoly price
m = 15

action_space = np.linspace(pn-xi*(pm-pn),pm+xi*(pm-pn),m)

q_value_1 = np.zeros((m,m,m,m))
q_value_2 = np.zeros((m,m,m,m))
for k in range(m):
    rewards = 0
    for l in range(m):
        rewards += process(action_space[k], action_space[l])[0]
    q_value_1[:, :, k, :] = rewards / ((1 - delta)*m)

for l in range(m):
    rewards = 0
    for k in range(m):
        rewards += process(action_space[k], action_space[l])[1]
    q_value_2[:, :, l, :] = rewards / ((1 - delta)*m)


def q_learning(q_value_1, q_value_2, step_size, beta):
    # Get initial state indices
    state_idx = [0, 0]
    state_idx[0] = np.random.randint(m)
    state_idx[1] = np.random.randint(m)
    state = [action_space[state_idx[0]], action_space[state_idx[1]]]
    
    rewards = [0, 0]
    time = 0
    action_idx = [0, 0]
    stay = 0
    
    while stay < 100000:
        time += 1
        #count += 1
        action_idx[0] = choose_action(state, q_value_1, beta, time, 1)  # index of price for good 1
        action_idx[1] = choose_action(state, q_value_2, beta, time, 2)  # index of price for good 2
        action = [action_space[action_idx[0]], action_space[action_idx[1]]]
        next_state = action
        
        if next_state == state:
            stay += 1
        else:
            stay = 0

        #print(stay)
        #print(action)
        
        reward = process(action[0], action[1])
        rewards[0] += reward[0]
        rewards[1] += reward[1]
        
        # Q-Learning update using indices
        q_value_1[state_idx[0], state_idx[1], action_idx[0], action_idx[1]] += step_size * (
                reward[0] + delta * np.max(q_value_1[action_idx[0], action_idx[1], :, :]) -
                q_value_1[state_idx[0], state_idx[1], action_idx[0], action_idx[1]])
        
        q_value_2[state_idx[0], state_idx[1], action_idx[1], action_idx[0]] += step_size * (
                reward[1] + delta * np.max(q_value_2[action_idx[0], action_idx[1], :, :]) -
                q_value_2[state_idx[0], state_idx[1], action_idx[1], action_idx[0]])
        
        state = next_state
        state_idx = action_idx
    
    return state

def choose_action(state, q_value, beta, time, index):
    EPSILON = np.exp(-beta*time)
        
    if np.random.binomial(1, EPSILON) == 1:
        return np.random.randint(len(action_space))
    else:
        # Convert state to indices
        state_idx_0 = np.where(action_space == state[0])[0][0]
        state_idx_1 = np.where(action_space == state[1])[0][0]
        values_ = q_value[state_idx_0, state_idx_1, :]
        return np.random.choice(np.where(values_ == np.max(values_))[0])

if __name__ == "__main__":
    prices = np.zeros((100, 100, 2))
    avg_profit = np.zeros((100, 100))
    alphas = np.linspace(0.025, 0.25, 100)
    betas = np.linspace(0.000000000000001, 0.00002, 100)
    for i in range(100):
        for j in range(100):
            p_optimal = q_learning(q_value_1, q_value_2, alphas[i], betas[j])
            prices[i, j, 0] = p_optimal[0]
            prices[i, j, 1] = p_optimal[1]
            avg_profit[i, j] = (profit(p_optimal[0], p_optimal[1]) + profit(p_optimal[1], p_optimal[0]))/2
            print(f"alpha: {alphas[i]}, beta: {betas[j]}, per-firm profit: {avg_profit[i, j]}")
    
    profit_gain = (avg_profit - profit(pn,pn)) / (profit(pm,pm)-profit(pn,pn))
    
    # Create the heatmap plot
    plt.figure(figsize=(10, 8), facecolor='none')
    heatmap = sns.heatmap(profit_gain, 
                      cmap='thermal',  # Using the thermal colormap as requested
                      xticklabels=np.round(betas[::10], 8),  # Show fewer ticks for readability
                      yticklabels=np.round(alphas[::10], 3),
                      cbar_kws={'label': 'Profit Gain Ratio'})

    # Set labels and title
    plt.xlabel('Beta')
    plt.ylabel('Alpha')

    # Improve tick label formatting
    plt.xticks(rotation=45)
    plt.tight_layout()

    # Set the axes background to transparent too
    ax = plt.gca()
    ax.patch.set_alpha(0)

    # Save the figure with transparent background
    plt.savefig('profit_gain_heatmap.png', 
            dpi=300, 
            bbox_inches='tight',
            transparent=True)  # This ensures transparency in the saved image

    # If you want to also display the plot
    plt.show()