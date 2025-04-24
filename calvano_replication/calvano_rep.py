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

q_value_1 = np.zeros((m,m,m))
q_value_2 = np.zeros((m,m,m))
for s1 in range(m): # State of p1
    for s2 in range(m): # State of p2
        for a1 in range(m): # Action of p1
            q_value_1[s1, s2, a1] = sum(process(action_space[a1], action_space[i])[0] for i in range(m))/(m)

for s1 in range(m): # State of p1
    for s2 in range(m): # State of p2
        for a2 in range(m): # Action of p2
            q_value_2[s1, s2, a2] = sum(process(action_space[i], action_space[a2])[1] for i in range(m))/(m)

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
        q_value_1[state_idx[0], state_idx[1], action_idx[0]] += step_size * (
                reward[0] + delta * np.max(q_value_1[action_idx[0], action_idx[1], :]) -
                q_value_1[state_idx[0], state_idx[1], action_idx[0]])
        
        q_value_2[state_idx[0], state_idx[1], action_idx[1]] += step_size * (
                reward[1] + delta * np.max(q_value_2[action_idx[0], action_idx[1], :]) -
                q_value_2[state_idx[0], state_idx[1], action_idx[1]])
        
        state = next_state
        state_idx = action_idx
    
    return state, time

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
    num_alphas = 15
    num_betas = 1
    prices = np.zeros((num_alphas, num_betas, 2))
    avg_profit = np.zeros((num_alphas, num_betas))
    alphas = np.linspace(0.1, 0.2, num_alphas)
    betas = np.linspace(0.000005, 0.000005, num_betas)
    for i in range(num_alphas):
        for j in range(num_betas):
            p_optimal, time_to_learn = q_learning(q_value_1, q_value_2, alphas[i], betas[j])
            prices[i, j, 0] = p_optimal[0]
            prices[i, j, 1] = p_optimal[1]
            avg_profit[i, j] = (profit(p_optimal[0], p_optimal[1]) + profit(p_optimal[1], p_optimal[0]))/2
            print(f"alpha: {alphas[i]}, beta: {betas[j]}, per-firm profit: {avg_profit[i, j]}, time to learn: {time_to_learn}")
    
    profit_gain = np.zeros((num_alphas, num_betas))
    for i in range(num_alphas):
        for j in range(num_betas):
            profit_gain[i, j] = (avg_profit[i, j] - profit(pn,pn)) / (profit(pm,pm)-profit(pn,pn))
    
    