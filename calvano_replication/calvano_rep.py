import numpy as np

a0 = 0
a = [2,2]
c = [1,1]
mu = 0.25
delta = 0.95
price = [0,0]

def demand(p1,p2,index):
    price = [p1,p2]
    q = np.exp((a[index]-p1)/mu)/(np.exp((a[index]-p1)/mu)+np.exp((a[1-index]-p2)/mu)+np.exp(a0/mu))
    return q

def profit(p1,p2,index):
    profit = (p1-c[index])*demand(p1,p2,index)
    return profit

def process(action_0, action_1):
    profit_0 = profit(action_0, action_1, 0)
    profit_1 = profit(action_1, action_0, 1)
    return [profit_0, profit_1]

xi = 0.1
pn = [1.47293,1.47293]      # Nash price
pm = [1.92498,1.92498]      # Monopoly price
m = 15

action_space_1 = np.linspace(pn[0]-xi*(pm[0]-pn[0]),pm[0]+xi*(pm[0]-pn[0]),m)
action_space_2 = np.linspace(pn[1]-xi*(pm[1]-pn[1]),pm[1]+xi*(pm[1]-pn[1]),m)

q_value_1 = np.zeros((len(action_space_1), len(action_space_2), len(action_space_1), len(action_space_2)))
q_value_2 = np.zeros((len(action_space_1), len(action_space_2), len(action_space_1), len(action_space_2)))
for k in range(len(action_space_1)):
    rewards = 0
    for l in range(len(action_space_2)):
        rewards += process(action_space_1[k], action_space_2[l])[0]
    q_value_1[:, :, k, :] = rewards / ((1 - delta)*m)

for l in range(len(action_space_2)):
    rewards = 0
    for k in range(len(action_space_1)):
        rewards += process(action_space_1[k], action_space_2[l])[1]
    q_value_2[:, :, :, l] = rewards / ((1 - delta)*m)

def q_learning(q_value_1, q_value_2, step_size, beta):
    state = [0,0] # initial state, need to be drawn randomly from action_space
    state[0] = np.random.choice(action_space_1)
    state[1] = np.random.choice(action_space_2)
    rewards = [0,0]
    time = 0
    action = [0,0]
    stay = 0
    while stay < 100000:
        time += 1
        action[0] = choose_action(state, q_value_1, beta, time, 1) # price of good 1
        action[1] = choose_action(state, q_value_2, beta, time, 2) # price of good 2
        next_state = action 
        if next_state == state:
            stay += 1
        else:
            stay = 0
        
        reward = process(action[0], action[1])
        rewards[0] += reward[0]
        rewards[1] += reward[1]
        # Q-Learning update
        q_value_1[state[0], state[1], action[0], action[1]] += step_size * (
                reward[0] + delta * np.max(q_value_1[next_state[0], next_state[1], :, :]) -
                q_value_1[state[0], state[1], action[0], action[1]])
        q_value_2[state[0], state[1], action[0], action[1]] += step_size * (
                reward[1] + delta * np.max(q_value_2[next_state[0], next_state[1], :, :]) -
                q_value_2[state[0], state[1], action[0], action[1]])
        state = next_state
    return state

# choose an action based on epsilon greedy algorithm
def choose_action(state, q_value, beta, time, index):
    EPSILON = np.exp(-beta*time)
    if index == 1:
        action_space = action_space_1
    else:
        action_space = action_space_2
        
    if np.random.binomial(1, EPSILON) == 1:
        return np.random.choice(action_space)
    else:
        values_ = q_value[state[0], state[1], :]
        action_ = action_space
        return np.random.choice([action_ for action_, value_ in enumerate(values_) if value_ == np.max(values_)])

if __name__ == "__main__":
    p_optimal = q_learning(q_value_1, q_value_2, 0.1, 0.00002)
    print(p_optimal)