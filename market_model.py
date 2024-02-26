import numpy as np

BUY  = 1
SELL = -1
HOLD = 0


class Market:
    """ First rudimentary market implementation of an agent based model"""
    def __init__(self, num_clusters, herd_coef, p_buy,p_sell,p_hold,t_end):
        print("Initializing market currently many parameters are missing")
        self.probs = [p_buy,p_sell,p_hold]
        self.return_hist = np.zeros(t_end, dtype=int)
        self.volatility_hist = np.zeros(t_end, dtype=int)
        self.t_end = t_end
        self.t = 0
        self.num_clusters = num_clusters
        self.cluster_sizes = np.zeros(num_clusters, dtype=int)
        self.num_agents  = num_clusters*herd_coef
        self._init_clusters()


    def step(self):
        ret = 0
        actions = [BUY,SELL,HOLD]
        
        for size in self.cluster_sizes:
            #randomly choose an action given the current probabilities NOTE: will error if probabilities do not sum to 1
            action = np.random.choice(actions, p=self.probs)
            ret += size * action

        volatility = np.abs(ret)

        #update histories and parameter for next step
        self.return_hist[self.t] = ret
        self.volatility_hist[self.t] = volatility
        self.update_clusters()
        self.update_probs()

        #increment time
        self.t += 1

    def run_market(self):
        while self.t < self.t_end:
            self.step()

    def update_probs(self):
        print("TODO update probailities")

    def update_clusters(self):
        print("TODO update the clusters")

    def _init_clusters(self):
        """Assign each agent to a cluster
        NOTE: for now agents are not there own thing we just track the size of each cluster"""
        for agent in range(self.num_agents):
            # pick a cluster at random
            cluster = np.random.randint(0,self.num_clusters)
            self.cluster_sizes[cluster] += 1

