import numpy as np
import utility
import csv

BUY  = 1
SELL = -1
HOLD = 0
BULL = 0
NEUTRAL = 1
BEAR = 2


class Market:
    """Implementation of an agent based model"""
    def __init__(self, N, alpha,p, M, t_end, delta_R=0, c=0, asymmetric_preference=False):
        """
        params: N: number of agents
                alpha: asymmetrics trading param
                p: trading probability param
                M: max investment horizon
                t_end: number of days to run simulation
                delta_R: assymetric herding param if 0 herding is symmetric
        """
        
        print("Initializing Market")
        #np.random.seed(0) 
        self.alpha=alpha
        self.probs = self._intialize_probs(alpha,p) 
        self.return_hist = np.zeros(t_end, dtype="int64")
        self.volatility_hist = np.zeros(t_end, dtype="int64")
        self.herding_hist = np.zeros(t_end, dtype=float)
        self.t_end = t_end
        self.t = 0
        self.delta_R = delta_R
        self.num_agents = N
        self.M = M
        # intially each agent in own cluster
        # also herding degree is not exact due to integer rounding
        self.herding_degree = 1/N
        self.cluster_sizes = self.get_new_clusters()
        self.market_state = NEUTRAL
        self.asymmetric_preference = asymmetric_preference
        self.p = p
        if asymmetric_preference:
            self.c = c
        self.total_gamma = utility.calc_total_gamma(M)

    def step(self):
        ret = 0
        actions = [BUY,SELL,HOLD]
        
        # if assymetric preference use different probs
        if self.asymmetric_preference:
            p_buy = self.p*(self.c*utility.integrated_volitility_perspective(self.volatility_hist,self.M,self.t)+(1-self.c))
            probs = [p_buy, 2*self.p-p_buy, 1-2*self.p]
            for size in self.cluster_sizes:
                #randomly choose an action given the current probabilities
                action = np.random.choice(actions, p=probs)
                ret += size * action

        else:
            for size in self.cluster_sizes:
                #randomly choose an action given the current probabilities
                action = np.random.choice(actions, p=self.probs[self.market_state])
                ret += size * action

        volatility = np.abs(ret)

        #update histories and parameter for next step
        self.return_hist[self.t] = ret
        self.volatility_hist[self.t] = volatility
        self.herding_hist[self.t] = self.herding_degree
        
        # update clusters
        R_prime = utility.calc_weighted_return(self.return_hist, self.M, self.t, self.total_gamma,k=0.01)
        self.update_clusters(R_prime)
        self.market_state = self.get_market_state(R_prime)

        #increment time
        self.t += 1

    def run_market(self):
        while self.t < self.t_end:
            self.step()


    def get_market_state(self,R_prime):
        if R_prime > 0:
            return BULL
        elif R_prime < 0:
            return BEAR
        else:
            return NEUTRAL

    def get_new_clusters(self):
        """Assign each agent to a cluster
        NOTE: agents are not there own thing we just track the size of each cluster"""
        num_clusters = int(1 / self.herding_degree) + 1 #determine number of clusters based on herding degree plus one so there is always at least one cluster
        

        # n random floats 
        clusters = np.random.rand(num_clusters)
        # extend the floats so the sum is approximately num_agents (might be less, because of flooring) 
        clusters = np.floor(clusters*self.num_agents/sum(clusters)).astype(int)
        # randomly add missing numbers 
        for i in range(self.num_agents - sum(clusters)): 
            clusters[np.random.randint(0,num_clusters)] += 1

        return clusters

    def update_clusters(self,R_prime):
        self.herding_degree = abs(R_prime - self.delta_R)/self.num_agents
        if self.herding_degree == 0:
            self.herding_degree = 1/self.num_agents # herding degree of 0 doesn't exist 
        self.cluster_sizes = self.get_new_clusters()

    def _intialize_probs(self,alpha,p):
        beta = 2-alpha 
        probs = np.zeros((3,3))
        probs[BEAR] = np.array([p*beta,p*beta,(1-2*p*beta)])
        probs[NEUTRAL] = np.array([p,p,(1-2*p)])
        probs[BULL] = np.array([p*alpha,p*alpha,(1-2*p*alpha)])
        return probs



    def save_run(self):
        """
        Saves the returns, volatilities, and L(t) values into a csv.
        Since the start of the simulation is volatile for t<M it only saves data after this point.
        If their are not enough steps it will error or have undetermined behavior.
        """
        lt = []
        # start from 2 because can't correlate arrays of length 0 and 1
        for t_prime in range(2,self.t-self.M):
            ret_arr = self.return_hist[self.M:self.M+t_prime]
            vol_arr = np.abs(self.return_hist[self.M:self.M+t_prime])
            lt.append(np.corrcoef(ret_arr,vol_arr)[0][1])

        data = {"returns": self.return_hist[self.M:],"volatilities": self.volatility_hist[self.M:],"L(t)":lt}

        with open(f"data/results_a={self.alpha}_dR={self.delta_R}.csv", "w") as outfile:
            writer = csv.writer(outfile)
            writer.writerow(data.keys())
            writer.writerows(zip(*data.values()))
