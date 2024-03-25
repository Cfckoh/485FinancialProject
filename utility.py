
import numpy as np

nu = 1.12

def return_from_price(price_arr):
    """
    Given an array of closing prices returns an np.array of returns as described in 2.1
    """
    # NOTE: the return on the first day is assumed to be 0
    returns = np.zeros(len(price_arr))
    for i in range(1,len(price_arr)):
        returns[i] = np.log(price_arr[i]/price_arr[i-1])
    return returns

def calc_normalized_return(return_history):
    """
    return_history: numpy array full of returns
    """
    # Calculates <R^2> and <R>
    R2 = 0
    R = 0
    for ret in return_history:
        R2 += ret**2
        R += ret
    R2 /= len(return_history)
    R /= len(return_history)

    sigma = (R2-R**2)**0.5
    return (return_history-R)/sigma
    
def calc_weighted_return(return_history, M, t, k = 1):
    """
    return_history: numpy array full of returns
    M is max investment horizon
    k is weight
    """
    outter_sum = 0
    end_step = min(M,t) # don't go off the end if history is shorter than max horizon
    for i in range(end_step):
        gamma = (i+1)**(-nu)
        r = return_history[t-(i+1):t]
        outter_sum += gamma * np.sum(r)
    return k * outter_sum

def calc_L(normal_return_history,t):
    """
    return-volatility correlation function
    """
    n = len(normal_return_history)-1 # indices go 0 to n-1
    
    if t > n:
        raise Exception("t is greater than n")
    
    summation = 0
    Z = 0
    for i in range(n-t):
        summation += normal_return_history[i] * np.abs(normal_return_history[i + t])**2
        Z += np.abs(normal_return_history[i])**2
    summation = summation/(n-t) # NOTE: May need +1 here in denominator because of indexing
    Z = Z/(n-t)
    Z = Z ** 2
    return summation/Z

def average_volatility(volitility_arr, i, t):
    end_step = min(i,t)
    temp = volitility_arr[t-end_step:t]
    return np.sum(volitility_arr)/i

def integrated_volitility_perspective(volitility_arr, M, t):
    vm = average_volatility(volitility_arr,M,t)
    outter_sum = 0
    end_step = min(M,t)
    for i in range(end_step):
        gamma = (i+1)**(-nu)
        outter_sum += gamma * average_volatility(volitility_arr,i,t)
    return outter_sum/vm









