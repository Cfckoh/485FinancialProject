
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
    
def calc_weighted_return(return_history, M, k = 1):
    """
    return_history: numpy array full of returns
    M is max investment horizon
    k is weight
    """
    outter_sum = 0
    for i in range(1,M+1):
        gamma = i**(-nu)
        inner_sum = 0
        for j in range(i):
            inner_sum += gamma * return_history[-j] #Not tested maybe wrong
        outter_sum += inner_sum
    return k * outter_sum

def calc_L():
    """
    return-volatility correlation function
    
    TODO: DISCUSS in class is not clear!!!
    
    """
    pass








