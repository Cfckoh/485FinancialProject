"""
File: utility.py
Purpose: This file contains the utility functions in order to assist market_model.py.
Course: CSC 485
"""

import numpy as np

nu = 1.12

def calc_total_gamma(M):
    """
    Calculates a weighted sum of gamma
    Arguments: M is the investment horizon
    Return Value: a float as the weighted sum of gamma
    Precondition: None
    """
    total_gamma = 0
    for i in range(1,M+1):
        total_gamma += i**(-nu)
    return total_gamma


def return_from_price(price_arr):
    """
    Given an array of closing prices returns an np.array of returns as described in 2.1
    Arguments: price_arr is a numpy array that contains the closing prices of previous days
    Return Value: np.array of returns
    Precondition: None
    """
    # NOTE: the return on the first day is assumed to be 0
    returns = np.zeros(len(price_arr))
    for i in range(1,len(price_arr)):
        returns[i] = np.log(price_arr[i]/price_arr[i-1])
    return returns

def calc_normalized_return(return_history):
    """
    calculates the normalized return of each day
    Arguments: return_history: numpy array full of previous returns
    Return Value: a numpy array full of normalized returns
    Precondition: None
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

def calc_weighted_return(return_history, M, t, total_gamma, k = 1):
    """
    Calculates the weighted returns of a single day based on an investment horizon and a weighted gamma
    Arguments: return_history: numpy array full of returns
               M is max investment horizon
               t is the current day
               total_gamma is the weighted gamma calculated above
               k is weight
    Return Value: None
    Precondition: None
    """
    outter_sum = 0
    end_step = min(M,t) # don't go off the end if history is shorter than max horizon
    for i in range(end_step):
        gamma = (i+1)**(-nu)/total_gamma
        r = return_history[t-(i+1):t]
        outter_sum += gamma * np.sum(r)
    return k * outter_sum
    
def calc_weighted_return_noGamma(return_history, M, t, k = 1):
    """
    Calculates the weighted returns of a single day based on an investment horizon and an unweighted gamma
    Arguments: return_history: numpy array full of returns
               M is max investment horizon
               t is the current day
               k is weight
    Return Value: None
    Precondition: None
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
    calculates the return-volatility correlation based on the paper
    Arguments: normal_return_history is a numpy array that represents normalized returns of each day
               t is the current day
    Return Value: returns the return-volatility correlation as a float
    Precondition: None
    """
    n = len(normal_return_history)-1 # indices go 0 to n-1
    
    if t > n:
        raise Exception("t is greater than n")
    
    summation = 0
    Z = 0
    for i in range(n-t):
        summation += normal_return_history[i] * np.abs(normal_return_history[i + t])**2
        Z += np.abs(normal_return_history[i])**2
    summation = summation/(n-t+1) # NOTE: May need +1 here in denominator because of indexing
    Z = Z/(n-t+1)
    Z = Z ** 2
    return summation/Z

def calc_L_new(t, M, return_hist):
    """
    calculates the return-volatility correlation based on the reference
    Arguments: t is the current day
               return_hist is a numpy array that represents normalized returns of each day
               M is the investment horizon
    Return Value: returns the return-volatility correlation as a float
    Precondition: None
    """
    lt = []
    # start from 2 because can't correlate arrays of length 0 and 1
    for t_prime in range(2,t-M):
        ret_arr = return_hist[M:M+t_prime]
        vol_arr = np.abs(return_hist[M:M+t_prime])
        lt.append(np.corrcoef(ret_arr,vol_arr)[0][1])
    return lt

def average_volatility(volitility_arr, i, t):
    """
    calculates the average volatility over a time
    Arguments: volitility_arr is the volitility over previous days
               i and t are the days that we want the average over
    Return Value: the average volitilities
    Precondition: None
    """
    end_step = min(i,t)
    temp = volitility_arr[t-end_step:t]
    return np.sum(volitility_arr)/i

def integrated_volitility_perspective(volitility_arr, M, t):
    """
    This functions calculates the integrated volitility perspective found in Assymetric Trading Preferences
    Arguments: volitility_arr is the volitility over previous days
               t is the current day
               M is the investment horizon
    Return Value: integrated volitility perspective as a float
    Precondition: None
    """
    vm = average_volatility(volitility_arr,M,t)
    outter_sum = 0
    end_step = min(M,t)
    for i in range(end_step):
        gamma = (i+1)**(-nu)
        outter_sum += gamma * average_volatility(volitility_arr,i+1,t)

    if vm == 0:
        return 0
    return outter_sum/vm