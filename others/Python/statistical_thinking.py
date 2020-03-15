https://towardsdatascience.com/5-quick-and-easy-data-visualizations-in-python-with-code-a2284bae952f

# -------------- Graphical EDA -------------------

import matplotlib.pyplot as plt
import seaborn as sns

sns.set() # Set default Seaborn style

_ = plt.hist(versicolor_petal_length) # Set default Seaborn style

plt.xlabel('petal length (cm)')
plt.ylabel('count')
plt.show() # Show histogram


# in histogram the number of bins is the square root number of samples
import numpy as np
n_data = len(versicolor_petal_length)
n_bins = np.sqrt(n_data)# Number of bins is the square root of number of data points: n_bins
n_bins = int(n_bins) # Convert number of bins to integer: n_bins

_ = plt.hist( versicolor_petal_length, n_bins) # Plot the histogram
_ = plt.xlabel('petal length (cm)')
_ = plt.ylabel('count')
plt.show()


# Beeswarm plot - to show distribution of different species for camparison
_ = sns.swarmplot(x='species', y='petal length (cm)', data=df)
_ = plt.xlabel('species')
_ = plt.ylabel('petal length (cm)')
plt.show()


# Empirical cumulative distribution function (ECDF) - x is sorted, y 
def ecdf(data):
    """Compute ECDF for a one-dimensional array of measurements."""
   
    n = len(data)  # Number of data points: n
    x = np.sort(data)   # x-data for the ECDF: x
    y = np.arange(1, n+1) / n # y-data to get 1,2,3.....n

    return x, y

x_set,y_set = ecdf(setosa_petal_length) # to call setosa, versicolor, and virginica in the same plot
x_vers,y_vers = ecdf(versicolor_petal_length)
x_virg,y_virg = ecdf(virginica_petal_length)

plt.plot(x_set, y_set, marker = '.', linestyle = 'none') # for ECDF plot it like this.
plt.plot(x_vers, y_vers, marker = '.', linestyle = 'none')
plt.plot(x_virg, y_virg, marker = '.', linestyle = 'none')


plt.legend(('setosa', 'versicolor', 'virginica'), loc='lower right')
_ = plt.xlabel('petal length (cm)')
_ = plt.ylabel('ECDF')
plt.show()

# -------------- Quantitative EDA -------------------
np.mean(versicolor_petal_length) # mean
np.std(versicolor_petal_length) == np.sqrt(np.var(versicolor_petal_length)) # variance - the average of squared distance from the mean

np.cov(versicolor_petal_length,versicolor_petal_width)
# percentiles 
percentiles = np.array([2.5,25,50,75,97.5])
ptiles_vers = np.percentile(versicolor_petal_length, percentiles) #percentiles
_ = plt.plot(x_vers, y_vers, '.') # Plot the ECDF
_ = plt.xlabel('petal length (cm)')
_ = plt.ylabel('ECDF')
_ = plt.plot(ptiles_vers, percentiles/100, marker='D', color='red', linestyle='none') # Overlay percentiles as red diamonds.
plt.show()

# boxplot - to visualise Q1,mean,Q3 
_ = sns.boxplot(x='species' , y ='petal length (cm)', data = df )
_ = plt.xlabel('species')
_ = plt.ylabel('petal length (cm)')
plt.show()

# scatterplot
_ = plt.plot(versicolor_petal_length, versicolor_petal_width, marker='.', linestyle='none')
_ = plt.xlabel('versicolor petal lenght (cm)')
_ = plt.ylabel('versicolor petal width (cm)')
plt.show()

#pearson's correlation
def pearson_r(x,y): # pearson's correlation = covariance / std(x)* std(y).
 # variance the spread of the data, covariance whether it goes to the same direction (inidicates correlation   
    corr_mat = np.corrcoef(x,y) # Compute correlation matrix: corr_mat
    return corr_mat[0,1] # Return entry [0,1]
r = pearson_r(versicolor_petal_length,versicolor_petal_width)
print(r)


# -------------- Thinking probabilistically: discrete values -------------------



def perform_bernoulli_trials(n, p):
    """Perform n Bernoulli trials with success probability p
    and return number of successes."""
    # Initialize number of successes: n_success
    n_success = 0

    # Perform trials
    for i in range(n):

        random_number = np.random.random() # Choose random number between zero and one: random_number

        # If less than p, it's a success  so add one to n_success
        if random_number < p:
            n_success += 1

    return n_success

# bernouli trials is random experiment with 2 possible outcomes. It is a binominal distribtuion with n_trial = 1
np.random.seed(42) # Seed random number generator
n_defaults = np.empty(1000)# Initialize the number of defaults: n_defaults

for i in range(1000): # Compute the number of defaults
    n_defaults[i] = perform_bernoulli_trials(100,0.05)

_ = plt.hist(n_defaults, normed=True) # Plot the histogram with default number of bins; label your axes
_ = plt.xlabel('number of defaults out of 100 loans')
_ = plt.ylabel('probability')
plt.show()

#will the bank fail? if bank exceeds 10 default, it will fail.

x,y = ecdf(n_defaults) # Compute ECDF: x, y
plt.plot(x, y, marker = '.', linestyle = 'none') # Plot the ECDF with labeled axes
_ = plt.xlabel('n_defaults')
_ = plt.ylabel('ECDF')
plt.show()

n_lose_money = np.sum(n_defaults >= 10) # Compute the number of 100-loan simulations with 10 or more defaults
print('Probability of losing money =', n_lose_money / len(n_defaults)) # Compute and print probability of losing money


# Use binomials to check distribution with n_trials= = 10,000
n_defaults = np.random.binomial (100, 0.05, size=10000)
x,y = ecdf(n_defaults)

plt.plot(x,y, marker = '.', linestyle='none')
plt.xlabel('n_default')
plt.ylabel('CDF')
plt.show()

#Plotting binomial PMF - probability mass function
bins = np.arange(0, max(n_defaults) + 1.5) - 0.5
plt.hist(n_defaults, normed=True, bins=bins) 
plt.xlabel('n_defaults')
plt.ylabel('y')
plt.show()

# possion distribution, the timing of the next bus is independent to the previous bus. It is used for low p - high n
# the unit there is 10 buses / hour

samples_poisson = np.random.poisson(10, 10000) # Poisson distribution with n=10,000 and arrival rate of 10

print('Poisson:     ', np.mean(samples_poisson),
                       np.std(samples_poisson))

n = [20, 100, 1000] #  Specify values of n and p to consider for Binomial so that np = 10
p = [0.5, 0.1, 0.01]

for i in range(3):
    samples_binomial = np.random.binomial(n[i], p[i], size =10000 )

    print('n =', n[i], 'Binom:', np.mean(samples_binomial),
                                 np.std(samples_binomial))

#is 2015 baseball season is a rare ocassion?
n_nohitters = np.random.poisson(251/115, 10000) 
n_large = np.sum(n_nohitters >= 7)
p_large = n_large / 10000
print('Probability of seven or more no-hitters:', p_large)


# -------------- Thinking probabilistically: Continous variables -------------------

#PDF - probability density function

mean = np.mean(data)
std = np.std(data)
samples = np.random.normal(mean, std, size=1000)
x,y = ecdf(data)
x_sample,y_sample = ecdf(samples)

# Normal PDF
samples_std1 = np.random.normal(20, 1, size=100000)
samples_std3 = np.random.normal(20, 3, size=100000)
samples_std10 = np.random.normal(20, 10, size=100000)
_ = plt.hist(samples_std1, bins=100, normed=True, histtype='step') # Make histograms
_ = plt.hist(samples_std3, bins=100, normed=True, histtype='step')
_ = plt.hist(samples_std10, bins=100, normed=True, histtype='step')
_ = plt.legend(('std = 1', 'std = 3', 'std = 10'))
plt.ylim(-0.01, 0.42)
plt.show()

# Normal CDF - all cdf on 0.5 pass thru mean
x_std1, y_std1 = ecdf(samples_std1)
x_std3, y_std3 = ecdf(samples_std3)
x_std10, y_std10 = ecdf(samples_std10)
plt.plot(x_std1, y_std1, marker='.', linestyle='none')
plt.plot(x_std3, y_std3, marker='.', linestyle='none')
plt.plot(x_std10, y_std10, marker='.', linestyle='none')
_ = plt.legend(('std = 1', 'std = 3', 'std = 10'), loc='lower right') #legend. 
plt.show()

# Normal distribution properties and warnings
# 1) things you think normally distributed are not. Normal distribution is not the best to describe outliers.

#Are belmont stakes, normally distributed?
mu = np.mean(belmont_no_outliers)
sigma = np.std(belmont_no_outliers)
samples = np.random.normal(mu, sigma, size=10000 )

x_theor, y_theor = ecdf(samples) #samples
x, y = ecdf(belmont_no_outliers) #real data

_ = plt.plot(x_theor, y_theor)
_ = plt.plot(x, y, marker='.', linestyle='none')
_ = plt.xlabel('Belmont winning time (sec.)')
_ = plt.ylabel('CDF')
plt.show()


# Get  p for x <= 144.
samples = np.random.normal(mu, sigma, size=1000000)
prob = np.count_nonzero(samples <= 144) / np.count_nonzero(samples) # Compute the fraction that are faster than 144 seconds: prob
# prob = np.sum(samples <= 144) / len(samples)
print('Probability of besting Secretariat:', prob)

# Compute time for arrival of 2 successive poisson processes
def successive_poisson(tau1, tau2, size=1):
    t1 = np.random.exponential(tau1, size)
    t2 = np.random.exponential(tau2, size)
    return t1 + t2

# Draw samples of waiting times
waiting_times = successive_poisson(764, 715, size=100000)
_ = plt.hist(waiting_times, bins=100, histtype='step', normed=True)
_ = plt.xlabel('total waiting time (games)')
_ = plt.ylabel('PDF')
plt.show()



