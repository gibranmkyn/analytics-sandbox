# get an index. But index is not as convinient as dictionary
countries = ['indonesia', 'france', 'germany', 'norway']
capitals = ['jakarta', 'paris', 'berlin', 'oslo']

ind_indonesia = countries.index("indonesia")
print(capitals[ind_indonesia])

# -------------- Dictionary -------------------
world = {'indonesia':'jakarta', 'france':'paris', 'germany':'berlin', 'norway':'oslo' }
print(world.keys()) #gives out indonesia, france, germany, and norway
print(europe['norway']) #oslo

print('malaysia' in world) #to check whether malaysia in the dictionary

del(europe['australia']) # delete australia from the dict

#double dictionary
europe = { 'spain': { 'capital':'madrid', 'population':46.77 },
           'france': { 'capital':'paris', 'population':66.03 },
           'germany': { 'capital':'berlin', 'population':80.62 },
           'norway': { 'capital':'oslo', 'population':5.084 } }

print(europe['france']['capital']) #get paris


# -------------- Pandas Dataframe -------------------
import pandas as pd
names = ['United States', 'Australia', 'Japan', 'India', 'Russia', 'Morocco', 'Egypt']
dr =  [True, False, False, False, True, True, True]
cpc = [809, 731, 588, 18, 200, 70, 45]

my_dict = {'country':names, 'drive_right':dr, 'cars_per_cap':cpc}
cars = pd.DataFrame(my_dict)
print(cars)
row_labels = ['US', 'AUS', 'JAP', 'IN', 'RU', 'MOR', 'EG']
cars.index = row_labels #define row lables

cars = pd.read_csv('cars.csv',index_col=0) # import from csv file

print(cars[['country','drives_right']]) # slice df

print(cars.loc[:,'drives_right']) # Print out drives_right column as Series

print(cars.loc[:,['drives_right']]) # Print out drives_right column as DataFrame


# -------------- Logic, control flow, and filtering -------------------


import numpy as np
my_house = np.array([18.0, 20.0, 10.75, 9.50])
your_house = np.array([14.0, 24.0, 14.25, 9.0])
np.logical_or(my_house > 18.5, my_house < 10) # or coniditions
np.logical_and() # and conditions
np.logical_not() # not conditions


cpc = cars.loc[:,'cars_per_cap'] # Create car_maniac: observations that have a cars_per_cap over 500
many_cars = cpc > 500 
x = cars[many_cars]


cpc = cars['cars_per_cap'] # Create medium: observations with cars_per_cap between 100 and 500
medium =  cars[np.logical_and(cpc >= 100, cpc <= 500)]


# -------------- Loops -------------------
offset = -6 # While loop
while offset != 0 :
    print("correcting...")
    if offset > 0 : 
        offset = offset - 1
    else :
        offset = offset + 1    
    print(offset)    

for lab, row in cars.iterrows() : #loops using columns name
    print(row['country'] + ": " + str(row['cars_per_cap']))

for lab, row in cars.iterrows() : # Add new column COUNTRY
    cars.loc[lab, "COUNTRY"] = row["country"].upper()

    cars["COUNTRY"] = cars["country"].apply(str.upper)  # same as the above, shorter version.




# -------------- Hacker Statistics -------------------

step = 50 
dice = np.random.randint(1,7) # Roll the dice
if dice <= 2 : #dice construct
    step = step - 1
elif dice <= 5 :
    step = step + 1
else :
    step = step + np.random.randint(1,7)

#.........

random_walk = [0]

for x in range(100) : # set random 100 times

    step = random_walk[-1] # Set step: last element in random_walk

    dice = np.random.randint(1,7)

    if dice <= 2: # Determine next step
        step = max(0, step-1) # Ensure step doesn't go below 0
    elif dice <= 5:
        step = step + 1
    else:
        step = step + np.random.randint(1,7)

    random_walk.append(step) # append next_step to random_walk

import matplotlib.pyplot as plt
plt.plot(random_walk) # Plot random_walk
plt.show() # Show the plot

#......
all_walks = []
for i in range(10) :
    random_walk = [0]
    for x in range(100) :
        step = random_walk[-1]
        dice = np.random.randint(1,7)
        if dice <= 2:
            step = max(0, step - 1)
        elif dice <= 5:
            step = step + 1
        else:
            step = step + np.random.randint(1,7)
        random_walk.append(step)
    all_walks.append(random_walk)

 if np.random.rand() <= 0.001 : #clumsiness of falling to step 0
            step = 0

np_aw_t = np.transpose(np.array(all_walks))
plt.plot(np_aw_t)
plt.show()


#......Create distribution

all_walks = []
for i in range(500) :
    random_walk = [0]
    for x in range(100) :
        step = random_walk[-1]
        dice = np.random.randint(1,7)
        if dice <= 2:
            step = max(0, step - 1)
        elif dice <= 5:
            step = step + 1
        else:
            step = step + np.random.randint(1,7)
        if np.random.rand() <= 0.001 :
            step = 0
        random_walk.append(step)
    all_walks.append(random_walk)

np_aw_t = np.transpose(np.array(all_walks))

: ends
ends = np_aw_t[-1] # Select last row from np_aw_t

plt.hist(ends)
plt.show()
