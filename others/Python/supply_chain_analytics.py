from pulp import *


# Initialize Class
model = LpProblem("Maximize Glass Co. Profits", LpMaximize)

# Define Decision Variables
wine = LpVariable('Wine', lowBound=0, upBound=None, cat='Integer')
beer = LpVariable('Beer', lowBound=0, upBound=None, cat='Integer')

# Define Objective Function
model += 5 * wine + 4.5 * beer

# Define Constraints
model += 6 * wine + 5 * beer <= 60
model += 10 * wine + 20 * beer <= 150
model += wine <= 6

# Solve Model
model.solve()
print("Produce {} batches of wine glasses".format(wine.varValue))
print("Produce {} batches of beer glasses".format(beer.varValue))


#You are consulting for kitchen oven manufacturer helping to plan their logistics for next month. 
#There are two warehouse locations (New York, and Atlanta), and 
#four regional customer locations (East, South, Midwest, West). 
#The expected demand next month for East it is 1,800, for South it is 1,200, for the Midwest it is 1,100, and for West it is 1000. 
#The cost for shipping each of the warehouse locations to the regional customer's is listed in the table below. 
#Your goal is to fulfill the regional demand at the lowest price.

# Initialize Model
model = LpProblem("Minimize Transportation Costs", LpMinimize)

# Build the lists and the demand dictionary
warehouse = ['New York', 'Atlanta']
customers = ['East', 'South', 'Midwest', 'West']
regional_demand = [1800, 1200, 1100, 1000]
demand = dict(zip(customers, regional_demand))

# Define Objective
model += lpSum([costs[(w, c)] * var_dict[(w, c)] 
                for c in customers for w in warehouse])

# Constraints: for each customer, sum warehouse shipments and set equal to customer demand
for c in customers:
    model += lpSum([var_dict[(w, c)] for w in warehouse]) == demand[c]



# -------------- Modelling PulP -----------------------------------------------------------------


# -------------- Logistics Planning Problem
# You are again consulting for kitchen oven manufacturer helping to plan their logistics. 
# This time you are attempting to put together a plan for the next six months (Jan.-Jun.). 
# There are still two warehouse locations (New York, and Atlanta), and four regional customer locations (East, South, Midwest, West). 
# The cost for shipping for each of the warehouse locations to the regional customer's is listed in the table below. 
# Your goal is to determine the number of shipments from each warehouse to customers that provides the lowest costs.


# Define decision variables
key = [(m, w, c) for m in months for w in warehouse for c in customers]
var_dict = LpVariable.dicts('num_of_shipments', 
                            key, 
                            lowBound=0, cat='Integer')

# Use the LpVariable dictionary variable to define objective
model += lpSum([costs[(w, c)] * var_dict[(m, w, c)] 
                for m in months for w in warehouse for c in customers])


# -------------- Travelling Salesman Problem
# The Traveling Salesman Problem (TSP) is a popular problem and has applications is logistics. 
# In the TSP a salesman is given a list of cities, and the distance between each pair. 
# He is looking for the shortest route going from the origin through all points before going back to the origin city again. 
# This is a computationally difficult problem to solve but Miller-Tucker-Zemlin (MTZ) showed it can be completed using Integer Linear Programing. 


# Define Decision Variables
x = LpVariable.dicts('X', [(c1, c2) for c1 in cities for c2 in cities], 
                     cat='Binary')
u = LpVariable.dicts('U', [c1 for c1 in cities], 
                     lowBound=0, upBound=(n-1), cat='Integer')

# Define Objective
model += lpSum([dist.iloc[c1, c2] * x[(c1, c2)] 
                for c1 in cities for c2 in cities])

for c2 in cities:
    model += lpSum([x[(c1, c2)] for c1 in cities]) == 1 # each city be arrived at from exactly one other city
for c1 in cities:
    model += lpSum([x[(c1, c2)] for c2 in cities]) == 1 # from each city there is a departure to exactly one other city.


# -------------- Schedulling Workers Problem
#You are looking to hire workers to work in a warehouse. 
#Each worker is expected to work 5 consecutive days and then have two days off. 
#The chart below has the estimated number of workers you will need each day. 
#You are looking to hire the minimum number of workers to handle the workload for each day.

# The class has been initialize, and x, days, and objective function defined
model = LpProblem("Minimize Staffing", LpMinimize)
days = list(range(7))

# Define Decision Variables
x = LpVariable.dicts('staff_', days, lowBound=0, cat='Integer')

# Define Objective
model += lpSum([x[i] for i in days])

# Define Constraints
model += x[0] + x[3] + x[4] + x[5] + x[6] >= 31 #mon
model += x[0] + x[1] + x[4] + x[5] + x[6] >= 45 #tue
model += x[0] + x[1] + x[2] + x[5] + x[6] >= 40 #wed
model += x[0] + x[1] + x[2] + x[3] + x[6] >= 40 #thu
model += x[0] + x[1] + x[2] + x[3] + x[4] >= 48 #fri
model += x[1] + x[2] + x[3] + x[4] + x[5] >= 30 #sat
model += x[2] + x[3] + x[4] + x[5] + x[6] >= 25 #sun

model.solve()


# -------------- Preventive Maintenance Schedulling
#At a quarry they use diamond saws to cut slabs of marble. 
#For preventative maintenance the saws are only allowed to run for 4 consecutive hours, 
#afterwards a 1 hour inspection is completed before they are allowed to go back into service. 
#The quarry operates 10-hour shifts. 
#At the end of the shift if the saw blades have not been used for 4 consecutive hours 
#the remaining time will be used at the start of the next shift. The expected number of saw blades needed for each hour is listed below. 
#Our goal is to determine the minimum number of saw blades are needed for the shift.


# The class has been initialize, and x, hours and objective fuction defined
model = LpProblem("Minimize Staffing", LpMinimize)
hours = list(range(10))

# Define decision variables
x = LpVariable.dicts('saws_', hours, lowBound=0, cat='Integer')

# Define objective function
model += lpSum([x[i] for i in hours])

# Define Constraints
model += x[0] + x[2] + x[3] + x[4] + x[5] + x[7] + x[8] + x[9] >= 7
model += x[0] + x[1] + x[3] + x[4] + x[5] + x[6] + x[8] + x[9] >= 7
model += x[0] + x[1] + x[2] + x[4] + x[5] + x[6] + x[7] + x[9] >= 7
model += x[0] + x[1] + x[2] + x[3] + x[5] + x[6] + x[7] + x[8] >= 6
model += x[1] + x[2] + x[3] + x[4] + x[6] + x[7] + x[8] + x[9] >= 5
model += x[0] + x[2] + x[3] + x[4] + x[5] + x[7] + x[8] + x[9] >= 6
model += x[0] + x[1] + x[3] + x[4] + x[5] + x[6] + x[8] + x[9] >= 6
model += x[0] + x[1] + x[2] + x[4] + x[5] + x[6] + x[7] + x[9] >= 7
model += x[0] + x[1] + x[2] + x[3] + x[5] + x[6] + x[7] + x[8] >= 7
model += x[1] + x[2] + x[3] + x[4] + x[6] + x[7] + x[8] + x[9] >= 6

model.solve()



# --------------  Capacitated Plant Location
# Continue the case study of the Capacitated Plant Location model of a car manufacture. 
# You are given four Pandas data frames demand, var_cost, fix_cost, and cap containing the regional demand (thous. of cars), 
# variable production costs (thous. $US), fixed production costs (thous. $US), and production capacity (thous. of cars).

# Initialize Class
model = LpProblem("Capacitated Plant Location Model", LpMinimize)

# Define Decision Variables
loc = ['USA', 'Germany', 'Japan', 'Brazil', 'India']
size = ['Low_Cap','High_Cap']
x = LpVariable.dicts("production_",
                     [(i,j) for i in loc for j in loc],
                     lowBound=0, upBound=None, cat='Continuous')
y = LpVariable.dicts("plant_", 
                     [(i,s) for i in size for s in size], cat='Binary')

# Define objective function
model += (lpSum([fix_cost.loc[i,s] * y[(i,s)] 
                 for s in size for i in loc])
          + lpSum([var_cost.loc[i,j] * x[(i,j)] 
                   for i in loc for j in loc]))


# --------------  Loading truck with logical constraint
# Your customer has ordered six products to be delivered over the next month. 
# You will need to ship multiple truck loads to deliver all of the products. 
# There is a weight limit on your trucks of 25,000 lbs. 
# For cash flow reasons you desire to ship the most profitable combination of products that can fit on your truck.

#prod has 'A':15000, 'B':25000, ...

model = LpProblem("Loading Truck Problem", LpMaximize)
x = LpVariable.dicts('ship_', prod, cat='Binary')
model += lpSum([prof[i] * x[i] for i in prod])

# Define Constraint
model += lpSum([weight[i] * x[i] for i in prod]) <= 25000
model += x['D'] + x['E'] + x['F'] <= 1 #only product 1 could be selected

model.solve()
for i in prod:
    print("{} status {}".format(i, x[i].varValue))



# You work at a trucking distribution center and you need to decide which of 6 customer locations you will send a truck to. 
# Your goal is to minimize the distance a truck travels.

# dist {'A': 86, 'B': 95, 'C': 205, 'D': 229, 'E': 101, 'F': 209}
# cust ['A', 'B', 'C', 'D', 'E', 'F']

model = LpProblem("Loading Truck Problem", LpMinimize)
x = LpVariable.dicts('ship_', cust, cat='Binary')
model += lpSum([dist[i]*x[i] for i in cust])

# Define Constraint
model += x['A'] + x['B'] + x['C'] + x['D'] + x['E'] + x['F'] >= 1 #  the model selects at least one location
model += x['A'] - x['D'] <= 0 #  if location A is selected then location D is also selected.
model += x['B'] - x['E'] <= 0

model.solve()
for i in cust:
    print("{} status {}".format(i, x[i].varValue))




