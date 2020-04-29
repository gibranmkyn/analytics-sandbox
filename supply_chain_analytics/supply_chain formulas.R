

750 *12 # Da = 9000
500 * 12 # Db = 6000
10*12# Dc = 120

Co = 50 + 10 = 60


# -- EOQ: sqrt(2 * Co * D/ Cc)
sqrt(2* 50 *9000/1) # EOQ unit A = 948.68 ~ 949

sqrt(2* 50 *6000/2) # EOQ unit B = 547.72 ~ 548

sqrt(2* 50 *120/3) # EOQ unit C = 63.24 ~ 64


# -- Order cost: Co * D / Q
50*9000/949 # Order cost A =  474.18
# -- Carrying cost: Cc * Q / 2
1*949/2 # Carrying cost cost A = 474.5
474.18 + 474.5 # Total inv cost A = 948.68


# -- Order cost: Co * D / Q
50*6000/548 # Order cost B =  547.44
# -- Carrying cost: Cc * Q / 2
2*548/2 # Carrying cost cost B = 548
547.44 + 548 # Total inv cost B = 1095.44


# -- Order cost: Co * D / Q
50*120/64 # Order cost C =  93.75
# -- Carrying cost: Cc * Q / 2
3*64/2 # Carrying cost cost C = 96
93.75 + 96 # Total inv cost C = 189.75

948.68 + 1095.44 + 189.75 # Total inv cost = 2233.87 pounds per annum



10/10 # mA = 1
10/21.21 # mB = 0.47 ~~ 1
10/4.24 # mC = 2.35 ~~ 3

9000/949

300/9.48

# ----------------------------------- ORDERING JOINTLY FOR A SELECTED SUBSET ========

# 3 products A (9000), B (6000), C (120), Co = 50, Co_i = 10, Cc_1 = 1, Cc_1 = 2, Cc_3 =3

# -- 1) Identify most frequently ordered product i, assuming each product ordered independently
# ni bar = sqrt( Cc_i * D_i / (2*(Co + Co_i)))
sqrt( 1 * 9000 / (2*(50 + 10))) # n1 A = 8.66
sqrt( 2 * 6000 / (2*(50 + 10))) # n2 B = 10
sqrt( 3 * 120 / (2*(50 + 10))) # n3 C = 1.73


# -- 2) For all products i!=i*, ignore flat fee
# ni tilda = sqrt( Cc_i * D_i / (2*Co_i))
sqrt( 1 * 9000 / (2*10)) # n2 tilda A = 21.21
sqrt( 3 * 120 / (2*10)) # n3 tilda C = 4.24

# -- 3) For all products i!=i*, evaluate frequency of product i relative to product i*, every m-th time
# mi = [ni* / n-tilda i]
10/10 # mA = 1
10/21.21 # mB = 0.47 ~~ 1
10/4.24 # mC = 2.35 ~~ 3

# -- 4) Recalculate ordering freq of the most freq ordered i* to be n
# n = sqrt( SUM(Cc_i * m_i * D_i)/ (2*(Co + SUM(Co_i / m_i))))

sqrt( (1 * 1 * 9000) + (2 * 1 * 6000) + (3 * 3 * 120) / 
        (2* (50 + 10/1 + 10/1 + 10/3)) ) # nA= 144.93 /year

144.93/1 # nB = 144.93 / year
144.93/3 # nC = 48.31 / year

# -- 5) Annual Order Cost
# nCo + nA*Co_A + nB*Co_B + nH*Co_H
144.93*50 + 144.93*10 + 144.93*10 + 48.31*10 # 10628.2



2233.87 - 12113.3




















# Check Purchasing cost + ss cost
3000 * 10400 # Purchase cost Reliable = 31200000
83 * 100 # ss cost = 8300
31200000 + 8300 # Reliable total cost = 31208300

2900 * 10400 # Purchase cost Netoptics = 30160000
184 * 100 # ss cost = 18400
30160000 + 18400 # Netoptics total cost = 30178400

31208300 - 30178400 # difference of 1029900. Choose Reliable!



# ----------------------------------- INVENTORY RELATED ========

# -- EOQ: sqrt(2 * Co * D/ Cc)
sqrt(2* 4000 *12000/100) # EOQ = 979.79 units

# -- Order cost: Co * D / Q
4000*12000/980 # Order cost =  48979.59 per annum

# -- Carrying cost: Cc * Q / 2
100*980/2 # Carrying cost cost = 49000 per annum

# -- Total Annual Inventory Cost: Order cost + Carrying Cost [* nr of products]. n.b. product cost(Cp*D is not relevant)
(48979.59 + 49000)*4 # total inventory cost = 1200

# -- Average time between orders : days * Q / D.... D/Q is number of orders pa
300*120/5000 # time between order 7.2 days








# -- ss : qnorm(CSL) * stdev * sqrt(L)
qnorm(0.99) * 10 * sqrt(7) # ss = 61.55 ~ 62

# -- Reorder point s : L*D + ss
7*40 + 62 # 342


4/200





# ----------------------------------- SAFETY STOCK ========


# -- ss : qnorm(CSL) * stdev * sqrt(L) [*k]
# k in case of multiple locals
qnorm(0.9) * 500 * sqrt(2) # ss = 905.1

# -- Reorder point s : L*D + ss
2*2500 + 905.1 # 5905.1

# -- ss aggregation: qnorm(CSL) * stdev_a * sqrt(L)
# if no correlation, D_a = kD, standev_a = standev_dec * sqrt(k) 
40 * sqrt(5)

# -- ss cost: ss * Cc



# ----------------------------------- FILL RATE ========
1/sqrt(2*3.14)*exp(1)^(-x^2)/2 /# std normal distribution = 

# Expected shortage ESC
# - ss * [1 - qnorm( ss/(sqrt(L)*stdev)] + (sqrt(L)*std_norm(ss/(sqrt(L)*stdev))




# ----------------------------------- NEWSVENDOR PROBLEM ========

prod_cost = 35
sell_price= 57
salvage = 25
35 - 25 # Co (overstocking) = 10
57 - 35 # Cu (understocking) = 22


# -- Critical Ratio: Cu / (Co + Cu)
22 / (22 + 10) # Critical Ratio 0.68, Q* = 290



# expected sales: sell price * p(higher than demand and lower)
57 * ( (0.45 * 290) + (0.10 * 280) + (0.20 * 270) + (0.15 * 260) + (0.10 * 250)) # expected sales = 15760.5

# expected salvage: salvage * prob(Demand lower than [Q*])
25 * ( (0.10 * 10) + (0.20 * 20) + (0.15 * 30) + (0.10 * 40)) # expected salvage = 337.5

# expected cost: prod_cost * [Q*]
35 * 290 # expected cost = 10150

# -- Expected Profit: expected_sales + expected salvages - expected cost
15760.5 + 337.5 - 10150 # expected profit = 5948



# ----------------------------------- GANTT CHART ========
(2) * 100 # MB = 200
(3*2) * 100 # MC = 600
(2) * 100 # MD = 200
(2*2) * 100 # ME = 400
(1*2) * 100 # MF = 200
(2*2 + 2 + 2*1*2) * 100 # MG = 1000


# ----------------------------------- CUMSUM TABLE ========
# -- K = slack / 2 * stdev
0.5 / 2 * 1 # K = 0.25

# -- Ci+ : max(0, xi - (Mu +K) + Ci-1+)
max(0, 10.69 - 10.25 + 0) # A = 0.44
max(0, 9.54 - 10.25 + 0.44) # C = 0
# -- Ci- : max(0, ((Mu - K) - xi) + Ci-1+)
max(0, 9.75 - 10.25 + 0) # B = 0
max(0, 9.75 - 9.54 + 0) # D = 0.21



# ----------------------------------- CONTROL CHART UCL/LCL ========
sqrt(( (21.6 - 22.9)^2 + (22.1 - 22.9)^2 + (24.7 - 22.9)^2 + (23.3 - 22.9)^2 ) / 3) # s1 = 1.38
sqrt(( (21.2 - 21.8)^2 + (20.9 - 21.8)^2 + (23.6 - 21.8)^2 + (21.4 - 21.8)^2 ) / 3) # s2 = 1.23
sqrt(( (21.8 - 21.3)^2 + (21.0 - 21.3)^2 + (20.4 - 21.3)^2 + (21.8 - 21.3)^2 ) / 3) # s3 = 0.68

(1.38 + 1.23 + 0.68) / 3 # average s = 1.09

(21.6+22.1+24.7+23.3) / 4 # x1 = 22.93
(21.2+20.9+23.6+21.4) / 4 # x2 = 21.78
(21.8+21.0+20.4+21.8) / 4 # x3 = 21.25

(22.93 + 21.78 + 21.25) / 3 # average x = 21.99

# -- UCL = xbar + (3*sbar / c *sqrt(n))
21.99 + (3*1.09 / (0.8862 * sqrt(4))) # UCL = 23.83

# -- LCL = xbar - (3*sbar / c *sqrt(n))
21.99 - (3*1.09 / (0.8862 * sqrt(4))) # LCL = 20.15



# ----------------------------------- SAVINGS HEURISTICS ========
# -- Sij = d0i + dj0 - dij
# compute for all, order from biggest savings
10 + 12 - 8 # s23 = 14
10 + 9 - 13 # s24 = 6
10 + 8 - 9 # s25 = 9



# ----------------------------------- ACCEPTANCE SAMPLING ========

# -- minimum n to accept 
1.42 / 0.025 # -1.42 + 0.025n = 0, n accept = 56.8 ~ 57

# -- minimum n to reject 
2.1/ 0.975 # 2.1 + 0.025n = n, n reject = 2.15 ~ 3













# ----------------------------------- ORDERING JOINTLY FOR A SELECTED SUBSET ========

# 3 products L (12000), M (1200), H (120), Co = 4000, Co_i = 1000, Cc_i = 100 for all products

# -- 1) Identify most frequently ordered product i, assuming each product ordered independently
# ni bar = sqrt( Cc_i * D_i / (2*(Co + Co_i)))
sqrt( 100 * 12000 / (2*(4000 + 1000))) # n1 L = 10.95 ~ 11
sqrt( 100 * 1200 / (2*(4000 + 1000))) # n2 M = 3.46 ~ 3.45
sqrt( 100 * 120 / (2*(4000 + 1000))) # n3 H = 1.09 ~ 1.1


# -- 2) For all products i!=i*, ignore flat fee
# ni tilda = sqrt( Cc_i * D_i / (2*Co_i))
sqrt( 100 * 1200 / (2*1000)) # n2 tilda M = 7.74 ~ 7.7
sqrt( 100 * 120 / (2*1000)) # n3 tilda H = 2.49 ~ 2.4

# -- 3) For all products i!=i*, evaluate frequency of product i relative to product i*, every m-th time
# mi = [ni* / n-tilda i]
11/11 # mL = 1
11/7.7 # mM = 1.42 ~~ 2
11/2.4 # mH = 4.58 ~~ 5

# -- 4) Recalculate ordering freq of the most freq ordered i* to be n
# n = sqrt( SUM(Cc_i * m_i * D_i)/ (2*(Co + SUM(Co_i / m_i))))

sqrt( (100 * 1 * 12000) + (100 * 2 * 1200) + (100 * 5 * 120) / 
        (2* (4000 + 1000/1 + 1000/2 + 1000/5)) ) # nL= 11.47 /year

11.47/2 # nM = 5.74 / year
11.47/5 # nH = 2.29 / year

# -- 5) Annual Order Cost
# nCo + nL*Co_L + nM*Co_M + nH*Co_H
11.47*4000 + 11.47*1000 + 5.74*1000 + 2.29*1000 # 65383.5


