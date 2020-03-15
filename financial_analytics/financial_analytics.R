# -------------- Financial Analytics -----------------------------------------------------------------





# define inputs
price <- 20
print_cost <- 0.5
ship_cost <- 2

# add revenue, expense, and profit variables
cashflow <- assumptions
cashflow$revenue <- cashflow$sales * price
cashflow$direct_expense <- cashflow$sales * (print_cost + ship_cost) 
cashflow$gross_profit <- cashflow$revenue - cashflow$direct_expense





# NET INCOME = OPERATING PROFIT - TAX
	# TAX = OPERATING PROFIT * TAX RATE
	# OPERATING PROFIT = GROSS PROFIT - OVERHEAD COST
					# GROSS PROFIT = OPERATING REVENUE - DIRECT EXPENSES
					# OVERHEAD EXPENSE = sales general & administratives (SGA) + DEPRECIATION + AMORTISATION
						# DEPRECIATION (straightline) = (book value (amount paid to buy asset) - salvage value (estimated amount sellable)) / lifetime


# NET INCOME = REVENUE - DIRECT EXPENSE - OPERATING EXPENSE - TAX
# CASHFLOW = NET INCOME + DEPRECIATION EXPENSE - CAPEX + net working capital (NWC)





# ---------- Premium subcription spotify

# Subscription Revenue: (Percent Active) x (Revenue / Subscription)
# Song Royalty Expense: (# Songs Played) x (Cost / Song)

# premium business models
premium_model <- premium
premium_model$SONGS_PLAYED <- premium$ACTIVITY_RATE * premium$HOURS_PER_MONTH / premium$SONG_LENGTH
premium_model$REV_SUBSCRIPTION <- premium$ACTIVITY_RATE * premium$REV_PER_SUBSCRIBER
premium_model$COST_SONG_PLAYED <- premium_model$SONGS_PLAYED * premium$COST_PER_SONG

# ---------- Freemium subcription spotify

Freemium

 MONTH ACTIVITY_RATE HOURS_PER_MONTH PROP_MUSIC REV_PER_AD REV_PER_CLICK
  1             1.0000          358.32     0.95        0.02            10
COST_PER_SONG AD_LENGTH SONG_LENGTH
     0.01      0.25           3

# Revenue for Ads Played:(# Ads Played) x (Revenue / Ad)
# Song Royalty Expense: (# Songs Played) x (Cost / Song)

# freemium business models
freemium_model <- freemium
freemium_model$SONGS_PLAYED <- freemium$ACTIVITY_RATE * freemium$HOURS_PER_MONTH * freemium$PROP_MUSIC / freemium$SONG_LENGTH
freemium_model$ADS_PLAYED <- freemium$ACTIVITY_RATE * freemium$HOURS_PER_MONTH * (1-freemium$PROP_MUSIC) / freemium$AD_LENGTH
freemium_model$REV_AD_PLAYED <- freemium_model$ADS_PLAYED * freemium$REV_PER_AD
freemium_model$COST_SONG_PLAYED <- freemium_model$SONGS_PLAYED * freemium$COST_PER_SONG




# Define function: calc_business_model
# assumptions {year: sales} year 1: 175, year 2: 200,.... 
calc_business_model <- function(assumptions, price, print_cost, ship_cost){
    cashflow <- assumptions
    cashflow$revenue <- cashflow$sales * price
    cashflow$direct_expense <- cashflow$sales * (print_cost + ship_cost) 
    cashflow$gross_profit <- cashflow$revenue - cashflow$direct_expense
    return(cashflow)
}

# Call calc_business_model function for different sales prices
calc_business_model(book_assumptions, 20, 0.5, 2)$gross_profit
calc_business_model(book_assumptions, 25, 0.5, 2)$gross_profit




# Income statement / P&L report


# Depcriciation

# Inputs
cost <- 100000
life <- 60
salvage <- 10000

# Compute depreciation
production$Depr_Straight <- (cost - salvage)/life #straightline
production$Depr_UnitsProd <- (cost - salvage)*(production$Units) / sum(production$Units) #units produced

# Plot two depreciation schedules
ggplot(production, aes(x = Month)) + 
    geom_line(aes(y = Depr_Straight)) + 
    geom_line(aes(y = Depr_UnitsProd))



# Calculate income statement
income_statement <- assumptions
income_statement$revenue <- income_statement$unit_sales * price_per_unit
income_statement$expenses <- income_statement$unit_sales * (cogs_per_unit + labor_per_unit)
income_statement$earnings <- income_statement$revenue - income_statement$expenses - income_statement$depreciation

# Summarize cumulative earnings
sum(income_statement$earnings)
sum(income_statement$earnings) / sum(income_statement$revenue) # 10% margin


# calculate free cashflow

#Ouch! After factoring in our capital investments/expenditures (that is, the machines), 
#-$530M in cash is a much less attractive prospect. 
#As Bezos concludes: it’s more subtle and complex in the real world, [but] this issue — 
#the duality between earnings and cash flow — comes up all the time.

cashflow <- income_statement
cashflow$operating_cf <- cashflow$earnings + cashflow$depreciation
cashflow$capex <- cashflow$machines_purchased * 160000000
cashflow$free_cf <- cashflow$operating_cf - cashflow$capex

# summarize free cashflow
sum(cashflow$free_cf)



# ---------- TIME VALUE OF MONEY

FUTURE_VALUE = PRESENT_VALUE * (1+r)^n
PRESENT_VALUE = FUTURE_VALUE / (1+r)^n


# Define PV function: calc_pv
calc_pv <- function(fv, r, n){
    pv <- fv /(1+r)^n
    return(pv)
}

# Use PV function for range of inputs
n_range <- 1:10
pv_range <- calc_pv(100, 0.08, n_range)
pv_range

# Calculate present values in dataframe
present_values <- data.frame(n = 1:10) %>% mutate(pv = calc_pv(100,0.08,n))

# Plot relationship between time periods versus present value
ggplot(present_values, 
       aes(x = n, y = pv)) +
  geom_line() +
  geom_label(aes(label = paste0("$",round(pv,0)))) +
  ylim(0,100) +
  labs(
    title = "Discounted Value of $100 by Year Received",
    x = "Number of Years in the Future",
    y = "Present Value ($)"




# PV by discount rate and time delay
# Wow! As time period increases, large discount rates decrease our value much faster.
# Calculate present values over range of time periods and discount rates
present_values <- 
  expand.grid(n = 1:10, r = seq(0.05,0.12,0.01)) %>%
  mutate(pv = calc_pv(100, r, n))
     
# Plot present value versus time delay with a separate colored line for each rate
ggplot(present_values, aes(x = n, y = pv, col = factor(r))) +
  geom_line() +
  ylim(0,100) +
  labs(
    title = "Discounted Value of $100 by Year Received",
    x = "Number of Years in the Future",
    y = "Present Value ($)",
    col = "Discount Rate"
  )


# Rates of different duration
# New Rate = (1 + Current Rate) ^ (# Current Time Periods / 1 New Time Period) - 1

# Convert monthly to other time periods
r1_mth <- 0.005
r1_quart <- (1 + r1_mth)^3 - 1
r1_semi <- (1 + r1_mth)^6 - 1
r1_ann <- (1 + r1_mth)^12 - 1

# Convert years to other time periods
r2_ann <- 0.08
r2_mth <- (1 + r2_ann)^(1/12) - 1
r2_quart <- (1 + r2_ann)^(1/4) - 1


# --- Real versus Nominal Rates
# Recall that we want to discount real (no inflation) cashflows with the real discount rate 
# or nominal (with inflation) cashflows with the nominal discount rate. 
# Nominal Rate = (1 + Real Rate) * (1 + Inflation Rate) - 1

# Convert real to nominal
r1_real <- 0.08
inflation1 <- 0.03
(r1_nom <- (1 + r1_real)*(1+inflation1) - 1)

# Convert nominal to real
r2_nom <- 0.2
inflation2 <- 0.05
(r2_real <- (1+r2_nom)/(1+inflation2) - 1)


# --- Selling a car
# Imagine you are selling your car and have received two offers:
# Buyer A will pay you $5,000 today
# Buyer B will pay you $1,000 per year for the next six years, starting one year from now
# Regardless of which offer you take, you will reinvest your profits for a 6% annual return. Let's evaluate which offer is better.

# Define PV function: calc_pv
calc_pv <- function(fv, r, n){
    pv <- fv /(1+r)^n
    return(pv)
}

# Define cashflows
cashflow_a <- c(5000, rep(0,6))
cashflow_b <- c(0, rep(1000, 6))

# Calculate pv for each time period
disc_cashflow_a <- calc_pv(cashflow_a, 0.06, 0:6)
disc_cashflow_b <- calc_pv(cashflow_b, 0.06 ,0:6)

# Calculate and report total present value for each option
(pv_a <- sum(disc_cashflow_a))
(pv_b <- sum(disc_cashflow_b))


# --- Licensing a software

# Your corporation is considering switching software vendors. For each license:

# the current software license costs $500 per year
# the new software has a one-time fixed cost of $2,200 per license in the present but only $300 per year in subsequent years
# The two software products are substitutes (they do an equally good job at meeting the same need) 
# and whichever one is chosen will be used for the next 10 years. After 10 years, either contact would expire.
# assume a 12% discount rate.

# Define cashflows
cashflow_old <- rep(-500, 11)
cashflow_new <- c(-2200, rep(-300, 10))
options <- 
    data.frame(time = rep(0:10, 2),
               option = c(rep("Old",11),rep("New",11)),
               cashflow = c(cashflow_old, cashflow_new))
                
# Calculate total expenditure with and without discounting
options %>%
    group_by(option) %>%
    summarize(sum_cashflow = sum(cashflow),
              sum_disc_cashflow = sum(calc_pv(cashflow, 0.12, time)) )


# ---------- Prioritising profitability


# Profitability metrics: quanitfy how and when a project contributes value to a firm
# Decision rules: interpret metrics for decision and comparisons
# Interpretation affected by absolute ($) and relative (%)


# Nice work! You can run payback_period in the console to see at what period you would expect to break even. 
# However, recall that this metric can be misleading because it isn't accounting for the factor that our future cashflows are worth less.

# Define payback function: calc_payback
calc_payback <- function(cashflows) {

  cum_cashflows <- cumsum(cashflows)
  payback_period <- min(which(cum_cashflows >= 0)) - 1
  payback_period

}

# Test out our function
cashflows <- c(-100, 50, 50, 50)
calc_payback(cashflows) == 2




#  However, it is informative to see what difference this would make. Since we now have both calc_payback() and calc_pv() functions



# normal payback period
payback_period <- calc_payback(cashflows)

# discounted payback period
discounted_cashflows <- calc_pv(cashflows, r = 0.06, n = 0:(length(cashflows)-1) )
payback_period_disc <- calc_payback(discounted_cashflows)



# --- Net Present Value (NPV): sum of all discounted cash flow - investment cost. Gold standard
# invest when NPV > 0. 
# pitfalls: Tend to ignore project size, and highly sensitive to the discount rate.

calc_npv <- function(cashflows, r) {

  n <- 0:(length(cashflows) - 1)
  npv <- sum( calc_pv(cashflows, r, n) )
  return(npv)

}

# --- Internal Rate of Return (IRR): the required rate for the investment to break-even
# prefers IRR > discount rate. 
# pitfalls : doesn't consider order of magnitude, could have zero or multipl IRRs, assumes 100% reinvestment of all cashflows at same rate

# Define IRR function: calc_irr
calc_irr <- function(cashflows) {

    stats::uniroot(calc_npv, 
        interval = c(0, 1), 
        cashflows = cashflows)$root
    
}

# Try out function on valid input
cashflows <- c(-100, 20, 20, 20, 20, 20, 20, 10, 5)
calc_irr(cashflows)


# --- Profitability Index: sum(future_discounted_value) / cost of initial investment
# PI > 1, take.
# pitfalls: doesn't capture magnitude, and highly sensitive to discount rate

# Define profitability index function: calc_profitability_index
calc_profitability_index <- function(init_investment, future_cashflows, r) {
    discounted_future_cashflows <- calc_npv(future_cashflows, r)
    discounted_future_cashflows / abs(init_investment)
}

# Try out function on valid input
init_investment <- -100
cashflows <- c(0, 20, 20, 20, 20, 20, 20, 10, 5)
calc_profitability_index(init_investment, cashflows, 0.08)


# --- Terminal Value (TV): all future cashflows after the forecasting period
# perpetuity method assumes constant growth rate forever (less than discount rate)
# Assume a discount rate of 15%. Let's try out the perpetuity growth model and 
# find the present value of the terminal value for growth rates of 10%, 1%, and -5%. I
# TV = cashflow period n / ((discount rate - tv growth rate) * (1 + discount rate))^period n

# pull last year cashflow from the cashflow vector
last_year_cashflow <- cashflow[length(cashflow)]
last_period_n <- length(cashflow) - 1

terminal_value_1 <- last_year_cashflow / ((0.15 - 0.1)*(1 + 0.15)^last_period_n)
terminal_value_2 <- last_year_cashflow / ((0.15 - 0.01)*(1 + 0.15)^last_period_n)
terminal_value_3 <- last_year_cashflow / ((0.15 - (-0.05))*(1 + 0.15)^last_period_n)



# ---- Comparing the metrics

# NPV = 0 <---------> IRR = Discount Rate
# In the video, we made the claim and explored the intuition that IRR and NPV will always lead to 
# the same conclusion regarding whether or not a potential project is profit


# calculate internal rate of return (IRR) for each stream of cashflows
r1 <- calc_irr(cashflow1)

# calculate net present value (NPV) for each stream of cashflows, assuming r = irr
npv1 <- calc_npv(cashflow1, r1)


# In the environment are cf1, and cf2, denoting two streams of cashflows for us to value. 
# Additionally, there is a vector rates with values of discount rates ranging from 0% to 25%, by 0.5% intervals.

# create dataset of NPV for each cashflow and rate
npv_by_rates <- data.frame(rates) %>%
	group_by(rates) %>%
    mutate(
        npv1 = calc_npv(cf1, rates),
        npv2 = calc_npv(cf2, rates))
   
# plot cashflows over different discount rates     
ggplot(npv_by_rates, aes(x = rates, y = npv1))+
  geom_line() +
  geom_line(aes(y = npv2)) +
  labs( title = "NPV by Discount Rate", subtitle = "A Tale of Two Troubling Cashflows",
      y = "NPV ($)",x = "Discount Rate (%)") +
  annotate("text", x = 0.2, y = -500, label = "Two break-even points") +
  annotate("text", x = 0.2, y = -2500, label = "No break-even point")




# ---- NPV vs IRR comparison plot
# This example shows how different metrics capture different aspects of value. 
# It's always good to keep their strengths and weaknesses in mind and to examine multiple metrics.

# calculate summary metrics
cashflow_comparison <-
  all_cashflows %>%
  group_by(option) %>%
  summarize( npv = calc_npv(cashflow, 0.1),
             irr = calc_irr(cashflow) )
             
# visualize summary metrics
ggplot(cashflow_comparison,
       aes(x = npv, y = irr, col = factor(option))) +
  geom_point(size = 5) +
  geom_hline(yintercept = 0.1) +
  scale_y_continuous(label = scales::percent) +
  scale_x_continuous(label = scales::dollar) +
  labs(title = "NPV versus IRR for Project Alternatives",
       subtitle = "NPV calculation assumes 10% discount rate",
       caption = "Line shows actual discount rate to asses IRR break-even",
       x = "NPV ($)", y = "IRR (%)", col = "Option")



# ----- Case study coffee
head(assumptions)
  year unit_sales_per_day capex pct_cannibalization maintenance_cost
    0                  0  5000                0.00                0
    1                 10     0                0.25              250
  depreciation_cost profit_margin_per_nitro profit_margin_per_regular
                 0                       3                         1
               500                       3                         1
  labor_cost_per_hour days_open_per_year
                   8                250
                   8                250




# Create the cashflow_statement dataframe
cashflow_statement <-
  mutate(assumptions,
         # business model
    sales_per_year = unit_sales_per_day * days_open_per_year,
    sales_revenue = sales_per_year * profit_margin_per_nitro,
    labor_cost = days_open_per_year * 0.5 * labor_cost_per_hour,
    cannibalization_cost = sales_per_year * pct_cannibalization * profit_margin_per_regular,
         # financial metrics
    total_revenue = sales_revenue,
    direct_expense = labor_cost + cannibalization_cost + maintenance_cost,
    gross_profit = total_revenue - direct_expense,
    operating_income = gross_profit - depreciation_cost,
    net_income = operating_income * (1 - tax_rate), 
    cashflow = net_income + depreciation_cost - capex    
  )



# ---- Scenario Analysis
library(purr)
library(tidyr)


# Realist: This is the "best guess" set of assumptions you worked with in exercise 3.
# Pessimist: Competition decreases sales by 20% and nitro coffee profit margins down to $1 from $3.
# Optimist: Sales are 20% higher than our estimate with cannibalization only accounting for 10%. This stuff is flying off the shelves!

# build individual scenarios
optimist <- mutate(assumptions, unit_sales_per_day = unit_sales_per_day * 1.2, pct_cannibalization = 0.1)
pessimist <- mutate(assumptions, unit_sales_per_day = unit_sales_per_day * 0.8, profit_margin_per_nitro = 1/3*profit_margin_per_nitro)

# combine into one dataset
scenarios <-
  bind_rows(
    mutate(pessimist, scenario = "pessimist"),
    mutate(assumptions, scenario = "realist"),
    mutate(optimist, scenario = "optimist")
  )