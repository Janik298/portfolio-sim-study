#### Preamble ####

library(caret)
library(readr)
library(readxl)
library(tidyverse)
library(zoo)
library(lubridate)
library(Metrics)




###### Part XX: Portfolio Insurance Strategy ######----------------

#### Parameter ####
# Risk-free Rate
r_f <- 0.1

# Time frame in years
time = 1

# Cash to be invested
cash <- 10000

# Initial Price of the underlying
S0 <- 100

# Expected Return - MSCI EM has a roughly 9% annualized return since its inception
mu <- 0.09 

# Volatility of the underlying
sigma <- 0.16 

# Number of time steps per year
N <- 252

# Number of simulated paths of the underlying
mc.loops <- 10000 



#### Simulation of the underlying ####
# Load the functions
source(func_gbm)



# Initialize price matrix for the different paths of our underlying
prices <- matrix(nrow = N + 1, ncol = mc.loops)

# Monte Carlo Simulation of our underlying price development
for (i in 1:mc.loops) {
    prices[, i] <- gbm(S0 = S0, mu = mu, sigma = sigma, T = time, N = N)
}

# End Price and Returns
asset_value_at_maturity <- prices[N+1, ]
returns <- (prices[N+1, ] / S0) - 1
returns <- tibble(returns)

# Histogram of my retruns without portfolio insurance
ggplot(returns, aes(returns)) +
    geom_histogram(bins = 200)


#### Performance Measures ####

# Value at Risk

# Lowest Value of the annual relative returns that is not exceeded in 95% of cases.

returns <- returns$returns

# Value at Risk (VaR) - 95%
var_95 <- quantile(returns, 0.05)
var_99 <- quantile(returns, 0.01)

cat("VaR 95%:", round(var_95*100, 2), "%\n")
cat("VaR 99%:", round(var_99*100, 2), "%\n")


# Quartilsabstand (Interquartile Range)
quartilsabstand <- quantile(returns, 0.75) - quantile(returns, 0.25)
cat("Quartilsabstand:", quartilsabstand, "\n")

# Expected Returns
expected_return <- mean(returns)
cat("Expected Returns:", expected_return, "\n")

# Realized Volatility
realized_vola <- sd(returns)
cat("Realized Volatility:", realized_vola, "\n")

# Sharpe Ratio
r <- r_f  # Assuming the risk-free rate is 0 (you can replace it with the appropriate value)
sharpe_ratio <- (expected_return - r) / realized_vola
cat("Sharpe Ratio:", sharpe_ratio, "\n")




##### Exercise 8: Performance Analysis with risk managament ####----------------------
# Same Params as before


# Function to calculate the Black-Scholes put price
black_scholes_put <- function(S0, K, T, r, vol) {
    d1 <- (log(S0/K) + (r + (vol^2/2)) * T) / (vol * sqrt(T))
    d2 <- d1 - vol * sqrt(T)
    put_price <- (K * exp(-r * T) * pnorm(-d2)) - (S0 * pnorm(-d1))
    return(put_price)
}



# Function to calculate the put values based on strike price and asset values
calculate_put_values <- function(K, asset_values) {
    strike_diff <- K - asset_values
    put_values <- pmax(strike_diff, 0)
    return(put_values)
}


# Parameter Strike Price
K <- 1100

# Calculate the Put Price
put_price <- black_scholes_put(S0, K, T, r_f, vol = sigma)


# Calculate Put Values
put_values <- calculate_put_values(K = K, asset_values = asset_value_at_maturity)


# Aggregate Information and Build a Portfolio
invest <- 10000
share_fraction <- 0.95
put_fraction <- 1 - share_fraction

# Number of Puts and MSCI I can buy
n_shares <- invest * share_fraction / S0
n_puts <- invest * put_fraction / put_price

# Calculate the hedged portfolio returns
hedged_portfolio_values <- (asset_value_at_maturity * n_shares + n_puts * put_values)
hedged_portfolio_returns <- (hedged_portfolio_values / invest - 1)


# Calculate Risk Measures
var_95 <- quantile(hedged_portfolio_returns, 0.05)
var_99 <- quantile(hedged_portfolio_returns, 0.01)
quartilsabstand <- quantile(hedged_portfolio_returns, 0.75) - quantile(hedged_portfolio_returns, 0.25)
expected_return <- mean(hedged_portfolio_returns)
realized_vola <- sd(hedged_portfolio_returns)
sharpe_ratio <- (expected_return - r) / realized_vola

# Print Results
cat("S0:", round(S0, 2), "€\n")
cat("Put Price:", round(put_price, 2), "€\n")
cat("N Shares:", round(n_shares, 2), "N Puts:", round(n_puts, 2), "\n")
cat("VaR 95%:", round(var_95 * 100, 2), "%\n")
cat("VaR 99%:", round(var_99 * 100, 2), "%\n")
cat("Quartilsabstand:", round(quartilsabstand, 4), "\n")
cat("Expected Returns:", round(expected_return, 4), "\n")
cat("Realized Volatility:", round(realized_vola, 4), "\n")
cat("Sharpe Ratio:", round(sharpe_ratio, 4), "\n")



#### Analyze different combinations -----------


# Variable Initialization
invest <- 10000
share_fractions <- seq(0.01, 1.00, 0.01)

# Create an empty data frame to store results
impact_fractions <- data.frame(
    "Strike Price" = numeric(),
    "Share Price" = numeric(),
    "Put Price" = numeric(),
    "Share Fraction" = numeric(),
    "Put Fraction" = numeric(),
    "N Shares" = numeric(),
    "N Puts" = numeric(),
    "VAR95" = numeric(),
    "VAR99" = numeric(),
    "Interquartile Range" = numeric(),
    "Expected Returns" = numeric(),
    "Expected Volatility" = numeric(),
    "Sharpe Ratio" = numeric()
)

bs_put_price <- function(S0, K, r, vol, T) {
    d1 <- (log(S0/K) + (r + (vol^2)/2) * T) / (vol * sqrt(T))
    d2 <- d1 - vol * sqrt(T)
    put_price <- (K * exp(-r * T) * pnorm(-d2)) - (S0 * pnorm(-d1))
    return(put_price)
}

# Loop through different share fractions
for (i in 1:length(share_fractions)) {
    curr_share_fraction <- share_fractions[i]
    curr_put_fraction <- 1 - curr_share_fraction
    
    curr_n_shares <- invest * curr_share_fraction / S0
    curr_n_puts <- invest * curr_put_fraction / bs_put_price(S0, K, r_f, sigma, T)
    
    hedged_portfolio_values <- (curr_n_shares * asset_value_at_maturity + curr_n_puts * put_values)
    hedged_portfolio_returns <- (hedged_portfolio_values / invest) - 1
    
    
    # Calculate Performance Measures
    var_95 <- quantile(hedged_portfolio_returns, 0.05)
    var_99 <- quantile(hedged_portfolio_returns, 0.01)
    quartilsabstand <- quantile(hedged_portfolio_returns, 0.75) - quantile(hedged_portfolio_returns, 0.25)
    expected_return <- mean(hedged_portfolio_returns)
    realized_vola <- sd(hedged_portfolio_returns)
    sharpe_ratio <- (expected_return - r_f) / realized_vola
    
    # Add results to the evaluation data frame
    curr_df <- data.frame(
        "Strike Price" = K,
        "Share Price" = round(S0, 4),
        "Put Price" = round(bs_put_price(S0, K, r, sigma, T), 4),
        "Share Fraction" = round(curr_share_fraction, 4),
        "Put Fraction" = round(curr_put_fraction, 4),
        "N Shares" = round(curr_n_shares, 4),
        "N Puts" = round(curr_n_puts, 4),
        "VAR95" = round(var_95, 4),
        "VAR99" = round(var_99, 4),
        "Interquartile Range" = round(quartilsabstand, 4),
        "Expected Returns" = round(expected_return, 4),
        "Expected Volatility" = round(realized_vola, 4),
        "Sharpe Ratio" = round(sharpe_ratio, 4)
    )
    
    impact_fractions <- rbind(impact_fractions, curr_df)
}

# Plot Share Fraction vs Sharpe Ratio
ggplot(impact_fractions, aes(x = Share.Fraction, y = Sharpe.Ratio)) +
    geom_line() +
    labs(title = "Share Fraction vs Sharpe Ratio",
         x = "Share Fraction",
         y = "Sharpe Ratio") +
    theme_minimal() +
    theme(text = element_text(family = "Georgia"))



## Analyze the Impact of the strike Price


strikes <- seq(900, 1500, 10)
impact_strikes <- data.frame(
    "Strike Price" = numeric(),
    "Share Price" = numeric(),
    "Put Price" = numeric(),
    "Share Fraction" = numeric(),
    "Put Fraction" = numeric(),
    "N Shares" = numeric(),
    "N Puts" = numeric(),
    "VAR95" = numeric(),
    "VAR99" = numeric(),
    "Interquartile Range" = numeric(),
    "Expected Returns" = numeric(),
    "Expected Volatility" = numeric(),
    "Sharpe Ratio" = numeric()
)

# Loop over the different Strike Values as before but now we must also calculate the put_price and the put_values for every strike

for (i in 1:length(strikes)) {
    K <- strikes[i]
    put_price <- bs_put_price(S0, K, r, sigma, T)
    strike_diff <- K - asset_value_at_maturity
    put_values <- pmax(strike_diff, 0)
    
    curr_n_shares <- invest * share_fraction / S0
    curr_n_puts <- invest * put_fraction / put_price
    
    hedged_portfolio_values <- (curr_n_shares * asset_value_at_maturity + curr_n_puts * put_values)
    hedged_portfolio_returns <- (hedged_portfolio_values / invest) - 1
    
    
    # Performance Measures
    
    var_95 <- quantile(hedged_portfolio_returns, 0.05)
    var_99 <- quantile(hedged_portfolio_returns, 0.01)
    quartilsabstand <- quantile(hedged_portfolio_returns, 0.75) - quantile(hedged_portfolio_returns, 0.25)
    expected_return <- mean(hedged_portfolio_returns)
    realized_vola <- sd(hedged_portfolio_returns)
    sharpe_ratio <- (expected_return - r_f) / realized_vola
    
    # Add results to the evaluation data frame
    curr_df <- data.frame(
        "Strike Price" = K,
        "Share Price" = round(S0, 4),
        "Put Price" = round(put_price, 4),
        "Share Fraction" = round(share_fraction, 4),
        "Put Fraction" = round(put_fraction, 4),
        "N Shares" = round(curr_n_shares, 4),
        "N Puts" = round(curr_n_puts, 4),
        "VAR95" = round(var_95, 4),
        "VAR99" = round(var_99, 4),
        "Interquartile Range" = round(quartilsabstand, 4),
        "Expected Returns" = round(expected_return, 4),
        "Expected Volatility" = round(realized_vola, 4),
        "Sharpe Ratio" = round(sharpe_ratio, 4)
    )
    
    impact_strikes <- rbind(impact_strikes, curr_df)
}

# Plot Strike Price vs Sharpe Ratio
ggplot(impact_strikes, aes(x = `Strike.Price`, y = `Sharpe.Ratio`)) +
    geom_line() +
    labs(title = "Strike Price vs Sharpe Ratio",
         x = "Strike Price",
         y = "Sharpe Ratio") +
    theme_minimal() +
    theme(text = element_text(family = "Georgia"))



##### Exercise 9 ####------------------------------------------------

# Stress Scenario Analysis

# Parameter

N <- 252
K <- 1200
share_fraction <- 0.95
S0 <- data$Price[length(data$Price)]
T <- 1

# Normal

sigma <- 0.11




#### Scenario 1 ############
# Shock of 20% and a Volatility of -5%


# Unhedged Portfolio
# Shock = 20%, Volatility = 11.48%

# Initialize parameters

sigma <- 0.11     # volatility (%)


# Put Price Normal
put_price <- bs_put_price(S0, K, r, sigma, T)

prices_scenario_1 <- matrix(nrow = N+1, ncol = mc.loops)

# Monte Carlo Simulation
for (i in 1:mc.loops) {
    prices_scenario_1[, i] <- gbm(S0 = S0, mu = mu, sigma = sigma, T = time, N = N)
}



# Calculate shock scenario

prices_pre_shock_1 <- prices_scenario_1[1:(length(prices_scenario_1[,1])/2), ]
prices_after_shock_1 <- prices_scenario_1[(length(prices_scenario_1[,1])/2):length(prices_scenario_1[, 1]), ] * 0.8

prices_scenario_1 <- rbind(prices_pre_shock_1, prices_after_shock_1)
put_values <- calculate_put_values(K = K, asset_values = prices_scenario_1[N+1, ])


# Hedged Portfolio

# Calculate number of shares and options according to investment amount, share fraction, and stock/put prices
invest <- 10000
share_fraction <- 0.95
put_fraction <- 1 - share_fraction
n_shares <- invest * share_fraction / S0
n_puts <- invest * put_fraction / put_price
asset_value_at_maturity <- prices_scenario_1[N+1,]

# Calculate resulting portfolio values and relative returns
hedged_portfolio_values <- asset_value_at_maturity * n_shares + n_puts * put_values
hedged_portfolio_returns <- (hedged_portfolio_values / invest) - 1

# Plot Return Distribution
ggplot(data = data.frame(Returns = hedged_portfolio_returns * 100), aes(x = Returns)) +
    geom_histogram(binwidth = 0.2, color = 'grey', fill = "darkorange") +
    labs(title = "Return Distribution of Hedged Portfolio",
         x = "Returns (%)",
         y = "Frequency") +
    theme_minimal() +
    theme(text = element_text(family = "Georgia")) +
    NULL

# Performance and Risk Measures (With Risk Management)
# Value at Risk (VaR) - 95%
var_95 <- quantile(hedged_portfolio_returns, 0.05)
var_99 <- quantile(hedged_portfolio_returns, 0.01)

# Quartile Range
quartile_range <- quantile(hedged_portfolio_returns, 0.75) - quantile(hedged_portfolio_returns, 0.25)

# Expected Returns
expected_return <- mean(hedged_portfolio_returns)

# Realized Volatility
realized_volatility <- sd(hedged_portfolio_returns)

# Sharpe Ratio
sharpe_ratio <- (expected_return - r) / realized_volatility

# Print Results
cat("VaR 95%:", round(var_95 * 100, 2), "%\n")
cat("VaR 99%:", round(var_99 * 100, 2), "%\n")
cat("Quartile Range:", round(quartile_range, 4), "\n")
cat("Expected Returns:", round(expected_return * 100, 4), "%\n")
cat("Realized Volatility:", round(realized_volatility * 100, 4), "%\n")
cat("Sharpe Ratio:", round(sharpe_ratio, 4), "\n")




#### Scenario 2  ######
# Shock of 20% and a Volatility of +5%

sigma <- 0.21     # volatility (%)


# Put Price Normal
put_price <- bs_put_price(S0, K, r, sigma, T)

prices_scenario_2 <- matrix(nrow = N+1, ncol = mc.loops)

# Monte Carlo Simulation
for (i in 1:mc.loops) {
    prices_scenario_2[, i] <- gbm(S0 = S0, mu = mu, sigma = sigma, T = time, N = N)
}



# Calculate shock scenario

prices_pre_shock_2 <- prices_scenario_2[1:(length(prices_scenario_2[,1])/2), ]
prices_after_shock_2 <- prices_scenario_2[(length(prices_scenario_2[,1])/2):length(prices_scenario_2[, 1]), ] * 0.8

prices_scenario_2 <- rbind(prices_pre_shock_2, prices_after_shock_2)
put_values <- calculate_put_values(K = K, asset_values = prices_scenario_2[N+1, ])


# Hedged Portfolio

# Calculate number of shares and options according to investment amount, share fraction, and stock/put prices
invest <- 10000
share_fraction <- 0.95
put_fraction <- 1 - share_fraction
n_shares <- invest * share_fraction / S0
n_puts <- invest * put_fraction / put_price
asset_value_at_maturity <- prices_scenario_2[N+1,]

# Calculate resulting portfolio values and relative returns
hedged_portfolio_values <- asset_value_at_maturity * n_shares + n_puts * put_values
hedged_portfolio_returns <- (hedged_portfolio_values / invest) - 1

# Plot Return Distribution
ggplot(data = data.frame(Returns = hedged_portfolio_returns * 100), aes(x = Returns)) +
    geom_histogram(binwidth = 0.2, color = 'grey', fill = "darkorange") +
    labs(title = "Return Distribution of Hedged Portfolio",
         x = "Returns (%)",
         y = "Frequency") +
    theme_minimal() +
    theme(text = element_text(family = "Georgia")) +
    NULL

# Performance and Risk Measures (With Risk Management)
# Value at Risk (VaR) - 95%
var_95 <- quantile(hedged_portfolio_returns, 0.05)
var_99 <- quantile(hedged_portfolio_returns, 0.01)

# Quartile Range
quartile_range <- quantile(hedged_portfolio_returns, 0.75) - quantile(hedged_portfolio_returns, 0.25)

# Expected Returns
expected_return <- mean(hedged_portfolio_returns)

# Realized Volatility
realized_volatility <- sd(hedged_portfolio_returns)

# Sharpe Ratio
sharpe_ratio <- (expected_return - r) / realized_volatility

# Print Results
cat("VaR 95%:", round(var_95 * 100, 2), "%\n")
cat("VaR 99%:", round(var_99 * 100, 2), "%\n")
cat("Quartile Range:", round(quartile_range, 4), "\n")
cat("Expected Returns:", round(expected_return * 100, 4), "%\n")
cat("Realized Volatility:", round(realized_volatility * 100, 4), "%\n")
cat("Sharpe Ratio:", round(sharpe_ratio, 4), "\n")



#### Scenario 3 #####
# Shock of 20% and a Volatility of +5%

sigma <- 0.16     # volatility (%)


# Put Price Normal
put_price <- bs_put_price(S0, K, r, sigma, T)

prices_scenario_3 <- matrix(nrow = N+1, ncol = mc.loops)

# Monte Carlo Simulation
for (i in 1:mc.loops) {
    prices_scenario_3[, i] <- gbm(S0 = S0, mu = mu, sigma = sigma, T = time, N = N)
}



# Calculate shock scenario

prices_pre_shock_3 <- prices_scenario_3[1:(length(prices_scenario_3[,1])/2), ]
prices_after_shock_3 <- prices_scenario_3[(length(prices_scenario_3[,1])/2):length(prices_scenario_3[, 1]), ] * 0.8

prices_scenario_3 <- rbind(prices_pre_shock_3, prices_after_shock_3)
put_values <- calculate_put_values(K = K, asset_values = prices_scenario_3[N+1, ])


# Hedged Portfolio

# Calculate number of shares and options according to investment amount, share fraction, and stock/put prices
invest <- 10000
share_fraction <- 0.95
put_fraction <- 1 - share_fraction
n_shares <- invest * share_fraction / S0
n_puts <- invest * put_fraction / put_price
asset_value_at_maturity <- prices_scenario_3[N+1,]

# Calculate resulting portfolio values and relative returns
hedged_portfolio_values <- asset_value_at_maturity * n_shares + n_puts * put_values
hedged_portfolio_returns <- (hedged_portfolio_values / invest) - 1

# Plot Return Distribution
ggplot(data = data.frame(Returns = hedged_portfolio_returns * 100), aes(x = Returns)) +
    geom_histogram(binwidth = 0.2, color = 'grey', fill = "darkorange") +
    labs(title = "Return Distribution of Hedged Portfolio",
         x = "Returns (%)",
         y = "Frequency") +
    theme_minimal() +
    theme(text = element_text(family = "Georgia")) +
    NULL

# Performance and Risk Measures (With Risk Management)
# Value at Risk (VaR) - 95%
var_95 <- quantile(hedged_portfolio_returns, 0.05)
var_99 <- quantile(hedged_portfolio_returns, 0.01)

# Quartile Range
quartile_range <- quantile(hedged_portfolio_returns, 0.75) - quantile(hedged_portfolio_returns, 0.25)

# Expected Returns
expected_return <- mean(hedged_portfolio_returns)

# Realized Volatility
realized_volatility <- sd(hedged_portfolio_returns)

# Sharpe Ratio
sharpe_ratio <- (expected_return - r) / realized_volatility

# Print Results
cat("VaR 95%:", round(var_95 * 100, 2), "%\n")
cat("VaR 99%:", round(var_99 * 100, 2), "%\n")
cat("Quartile Range:", round(quartile_range, 4), "\n")
cat("Expected Returns:", round(expected_return * 100, 4), "%\n")
cat("Realized Volatility:", round(realized_volatility * 100, 4), "%\n")
cat("Sharpe Ratio:", round(sharpe_ratio, 4), "\n")
