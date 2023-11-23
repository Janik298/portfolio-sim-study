#### Simulate the stock prices for the sigma and time to maturity ###############

# Geometric Brownian Motion
# Simulates one trajectory of the Geometric Brownian Motion at time intervals dt=T/N
# 
# S0 = start value at t0
# mu = expected return
# sigma = volatility
# T = time in years
# @return vektor of length (N+1) with prices at (i*T/N) for i=0..N.

gbm <- function(S0, mu, sigma, T, N) {
    # time interval, grid size
    dt <- T/N
    
    S_t <- numeric(N+1)
    S_t[1] <- S0
    
    # Wiener Prozess
    W_t  <- rnorm(n = N, mean = 0, sd = 1)
    
    S_t[2:(N+1)] <- S0 * exp(cumsum((mu - (sigma^2)/2) * dt  + sigma * sqrt(dt) * W_t))
    S_t
}