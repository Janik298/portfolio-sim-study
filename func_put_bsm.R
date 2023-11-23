# Function to calculate the Black-Scholes put price
black_scholes_put <- function(S0, K, T, r, vol) {
    d1 <- (log(S0 / K) + (r + (vol ^ 2 / 2)) * T) / (vol * sqrt(T))
    d2 <- d1 - vol * sqrt(T)
    put_price <- (K * exp(-r * T) * pnorm(-d2)) - (S0 * pnorm(-d1))
    return(put_price)
}