# Function to calculate the Black-Scholes put price
black_scholes_put <- function(S0, K, t, r, vol) {
    d1 <- (log(S0 / K) + (r + (vol ^ 2 / 2)) * t) / (vol * sqrt(t))
    d2 <- d1 - vol * sqrt(t)
    put_price <- (K * exp(-r * t) * pnorm(-d2)) - (S0 * pnorm(-d1))
    return(put_price)
}