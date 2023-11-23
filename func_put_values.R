# Function to calculate the put values based on strike price and asset values
calculate_put_values <- function(K, asset_values) {
    strike_diff <- K - asset_values
    put_values <- pmax(strike_diff, 0)
    return(put_values)
}