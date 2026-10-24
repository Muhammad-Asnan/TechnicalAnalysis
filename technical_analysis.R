# 1. Install required packages (Run this once)
# install.packages(c("quantmod", "TTR"))

library(quantmod)
library(TTR)

# 2. Function to Import and Load Stocks Data
load_stock_data <- function(filepath) {
  symbols <- readLines(filepath)
  stock_data <- list()
  for(sym in symbols) {
    getSymbols(sym, src = "yahoo", auto.assign = FALSE) -> data
    stock_data[[sym]] <- data
  }
  return(stock_data)
}

# Helper function to calculate Mode
get_mode <- function(v) {
  uniqv <- unique(na.omit(v))
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# 3. Function to Compute Basic Statistics
calculate_statistics <- function(stock_list) {
  stats_list <- list()
  for(sym in names(stock_list)) {
    data <- stock_list[[sym]]
    close_prices <- as.numeric(Cl(data)) # Extract closing prices
    
    # Calculate statistics
    stats <- data.frame(
      Symbol = sym,
      Mean = mean(close_prices, na.rm = TRUE),
      Median = median(close_prices, na.rm = TRUE),
      Mode = get_mode(close_prices),
      Std_Dev = sd(close_prices, na.rm = TRUE)
    )
    
    # Calculate 14-day Moving Average using TTR package
    moving_avg <- SMA(close_prices, n = 14)
    
    stats_list[[sym]] <- list(Basic = stats, MA = tail(moving_avg, 5))
  }
  return(stats_list)
}

# 4. Utility to Display Output
display_output <- function(stock_list, stats_list) {
  for(sym in names(stock_list)) {
    cat("\n========================================\n")
    cat("Stock Data & Statistics for:", sym, "\n")
    cat("========================================\n")
    
    cat("\n--- Recent Market Data ---\n")
    print(tail(stock_list[[sym]], 3))
    
    cat("\n--- Basic Statistics ---\n")
    print(stats_list[[sym]]$Basic)
    
    cat("\n--- Recent 14-Day Moving Averages ---\n")
    print(stats_list[[sym]]$MA)
  }
}

# --- Execution Steps ---
# Ensure your working directory is set to where portfolio.txt is located
my_portfolio <- load_stock_data("portfolio.txt")
my_stats <- calculate_statistics(my_portfolio)
display_output(my_portfolio, my_stats)