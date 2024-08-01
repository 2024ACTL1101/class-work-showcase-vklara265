
## Algorithmic Trading Strategy

## Introduction

In this assignment, you will develop an algorithmic trading strategy by incorporating financial metrics to evaluate its profitability. This exercise simulates a real-world scenario where you, as part of a financial technology team, need to present an improved version of a trading algorithm that not only executes trades but also calculates and reports on the financial performance of those trades.

## Background

Following a successful presentation to the Board of Directors, you have been tasked by the Trading Strategies Team to modify your trading algorithm. This modification should include tracking the costs and proceeds of trades to facilitate a deeper evaluation of the algorithm’s profitability, including calculating the Return on Investment (ROI).

After meeting with the Trading Strategies Team, you were asked to include costs, proceeds, and return on investments metrics to assess the profitability of your trading algorithm.

## Objectives

1. **Load and Prepare Data:** Open and run the starter code to create a DataFrame with stock closing data.

2. **Implement Trading Algorithm:** Create a simple trading algorithm based on daily price changes.

3. **Customize Trading Period:** Choose your entry and exit dates.

4. **Report Financial Performance:** Analyze and report the total profit or loss (P/L) and the ROI of the trading strategy.

5. **Implement a Trading Strategy:** Implement a trading strategy and analyze the total updated P/L and ROI. 

6. **Discussion:** Summarise your finding.


## Instructions

### Step 1: Data Loading

Start by running the provided code cells in the "Data Loading" section to generate a DataFrame containing AMD stock closing data. This will serve as the basis for your trading decisions. First, create a data frame named `amd_df` with the given closing prices and corresponding dates. 

```r
# Load data from CSV file
amd_df <- read.csv("AMD.csv")
# Convert the date column to Date type and Adjusted Close as numeric
amd_df$date <- as.Date(amd_df$Date)
amd_df$close <- as.numeric(amd_df$Adj.Close)
amd_df <- amd_df[, c("date", "close")]
```

#### Plotting the Data
Plot the closing prices over time to visualize the price movement.
```r
plot(amd_df$date, amd_df$close,'l')
```

### Step 2: Trading Algorithm
Implement the trading algorithm as per the instructions. You should initialize necessary variables, and loop through the dataframe to execute trades based on the set conditions.

- Initialize Columns: Start by ensuring dataframe has columns 'trade_type', 'costs_proceeds' and 'accumulated_shares'.
- Change the algorithm by modifying the loop to include the cost and proceeds metrics for buys of 100 shares. Make sure that the algorithm checks the following conditions and executes the strategy for each one:
  - If the previous price = 0, set 'trade_type' to 'buy', and set the 'costs_proceeds' column to the current share price multiplied by a `share_size` value of 100. Make sure to take the negative value of the expression so that the cost reflects money leaving an account. Finally, make sure to add the bought shares to an `accumulated_shares` variable.
  - Otherwise, if the price of the current day is less than that of the previous day, set the 'trade_type' to 'buy'. Set the 'costs_proceeds' to the current share price multiplied by a `share_size` value of 100.
  - You will not modify the algorithm for instances where the current day’s price is greater than the previous day’s price or when it is equal to the previous day’s price.
  - If this is the last day of trading, set the 'trade_type' to 'sell'. In this case, also set the 'costs_proceeds' column to the total number in the `accumulated_shares` variable multiplied by the price of the last day.



```r
# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA  # Corrected column name
amd_df$accumulated_shares <- 0  # Initialize if needed for tracking

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

for (i in 1:nrow(amd_df)) {
if (previous_price == 0) {
amd_df$trade_type[i] <- 'buy'
amd_df$costs_proceeds[i] <- -amd_df$close[i] * share_size accumulated_shares <- accumulated_shares + share_size
} else if (amd_df$close[i] < previous_price) { amd_df$trade_type[i] <- 'buy'
amd_df$costs_proceeds[i] <- -amd_df$close[i] * share_size accumulated_shares <- accumulated_shares + share_size
}
  # Update accumulated shares
amd_df$accumulated_shares[i] <- accumulated_shares
  # Update the previous price
previous_price <- amd_df$close[i]
}
# Sell on the last day
last_row <- nrow(amd_df)
amd_df$trade_type[last_row] <- 'sell'
amd_df$costs_proceeds[last_row] <- accumulated_shares * amd_df$close[last_row] amd_df$accumulated_shares[last_row] <- 0
```


### Step 3: Customize Trading Period
- Define a trading period you wanted in the past five years 
```r
# Define dates
start_date <- as.Date('2020-01-01')
end_date <- as.Date('2021-01-04')
# Filter the data to include only the trading period
amd_df <- amd_df[amd_df$date >= start_date & amd_df$date <= end_date, ]
```


### Step 4: Run Your Algorithm and Analyze Results
After running your algorithm, check if the trades were executed as expected. Calculate the total profit or loss and ROI from the trades.

- Total Profit/Loss Calculation: Calculate the total profit or loss from your trades. This should be the sum of all entries in the 'costs_proceeds' column of your dataframe. This column records the financial impact of each trade, reflecting money spent on buys as negative values and money gained from sells as positive values.
- Invested Capital: Calculate the total capital invested. This is equal to the sum of the 'costs_proceeds' values for all 'buy' transactions. Since these entries are negative (representing money spent), you should take the negative sum of these values to reflect the total amount invested.
- ROI Formula: $$\text{ROI} = \left( \frac{\text{Total Profit or Loss}}{\text{Total Capital Invested}} \right) \times 100$$

```r

total_profit_loss <- sum(amd_df$costs_proceeds, na.rm = TRUE)
# Calculate the total capital invested
total_invested <- sum(-amd_df$costs_proceeds[amd_df$trade_type == 'buy'], na.rm = TRUE)
# Calculate ROI
roi <- (total_profit_loss / total_invested) * 100
# Print results
cat("Total Profit/Loss: ", total_profit_loss, "\n")
cat("Total Capital Invested: ", total_invested, "\n")
cat("ROI: ", roi, "%\n")
```

### Step 5: Profit-Taking Strategy or Stop-Loss Mechanisum (Choose 1)
- Option 1: Implement a profit-taking strategy that you sell half of your holdings if the price has increased by a certain percentage (e.g., 20%) from the average purchase price.
- Option 2: Implement a stop-loss mechanism in the trading strategy that you sell half of your holdings if the stock falls by a certain percentage (e.g., 20%) from the average purchase price. You don't need to buy 100 stocks on the days that the stop-loss mechanism is triggered.


```r
# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0
total_cost <- 0
stop_loss_percentage <- 0.15
# Implementing the trading algorithm with stop-loss mechanism
for (i in 1:nrow(amd_df)) { if (previous_price == 0) {
    # Initial buy to set up the first position
amd_df$trade_type[i] <- 'buy'
amd_df$costs_proceeds[i] <- -amd_df$close[i] * share_size accumulated_shares <- accumulated_shares + share_size total_cost <- total_cost + amd_df$close[i] * share_size
} else if (amd_df$close[i] < previous_price) {
# Check stop-loss condition before buying
avg_purchase_price <- total_cost / accumulated_shares
if (amd_df$close[i] <= avg_purchase_price * (1 - stop_loss_percentage)) {
      # Implement stop-loss mechanism: sell half of the holdings if the stock falls by 15%
sell_shares <- accumulated_shares / 2
amd_df$trade_type[i] <- 'sell'
amd_df$costs_proceeds[i] <- sell_shares * amd_df$close[i] accumulated_shares <- accumulated_shares - sell_shares total_cost <- total_cost - sell_shares * avg_purchase_price
} else {
# Buy when the current day's price is less than the previous day's price and no stop amd_df$trade_type[i] <- 'buy'
amd_df$costs_proceeds[i] <- -amd_df$close[i] * share_size
accumulated_shares <- accumulated_shares + share_size
total_cost <- total_cost + amd_df$close[i] * share_size
} }
  # Update accumulated shares
amd_df$accumulated_shares[i] <- accumulated_shares # Update the previous price
previous_price <- amd_df$close[i]
}
# Sell all shares on the last day if there are any remaining
last_row <- nrow(amd_df)
if (accumulated_shares > 0) {
amd_df$trade_type[last_row] <- 'sell'
amd_df$costs_proceeds[last_row] <- accumulated_shares * amd_df$close[last_row] accumulated_shares <- 0
amd_df$accumulated_shares[last_row] <- accumulated_shares # Selling all shares
}
```


### Step 6: Summarize Your Findings
- Did your P/L and ROI improve over your chosen period?
- Relate your results to a relevant market event and explain why these outcomes may have occurred.


```r
# Calculate the total profit/loss
total_profit_loss_after <- sum(amd_df$costs_proceeds, na.rm = TRUE) # Calculate the total capital invested
total_invested_after <- -sum(amd_df$costs_proceeds[amd_df$trade_type == 'buy'], na.rm = TR # Calculate ROI
roi_after <- (total_profit_loss_after / total_invested_after) * 100
# Print results
cat("After Implementing Stop-Loss Mechanism:\n")
cat("Total Profit/Loss: ", total_profit_loss_after, "\n")
cat("Total Capital Invested: ", total_invested_after, "\n")
cat("ROI: ", roi_after, "%\n")
```

Discussion: During the period from 01/01/2020 to 04/01/2021, the COVID-19 pandemic led to significant volatility in the stock market, especially in the early months of 2020. Market experienced sharp declines in March 2020 due to economic uncertainty, followed by a rapid recovery as governments introduced stimulus measures. Meanwhile, AMD benefited from the increased demand for technology products and services during the pandemic. Remote learning and working had boosted the demand for PCs, laptops, and related components, positively impacting AMD’s stock price. During this period, the stop-loss mechanism reduced the ROI from 40.75% to 25.96%, profit has reduced from 336715 dollars to 210475 dollars . This reduction is likely due to the high market volatility in 2020, which triggered the stop-loss and resulted in missed opportunities for gains during the market recovery.




