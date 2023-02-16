################################################################################
###    Stocks Portfolio PCA Analysis                                         ###
###    by Luis Urso                                                          ###
###    Updated: 01-DEC-2022 - Version 1                                      ###
################################################################################

# Load required packages 

packages <- c("quantmod","PerformanceAnalytics","magrittr","ggplot2",
              "fPortfolio","timeSeries","dplyr","yfR","tidyr")

if(sum(as.numeric(!packages %in% installed.packages())) != 0){
  installer <- packages[!packages %in% installed.packages()]
  for(i in 1:length(installer)) {
    install.packages(installer, dependencies = T)
    break()}
  sapply(packages, require, character = T) 
} else {
  sapply(packages, require, character = T) 
}

# Setup evaluation time range

date_start=Sys.Date()-365
date_finish=Sys.Date()

# Define the stock tickers to get to closure prices in the list
# below. Just include new or remove a new ticker.

stock_ids <-c('NVDA','LUMN','WBD','CCI')



# Iterate amongst all tickets entered to get the price and include in the 
# DataFrame - df_return 

for(i in 1:length(stock_ids)) {


  stock_data=yf_get(tickers = stock_ids[i], 
                    first_date = date_start,
                    last_date = date_finish)

  
  tickers = stock_data$ticker[-1]
  date_reference <- stock_data$ref_date[-1]
  stock_closure <- stock_data$ret_closing_prices[-1]

  if (i==1){
    df_return <- data.frame(date_reference,stock_closure)
  }
  
  else {
    df_temp <- data.frame(date_reference,stock_closure)
    df_return = merge(x=df_return,y=df_temp,by="date_reference",all=TRUE)
  }

  df_return <- df_return %>% set_names(c("date_reference",stock_ids[1:i]))
  
}

# Remove the lines with NA 

df_return <- drop_na(df_return)

## Convert the DataFrame to Matrix and Time Series Object 

mx_return <- data.matrix(df_return[,1:length(stock_ids)+1])

ret_data=as.timeSeries(mx_return)


# Plot the stocks just for an initial 
plot(ret_data)

ret_data


# Calculate the Average of Returns (expected return) in %

exp_return=colMeans(ret_data)
exp_return*100


# Calculate the COVARIANCE Matrix (based on Marckowitz Theory)

mat.cov = cov(ret_data)
mat.cov

x=nrow(df_return)
x

cov.pop <- cov(ret_data)*(x-1)/(x)
cov.pop

#
# Show the Minimum Risk Stock Portfolio (risks must be multiplied by 100)
#

pf_min_risk = minvariancePortfolio(ret_data)
pf_min_risk

#
# Show the Optimum Risk Stock Portfolio (risks must be multiplied by 100)
#

pf_optim = tangencyPortfolio(ret_data)
pf_optim


#
# Define the Frontier Data 
#

frontier = portfolioFrontier(ret_data)
frontier

#
# Show the Frontier Graph (Markowitz Hyperbole)
# Numbers must by multiplied by 100 
# 


frontierPlot(frontier, col=c("blue", "Orange"), pch=19)

# Simulate various Stock Portfolio Arrangements using MonteCarlos Simulation

monteCarloPoints(frontier, mcSteps = 1000, cex=0.25, pch=19)

# Show the place in the Stock Portfolio with the same proportions

equalWeightsPoints(frontier, pch=20, col="red")

# Show the related point for each of the stocks that composes the portfolio 

singleAssetPoints(frontier, pch=19, cex=1.5, col=topo.colors(6))






