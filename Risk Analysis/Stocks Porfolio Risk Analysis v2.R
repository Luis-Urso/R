################################################################################
###    Stocks Portfolio Risk Analysis - Best Portfolio Selection             ###
###    by Luis Urso                                                          ###
###    Updated: 21-Nov-2022 - Version 2                                      ###
################################################################################

# Load required packages 

packages <- c("quantmod","PerformanceAnalytics","magrittr","ggplot2",
             "fPortfolio","timeSeries","dplyr","yfR")

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

date_start=Sys.Date()-120
date_finish=Sys.Date()
 
# Get the Stocks Prices 

stock_id='LLY'

stock_data=yf_get(tickers = stock_id, 
                first_date = date_start,
                last_date = date_finish)

# Show Stock Data Output 

stock_data

# Get only Closure Field

lly=stock_data$ret_closing_prices

lly=lly[-1]
lly 

# Show the Histogram 

hist(lly)

# Show the Risk of the Stock per Day in %

risco_lly=sd(lly)
risco_lly*100


#
# Get another stock
#


stock_id='VRTX'

stock_data=yf_get(tickers = stock_id, 
              first_date = date_start,
              last_date = date_finish)

vrtx=stock_data$ret_closing_prices
vrtx=vrtx[-1]
vrtx

# Show the Risk of the Stock per Day in %

risk_vrtx=sd(vrtx)
risk_vrtx*100


#
# Get another stock
#


stock_id='BMY'


stock_data=yf_get(tickers = stock_id, 
              first_date = date_start,
              last_date = date_finish)

bmy=stock_data$ret_closing_prices
bmy=bmy[-1]
bmy

# Show the Risk of the Stock per Day in %

risk_bmy=sd(bmy)
risk_bmy*100


#
# Make the Returns Matrix
#

returns=cbind(lly,vrtx,bmy)

returns

# Transform to a time series 

ret_data=as.timeSeries(returns)
ret_data
plot(ret_data)

# Calculate the Average of Returns (expected return) in %

exp_return=colMeans(ret_data)
exp_return*100
  

# Calculate the COVARIANCE Matrix (based on Marckowitz Theory)

mat.cov = cov(ret_data)
mat.cov

x=length(lly)
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

######################################################################
##                          E N D                                   ##
######################################################################
