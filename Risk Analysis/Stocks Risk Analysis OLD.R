################################################################################
###    Stocks Portfolio Risk Analysis - Best Portfolio Selection             ###
###    by Luis Urso                                                          ###
###    Updated: 21-Nov-2022                                                  ###
################################################################################

# Load required packages 

pacotes <- c("quantmod","PerformanceAnalytics","magrittr","ggplot2",
             "fPortfolio","timeSeries","dplyr","yfR")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# Setup evaluation time range

date_start=Sys.Date()-100
data_finish=Sys.Date()
 

# Get the Stocks Prices 

acao='LLY'

# Rotina de captura dos preços dos ativos 

buscar=yf_get(tickers = acao, 
                first_date = date_start,
                last_date = date_finish)

# ver a saída da busca

buscar


# pegar apenas as cotações de fechamento

lly=buscar$ret_closing_prices

lly=lly[-1]
lly 

# Apenas algumas estatísticas para ilustrar

hist(lly)
risco_lly=sd(lly)
risco_lly



# Buscar outro ativo

acao='VRTX'

# Rotina de captura dos preços dos ativos 

buscar=yf_get(tickers = acao, 
              first_date = date_start,
              last_date = data_final)

vrtx=buscar$ret_closing_prices
vrtx=vrtx[-1]
vrtx

acao='BMY'

# Rotina de captura dos preços dos ativos 

buscar=yf_get(tickers = acao, 
              first_date = date_start,
              last_date = data_final)

bmy=buscar$ret_closing_prices
bmy=bmy[-1]
bmy

# Montar a Matriz de Retornos

retornos=cbind(lly,vrtx,bmy)

retornos

dados=as.timeSeries(retornos)
dados
plot(dados)

# Cálculo dos Retornos Médios

ret.esperados=colMeans(dados)
ret.esperados
  

# Cálculo da Matriz de covariâncias

mat.cov = cov(dados)
mat.cov

x=length(lly)
x
cov.pop <- cov(dados)*(x-1)/(x)

cov.pop


# Carteira de Minimo Risco
cart.min.risco = minvariancePortfolio(dados)
cart.min.risco

# Carteira Ótima
cart.otima = tangencyPortfolio(dados)
cart.otima

# Fronteira
fronteira = portfolioFrontier(dados)
fronteira

# Gráfico da Fronteira
frontierPlot(fronteira, col=c("blue", "Orange"), pch=19)

# traçar várias carteiras

monteCarloPoints(fronteira, mcSteps = 1000, cex=0.25, pch=19)

# Mostrar o local da carteira com proporções iguais de cada ativo
equalWeightsPoints(fronteira, pch=20, col="red")

# Mostrar os pontos relativos a cada um dos ativos escolhidos individualmente
singleAssetPoints(fronteira, pch=19, cex=1.5, col=topo.colors(6))

################## FIM #########################################################

