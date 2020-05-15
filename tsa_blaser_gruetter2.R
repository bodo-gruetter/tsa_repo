library(quantmod)
library(PortfolioAnalytics)
library(PerformanceAnalytics)
library(ROI)

#col_names <- c("Zurich", "Roche", "UBS", "Novartis", "SwissRe", "ABB", "SwissLife", "Lonza", "CreditSuisse", "Sika", 
#               "JuliusBar", "Givaudan", "Nestle", "Swatch", "Richemont", "Adecco", "Swisscom", "LafargeHolcim", "SGS", 
#               "Geberit")

col_names <- c("Zurich", "Roche", "UBS", "Novartis", "SwissRe", "ABB", "SwissLife", "Lonza", "CreditSuisse", 
               "JuliusBar", "Givaudan", "Nestle", "Swatch", "Richemont", "Adecco", "Swisscom", "LafargeHolcim", "SGS", 
               "Geberit")

# Downloading required data via Yahoo Finance API
# it contains all SMI stock prices
data <- NULL

#tickers_index <- c("ZURN.SW", "ROG.SW", "UBSG.SW", "NOVN.SW", "SREN.SW", "ABBN.SW", "SLHN.SW", "LONN.SW", "CSGN.SW", "SIKA.SW", "BAER.SW", "GIVN.SW", "NESN.SW", "UHR.SW", "CFR.SW", "ADEN.SW", "SCMN.SW", "LHN.SW", "SGSN.SW", "GEBN.SW")
tickers_index <- c("ZURN.SW", "ROG.SW", "UBSG.SW", "NOVN.SW", "SREN.SW", "ABBN.SW", "SLHN.SW", "LONN.SW", "CSGN.SW", "BAER.SW", "GIVN.SW", "NESN.SW", "UHR.SW", "CFR.SW", "ADEN.SW", "SCMN.SW", "LHN.SW", "SGSN.SW", "GEBN.SW")

for (Ticker in tickers_index){
  data <- cbind(data,
                getSymbols.yahoo(Ticker, from="2010-01-01", to="2020-05-08", periodicity = "weekly",
                                 auto.assign=FALSE)[,6])

}

colnames(data) <- col_names

head(data)

returns <- Return.calculate(data, method = "simple")
returns <- returns[-1, ]
head(returns)

port_spec <- portfolio.spec(assets = col_names)
port_spec <- add.constraint(portfolio = port_spec, type = "full_investment")
port_spec <- add.constraint(portfolio = port_spec, type = "long_only")
port_spec <- add.constraint(portfolio = port_spec, type = "box", min = 0.0, max = 0.9)

portMeanVar = port_spec
portMeanVar <- add.objective(portfolio = portMeanVar, type = "risk", name = "StdDev")
portMeanVar <- add.objective(portfolio = portMeanVar, type = "return", name = "mean")
portMeanVar

opt_single <- optimize.portfolio(R = returns, portfolio = portMeanVar, optimize_method = "ROI")
opt_rebal  <- optimize.portfolio(R = returns, portfolio = portMeanVar, optimize_method = "ROI", rebalance_on = "years")

extractWeights(opt_single)
minVarReturns <- Return.portfolio(returns, weight = extractWeights(opt_single))
table.AnnualizedReturns(R = minVarReturns, Rf = 0.01/250)
chart.Weights(opt_single)
charts.PerformanceSummary(returns, weights = extractWeights(opt_single))

extractWeights(opt_rebal)
minVarReturns <- Return.portfolio(returns, weight = extractWeights(opt_rebal))
chart.Weights(opt_rebal)
charts.PerformanceSummary(returns, weights = extractWeights(opt_rebal))

meanvar.portf <- add.objective(portfolio = port_spec, type = "risk", name = "var", risk_aversion = 10)
meanvar.portf <- add.objective(portfolio = meanvar.portf, type = "return", name = "mean")
meanvar.ef <- create.EfficientFrontier(R = returns, portfolio = port_spec, type = "mean-StdDev")
meanvar.ef


chart.EfficientFrontier(meanvar.ef, match.col="StdDev", type="l", 
                        RAR.text="Sharpe Ratio", pch=4)
chart.EF.Weights(meanvar.ef, colorset=bluemono, match.col="StdDev")


