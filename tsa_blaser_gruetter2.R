#install.packages("quantmod")
#install.packages("PortfolioAnalytics")
#install.packages("PerformanceAnalytics")
#install.packages("ROI")
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("forecast")

library(quantmod)
library(PortfolioAnalytics)
library(PerformanceAnalytics)
library(ROI)
library(ggplot2)
library(dplyr)
library(forecast)

# colum names for later use, contains all names of SMI companies (except SIKA)
col_names <- c("Zurich", "Roche", "UBS", "Novartis", "SwissRe", "ABB", "SwissLife", "Lonza", "CreditSuisse", 
               "JuliusBar", "Givaudan", "Nestle", "Swatch", "Richemont", "Adecco", "Swisscom", "LafargeHolcim", "SGS", 
               "Geberit")

# Downloading required data via Yahoo Finance API
# it contains all SMI stock prices
data <- NULL

# contains the ticker names from Yahoo Finance to download. It has all tickers from the SMI except SIKA
tickers_index <- c("ZURN.SW", "ROG.SW", "UBSG.SW", "NOVN.SW", "SREN.SW", "ABBN.SW", "SLHN.SW", "LONN.SW", "CSGN.SW", "BAER.SW", "GIVN.SW", "NESN.SW", "UHR.SW", "CFR.SW", "ADEN.SW", "SCMN.SW", "LHN.SW", "SGSN.SW", "GEBN.SW")

# Download the share data from Yahoo Finance. Only use the adjusted column
for (Ticker in tickers_index){
  data <- cbind(data,
                getSymbols.yahoo(Ticker, from="2010-01-01", to="2020-05-08", periodicity = "weekly",
                                 auto.assign=FALSE)[,6])
  
}

colnames(data) <- col_names

head(data)

# calculate the returns for the alle shares
returns <- Return.calculate(data, method = "simple")
# eliminate the first row because there isn't any return
returns <- returns[-1, ]
head(returns)

# define the portfolio definition. One share can have 30% maximum. This is defined in the box constrained.
port_spec <- portfolio.spec(assets = col_names)
port_spec <- add.constraint(portfolio = port_spec, type = "full_investment")
port_spec <- add.constraint(portfolio = port_spec, type = "long_only")
port_spec <- add.constraint(portfolio = port_spec, type = "box", min = 0.0, max = 0.3)

# define the standard deviation and mean for the portfolio as objectives
portMeanVar = port_spec
portMeanVar <- add.objective(portfolio = portMeanVar, type = "risk", name = "StdDev")
portMeanVar <- add.objective(portfolio = portMeanVar, type = "return", name = "mean")
portMeanVar

# calculate the optimal portfolio based on the ROI method
opt_single <- optimize.portfolio(R = returns, portfolio = portMeanVar, optimize_method = "ROI")

minVarReturns <- Return.portfolio(returns, weight = extractWeights(opt_single))
table.AnnualizedReturns(R = minVarReturns, Rf = 0.01/250)
chart.Weights(opt_single)
charts.PerformanceSummary(returns, weights = extractWeights(opt_single), main = "Performance Summary")

meanvar.portf <- add.objective(portfolio = port_spec, type = "risk", name = "var", risk_aversion = 10)
meanvar.portf <- add.objective(portfolio = meanvar.portf, type = "return", name = "mean")
meanvar.ef <- create.EfficientFrontier(R = returns, portfolio = port_spec, type = "mean-StdDev")
meanvar.ef

# plot the Efficient Frontier for the whole SMI
chart.EfficientFrontier(meanvar.ef, match.col="StdDev", type="l", 
                        RAR.text="Sharpe Ratio", pch=4)
# show the weigths for each SMI title by standard deviation
chart.EF.Weights(meanvar.ef, match.col="StdDev", main = "Efficient Frontier Weights by StdDev")

# define the porfolio size in CHF
total_investment <- 10000

# show the performance for the porfolio
chart.CumReturns(minVarReturns, main = "Return Performance")

shares <- matrix(nrow = 4, ncol = length(col_names))

# calculate the invested sum in CHF and number of shares per company
for (i in 1:length(col_names))
{
  first <- as.numeric(first(data[, col_names[i]]))
  last <- as.numeric(last(data[, col_names[i]]))

  share_number <- round((total_investment * opt_single$weights[i] / first), 0)
  if (share_number < 0)
    share_number = 0

  shares[1, i] <- share_number
  shares[2, i] <- share_number * first
  shares[3, i] <- share_number * last
  shares[4, i] <- share_number * (last - first)
}

colnames(shares) <- col_names
rownames(shares) <- c("Share Number", "Start Value", "End Value", "Difference")
shares

# calculate the information used for the Plots
share_number <- data.frame(number = shares[1, ], company = col_names)
start_val <- data.frame(money=shares[2, ], company=col_names)
end_val <- data.frame(money=shares[3, ], company=col_names)
start_val <- start_val %>%
  group_by(company) %>%
  filter(money > 0) %>%
  arrange(desc(company)) %>%
  mutate(prop = round(money*100/sum(sum(shares[2, ])), 1), 
         pie_text = paste("\n\n", prop, "%", sep = ""),
         num_p = paste(money, pie_text, sep = "\n"),
         money = round(money, 2))
end_val <- end_val %>%
  group_by(company) %>%
  filter(money > 0) %>%
  arrange(desc(company)) %>%
  mutate(prop = round(money*100/sum(sum(shares[3, ])), 1), 
         performance = round(money * 100 / total_investment, 1),
         pie_text = paste("\n\n", prop, "%", sep = ""),
         perf_text = paste("\n\n\n\n", "Performance = ", performance, "%", sep = ""),
         num_p = paste(money, pie_text, perf_text, sep = "\n"),
         money = round(money, 2))

# plot the number of share per company
ggplot(data = share_number, aes(x=company, y=number, color = company))+
  geom_bar(stat = "identity", fill = "white") +
  geom_text(aes(label=number), vjust=1.6, color = "black", size = 3.5) +
  ggtitle("Number of Shares per Company") +
  theme(plot.title = element_text(hjust = 0.5))
# plot the the start value in % and CHF per company which has at least 1 share
ggplot(data = start_val, aes(x = "", y = money, fill = company)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  geom_text(aes(label=money), position = position_stack(vjust = 0.5), color = "black") +
  geom_text(aes(label=pie_text), position = position_stack(vjust = 0.5), color = "black") +
  coord_polar("y", direction = -1) +
  labs(x = "", y = "", title = "Investment at Start") +
  theme(axis.ticks = element_blank(), axis.text = element_blank(), legend.position = c(0.2, 0), legend.justification = c(0.1, 0), plot.title = element_text(hjust = 0.5)) +
  guides(fill = guide_legend(title = NULL, nrow = 1))
# plot the the end value in %, performance in %  and CHF per company which has at least 1 share
ggplot(data = end_val, aes(x = "", y = money, fill = company)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  geom_text(aes(label=money), position = position_stack(vjust = 0.5), color = "black") +
  geom_text(aes(label=pie_text), position = position_stack(vjust = 0.5), color = "black") +
  geom_text(aes(label=perf_text), position = position_stack(vjust = 0.5), color = "black") +
  coord_polar("y", direction = -1) +
  labs(x = "", y = "", title = "Investment at End") +
  theme(axis.ticks = element_blank(), axis.text = element_blank(), legend.position = c(0.2, 0), legend.justification = c(0.1, 0), plot.title = element_text(hjust = 0.5)) +
  guides(fill = guide_legend(title = NULL, nrow = 1))

# cumulative all returns
cumret <- cumprod(1+minVarReturns)
acf(cumret, main = "Optimal Portfolio")
pacf(cumret, main = "Optimal Portfolio")
# calculate Arima from the cumulative returns
fit <- Arima(cumret, order = c(7, 0, 0), include.drift = TRUE)
summary(fit)

# print the residuals
checkresiduals(fit)

# calculate the forecast for the next 12 and 24 Lag
smi_forecast <- forecast(fit, h = 24)
smi_forecast <- forecast(fit, h = 12)

# print the forecast
plot(smi_forecast)

# Get the SMI Data
smi <- getSymbols.yahoo("^SSMI", from="2010-01-01", to="2020-05-08", periodicity = "weekly",
                        auto.assign=FALSE)[,6]

# calculate the SMI returns
smi_returns <- Return.calculate(smi, method = "simple")
smi_returns <- smi_returns[-1, ]
# print a chart to compare smi_returns vs. portfolio returns
compare <- cbind(smi_returns, minVarReturns)
colnames(compare) <- c("SMI", "Portfolio")
chart.CumReturns(compare, main = "Performance Portfolio vs. SMI", legend.loc = "topleft")
