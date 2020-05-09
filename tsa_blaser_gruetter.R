# Loading required packages
library(quantmod)
library(lmtest)
library(matrixStats)
library(dplyr)

# Downloading required data via Yahoo Finance API
# it contains all SMI stock prices
data <- NULL
tickers_index <- c("ZURN.SW", "ROG.SW", "UBSG.SW", "NOVN.SW", "SREN.SW", "ABBN.SW", "SLHN.SW", "LONN.SW", "CSGN.SW", "SIKA.SW", "BAER.SW", "GIVN.SW", "NESN.SW", "UHR.SW", "CFR.SW", "ADEN.SW", "SCMN.SW", "LHN.SW", "SGSN.SW", "GEBN.SW")

for (Ticker in tickers_index){
  data <- cbind(data,
                getSymbols.yahoo(Ticker, from="2010-01-01", to="2020-05-01", periodicity = "weekly",
                                 auto.assign=FALSE)[,6])
}

col_names <- c("Zurich", "Roche", "UBS", "Novartis", "SwissRe", "ABB", "SwissLife", "Lonza", "CreditSuisse", "Sika", 
               "JuliusBar", "Givaudan", "Nestle", "Swatch", "Richemont", "Adecco", "Swisscom", "LafargeHolcim", "SGS", 
               "Geberit")
colnames(data) <- col_names

log_returns <- na.omit(diff(log(data)))
risk_free_rate <- 0

head(data)

# calculating expected returns
er_smi <- colMeans(log_returns, na.rm = T)

# standard deviation
sd_smi <- colSds(log_returns)

# sharpe-ratio
sr_smi <- (er_smi - risk_free_rate) / sd_smi

# create portfolio weights
x_weights <- seq(from = 0, to = 1, length.out = 1000)

# Creating a data.frame that contains the weights for the all assets and empty columns for the portfolio return, standard deviation and Sharpe-rations
pf <- data.frame(Zurich = rep(x_weights, each = length(x_weights)),
                 Roche = rep(x_weights, length(x_weights)),
                 UBS = rep(x_weights, length(x_weights)),
                 Novartis = rep(x_weights, length(x_weights)),
                 SwissRe = rep(x_weights, length(x_weights)),
                 ABB = rep(x_weights, length(x_weights)),
                 SwissLife = rep(x_weights, length(x_weights)),
                 Lonza = rep(x_weights, length(x_weights)),
                 CreditSuisse = rep(x_weights, length(x_weights)),
                 Sika = rep(x_weights, length(x_weights)),
                 JuliusBar = rep(x_weights, length(x_weights)),
                 Givaudan = rep(x_weights, length(x_weights)),
                 Nestle = rep(x_weights, length(x_weights)),
                 Swatch = rep(x_weights, length(x_weights)),
                 Richemont = rep(x_weights, length(x_weights)),
                 Adecco = rep(x_weights, length(x_weights)),
                 Swisscom = rep(x_weights, length(x_weights)),
                 LafargeHolcim = rep(x_weights, length(x_weights)),
                 SGS = rep(x_weights, length(x_weights)),
                 Geberit = rep(x_weights, length(x_weights)),
                 er_p = NA, sd_p = NA, sr_p = NA)

# calculating the rowsum for all SMI stocks and keep only records with rowSum <= 1
#pf <- cbind(pf, rowSums = pf %>%
#                select(col_names) %>%
#                rowSums())
#pf <- pf %>% 
#  filter(rowSums <= 1) %>%
#  select(-rowSums)

# Calculating the expected returns and standard deviations for the 1000 portfolios
#for(i in 1:nrow(pf)){
#  pf$er_p[i] <- pf$w_bonds[i] * er_bonds + pf$w_stocks[i] * er_stocks # Formula for calculating portfolio returns (see slide 28)
#  pf$sd_p[i] <- sqrt(pf$w_bonds[i]^2 * sd_bonds^2 + 
#                       pf$w_stocks[i]^2 * sd_stocks^2 + 
#                       2 * pf$w_bonds[i] * pf$w_stocks[i] * sd_bonds * sd_stocks * cor_bs) # Formula for calculating portfolio standard deviation (see slide 29 or 32)
#}

head(pf)

