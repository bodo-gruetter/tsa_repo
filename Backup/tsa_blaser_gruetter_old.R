# Loading required packages
library(quantmod)
library(lmtest)
library(matrixStats)
library(dplyr)

col_names <- c("Zurich", "Roche", "UBS", "Novartis", "SwissRe", "ABB", "SwissLife", "Lonza", "CreditSuisse", "Sika", 
               "JuliusBar", "Givaudan", "Nestle", "Swatch", "Richemont", "Adecco", "Swisscom", "LafargeHolcim", "SGS", 
               "Geberit")

# Downloading required data via Yahoo Finance API
# it contains all SMI stock prices
data <- NULL
tickers_index <- c("ZURN.SW", "ROG.SW", "UBSG.SW", "NOVN.SW", "SREN.SW", "ABBN.SW", "SLHN.SW", "LONN.SW", "CSGN.SW", "SIKA.SW", "BAER.SW", "GIVN.SW", "NESN.SW", "UHR.SW", "CFR.SW", "ADEN.SW", "SCMN.SW", "LHN.SW", "SGSN.SW", "GEBN.SW")

for (Ticker in tickers_index){
  data <- cbind(data,
                getSymbols.yahoo(Ticker, from="2010-01-01", to="2020-05-08", periodicity = "weekly",
                                 auto.assign=FALSE)[,6])
}

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
cor_smi <- cor(log_returns)

# Creating a data.frame that contains the weights for the all assets and empty columns for the portfolio return, standard deviation and Sharpe-rations
pf <- data.frame(Zurich = rep(x_weights, length(x_weights)),
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

# calculating the rowsum for all SMI stocks and keep only records with rowSums <= 1 and rowSums > 0
pf <- cbind(pf, rowSums = pf %>%
                select(col_names) %>%
                rowSums())
pf <- pf %>% 
  filter(rowSums <= 1) %>%
  filter(rowSums > 0) %>%
  select(-rowSums)

# Calculating the expected returns and standard deviations for the 1000 portfolios
for(i in 1:nrow(pf)){
  # calculate expected return from all SMI stocks
  pf$er_p[i] <- pf$Zurich[i] * er_smi["Zurich"] + pf$Roche[i] * er_smi["Roche"] + pf$UBS[i] * er_smi["UBS"] + 
                pf$Novartis[i] * er_smi["Novartis"] + pf$SwissRe[i] * er_smi["SwissRe"] + pf$ABB[i] * er_smi["ABB"] +
                pf$SwissLife[i] * er_smi["SwissLife"] + pf$Lonza[i] * er_smi["Lonza"] + pf$CreditSuisse[i] * er_smi["CreditSuisse"] +
                pf$Sika[i] * er_smi["Sika"] + pf$JuliusBar[i] * er_smi["JuliusBar"] + pf$Givaudan[i] * er_smi["Givaudan"] +
                pf$Nestle[i] * er_smi["Nestle"] + pf$Swatch[i] * er_smi["Swatch"] + pf$Richemont[i] * er_smi["Richemont"] +
                pf$Adecco[i] * er_smi["Adecco"] + pf$Swisscom[i] * er_smi["Swisscom"] + 
                pf$LafargeHolcim[i] * er_smi["LafargeHolcim"] + pf$SGS[i] * er_smi["SGS"] + pf$Geberit[i] * er_smi["Geberit"]
  # calculate standard deviation form all SMI stocks
  pf$sd_p[i] <- sqrt(pf$Zurich[i]^2 * sd_smi[1]^2 + pf$Roche[i]^2 * sd_smi[2]^2 + pf$UBS[i]^2 * sd_smi[3]^2 +
                     pf$Novartis[i]^2 * sd_smi[4]^2 + pf$SwissRe[i]^2 * sd_smi[5]^2 + pf$ABB[i]^2 * sd_smi[6]^2 +
                     pf$SwissLife[i]^2 * sd_smi[7]^2 + pf$Lonza[i]^2 * sd_smi[8]^2 + pf$CreditSuisse[i]^2 * sd_smi[9]^2 +
                     pf$Sika[i]^2 * sd_smi[10]^2 + pf$JuliusBar[i]^2 * sd_smi[11]^2 + pf$Givaudan[i]^2 * sd_smi[12]^2 +
                     pf$Nestle[i]^2 * sd_smi[13]^2 + pf$Swatch[i]^2 * sd_smi[14]^2 + pf$Richemont[i]^2 * sd_smi[15]^2 +
                     pf$Adecco[i]^2 * sd_smi[16]^2 + pf$Swisscom[i]^2 * sd_smi[17]^2 + pf$LafargeHolcim[i] * sd_smi[18]^2 + 
                     pf$SGS[i]^2 * sd_smi[19]^2 + pf$Geberit[i]^2 * sd_smi[20]^2 +
                     2 * pf$Zurich[i] * pf$Roche[i] * pf$Novartis[i] * pf$SwissRe[i] * pf$ABB[i] * pf$SwissLife[i] * pf$Lonza[i] * 
                     pf$CreditSuisse[i] * pf$Sika[i] * pf$JuliusBar[i] * pf$Givaudan[i] * pf$Nestle[i] * pf$Swatch[i] * pf$Richemont[i] * 
                     pf$Adecco[i] * pf$Swisscom[i] * pf$LafargeHolcim[i] * pf$SGS[i] * pf$Geberit[i] * 
                     sd_smi[1] * sd_smi[2] * sd_smi[3] * sd_smi[4] * sd_smi[5] * sd_smi[6] * sd_smi[7] * sd_smi[8] *
                     sd_smi[9] * sd_smi[10] * sd_smi[11] * sd_smi[12] * sd_smi[13] * sd_smi[14] * sd_smi[15] *
                      sd_smi[16]  * sd_smi[17] * sd_smi[18] * sd_smi[19] * sd_smi[20] *  mean(cor_smi)) # Formula for calculating portfolio standard deviation
}

# Plotting the efficient frontier
plot(x=pf$sd_p, y=pf$er_p, xlab="SD Portfolio", ylab="Expected Return Portfolio")
grid()

# Deriving the weightings of the market portfolio, i.e. the one that maximizes the Sharpe-ratio
# Calculating the Sharpe-ratio per portfolio (see slide 37)
pf$sr_p <- (pf$er_p-risk_free_rate)/pf$sd_p

# Identifying the index of the market portfolio, i.e. the row number of the Sharpe-ratio-maximizing portfolio
indx_mp <- which.max(pf$sr_p)

# Identifying the weightings of the market portfolio
weightings <- cbind(pf$Zurich[indx_mp], pf$Roche[indx_mp], pf$UBS[indx_mp], pf$Novartis[indx_mp], pf$SwissRe[indx_mp], pf$ABB[indx_mp],
                    pf$SwissLife[indx_mp], pf$Lonza[indx_mp], pf$CreditSuisse[indx_mp], pf$Sika[indx_mp], 
                    pf$JuliusBar[indx_mp], pf$Givaudan[indx_mp], pf$Nestle[indx_mp], pf$Swatch[indx_mp],
                    pf$Richemont[indx_mp], pf$Adecco[indx_mp], pf$Swisscom[indx_mp], pf$LafargeHolcim[indx_mp],
                    pf$SGS[indx_mp], pf$Geberit[indx_mp])
colnames(weightings) <- col_names
pie(weightings, labels = paste(round(weightings*100), "% ", colnames(weightings),sep = ""), main = "Asset allocation of market portfolio")

# Extracting the Sharpe-ratio of the market portfolio
sr_mp <- pf$sr_p[indx_mp]
cbind(sr_smi, sr_mp)
plot(x=pf$sd_p, y=pf$er_p, xlab="SD Portfolio", ylab="Expected Return Portfolio")
abline(a=risk_free_rate, b=sr_mp, lty=2, col="red")
grid()
