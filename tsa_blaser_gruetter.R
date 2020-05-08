# Loading required packages
library(quantmod)
library(lmtest)

# Downloading required data via Yahoo Finance API
data <- NULL
tickers_index <- c("ZURN.SW", "ROG.SW", "UBSG.SW", "NOVN.SW", "SREN.SW", "ABBN.SW", "SLHN.SW", "LONN.SW", "CSGN.SW", "SIKA.SW", "BAER.SW", "GIVN.SW", "NESN.SW", "UHR.SW", "CFR.SW", "ADEN.SW", "SCMN.SW", "LHN.SW", "SGSN.SW", "GEBN.SW")

for (Ticker in tickers_index){
  data <- cbind(data,
                getSymbols.yahoo(Ticker, from="2010-01-01", to="2020-05-01", periodicity = "weekly",
                                 auto.assign=FALSE)[,6])
}

colnames(data)<-c("Zurich", "Roche", "UBS", "Novartis", "Swiss Re", "ABB", "Swiss Life", "Lonza", "CS", "Sika", "Julius Bar", "Givaudan", "Nestle", "Swatch", "Richemont", "Adecco", "Swisscom", "LafargeHolcim", "SGS", "Geberit")
data