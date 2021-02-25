library(data.table)
library(quantmod)
library(TTR)



StrategiesBacktester <- R6::R6Class(classname = "StrategiesBacktester",
                                    public = list(),
                                    private = list())


# StrategiesBacktester$lBacktestQuantmodSMA <- function(
#   cTicker, iShortSMA = 50L, iLongSMA = 200L, cSource = "yahoo",
#   dPainThresholdStdMultiplier = 1.5,
#   dTakeProfitMultiplier = 2.0) {
#
#   # load the data
#   envDataEnv <- new.env()
#   quantmod::getSymbols(Symbols = cTicker, src = "yahoo", env = envDataEnv)
#   xtsData <- envDataEnv[[cTicker]]
#
#   # add simple moving averages
#   xtsShortSMA <- TTR::SMA(x = quantmod::Cl(x = xtsData), n = iShortSMA)
#   xtsLongSMA <- TTR::SMA(x = quantmod::Cl(x = xtsData), n = iLongSMA)
#   # add Bollinger bands
#   xtsBBands <- TTR::BBands(n = iShortSMA,
#                            sd = dPainThresholdStdMultiplier,
#                            HLC = quantmod::Cl(x = xtsData))
#
#
#   # prepare data.table to work on
#   dtShortSMA <- data.table::as.data.table(x = xtsShortSMA)
#   dtLongSMA <- data.table::as.data.table(x = xtsLongSMA)
#   dtClosePrices <- data.table::as.data.table(x = quantmod::Cl(x = xtsData))
#
#   # standardizing the naming of columns
#   data.table::setnames(x = dtShortSMA,
#                        old = c("index", "SMA"),
#                        new = c("quote_date", "short_SMA"))
#   data.table::setnames(x = dtLongSMA,
#                        old = c("index", "SMA"),
#                        new = c("quote_date", "long_SMA"))
#   data.table::setnames(x = dtClosePrices,
#                        old = c("index", colnames(dtClosePrices)[[2]]),
#                        new = c("quote_date", cTicker))
#
#   # standardizing the type of the quote_date
#   dtShortSMA[, quote_date := as.Date(x = quote_date, format = "%Y-%m-%d")]
#   dtLongSMA[, quote_date := as.Date(x = quote_date, format = "%Y-%m-%d")]
#   dtClosePrices[, quote_date := as.Date(x = quote_date, format = "%Y-%m-%d")]
#
#   # merging the data
#   data.table::setkey(x = dtShortSMA, "quote_date")
#   data.table::setkey(x = dtLongSMA, "quote_date")
#   data.table::setkey(x = dtClosePrices, "quote_date")
#   dtData <- merge(x = dtShortSMA, y = dtLongSMA)
#   dtData <- merge(x = dtData, y = dtClosePrices, by = "quote_date")
#
#   # keep only the complete cases
#   dtData <- dtData[complete.cases(dtData), ]
#
#   # detect SMAs crossings and their directions over the complete history
#   dtData[, crossover_check := logical(length = nrow(dtData))]
#   dtData[, crossover_direction := integer(length = nrow(dtData))]
#   dtData[, crossover_direction := integer(length = nrow(dtData))]
#   # for (k in 2:nrow(dtData)) {
#   #   if (dtData) {
#   #
#   #   } else if () {
#   #
#   #   } else {
#   #     dtData[]
#   #   }
#   # }
#
#
#
# }
