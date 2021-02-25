
#' Class providing interface for standard signals checks
#'
#' @export
MarketCrawler <- R6::R6Class(classname = "MarketCrawler",
                             public = list(),
                             private = list())



MarketCrawler$posixGetCurrentUtcDatetime <- function() {
  return(lubridate::with_tz(time = Sys.time(), tzone = "UTC"))
}


# dateCheckDate <- as.Date("2020-08-21")

MarketCrawler$lCheckForSignalsSp500 <- function(dateCheckDate) {

  # 1. load the tickers for which the checks are done
  message("Loading the list of the monitored stocks...")
  dtMonitoredSpStockTickers <- VariousUtils::dtMonitoredSpStockTickers
  message("Check will be performed for ", nrow(dtMonitoredSpStockTickers), " stocks. ")
  cTickers <- dtMonitoredSpStockTickers$Ticker

  # 2. iterate over the list of tickers and do the crossover checks
  message("Checking for SMA 50D vs 200D crossovers for selected S&P 500 members...")
  message("Making the check for the date: ", dateCheckDate)
  lCheckResults <- vector(mode = "list", length = length(cTickers))
  names(lCheckResults) <- cTickers

  for (cIterTicker in cTickers) {
    # cIterTicker <- cTickers[[1]]
    message("Checking for SMA 50D vs 200D crossover for ticker: ", cIterTicker)

    # call the SignalsDetector
    res <- try(expr = {
      SignalsDetector$dtCheckQuantmodSMA(cTicker = cIterTicker,
                                         dateCheckDate = dateCheckDate)
    }, silent = TRUE)

    # handle the call output
    if (methods::is(object = res, class2 = "try-error")) {
      warning("Failure to check the crossover for ", cIterTicker,
              " due to unhandled error! removing the ticker from the list of ",
              "tickers on output; error message: ", res, immediate. = TRUE)
      lCheckResults[[cIterTicker]] <- NULL
    } else {
      message("Saving check for crossover for: ", cIterTicker)
      lCheckResults[[cIterTicker]] <- res
    }
  }
  message("...done checking for SMA 50D vs 200D crossovers for selected S&P 500 members!")

  dtCheckResults <-
    data.table::rbindlist(l = lCheckResults, use.names = TRUE, fill = TRUE)

  return(dtCheckResults)
}
