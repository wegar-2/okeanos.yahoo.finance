library(R6)
library(dplyr)
library(lubridate)
library(zoo)
library(lubridate)
library(data.table)
library(xts)
library(TTR)
library(quantmod)
library(VariousUtils)
library(magrittr)
library(rtweet)
library(ggplot2)
library(XBRL)
library(finreportr)
source("R/DataHandler.R")
source("R/SignalsDetector.R")
source("R/MarketCrawler.R")
source("R/TwitterUtils.R")
source("R/TwitterInterface.R")



ProcessesManager <- R6::R6Class(classname = "ProcessesManager",
                                public = list(),
                                private = list())



ProcessesManager$RunDailySimpleMovingAverageChecks <- function(lTwitterApiToken) {

  message("performing daily check...")

  # ============================================================================
  # 1. check today's date ======================================================
  # posixCurrentTime <- MarketCrawler$posixGetCurrentUtcDatetime()
  # dateCurrentDate <- as.Date(posixCurrentTime)
  dateCurrentDate <- as.Date(x = "2020-08-28")
  message("current date: ", dateCurrentDate)

  # ============================================================================



  # ============================================================================
  # 2. call the crawler to do SMA crossovers checks ============================
  res <- try(expr = {
    MarketCrawler$lCheckForSignalsSp500(dateCheckDate = dateCurrentDate)
  })
  if (methods::is(object = res, class2 = "try-error")) {
    warning("Failed call to MarketCrawler$lCheckForSignalsSp500; ",
            "error message: ", res, immediate. = TRUE)
    return(NULL)
  } else {
    dtCheckResults <- data.table::as.data.table(x = res)
  }
  # ============================================================================



  # ============================================================================
  # 3. prepare summary variables ===============================================
  iNumOfMonitoredCompanies <- nrow(x = dtCheckResults)
  iNumOfSuccessfullyCheckedCompanies <-
    dtCheckResults[crossover_check_complete == TRUE, .N]
  iCrossoversCount <- res[crossover_check == TRUE, .N]
  if (iCrossoversCount > 0L) {
    iCrossoversUp <- res[crossover_check == TRUE & crossover_up_or_down == "up", .N]
    iCrossoversDown <- res[crossover_check == TRUE & crossover_up_or_down == "down", .N]
  }
  message("#companies in the list: ", iNumOfMonitoredCompanies)
  message("#successfully checked companies: ", iNumOfSuccessfullyCheckedCompanies)
  message("#crossovers: ", iCrossoversCount)
  message("#crossovers up: ", iCrossoversUp)
  message("#crossovers down: ", iCrossoversDown)
  cTickersCompaniesCrossover <-
    dtCheckResults[crossover_check_complete == TRUE & crossover_check == TRUE, ticker] %>%
    sort.default(decreasing = FALSE)
  cTickersCompaniesCrossoverUp <-
    dtCheckResults[crossover_check_complete == TRUE & crossover_check == TRUE &
                     crossover_up_or_down == "up", ticker] %>%
    sort.default(decreasing = FALSE)
  cTickersCompaniesCrossoverDown <-
    dtCheckResults[crossover_check_complete == TRUE & crossover_check == TRUE &
                     crossover_up_or_down == "down", ticker] %>%
    sort.default(decreasing = FALSE)
  # ============================================================================



  # ============================================================================
  # 4. prepare the summary message to publish ==================================
  if (iCrossoversCount == 0L) {
    # 4.1. there are no crossovers detected
    cTextToPublish <- paste0(
      "The TSG completed checking for the SMA 50D-200D crossovers. ",
      "There haven't been any crossovers on ", dateCurrentDate, " in the list of ",
      nrow(x = res), " checked companies, out of which ", iCountSuccessfulChecks,
      " checks have been successful. ")
  } else {
    # 4.2. there are crossovers detected
    # 4.2.1. prepare core of the message
    cTextToPublish <- paste0(
      "The TSG completed checking for the SMA 50D-200D crossovers. ",
      "There have been ", iCrossoversCount, " crossovers on ", dateCurrentDate, ". ",
      "Number of successful checks: ", iNumOfSuccessfullyCheckedCompanies, ". ",
      "Number of crossovers up: ", iCrossoversUp, ". ",
      "Number of crossovers down: ", iCrossoversDown, ". ")
    # 4.2.2. add list of companies with crossover up
    if (length(x = cTickersCompaniesCrossoverUp) > 0L) {
      cTextToPublish <-
        paste0(cTextToPublish,
          paste0(" Tickers of companies with crossover up: ",
                 paste0(cTickersCompaniesCrossoverUp, collapse = ", "), ". "))
    }
    # 4.2.3. add list of companies with crossover down
    if (length(x = cTickersCompaniesCrossoverDown) > 0L) {
      cTextToPublish <-
        paste0(cTextToPublish,
          paste0(" Tickers of companies with crossover up: ",
                 paste0(cTickersCompaniesCrossoverDown, collapse = ", "), ". "))
    }
  }
  # ============================================================================



  # ============================================================================
  # 5. publish the checks summary ==============================================
  # 5.1. cut text into pieces
  lTexts <- TwitterInterface$lFormatTextAsTwitterThread(
    cTextOfThread = cTextToPublish)
  # 5.2. make postable list
  lTweets <- TwitterInterface$BindTextsAndMediasIntoTweets(
    lTexts = lTexts, lMedias = NULL)
  # 5.3. create the Token
  objTwitterToken <-
    TwitterUtils$objMakeTwitterToken(lTwitterApiToken = lTwitterApiToken)
  # 5.4. post the tweets
  TwitterInterface$PostThread(lTweets = lTweets,
                              objTwitterToken = objTwitterToken)
  # ============================================================================



  # ============================================================================
  # 6. publish plots for stocks with crossovers ================================
  # If there are crossovers, publish the corresponding charts with brief messages
  Sys.sleep(time = 30L)
  if (iCrossoversCount > 0L) {

    for (k in 1:length(x = cTickersCompaniesCrossover)) {
      # 6.1. load the name of the ticker
      cIterTicker <- cTickersCompaniesCrossover[[k]]

      # 6.2. prepare the text of the message
      cIterText <- paste0(

      )

      # 6.3. prepare the chart for the ticker


      # 6.4. prepare the list of tweets to publish

      # 6.5. publich the tweet

      # 6.6. take a break of 45 second before publishing next message

    }



  }

  # ============================================================================


  return(TRUE)
}



ProcessesManager$PublishSelectedNbpQuotes <- function(lTwitterApiToken) {

}



ProcessesManager$PublishUsTreasuryYieldCurve <- function(lTwitterApiToken) {

}
