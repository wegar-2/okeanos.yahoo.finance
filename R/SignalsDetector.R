
SignalsDetector <- R6::R6Class(classname = "SignalsDetector",
                               public = list(),
                               private = list())


SignalsDetector$lCheckTwoRowsForCrossover <- function(
  dtData, cColnameShortRun = "short_SMA", cColnameLongRun = "long_SMA") {

  # 1. validate the input
  if (!data.table::is.data.table(x = dtData)) {
    stop("Error in call to SignalsDetector$lCheckTwoRowsForCrossover: ",
         "the function parameter dtData is not a data.table! ")
  }
  if (nrow(x = dtData) != 2L) {
    stop("Error in call to SignalsDetector$lCheckTwoRowsForCrossover: ",
         "the function parameter dtData does not have two rows! ")
  }

  # 2. do the check on the data.table using the indicated columns
  if ((dtData[1, ][[cColnameShortRun]] < dtData[1, ][[cColnameLongRun]]) &
      (dtData[2, ][[cColnameShortRun]] > dtData[2, ][[cColnameLongRun]])) {
    # crossover "up"
    bCrossoverCheck <- TRUE
    iCrossoverDirection <- 1L
  } else if ((dtData[1, ][[cColnameShortRun]] > dtData[1, ][[cColnameLongRun]]) &
             (dtData[2, ][[cColnameShortRun]] < dtData[2, ][[cColnameLongRun]])) {
    # crossover "down"
    bCrossoverCheck <- TRUE
    iCrossoverDirection <- -1L
  } else {
    # no crossover
    bCrossoverCheck <- FALSE
    iCrossoverDirection <- 0L
  }

  return(list(bCrossoverCheck = bCrossoverCheck,
              iCrossoverDirection = iCrossoverDirection))
}



SignalsDetector$dtCheckQuantmodSMA <- function(cTicker, dateCheckDate,
                                               cSource = "yahoo") {

  # prepare internal function parameter
  dtOut <- data.table::data.table(
    ticker = cTicker,
    crossover_check_complete = FALSE,
    crossover_check = FALSE,
    crossover_up_or_down = vector(mode = "character", length = 1L)
  )

  # pull the data required to do the check
  dtCheckData <- try(expr = {
    DataHandler$lGetQuantmodDataForSimpleSmaCrossoverCheck(
      cTicker = cTicker, dateCheckDate = dateCheckDate)
  }, silent = TRUE)
  if (methods::is(object = dtCheckData, class2 = "try-error")) {
    warning("Error occured when fetching data for the ticker: ", cTicker,
         "; error message: ", dtCheckData, immediate. = TRUE)
    return(dtOut)
  } else {
    if (dtCheckData[2, quote_date] != dateCheckDate) {
      warning("The check date ", dateCheckDate, " is not in the data set retrieved for ",
              "ticker: ", cTicker)
      return(dtOut)
    } else {
      message("successfully fetched data for ticker: ", cTicker)
    }
  }

  # check if there has been a crossover
  lCheckResult <- try(expr = {
    SignalsDetector$lCheckTwoRowsForCrossover(dtData = dtCheckData,
                                              cColnameShortRun = "short_SMA",
                                              cColnameLongRun = "long_SMA")
  }, silent = TRUE)
  # handle errors during the check
  if (methods::is(object = lCheckResult, class2 = "try-error")) {
    warning("Error occured when fetching the crossover for the ticker: ", cTicker, "; ",
         "error message: ", lCheckResult, immediate. = TRUE)
    return(dtOut)
  } else {
    message("Completed check for crossover for ", cTicker)
    dtOut[1, crossover_check_complete := TRUE]
  }

  # return the check result
  bCrossoverCheck <- lCheckResult$bCrossoverCheck
  if (bCrossoverCheck) {
    dtOut[1, crossover_check := TRUE]
    # check if crossover is up or down
    if (lCheckResult$iCrossoverDirection == 1L) {
      dtOut[1, crossover_up_or_down := "up"]
      message("There has been a crossover UP for ticker ", cTicker, " for check date: ",
              dateCheckDate)
    } else {
      dtOut[1, crossover_up_or_down := "down"]
      message("There has been a crossover DOWN for ticker ", cTicker, " for check date: ",
              dateCheckDate)
    }
  } else {
    message("There has been no crossover for ticker ", cTicker, " for check date: ",
            dateCheckDate)
    dtOut[, crossover_up_or_down := NA_character_]
  }

  return(dtOut)
}

# cTicker <- "MU"
# dateCheckDate <- as.Date(x = "2020-08-21")
SignalsDetector$MakeQuantmodPlotForDailySmaTickerCheck <- function(cTicker, dateCheckDate) {

  # 1. input validation --------------------------------------------------------
  # 1.1. cTicker
  if (!VariousUtils::BasicUtils$bIsScalarOfClass(
    objIn = cTicker, cClassName = "character")) {
    stop("Error in DataHandler$dtAddAverage function: the function parameter cTicker ",
         "is not a character scalar! ")
  }
  # 1.2. dateCheckDate
  if (!VariousUtils::BasicUtils$bIsScalarOfClass(
    objIn = dateCheckDate, cClassName = "Date")) {
    stop("Error in DataHandler$dtAddAverage function: the function parameter dateCheckDate ",
         "is not a Date class scalar! ")
  }

  # 2. prepare the range for which data is to be fetched -----------------------
  dateLastWindowDate <- dateCheckDate
  dateFirstWindowDate <- dateCheckDate
  lubridate::year(x = dateFirstWindowDate) <- lubridate::year(x = dateFirstWindowDate) - 2
  dateFirstWindowDate <- dateFirstWindowDate + 1

  # 3. prepare the time span for which the chart will be made ------------------
  dateCheckDateMinusOneYear <- dateCheckDate
  lubridate::year(x = dateCheckDateMinusOneYear) <-
    lubridate::year(x = dateCheckDateMinusOneYear) - 1
  dateCheckDateMinusOneYear <- dateCheckDateMinusOneYear + 1

  # 4. prepare the data to make the plot ---------------------------------------
  # 4.1. fetch the data
  dtData <- DataHandler$dtFetchYahooDataForSymbol(cTicker = cTicker,
                                        dateStartDate = dateFirstWindowDate,
                                        dateEndDate = dateLastWindowDate)
  # 4.2. add the averages: 50D SMA
  dtData <- DataHandler$dtAddAverage(
    dtData = dtData,  cBaseColumnName = paste0(cTicker, "_close"),
    cAverageType = "SMA", iWindowSize = 50L)
  # 4.3. add the averages: 200D SMA
  dtData <- DataHandler$dtAddAverage(
    dtData = dtData,  cBaseColumnName = paste0(cTicker, "_close"),
    cAverageType = "SMA", iWindowSize = 200L)
  # 4.4 filter for the relevant dates range only
  dtDataPlot <- dtData[quote_date >= dateCheckDateMinusOneYear, ]
  # 4.5. melt plot
  dtDataPlotMelted <- data.table::melt.data.table(
    data = dtDataPlot,
    measure.vars = c(paste0(cTicker, "_close"),
                     paste0(cTicker, "_close_SMA50D"),
                     paste0(cTicker, "_close_SMA200D")),
    variable.name = "which_price",
    id.vars = "quote_date",
    value.name = "price_value")
  # View(dtDataPlot)

  # 5. make a ggplot2 plot -----------------------------------------------------
  # 5.1. prepare the variables to use in the plot
  cCompanyName <- VariousUtils::dtStocksTickers[Ticker == cTicker, Name]
  cTimeRange <- paste0("from ", as.character(x = dateCheckDateMinusOneYear),
                       " to ", as.character(x = dateCheckDate))
  # 5.2. create the proper plot
  plotOut <- ggplot2::ggplot(data = dtDataPlot) +


  return(plotOut)
}


