
DataHandler <- R6::R6Class(classname = "DataHandler",
                           public = list(),
                           private = list())

# cTicker <- "GS"
DataHandler$lGetQuantmodDataForSimpleSmaCrossoverCheck <- function(cTicker, dateCheckDate) {

  # prepare the range for which data is to be fetched
  dateLastWindowDate <- dateCheckDate
  dateFirstWindowDate <- rlang::duplicate(x = dateCheckDate, shallow = FALSE)
  lubridate::year(x = dateFirstWindowDate) <- lubridate::year(x = dateFirstWindowDate) - 1
  dateFirstWindowDate <- dateFirstWindowDate + 1

  # load the data
  envDataEnv <- new.env()
  quantmod::getSymbols(Symbols = cTicker, src = "yahoo", env = envDataEnv,
                       from = dateFirstWindowDate, to = dateLastWindowDate + 1L)
  xtsData <- envDataEnv[[cTicker]]

  # check if there are at least 201 observations in the time series
  if (nrow(x = xtsData) < 202) {
    stop("The time series of stock prices for the ticker ", cTicker, " is too short: ",
         "it contains fewer than 202 observations, so the crossover check cannot be done! ")
  }

  # calculate the moving averages
  xtsShortSMA <- TTR::SMA(x = quantmod::Cl(x = xtsData), n = 50)
  xtsLongSMA <- TTR::SMA(x = quantmod::Cl(x = xtsData), n = 200)

  # prepare data.table to work on
  dtShortSMA <- data.table::as.data.table(x = xtsShortSMA)
  dtLongSMA <- data.table::as.data.table(x = xtsLongSMA)
  dtClosePrices <- data.table::as.data.table(x = quantmod::Cl(x = xtsData))

  # standardizing the naming of columns
  data.table::setnames(x = dtShortSMA,
                       old = c("index", "SMA"),
                       new = c("quote_date", "short_SMA"))
  data.table::setnames(x = dtLongSMA,
                       old = c("index", "SMA"),
                       new = c("quote_date", "long_SMA"))
  data.table::setnames(x = dtClosePrices,
                       old = c("index", colnames(dtClosePrices)[[2]]),
                       new = c("quote_date", cTicker))

  # standardizing the type of the quote_date
  dtShortSMA[, quote_date := as.Date(x = quote_date, format = "%Y-%m-%d")]
  dtLongSMA[, quote_date := as.Date(x = quote_date, format = "%Y-%m-%d")]
  dtClosePrices[, quote_date := as.Date(x = quote_date, format = "%Y-%m-%d")]

  # merging the data
  data.table::setkey(x = dtShortSMA, "quote_date")
  data.table::setkey(x = dtLongSMA, "quote_date")
  data.table::setkey(x = dtClosePrices, "quote_date")
  dtData <- merge(x = dtShortSMA, y = dtLongSMA)
  dtData <- merge(x = dtData, y = dtClosePrices, by = "quote_date")

  # keep only the complete cases
  dtData <- dtData[complete.cases(dtData), ]

  # check if enough rows and return
  if (nrow(x = dtData) >= 2L) {
    return(tail(x = dtData, 2L))
  } else {
    stop("Failure inside the DataHandler$lGetQuantmodDataForSimpleSmaCrossoverCheck ",
         "function calls: not enough rows on the output! ")
  }

}

# cTicker <- "MU"
# dateStartDate <- as.Date("2017-01-01")
# dateEndDate <- as.Date("2019-12-31")
DataHandler$dtFetchYahooDataForSymbol <- function(cTicker,
                                                  dateStartDate = NULL,
                                                  dateEndDate = NULL) {

  # 1. input validation --------------------------------------------------------
  # 1.1. cTicker - time series name
  if (!VariousUtils::BasicUtils$bIsScalarOfClass(
    objIn = cTicker, cClassName = "character")) {
    stop("Error inside the DataHandler$dtFetchYahooDataForSymbol class: the function ",
         "parameter cTicker is not a character scalar!")
  }
  # 1.2. dateStartDate,
  if (!is.null(dateStartDate)) {
    if (!VariousUtils::BasicUtils$bIsScalarOfClass(
      objIn = dateStartDate, cClassName = "Date")) {
      stop("Error inside the DataHandler$dtFetchYahooDataForSymbol class: the function ",
           "parameter dateStartDate is not a Data class scalar!")
    }
  }
  # 1.3. dateEndDate
  if (!is.null(dateEndDate)) {
    if (!VariousUtils::BasicUtils$bIsScalarOfClass(
      objIn = dateEndDate, cClassName = "Date")) {
      stop("Error inside the DataHandler$dtFetchYahooDataForSymbol class: the function ",
           "parameter dateEndDate is not a Data class scalar!")
    }
  }
  # 1.4. if both dates not null, check if dateEndDate is later than dateStartDate
  if (!is.null(x = dateStartDate) & !is.null(x = dateEndDate)) {
    if (dateStartDate > dateEndDate) {
      stop("Error inside the DataHandler$dtFetchYahooDataForSymbol class: the function ",
           "parameter dateStartDate is later than dateEndDate!")
    }
  }

  # 2. fetch the data ----------------------------------------------------------
  # 2.1. prepare temporary environment into which quantmod will load the data
  tempEnv <- new.env()
  # 2.2. perform the data fetch
  if (is.null(x = dateStartDate) & is.null(x = dateEndDate)) {
    res <- try(expr = {
      quantmod::getSymbols(Symbols = cTicker, src = "yahoo", env = tempEnv)
    }, silent = TRUE)
  } else if (!is.null(x = dateStartDate) & is.null(x = dateEndDate)) {
    res <- try(expr = {
      quantmod::getSymbols(Symbols = cTicker, src = "yahoo", env = tempEnv,
                           from = dateStartDate)
    }, silent = TRUE)
  } else if (is.null(x = dateStartDate) & !is.null(x = dateEndDate)) {
    res <- try(expr = {
      quantmod::getSymbols(Symbols = cTicker, src = "yahoo", env = tempEnv,
                           to = dateEndDate + 1L)
    }, silent = TRUE)
  } else {
    res <- try(expr = {
      quantmod::getSymbols(Symbols = cTicker, src = "yahoo", env = tempEnv,
                           from = dateStartDate, to = dateEndDate + 1L)
    }, silent = TRUE)
  }
  # 2.3. handle the output of the data fetch
  if (methods::is(object = res, class2 = "try-error")) {
    warning("Failed to fetch the data for ticker symbol: ", cTicker, "; the error",
            " message is: ", res, "; returning NULL! ")
    return(NULL)
  } else {
    dtData <- data.table::as.data.table(x = tempEnv[[cTicker]])
  }

  # 3. process the data: indexing, naming --------------------------------------
  # 3.1. renaming
  data.table::setnames(x = dtData,
                       old = c("index",
                               paste0(cTicker, c(".Open", ".High", ".Low",
                                                 ".Close", ".Volume", ".Adjusted"))),
                       new = c("quote_date",
                               paste0(cTicker, c("_open", "_high", "_low",
                                                 "_close", "_volume", "_adjusted"))))

  # 3.2. set the index
  data.table::setkey(x = dtData, "quote_date")

  return(dtData)
}

# cTicker <- "MU"
# dtData <- DataHandler$dtFetchYahooDataForSymbol(cTicker = cTicker)
# cBaseColumnName <- "MU_close"
# cAverageType <- "SMA"
# iWindowSize <- 200L
# dExpSmoothingFactor <- NULL
DataHandler$dtAddAverage <- function(dtData, cBaseColumnName,
                                     cAverageType, iWindowSize = 50L,
                                     dExpSmoothingFactor = NULL) {

  # 1. input validation --------------------------------------------------------
  # 1.1. cAverageType
  if (!(cAverageType %in% c("SMA", "EMA"))) {
    stop("Error in DataHandler$dtAddAverage function: the function parameter cAverageType ",
         "is neither 'SMA' or 'EMA'! ")
  }
  # 1.2. dtData
  if (!data.table::is.data.table(x = dtData)) {
    stop("Error in DataHandler$dtAddAverage function: the function parameter dtData ",
         "is not a data.table! ")
  }
  # 1.3. cBaseColumnName
  if (!VariousUtils::BasicUtils$bIsScalarOfClass(
    objIn = cBaseColumnName, cClassName = "character")) {
    stop("Error in DataHandler$dtAddAverage function: the function parameter cBaseColumnName ",
         "is not a character scalar! ")
  }
  if (!(cBaseColumnName %in% colnames(dtData))) {
    stop("Error in DataHandler$dtAddAverage function: the function parameter cBaseColumnName ",
         "is not one of the columns of the dtData parameter! ")
  }
  # 1.4. iWindowSize
  if (!VariousUtils::BasicUtils$bIsScalarOfClass(
    objIn = iWindowSize, cClassName = "integer")) {
    stop("Error in DataHandler$dtAddAverage function: the function parameter iWindowSize ",
         "is not an integer scalar! ")
  }
  if (iWindowSize > nrow(x = dtData)) {
    stop("Error in DataHandler$dtAddAverage function: the function parameter iWindowSize ",
         "is not an integer scalar! ")
  }
  if (iWindowSize <= 1L) {
    stop("Error in DataHandler$dtAddAverage function: the function parameter iWindowSize ",
         "is not larger than one! ")
  }
  # 1.5. dExpSmoothingFactor
  if (!is.null(x = dExpSmoothingFactor)) {
    # user-defined smoothing factor
    if (!VariousUtils::BasicUtils$bIsScalarOfClass(
      objIn = dExpSmoothingFactor, cClassName = "double")) {
      stop("Error in DataHandler$dtAddAverage function: the function parameter dExpSmoothingFactor",
           "is not a double type scalar! ")
    }
  } else {
    # if the smoothing factor not defined by the user get its default value
    dExpSmoothingFactor <- 2/(as.double(iWindowSize) + 1)
  }

  # 2. calculation of the average ----------------------------------------------
  # 2.1. simple average
  xtsData <- xts::as.xts(x = dtData)
  if (cAverageType == "SMA") {
    xtsAverage <- try(expr = {
      TTR::SMA(x = xtsData[, cBaseColumnName], n = iWindowSize)
    }, silent = TRUE)
  }
  # 2.2. exponential average
  if (cAverageType == "EMA") {
    xtsAverage <- try(expr = {
      TTR::EMA(x = xtsData[, cBaseColumnName], n = iWindowSize, ratio = dExpSmoothingFactor)
    }, silent = TRUE)
  }
  # 2.3. handle errors
  if (methods::is(object = xtsAverage, class2 = "try-error")) {
    warning("Error occurred when calculating the indicated average: returning NULL; ",
            "the full error message: ", xtsAverage)
    return(NULL)
  }

  # 3. post-processing and returning the data ----------------------------------
  # 3.1. cast as data.table
  dtAverage <- data.table::as.data.table(x = xtsAverage)
  # 3.2. adjust naming
  data.table::setnames(x = dtAverage, old = "index", new = "quote_date")
  dtAverage[, quote_date := as.Date(x = quote_date)]
  data.table::setkey(x = dtAverage, "quote_date")
  data.table::setnames(x = dtAverage, old = cAverageType,
                       new = paste0(cBaseColumnName, "_", cAverageType, iWindowSize, "D"))
  # 3.3. do the left join
  dtOut <- merge(x = dtData, y = dtAverage, by = "quote_date")
  return(dtOut)
}


