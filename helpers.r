
#' @title get_bdh
#' @description Loads data from Bloomberg given list of tickers and one field.
#' @param tickers A character vector of the ticker names
#' @param field A single field to be downloaded for all the tickers.
#' @param start_date A date object
#' @param end_date A date object. Default - \code{Sys.Date()}
#' @param bbg_options Other options for \code{bdh}
#' @param bbg_overrides Overrides for \code{bdh}
#' @param ... Pass through param
#' @example test <- get_bbg(c("TY1.Comdty", "XM1.Comdty"), start_date = as.Date("2000-01-01"))
#' @export

get_bdh <- function(tickers, field = NULL, start_date, end_date = NULL, bbg_options = NULL, bbg_overrides = NULL, include_non_trading_days=FALSE, ...)
{
  # Ensure field is singular
  if (length(field) > 1) stop("get_bdh: does not support more than one field.")
  
  # Assign default for field as PX_LAST
  if (is.null(field)) field <- "PX_LAST"
  
  # Assign end_date default as Sys.Date()
  if (is.null(end_date)) end_date <- Sys.Date()
  
  # Ensure dates are dates
  tryCatch(start_date <- as.Date(start_date), error = function(e) stop("get_bdh: start_date cannot be coerced into Date object."))
  tryCatch(end_date   <- as.Date(end_date), error = function(e) stop("get_bdh: end_date cannot be coerced into Date object."))
  
  # Get req.tickers
  if (is.list(tickers)) {
    req.tickers <- names(tickers)
  } else {
    req.tickers <- tickers
  }
  
  # Connect to Bloomberg terminal
  Rblpapi::blpConnect()
  
  # remove NA values from tickers if any
  filterTicker <- stats::na.omit(req.tickers)
  
  # Returns a list of data.frame for each ticker.
  
  # ~~~ Code chunk below for debugging Rblpapi::bdh. Call to Rblpapi:::bdh_Impl can be corrupt if data type returned is date. ~~~
  # for (f in filterTicker) {
  #
  #   print(f)
  #
  #   rawData <- Rblpapi::bdh(securities = f,
  #                           fields     = field,
  #                           start.date = start_date,
  #                           end.date   = end_date,
  #                           include.non.trading.days = include_non_trading_days,
  #                           options    = bbg_options,
  #                           overrides  = bbg_overrides)
  #
  # }
  # ~~~ Code chunk end ~~~
  
  rawData <- Rblpapi::bdh(securities = filterTicker,
                          fields     = field,
                          start.date = start_date,
                          end.date   = end_date,
                          include.non.trading.days = include_non_trading_days,
                          options    = bbg_options,
                          overrides  = bbg_overrides)
  
  if (length(req.tickers) > 1) {
    
    # There is a possibility of empty data.frame for some tickers.
    emptyDfTickers <- unlist(lapply(rawData, nrow))
    emptyDfTickers <- names(emptyDfTickers[emptyDfTickers == 0])
    
    # Convert data into xts
    rawData <- lapply(rawData, function(x){xts::xts(x[, -1], order.by = x[, 1])})
    
    if (length(field) == 1) {
      
      # If the number of field is 1, then these can be merged into a single xts.
      rawData <- do.call(merge, rawData)
      
      # Logic to add back the NA columns to ensure a consistent dimension
      naIdx <- attr(filterTicker, "na.action")
      
      if (!is.null(naIdx)) {
        # Fill columns of NA
        naXts <- xts::xts(matrix(NA, ncol = length(naIdx), nrow = nrow(rawData)), order.by = zoo::index(rawData))
        names(naXts) <- rep("NA.", ncol(naXts))
        rawData <- merge(rawData, naXts)
      }
      
      if (!isTRUE(all.equal(emptyDfTickers, character(0)))) {
        
        # Add extra NA columns
        warning(paste0("Following tickers had no data : ", paste0(emptyDfTickers, collapse = ", ")))
        emptyXts <- xts::xts(matrix(NA, ncol = length(emptyDfTickers), nrow = nrow(rawData)),
                             order.by = zoo::index(rawData))
        
        names(emptyXts) <- make.names(emptyDfTickers)
        rawData <- merge(rawData, emptyXts)
      }
      
      rawData <- rawData[, make.names(req.tickers)] # re-order to original sequence
    }
    
  } else {
    
    # If there is only one ticker then the result is a data.frame
    rawData <- xts::xts(rawData[, -1], order.by = rawData[, 1])
    names(rawData) <- make.names(req.tickers) # ensure names are made accordingly
    
  }
  
  # 4. Rename if required
  if (is.list(tickers)) {
    
    ## Get column_mapping as list
    mapping <- tickers
    names(mapping) <- make.names(names(mapping))
    
    ## Special case: if mapping is a nested list, we convert each element to JSON
    mapping <- lapply(mapping, function(x) {
      if (is.list(x)) {
        return(jsonlite::toJSON(x, auto_unbox = TRUE))
      }
    })
    
    ## Rename
    xts <- plyr::rename(rawData, mapping)
    
  } else {
    
    xts <- rawData
    
  }
  
  dt <- data.table::as.data.table(xts)
  data.table::setnames(dt, "index", "Date", skip_absent=TRUE)
  
  return(dt)
}


#' @title get_bdh_bars
#' @description Loads intraday data from Bloomberg given list of tickers.
#' @param tickers A character vector of the ticker names
#' @param start_time must be POSITxt
#' @param end_time must be POSITxt
#' @export

get_bdh_bars <- function(tickers, start_time = Sys.time() - 60 * 60 * 6, end_time = Sys.time())
{
  # Ensure dates are dates
  tryCatch(start_time <- as.POSIXct(start_time), error = function(e) stop("get_bdh_bars: start_time cannot be coerced into POSIXct object."))
  tryCatch(end_time   <- as.POSIXct(end_time), error = function(e) stop("get_bdh_bars: end_time cannot be coerced into POSIXct object."))
  
  # Connect to Bloomberg terminal
  Rblpapi::blpConnect()
  
  # Returns a list of data.frame for each ticker.
  x <- lapply(tickers, function(tx) {
    
    dt <- Rblpapi::getBars(tx, startTime=start_time, endTime=end_time)
    dt <- data.table::as.data.table(dt)
    dt <- dt[, ticker := tx]
    
  }) %>% data.table::rbindlist()
  
  return(x)
}

