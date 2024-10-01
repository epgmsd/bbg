
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
  
  return(xts)
}


#' @title locate
#' @description Internal function to automatically detect path of calling script. Works with Rscript from command line, sourcing from another R script, running in Rstudio on the fly, as well as rendering into Rmarkdown with knitr.

locate <- function()
{
  ## Get the file needle if R script has been run from command line
  cmdArgs <- commandArgs(trailingOnly = FALSE)
  needle  <- "--file="
  match   <- grep(needle, cmdArgs)
  
  ## Attempt to get envir that's 1 frame earlier
  frame1 <- tryCatch(sys.frames()[[1]], error = function(e) return(NULL))
  
  ## Attempt to get Rstudio api paths
  active_doc_path    <- tryCatch(rstudioapi::getActiveDocumentContext()$path, error = function(e) return(NULL))
  source_editor_path <- tryCatch(rstudioapi::getSourceEditorContext()$path,   error = function(e) return(NULL))
  
  ## Attempt to get Knitr path
  knitr_path <- tryCatch(knitr::current_input(dir = TRUE), error = function(e) return(NULL))
  
  ## If Rscript via command line
  if (length(match) > 0) {
    
    return(sub(needle, "", cmdArgs[match]))
    
    ## If sourced via Rstudio
  } else if ("fileName" %in% ls(frame1)) {
    
    return(frame1$fileName)
    
    ## If sourced via R console
  } else if (!is.null(frame1$ofile)) {
    
    return(frame1$ofile)
    
    ## If Rstudio run selection (http://stackoverflow.com/a/35842176/2292993)
  } else if (!is.null(active_doc_path) && active_doc_path != '') {
    
    return(active_doc_path)
    
    ## If run from Rstudio console
  } else if (!is.null(source_editor_path) && source_editor_path != '') {
    
    return(source_editor_path)
    
    ## If run from knitr / Rmarkdown
  } else if (!is.null(knitr_path) && knitr_path != '') {
    
    return(knitr_path)
    
    ## If nothing works, return NULL
  } else {
    
    return(NULL)
    
  }
}


#' @title parse_request
#' @description 


loc  <- locate()
root <- dirname(loc)

requests <- yaml::read_yaml(paste0(root, "/requests.yml"))

for (i in 1:length(requests)) {
  
  ## Get name
  n <- names(requests)[[i]]
  
  ## Get bbg
  r <- requests[[i]]
  x <- get_bbg(tickers=r$tickers, field=r$field, start_date=r$start_date, end_date=r$end_date, 
               bbg_options=r$bbg_options, bbg_overrides=r$bbg_overrides, include_non_trading_days=r$include_non_trading_days)
  
  ## Record as flat file
  dt <- data.table::as.data.table(x)
  data.table::setnames(dt, "index", "Date", skip_absent=TRUE)
  
  ## Write to file
  data.table::fwrite(dt, file=paste0(root, "/data/", n, ".csv"))
}



