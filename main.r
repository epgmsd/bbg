
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Main script to run all requests in requests.yml #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

library(magrittr)
# other packages required: yaml, data.table, xts, Rblpapi

# ~~~
# STEP 1:  Locate where this script so that it knows where to find the other files
# ~~~

locate <- function() {
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

loc  <- locate()
root <- dirname(loc)

# ~~~
# STEP 2:  Source all helpers
# ~~~

paste0(root, "/helpers.r") %>% source()

# ~~~
# STEP 3:  Load requests
# ~~~

requests <- yaml::read_yaml(paste0(root, "/requests.yml"))

# ~~~
# STEP 4:  Load requests
# ~~~

for (i in 1:length(requests)) {
  
  ## Get function
  .n <- names(requests)[[i]]
  .f <- get(.n)
  
  ## For each requests under each function, write a file
  for (j in 1:length(requests[[i]])) {
    
    n <- names(requests[[i]])[[j]]
    x <- do.call(.f, requests[[i]][[j]])
    
    ## Write to file
    if (!"data.table" %in% class(x)) stop("output of function must be data.table")
    data.table::fwrite(x, file=paste0(root, "/data/", n, ".csv"))
    
  }
}



