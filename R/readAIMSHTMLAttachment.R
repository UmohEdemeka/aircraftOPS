#################################
#' readAIMSHTMLAttachment
################################# 
#' 
#' readAIMSHTMLAttachment reads data in AIMS SMS message attachments.
#' @param file The file to be read. By default, this opens a file selection browser. 
#' @param make.numeric The names of any data columns which, if found, should be converted to numerics.
#' @param verbose If TRUE, the function provides a full read report.
#' @return A data.frame of AIMS flight reports.
#' @author Karl Ropkins (2015/07/09, v 0.3) 
#' @keywords methods 
#' @export
#' @examples
#' #to do

#to do 
###################
#can we tidy this function any more?
#

readAIMSHTMLAttachment <- function (file = file.choose(), 
                                    make.numeric = c("fob.on", "fob.off"),
                                    verbose = FALSE) 
{

  #require(XML)
  #use XML to read in HTML table 
  tables <- XML::readHTMLTable(file, stringsAsFactors = FALSE)
  #check it looks right
  if (length(tables) < 1) {
    warning("Empty AIMS data attachment suspected!", call. = FALSE)
    return(NULL)
  }
  #get right table
  n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
  if (verbose) {
    print(n.rows)
    print(length(tables))
  }
  if (length(n.rows) < length(tables)) 
    n.rows <- c(0, n.rows)
  tables <- tables[[which.max(n.rows)]]
  #get rows making different days
  heads <- grep("Date", tables[, 1])
  #check that looks right
  if (length(heads) < 1) {
    warning("Empty AIMS data attachment suspected!", call. = FALSE)
    return(NULL)
  }
  heads2 <- c(heads[-1] - 1, nrow(tables) + 2)
  #arrange data
  ans <- lapply(1:length(heads), function(x) {
    ref <- as.character(tables[heads[x] - 1, ])[1]
    names <- as.character(tables[heads[x], ])
    data <- tables[(heads[x] + 1):(heads2[x] - 2), ]
    names(data) <- names
    temp <- data.frame(ref = rep(ref, nrow(data)))
    cbind(temp, data)
  })
  ans <- rbind.fill(ans)
  #tidy names
  names(ans) <- tolower(make.names(names(ans)))
  names(ans) <- gsub("\\.{2}", ".", names(ans))
  names(ans) <- gsub("\\.$", "", names(ans))
  #numeric resets
  if (is.logical(make.numeric)) 
    make.numeric <- if (make.numeric) 
      c("fob.on", "fob.off")
  else NULL
  if (is.numeric(make.numeric)) 
    make.numeric <- names(ans)[make.numeric]
  make.numeric <- make.numeric[make.numeric %in% names(ans)]
  if (length(make.numeric) > 0) 
    for (i in 1:length(make.numeric)) 
      ans[, make.numeric[i]] <- as.numeric(ans[, 
                                               make.numeric[i]])
  #output
  ans
}
