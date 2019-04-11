#################################
#' extractHTMLAttachmentFromMSG
################################# 
#' 
#' extractHTMLAttachmentFromMSG extracts attachments from AIMS SMS messages.
#' @param file The file to be read. By default, this opens a file selection browser. 
#' @param output Output require, by default.
#' @return The function returns an AIMS SMS attachment.
#' @author Karl Ropkins (2015/07/09, v 0.3) 
#' @keywords methods 
#' @export
#' @examples
#' #to do

#to do 
###################
#can we tidy this function any more?
#

extractHTMLAttachmentFromMSG <- function (file = file.choose(), 
                                          output = NULL) 
{
  zz <- file(file, "rb")
  a <- c()
  i <- 1
  while (1) {
    temp <- readChar(zz, 1)
    if (length(temp) < 1) 
      break
    else {
      a[i] <- temp
      i <- i + 1
    }
    if (i == 1e+06) 
      break
  }
  close(zz)
  a <- paste(a, sep = "", collapse = "")
  if (!is.null(output)) {
    if (output == "file") {
      file.name <- strsplit(a, ".htm")[[1]][1]
      temp <- strsplit(file.name, "Flight Summary Report")[[1]]
      file.name <- temp[[1]][length(temp[[1]])]
      file.name <- paste("Flight Summary Report", file.name, 
                         sep = "")
      file.name <- paste(file.name, ".htm", sep = "")
      print(file.name)
    }
    else file.name <- output
  }
  a <- strsplit(a, "<html")[[1]][2]
  a <- strsplit(a, "</html>")[[1]][1]
  a <- paste("<html", a, sep = "")
  a <- paste(a, "</html>", sep = "")
  a <- sub("<tr><td class", "<table><tr><td class", a)
  if (is.null(output)) 
    return(a)
  write.table(a, file.name, quote = FALSE, sep = "ppppppppppppppppp", 
              row.names = FALSE, col.names = FALSE)
  invisible(NULL)
}
