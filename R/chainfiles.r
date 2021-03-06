# Download chain file to a temporary directory if it doesn't exist already.
# Returns a character vector giving the name of the downloaded file. Additional
# arguments are passed to download.file()
chain_download <- function(url, chain.path = getOption("chain.path"), ...) {
  
  destfile <- file.path(chain.path, basename(url))
  
  file <- sub(".gz", "", destfile)
  if (file.exists(file)) return(file)
  
  if (!file.exists(chain.path)) {
    dir.create(chain.path, showWarnings = FALSE, recursive = TRUE)
  } 
  
  download.file(url, destfile, ...)
  
  # TODO: Need a more efficient way to decompress
  file.con <- gzfile(destfile)
  writeLines(readLines(file.con), file)
  close.connection(file.con)
  
  return(file)
}

# Construct valid chain file URL based on the original genome ('from') and
# the output genome ('to') 
chain_url <- function(from, to) {
  url <- "http://hgdownload.cse.ucsc.edu/goldenPath/FROM/liftOver/FROMToTO.over.chain.gz"
  to <- sub("(^\\w)", "\\U\\1", to, perl = TRUE)
  url <- sub("TO",    to,   url)
  url <- gsub("FROM", from, url)
  return(url)
}
