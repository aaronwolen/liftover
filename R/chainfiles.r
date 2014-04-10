

# Download chain file to a temporary directory if it doesn't exist already.
# Returns a character vector giving the name of the downloaded file. Additional
# arguments are passed to download.file()
chain_download <- function(url, destfile, ...) {
  if (missing(destfile)) destfile <- file.path(tempdir(), basename(url))
  if (file.exists(destfile)) return(destfile)
  download.file(url, destfile, ...)
  return(destfile)
}

# Construct valid chain file URL
chain_url <- function(from, to) {
  url <- "http://hgdownload.cse.ucsc.edu/goldenPath/FROM/liftOver/FROMToTO.over.chain.gz"
  to <- sub("(^\\w)", "\\U\\1", to, perl = TRUE)
  url <- sub("TO",    to,   url)
  url <- gsub("FROM", from, url)
  return(url)
}
