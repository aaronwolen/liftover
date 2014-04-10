
# Construct valid chain file URL
chain_url <- function(from, to) {
  url <- "http://hgdownload.cse.ucsc.edu/goldenPath/FROM/liftOver/FROMToTO.over.chain.gz"
  to <- sub("(^\\w)", "\\U\\1", to, perl = TRUE)
  url <- sub("TO",    to,   url)
  url <- gsub("FROM", from, url)
  return(url)
}
