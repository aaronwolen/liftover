#' Remap genomic coordinates to a different genome assembly
#' 
#' @param x GRanges object
#' @param from genome assembly to which x is currently mapped
#' @param to genome assembly to which x should be mapped
#' @importFrom rtracklayer ucscGenomes import.chain liftOver
#' @importFrom GenomicRanges unlist genome
#' 
#' @export

liftover <- function(x, from = genome(x), to, chain.dir = getOption("chain.dir")) {
  
  genomes <- ucscGenomes()
  from <- match.arg(from[1], genomes$db)
  to   <- match.arg(to,   genomes$db)
    
  chain.url <- chain_url(from, to)
  chain.file <- chain_download(chain.url)
  chain <- import.chain(chain.file)
  
  # Verify GRanges and chain chromosomes match
  if( !any(names(chain) %in% seqlevels(x)) ) {
    # Check for 'chr' prefix
    if(!grepl("^chr", seqlevels(x)[1])) {
      seqlevels(x) <- paste0("chr", seqlevels(x))
    }  else {
     stop("seqlevels do not match chain chromosomes", call. = FALSE) 
    }
  }
  
  lifted <- liftOver(x, chain = chain)
  
  # TODO: Deal with cases of multiple overlapping ranges
  nchained <- elementLengths(lifted)
  if (any(nchained > 1 )) stop("Multiple overlapping ranges detected.")
  
  # Identify unchained sequences
  unchained <- x[nchained == 0]
  if (length(unchained) > 0) {
    warning(length(unchained), " ranges could not be converted and were",
            " dropped from the lifted results.\n",
            call. = FALSE, immediate. = TRUE)
  }
    
  lifted <- lifted[nchained > 0]
  lifted <-GenomicRanges::unlist(lifted)
  genome(lifted) <- to
  
  return(lifted)
}
