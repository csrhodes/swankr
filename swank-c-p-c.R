`swank:completions` <- function(slimeConnection, sldbState, prefix, package) {
  bits <- strsplit(prefix, "(?=[A-Z._])", perl=TRUE)[[1]];
  lrx <- literal2rx(bits)
  ## FIXME: this includes slightly too much .*, because the strsplit
  ## seems to split before /and/ after the match.
  rx <- paste(lrx, collapse=".*")
  matches <- apropos(sprintf("^%s", rx), ignore.case=FALSE)
  nmatches <- length(matches)
  if((nmatches == 0) && ((dollar <- regexpr("$", prefix, fixed=TRUE)) > -1)) {
    
    symbolFieldsCompletion(globalenv(), prefix, prefix)
  } else {
    returnMatches(matches)
  }
}
