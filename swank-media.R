makeMediaReplResult <- function(value) {
  UseMethod("makeMediaReplResult")
}

makeMediaReplResult.default <- function(value) {
  makeReplResult(value)
}

makeMediaReplResult.trellis <- function(value) {
  filename <- tempfile("swank-media-")
  png(filename, width=320, height=200)
  print(value)
  dev.off()
  list(quote(`:write-image`), list(list(quote(`:type`), quote(png),
                                        quote(`:file`), filename)),
       deparse(value$call, nlines=1))
}

makeReplResultFunction <- makeMediaReplResult
