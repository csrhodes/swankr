swank <- function(port=4005) {
  acceptConnections(port, FALSE)
}

startSwank <- function(portFile) {
  acceptConnections(FALSE, portFile)
}

acceptConnections <- function(port, portFile) {
  s <- socketConnection(host="localhost", server=TRUE, port=port, open="r+b")
  on.exit(close(s))
  serve(s)
}

serve <- function(io) {
  mainLoop(io)
}

mainLoop <- function(io) {
  while(TRUE) {
    tryCatch(dispatch(io, readPacket(io)),
             swankTopLevel=function(c) NULL)
  }
}

dispatch <- function(io, event, sldbState=NULL) {
  str(event)
  kind <- event[[1]]
  if(kind == quote(`:emacs-rex`)) {
    do.call("emacsRex", c(list(io), list(sldbState), event[-1]))
  }
}

sendToEmacs <- function(io, obj) {
  str(obj)
  payload <- writeSexpToString(obj)
  writeChar(sprintf("%06x", nchar(payload)), io, eos=NULL)
  writeChar(payload, io, eos=NULL)
  flush(io)
  cat(sprintf("%06x", nchar(payload)), payload, sep="")
}

emacsRex <- function(io, sldbState, form, pkg, thread, id, level=0) {
  ok <- FALSE
  value <- NULL
  tryCatch({
    withCallingHandlers({
      value <- do.call(eval(form[[1]]), c(list(io), list(sldbState), form[-1]))
      ok <- TRUE
    }, error=function(c) {
      newSldbState <- makeSldbState(c, if(is.null(sldbState)) 0 else sldbState$level+1, id)
      sldbLoop(io, newSldbState, id) })},
    finally=sendToEmacs(io, list(quote(`:return`), if(ok) list(quote(`:ok`), value) else list(quote(`:abort`)), id)))
}

makeSldbState <- function(condition, level, id) {
  ret <- list(condition=condition, level=level, id=id)
  class(ret) <- c("sldbState", class(ret))
  ret
}

sldbLoop <- function(io, sldbState, id) {
  sendToEmacs(io, c(list(quote(`:debug`), id, sldbState$level), debuggerInfoForEmacs(sldbState)))
  sendToEmacs(io, list(quote(`:debug-activate`), id, sldbState$level, FALSE))
  while(TRUE) {
    dispatch(io, readPacket(io), sldbState)
  }
}

debuggerInfoForEmacs <- function(sldbState, from=0, to=NULL) {
  backtraceForEmacs <- function() {
    calls <- sys.calls()
    if(is.null(to)) to <- length(calls)
    from <- from+1
    calls <- lapply(calls[from:to], { frameNumber <- from-1;
                             function (x) { ret <- list(frameNumber, paste(format(x), sep="", collapse=" ")); frameNumber <<- 1+frameNumber; ret }})
  }
  computeRestartsForEmacs <- function () {
    lapply(computeRestarts(sldbState$condition),
           function(x) {
             ## this is all a little bit internalsy
             restartName <- x[[1]][[1]]
             description <- restartDescription(x)
             list(restartName, if(is.null(description)) restartName else description)
           })
  }
  list(list(as.character(sldbState$condition), sprintf("  [%s]", class(sldbState$condition)[[1]]), FALSE),
       computeRestartsForEmacs(),
       backtraceForEmacs(),
       list(sldbState$id))
}

readPacket <- function(io) {
  header <- readChunk(io, 6)
  len <- strtoi(header, base=16)
  payload <- readChunk(io, len)
  readSexpFromString(payload)
}

readChunk <- function(io, len) {
  buffer <- readChar(io, len)
  if(nchar(buffer) != len) {
    stop("short read in readChunk")
  }
  buffer
}

readSexpFromString <- function(string) {
  pos <- 1
  read <- function() {
    skipWhitespace()
    char <- substr(string, pos, pos)
    switch(char,
           "("=readList(),
           "\""=readString(),
           "'"=readQuote(),
           {
             if(pos > nchar(string))
               stop("EOF during read")
             obj <- readNumberOrSymbol()
             if(obj == quote(`.`)) {
               stop("Consing dot not implemented")
             }
             obj
           })
  }
  skipWhitespace <- function() {
    while(substr(string, pos, pos) %in% c(" ", "\t", "\n")) {
      pos <<- pos + 1
    }
  }
  readList <- function() {
    ret <- list()
    pos <<- pos + 1
    while(TRUE) {
      skipWhitespace()
      char <- substr(string, pos, pos)
      if(char == ")") {
        pos <<- pos + 1
        break
      } else {
        obj <- read()
        if(length(obj) == 1 && obj == quote(`.`)) {
          stop("Consing dot not implemented")
        }
        ret <- c(ret, list(obj))
      }
    }
    ret
  }
  readString <- function() {
    ret <- ""
    addChar <- function(c) { ret <<- paste(ret, c, sep="") }
    while(TRUE) {
      pos <<- pos + 1
      char <- substr(string, pos, pos)
      switch(char,
             "\""={ pos <<- pos + 1; break },
             "\\"={ pos <<- pos + 1
                    char2 <- substr(string, pos, pos)
                    switch(char2,
                           "\""=addChar(char2),
                           "\\"=addChar(char2),
                           stop("Unrecognized escape character")) },
             addChar(char))
    }
    ret
  }
  readNumberOrSymbol <- function() {
    token <- readToken()
    if(nchar(token)==0) {
      stop("End of file reading token")
    } else if(grepl("^[0-9]+$", token)) {
      strtoi(token)
    } else if(grepl("^[0-9]+\\.[0-9]+$", token)) {
      as.double(token)
    } else {
      as.name(token)
    }
  }
  readToken <- function() {
    token <- ""
    while(TRUE) {
      char <- substr(string, pos, pos)
      if(char == "") {
        break;
      } else if(char %in% c(" ", "\n", "\t", "(", ")", "\"", "'")) {
        break;
      } else {
        token <- paste(token, char, sep="")
        pos <<- pos + 1
      }
    }
    token
  }
  read()
}

writeSexpToString <- function(obj) {
  writeSexpToStringLoop <- function(obj) {
    switch(typeof(obj),
           "character"={ string <- paste(string, "\"", gsub("([\"\\])", "\\\\\\1", obj), "\"", sep="") },
           "list"={ string <- paste(string, "(", sep="")
                    max <- length(obj)
                    if(max > 0) {
                      for(i in 1:max) {
                        string <- paste(string, writeSexpToString(obj[[i]]), sep="")
                        if(i != max) {
                          string <- paste(string, " ", sep="")
                        }
                      }
                    }
                    string <- paste(string, ")", sep="") },
           "symbol"={ string <- paste(string, as.character(obj), sep="") },
           "logical"={ string <- if(obj) { paste(string, "t", sep="") } else { paste(string, "nil", sep="") }},
           "double"={ string <- paste(string, as.character(obj), sep="") },
           "integer"={ string <- paste(string, as.character(obj), sep="") },
           stop(paste("can't write object ", obj, sep="")))
    string
  }
  string <- ""
  writeSexpToStringLoop(obj)
}

`swank:connection-info` <- function (io, sldbState) {
  list(quote(`:pid`), Sys.getpid(),
       quote(`:package`), list(quote(`:name`), "R", quote(`:prompt`), "R> "),
       quote(`:lisp-implementation`), list(quote(`:type`), "R",
                                           quote(`:name`), "R",
                                           quote(`:version`), paste(R.version$major, R.version$minor, sep=".")))
}

`swank:swank-require` <- function (io, sldbState, contribs) {
  list()
}

`swank:create-repl` <- function(io, sldbState, env, ...) {
  list("R", "R")
}

`swank:listener-eval` <- function(io, sldbState, string) {
  val <- eval(parse(text=string), envir = globalenv())
  f <- fifo("")
  sink(f)
  print(val)
  sink()
  lines <- readLines(f)
  list(quote(`:values`), paste(lines, collapse="\n"))
}

`swank:autodoc` <- function(io, sldbState, rawForm, ...) {
  "No Arglist Information"
}

`swank:throw-to-toplevel` <- function(io, sldbState) {
  condition <- simpleCondition("Throw to toplevel")
  class(condition) <- c("swankTopLevel", class(condition))
  signalCondition(condition)
}

`swank:debugger-info-for-emacs` <- function(io, sldbState, from, to) {
  debuggerInfoForEmacs(sldbState, from=from, to=to)
}

`swank:invoke-nth-restart-for-emacs` <- function(io, sldbState, level, n) {
  if(sldbState$level == level) {
    invokeRestart(computeRestarts()[[n+1]])
  }
}
