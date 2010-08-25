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
    withRestarts(tryCatch(dispatch(io, readPacket(io)),
                          swankTopLevel=function(c) NULL),
                 abort="return to SLIME's toplevel")
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
      withRestarts(sldbLoop(io, newSldbState, id), abort=paste("return to sldb level", newSldbState$level)) })},
    finally=sendToEmacs(io, list(quote(`:return`), if(ok) list(quote(`:ok`), value) else list(quote(`:abort`)), id)))
}

makeSldbState <- function(condition, level, id) {
  calls <- rev(sys.calls())[-1]
  frames <- rev(sys.frames())[-1]
  restarts <- rev(computeRestarts(condition))[-1]
  ret <- list(condition=condition, level=level, id=id, restarts=restarts, calls=calls, frames=frames)
  class(ret) <- c("sldbState", class(ret))
  ret
}

sldbLoop <- function(io, sldbState, id) {
  tryCatch({
    sendToEmacs(io, c(list(quote(`:debug`), id, sldbState$level), `swank:debugger-info-for-emacs`(io, sldbState)))
    sendToEmacs(io, list(quote(`:debug-activate`), id, sldbState$level, FALSE))
    while(TRUE) {
      dispatch(io, readPacket(io), sldbState)
    }
  }, finally=sendToEmacs(io, c(list(quote(`:debug-return`), id, sldbState$level, FALSE))))
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

printToString <- function(val) {
  f <- fifo("")
  tryCatch({ sink(f); print(val); sink(); readLines(f) },
           finally=close(f))
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
  string <- printToString(val)
  list(quote(`:values`), paste(string, collapse="\n"))
}

`swank:autodoc` <- function(io, sldbState, rawForm, ...) {
  "No Arglist Information"
}

`swank:operator-arglist` <- function(io, sldbState, op, package) {
  list()
}

`swank:throw-to-toplevel` <- function(io, sldbState) {
  condition <- simpleCondition("Throw to toplevel")
  class(condition) <- c("swankTopLevel", class(condition))
  signalCondition(condition)
}

`swank:backtrace` <- function(io, sldbState, from=0, to=NULL) {
  calls <- sldbState$calls
  if(is.null(to)) to <- length(calls)
  from <- from+1
  calls <- lapply(calls[from:to],
                  { frameNumber <- from-1;
                    function (x) {
                      ret <- list(frameNumber, paste(format(x), sep="", collapse=" "))
                      frameNumber <<- 1+frameNumber
                      ret
                    }
                  })
}

computeRestartsForEmacs <- function (sldbState) {
  lapply(sldbState$restarts,
         function(x) {
           ## this is all a little bit internalsy
           restartName <- x[[1]][[1]]
           description <- restartDescription(x)
           list(restartName, if(is.null(description)) restartName else description)
         })
}

`swank:debugger-info-for-emacs` <- function(io, sldbState, from=0, to=NULL) {
  list(list(as.character(sldbState$condition), sprintf("  [%s]", class(sldbState$condition)[[1]]), FALSE),
       computeRestartsForEmacs(sldbState),
       `swank:backtrace`(io, sldbState, from, to),
       list(sldbState$id))
}

`swank:invoke-nth-restart-for-emacs` <- function(io, sldbState, level, n) {
  if(sldbState$level == level) {
    invokeRestart(sldbState$restarts[[n+1]])
  }
}

`swank:buffer-first-change` <- function(io, sldbState, filename) {
  FALSE
}

`swank:frame-locals-and-catch-tags` <- function(io, sldbState, index) {
  str(sldbState$frames)
  frame <- sldbState$frames[[1+index]]
  objs <- ls(envir=frame)
  list(lapply(objs, function(name) { list(quote(`:name`), name,
                                          quote(`:id`), 0,
                                          quote(`:value`), paste(printToString(eval(parse(text=name), envir=frame)), sep="", collapse="\n")) }),
       list())
}

`swank:simple-completions` <- function(io, sldbState, prefix, package) {
  ## fails multiply if prefix contains regexp metacharacters
  matches <- apropos(sprintf("^%s", prefix), ignore.case=FALSE)
  nmatches <- length(matches)
  if(nmatches == 0) {
    list(list(), "")
  } else {
    longest <- matches[order(nchar(matches))][1]
    while(length(grep(sprintf("^%s", longest), matches)) < nmatches) {
      longest <- substr(longest, 1, nchar(longest)-1)
    }
    list(as.list(matches), longest)
  }
}

`swank:compile-string-for-emacs` <- function(io, sldbState, string, buffer, position, filename, policy) {
  # FIXME: I think in parse() here we can use srcref to associate
  # buffer/filename/position to the objects.  Or something.
  withRestarts({ times <- system.time(eval(parse(text=string), envir = globalenv())) },
               abort="abort compilation")
  list(quote(`:compilation-result`), list(), TRUE, times[3])
}

`swank:interactive-eval` <-  function(io, sldbState, string) {
  retry <- TRUE
  value <- ""
  while(retry) {
    retry <- FALSE
    withRestarts(value <- eval(parse(text=string), envir = globalenv()),
                 retry=list(description="retry SLIME interactive evaluation request", handler=function() retry <<- TRUE))
  }
  printToString(value)
}

`swank:eval-and-grab-output` <- function(io, sldbState, string) {
  retry <- TRUE
  value <- ""
  output <- NULL
  f <- fifo("")
  tryCatch({
    sink(f)
    while(retry) {
      retry <- FALSE
      withRestarts(value <- eval(parse(text=string), envir = globalenv()),
                   retry=list(description="retry SLIME interactive evaluation request", handler=function() retry <<- TRUE))}},
           finally={sink(); output <- readLines(f); close(f)})
  list(output, printToString(value))
}
