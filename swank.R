### This program is free software; you can redistribute it and/or
### modify it under the terms of the GNU General Public Licence as
### published by the Free Software Foundation; either version 2 of the
### Licence, or (at your option) any later version.
###
### This program is distributed in the hope that it will be useful,
### but WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### GNU General Public Licence for more details.
###
### A copy of version 2 of the GNU General Public Licence is available
### at <http://www.gnu.org/licenses/old-licenses/gpl-2.0.txt>; the
### latest version of the GNU General Public Licence is available at
### <http://www.gnu.org/licenses/gpl.txt>.

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
  slimeConnection <- new.env()
  slimeConnection$io <- io
  while(TRUE) {
    withRestarts(tryCatch(dispatch(slimeConnection, readPacket(io)),
                          swankTopLevel=function(c) NULL),
                 abort="return to SLIME's toplevel")
  }
}

dispatch <- function(slimeConnection, event, sldbState=NULL) {
  str(event)
  kind <- event[[1]]
  if(kind == quote(`:emacs-rex`)) {
    do.call("emacsRex", c(list(slimeConnection), list(sldbState), event[-1]))
  }
}

sendToEmacs <- function(slimeConnection, obj) {
  io <- slimeConnection$io
  str(obj)
  payload <- writeSexpToString(obj)
  writeChar(sprintf("%06x", nchar(payload)), io, eos=NULL)
  writeChar(payload, io, eos=NULL)
  flush(io)
  cat(sprintf("%06x", nchar(payload)), payload, sep="")
}

callify <- function(form) {
  ## we implement here the conversion from Lisp S-expression (or list)
  ## expressions of code into our own, swankr, calling convention,
  ## with slimeConnection and sldbState as first and second arguments.
  ## as.call() gets us part of the way, but we need to walk the list
  ## recursively to mimic CL:EVAL; we need to avoid converting R
  ## special operators which we are punning (only `quote`, for now)
  ## into this calling convention.
  if(is.list(form)) {
    if(form[[1]] == quote(quote)) {
      as.call(form)
    } else {
      as.call(c(list(form[[1]], quote(slimeConnection), quote(sldbState)), lapply(form[-1], callify)))
    }
  } else {
    form
  }
}

emacsRex <- function(slimeConnection, sldbState, form, pkg, thread, id, level=0) {
  ok <- FALSE
  value <- NULL
  tryCatch({
    withCallingHandlers({
      call <- callify(form)
      value <- eval(call)
      ok <- TRUE
    }, error=function(c) {
      newSldbState <- makeSldbState(c, if(is.null(sldbState)) 0 else sldbState$level+1, id)
      withRestarts(sldbLoop(slimeConnection, newSldbState, id), abort=paste("return to sldb level", newSldbState$level)) })},
    finally=sendToEmacs(slimeConnection, list(quote(`:return`), if(ok) list(quote(`:ok`), value) else list(quote(`:abort`)), id)))
}

makeSldbState <- function(condition, level, id) {
  calls <- rev(sys.calls())[-1]
  frames <- rev(sys.frames())[-1]
  restarts <- rev(computeRestarts(condition))[-1]
  ret <- list(condition=condition, level=level, id=id, restarts=restarts, calls=calls, frames=frames)
  class(ret) <- c("sldbState", class(ret))
  ret
}

sldbLoop <- function(slimeConnection, sldbState, id) {
  tryCatch({
    io <- slimeConnection$io
    sendToEmacs(slimeConnection, c(list(quote(`:debug`), id, sldbState$level), `swank:debugger-info-for-emacs`(slimeConnection, sldbState)))
    sendToEmacs(slimeConnection, list(quote(`:debug-activate`), id, sldbState$level, FALSE))
    while(TRUE) {
      dispatch(slimeConnection, readPacket(io), sldbState)
    }
  }, finally=sendToEmacs(slimeConnection, c(list(quote(`:debug-return`), id, sldbState$level, FALSE))))
}

readPacket <- function(io) {
  socketSelect(list(io))
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

withOutputToString <- function(expr) {
  call <- substitute(expr)
  f <- fifo("")
  sink(f)
  tryCatch({ tryCatch(eval.parent(call), finally=sink())
             readLines(f) },
           finally=close(f))
}

printToString <- function(val) {
  withOutputToString(str(val, indent.str="", list.len=5, max.level=2))
}

`swank:connection-info` <- function (slimeConnection, sldbState) {
  list(quote(`:pid`), Sys.getpid(),
       quote(`:package`), list(quote(`:name`), "R", quote(`:prompt`), "R> "),
       quote(`:lisp-implementation`), list(quote(`:type`), "R",
                                           quote(`:name`), "R",
                                           quote(`:version`), paste(R.version$major, R.version$minor, sep=".")))
}

`swank:swank-require` <- function (slimeConnection, sldbState, contribs) {
  for(contrib in contribs) {
    filename <- sprintf("%s.R", as.character(contrib))
    if(file.exists(filename)) {
      source(filename, verbose=TRUE)
    }
  }
  list()
}

`swank:create-repl` <- function(slimeConnection, sldbState, env, ...) {
  list("R", "R")
}

sendReplResult <- function(slimeConnection, value) {
  string <- printToString(value)
  sendToEmacs(slimeConnection,
              list(quote(`:write-string`),
                   paste(string, collapse="\n"),
                   quote(`:repl-result`)))
}

sendReplResultFunction <- sendReplResult

`swank:listener-eval` <- function(slimeConnection, sldbState, string) {
  string <- gsub("#\\.\\(swank:lookup-presented-object-or-lose([^)]*)\\)", ".(`swank:lookup-presented-object-or-lose`(slimeConnection, sldbState,\\1))", string)
  expr <- parse(text=string)[[1]]
  lookedup <- do.call("bquote", list(expr))
  value <- eval(lookedup, envir = globalenv())
  sendReplResultFunction(slimeConnection, value)
  list()
}

`swank:autodoc` <- function(slimeConnection, sldbState, rawForm, ...) {
  "No Arglist Information"
}

`swank:operator-arglist` <- function(slimeConnection, sldbState, op, package) {
  list()
}

`swank:throw-to-toplevel` <- function(slimeConnection, sldbState) {
  condition <- simpleCondition("Throw to toplevel")
  class(condition) <- c("swankTopLevel", class(condition))
  signalCondition(condition)
}

`swank:backtrace` <- function(slimeConnection, sldbState, from=0, to=NULL) {
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

`swank:debugger-info-for-emacs` <- function(slimeConnection, sldbState, from=0, to=NULL) {
  list(list(as.character(sldbState$condition), sprintf("  [%s]", class(sldbState$condition)[[1]]), FALSE),
       computeRestartsForEmacs(sldbState),
       `swank:backtrace`(slimeConnection, sldbState, from, to),
       list(sldbState$id))
}

`swank:invoke-nth-restart-for-emacs` <- function(slimeConnection, sldbState, level, n) {
  if(sldbState$level == level) {
    invokeRestart(sldbState$restarts[[n+1]])
  }
}

`swank:frame-source-location` <- function(slimeConnection, sldbState, n) {
  call <- sldbState$calls[[n+1]]
  srcref <- attr(call, "srcref")
  srcfile <- attr(srcref, "srcfile")
  if(is.null(srcfile)) {
    list(quote(`:error`), "no srcfile")
  } else {
    list(quote(`:location`),
         list(quote(`:file`), sprintf("%s/%s", srcfile$wd, srcfile$filename)),
         list(quote(`:line`), srcref[[1]], srcref[[2]]-1),
         FALSE)
  }
}

`swank:buffer-first-change` <- function(slimeConnection, sldbState, filename) {
  FALSE
}

`swank:frame-locals-and-catch-tags` <- function(slimeConnection, sldbState, index) {
  str(sldbState$frames)
  frame <- sldbState$frames[[1+index]]
  objs <- ls(envir=frame)
  list(lapply(objs, function(name) { list(quote(`:name`), name,
                                          quote(`:id`), 0,
                                          quote(`:value`), paste(printToString(eval(parse(text=name), envir=frame)), sep="", collapse="\n")) }),
       list())
}

`swank:simple-completions` <- function(slimeConnection, sldbState, prefix, package) {
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

`swank:compile-string-for-emacs` <- function(slimeConnection, sldbState, string, buffer, position, filename, policy) {
  # FIXME: I think in parse() here we can use srcref to associate
  # buffer/filename/position to the objects.  Or something.
  withRestarts({ times <- system.time(eval(parse(text=string), envir = globalenv())) },
               abort="abort compilation")
  list(quote(`:compilation-result`), list(), TRUE, times[3])
}

withRetryRestart <- function(description, expr) {
  call <- substitute(expr)
  retry <- TRUE
  while(retry) {
    retry <- FALSE
    withRestarts(eval.parent(call),
                 retry=list(description=description,
                   handler=function() retry <<- TRUE))
  }
}

`swank:interactive-eval` <-  function(slimeConnection, sldbState, string) {
  withRetryRestart("retry SLIME interactive evaluation request",
                   value <- eval(parse(text=string), envir=globalenv()))
  printToString(value)
}

`swank:eval-and-grab-output` <- function(slimeConnection, sldbState, string) {
  withRetryRestart("retry SLIME interactive evaluation request",
                   { output <-
                       withOutputToString(value <- eval(parse(text=string),
                                                        envir=globalenv())) })
  list(output, printToString(value))
}

`swank:find-definitions-for-emacs` <- function(slimeConnection, sldbState, string) {
  if(exists(string, envir = globalenv())) {
    thing <- get(string, envir = globalenv())
    if(inherits(thing, "function")) {
      body <- body(thing)
      srcref <- attr(body, "srcref")
      srcfile <- attr(body, "srcfile")
      if(is.null(srcfile)) {
        list()
      } else {
        filename <- get("filename", srcfile)
        list(list(sprintf("function %s", string),
                  list(quote(`:location`),
                       list(quote(`:file`), sprintf("%s/%s", srcfile$wd, srcfile$filename)),
                       list(quote(`:line`), srcref[[2]][[1]], srcref[[2]][[2]]-1),
                       list())))
      }
    } else {
      list()
    }
  } else {
    list()
  }
}

`swank:value-for-editing` <- function(slimeConnection, sldbState, string) {
  paste(deparse(eval(parse(text=string), envir = globalenv()), control="all"),
        collapse="\n", sep="")
}

`swank:commit-edited-value` <- function(slimeConnection, sldbState, string, value) {
  eval(parse(text=sprintf("%s <- %s", string, value)), envir = globalenv())
  TRUE
}
