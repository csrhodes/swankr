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
  dispatch <- function(event) {
    str(event)
    kind <- event[[1]]
    if(kind == quote(`:emacs-rex`)) {
      do.call("emacsRex", event[-1])
    }
  }
  sendToEmacs <- function(obj) {
    payload <- writeSexpToString(obj)
    writeChar(sprintf("%06x", nchar(payload)), io, eos=NULL)
    writeChar(payload, io, eos=NULL)
    flush(io)
    cat(sprintf("%06x", nchar(payload)), payload, sep="")
  }
  emacsRex <- function(form, pkg, thread, id) {
    value <- do.call(eval(form[[1]]), form[-1])
    sendToEmacs(list(quote(`:return`), list(quote(`:ok`), value), id))
  }
  
  while(TRUE) {
    tryCatch(dispatch(readPacket(io)),
             swankTopLevel=NULL)
  }
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
           "logical"={ if(obj) { paste(string, "t", sep="") } else { paste(string, "nil", sep="") }},
           "double"={ string <- paste(string, as.character(obj), sep="") },
           "integer"={ string <- paste(string, as.character(obj), sep="") },
           stop(paste("can't write object ", obj, sep="")))
    string
  }
  string <- ""
  writeSexpToStringLoop(obj)
}

`swank:connection-info` <- function () {
  list(quote(`:pid`), Sys.getpid(),
       quote(`:package`), list(quote(`:name`), "R", quote(`:prompt`), "R> "),
       quote(`:lisp-implementation`), list(quote(`:type`), "R",
                                           quote(`:name`), "R",
                                           quote(`:version`), paste(R.version$major, R.version$minor, sep=".")))
}

`swank:swank-require` <- function (contribs) {
  list()
}

`swank:create-repl` <- function(env, ...) {
  list("R", "R")
}

`swank:listener-eval` <- function(string) {
  val <- eval(parse(text=string))
  f <- fifo("")
  sink(f)
  print(val)
  sink()
  lines <- readLines(f)
  list(quote(`:values`), paste(lines, collapse="\n"))
}

`swank:autodoc` <- function(rawForm, ...) {
  "No Arglist Information"
}
