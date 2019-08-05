# hidden
nullToNA <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}
changenull <- function(x, to = ""){
  ifelse(is.null(x),to,x)
}

# hidden
mouselight_progress <- function (x, max = 100, message = "querying ms") {
  percent <- x / max * 100
  cat(sprintf('\r|%-50s| ~%d%% %s',
              paste(rep('+', percent / 2), collapse = ''),
              floor(percent), message))
  if (x == max)
    cat('\n')
}

# hidden
FirstLower <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(tolower(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# hidden
mouselight_url <- function(...) {
  baseurl=getOption("nat.mouselight.url", "http://ml-neuronbrowser.janelia.org")
  do.call(file.path, c(baseurl, list(...), fsep = "/"))
}
