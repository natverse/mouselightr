ml_url <- function(...) {
  baseurl=getOption("nat.mouselight.url", "http://ml-neuronbrowser.janelia.org")
  do.call(file.path, c(baseurl, list(...), fsep = "/"))
}
