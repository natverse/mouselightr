# hidden
mouselight_fetch <- function(path,
                    body = NULL,
                    parse.json = TRUE,
                    simplifyVector=FALSE,
                    include_headers = FALSE, ...){
  path = gsub("\\/$|^\\/","",path)
  req <-
    if (is.null(body)) {
      httr::GET(url = file.path(mouselight_url(path), path, fsep = "/"), ...)
    }else {
      httr::POST(url = file.path(mouselight_url(path), path, fsep = "/"),
                 body = body, ...)
    }
  httr::stop_for_status(req)
  if (parse.json) {
    parsed = mouselight_parse_json(req, simplifyVector = simplifyVector, raw = FALSE)
    mouselight_error_check(parsed)
    if (include_headers) {
      fields_to_include = c("url", "headers")
      attributes(parsed) = c(attributes(parsed), req[fields_to_include])
    }
    parsed
  }
  else req
}

# hidden
mouselight_parse_json <- function (req, simplifyVector = FALSE, raw = TRUE, ...) {
  if(raw){
    text <- rawToChar(req$content)
  }else{
    text <- httr::content(req, as = "text", encoding = "UTF-8")
  }
  if (identical(text, "")){
    warning("No output to parse", call. = FALSE)
    return(NULL)
  }
  p = tryCatch(jsonlite::fromJSON(text, simplifyVector = simplifyVector, ...), error = function(e) NULL)
  if(is.null(p)){
    warning("error parsing JSON")
  }
  nullToNA(p)
}

# hidden
mouselight_error_check <- function(x){
    err_fields = c("error", "message")
    if (sum(names(x) %in% err_fields)>1) {
      stop(x$error, ": ", x$message)
    }
    NULL
}
