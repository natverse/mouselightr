#' Read one or more mouselight tracings into a neuronlist
#'
#' @param x ids of mouselight neurons
#'
#' @return A \code{nat::\link{neuronlist}}
#' @export
#'
#' @examples
#' \donttest{
#' ids=c("1ccbc478-88fb-4181-a4de-d0c22ed9738b",
#'   "3afa5e41-374d-45bc-a546-03f362d93649")
#' nn=read.neurons.mouselight(ids)
#' library(nat)
#' plot3d(nn)
#' }
read.neurons.mouselight <- function(x) {
  pb <- progress::progress_bar$new(total = length(x),
    format = "  downloading [:bar] :percent eta: :eta")

  rr=sapply(x, function(y) {pb$tick();fetch_raw_tracings(y)}, simplify = F)
  ul=unlist(lapply(rr, function(x) x$tracings), recursive = F)
  nl=nat::nlapply(ul, process_tracing_list, OmitFailures = T)
  nl
}

# This gets the raw tracing info for one or more ids
fetch_raw_tracings <- function(ids, baseurl='http://ml-neuronbrowser.janelia.org/tracings') {
  res=httr::POST(baseurl, body = list(ids=ids), encode='json')
  # nb auto_unbox must be F for length one queries
  body=jsonlite::toJSON(list(ids=ids), auto_unbox = F)
  res = httr::POST(
    url = ml_url('tracings'),
    body = body,
    encode = 'raw',
    httr::content_type_json())
  httr::stop_for_status(res)
  resj = httr::content(res, as="text", type='application/json', encoding="UTF-8")
  jsonlite::fromJSON(resj, simplifyVector=FALSE)
}

# this processes a list containing raw tracing data and converts to a neuron
process_tracing_list <- function(x) {
  if (!isTRUE(all.equal(names(x), c("id", "nodes"))))
    stop("Expect a 2 element list (id, nodes)")
  bb=try(dplyr::bind_rows(x$nodes))
  if(inherits(bb, 'try-error')) {
    bb=dplyr::bind_rows(lapply(x$nodes, jsonlite:::null_to_na))
  }
  rawdf2neuron(bb, id=x$id)
}

structureIdentifiers <-
  data.frame(
    stringsAsFactors = FALSE,
    id = c(
      "9b2cf056-1fba-468f-a877-04169dd9f708",
      "2a8efa78-1067-4ce8-8e4f-cfcf9cf7d315",
      "a1df739e-f4a8-4b88-9a25-2cd6b9a7563c",
      "a3dec6a1-7484-45a7-bc05-cf3d6014c44d",
      "c37953e1-a1e9-4b9a-847e-08d9566ced65",
      "d8eb210f-65fe-4983-bdcb-e34de5ca2e13",
      "6afcafa5-ec7f-4899-8941-3e1f812682ce"
    ),
    name = c(
      "path",
      "branch point",
      "axon",
      "apical dendrite",
      "end point",
      "(basal) dendrite",
      "soma"
    ),
    value = c(0L, 5L, 2L, 4L, 6L, 3L, 1L),
    `__typename` = c(
      "StructureIdentifier",
      "StructureIdentifier",
      "StructureIdentifier",
      "StructureIdentifier",
      "StructureIdentifier",
      "StructureIdentifier",
      "StructureIdentifier"
    )
  )

# convert the pseudo SWC format returned by mouselight into what we need
rawdf2neuron <- function(x, ...) {
  selcols=c("x", "y", "z", "radius", "parentNumber", "sampleNumber", "structureIdentifierId")
  newcols=c("X","Y","Z","W", "Parent", "PointNo","Label")
  ndf=x[selcols]
  names(ndf)=newcols
  ndf[['W']]=ndf[['W']]*2
  ndf[['Label']]=structureIdentifiers$value[match(ndf[['Label']], structureIdentifiers$id)]
  nat::as.neuron(ndf, ...)
}

