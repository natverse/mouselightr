#' Read one or more MouseLight tracings into a neuronlist
#'
#' @description read tracings for neurons from the MouseLight Neuron Browser. Each neuron may have multiple tracgings, typically one for its
#' axon and another for its dendrite. To read neuron, a vector of tracing IDs needs to be supplied. To find there, use \code{\link{mouselight_neuron_info}}.
#' @details Note that mouselight neurons have numerous associated
#'   identifiers. Some appear to occur only once per neuron:
#'   \itemize{
#'
#'   \item UUID (called neuron.id) e.g. "780dfeef-e644-4e85-9aad-b31c9957b440"
#'
#'   \item idString e.g. AA0302
#'
#'   }
#'
#'   Others may reflect multiple entitities:
#'
#'   \itemize{
#'
#'   \item tracing.id e.g. "b0cfcfe1-9870-4e61-ad3e-cf09b114fc3a"
#'
#'   \item soma.id e.g. 8abce9c1-479b-43a3-b5f2-080de966f068 }
#'
#'   There are nearly always two tracing ids per neuron (axon, dendrite).
#'   And seemingly multiple soma ids.
#'
#'   Neurons read with method = "native" will have, at neuron$d, the brain region
#'   within which each node falls and a value, "Label", denoted whether the tracings is
#'   an axon (2) or dendrite (3).
#'
#' @param tracing.ids ids of tracings.ids for MouseLight neurons. Note, this is different from neuron.ids and other
#' ids used by the project. See available neurons and their tracing.ids using \code{\link{mouselight_neuron_info}}.
#' @param method whether to read from MouseLight's own raw tracing format or stored SWC files
#' @param meta whether or not to read meta data for tracings using \code{mouselight_neuron_metadata}
#' @param ... methods sent to \code{mouselight_fetch_swc}
#'
#' @return A \code{nat::\link{neuronlist}}
#' @seealso \code{\link{mouselight_read_brain}}, \code{\link{mouselight_brain_region_info}}, \code{\link{mouselight_neuron_info}}
#'
#' @examples
#' \donttest{
#' ids=c("1ccbc478-88fb-4181-a4de-d0c22ed9738b",
#'   "3afa5e41-374d-45bc-a546-03f362d93649")
#' nn=mouselight_read_neurons(ids)
#' plot3d(nn)
#' }
#' @export
#' @rdname mouselight_read_neurons
mouselight_read_neurons <- function(tracing.ids, method=c("native","swc"), meta = TRUE, ...) {
  method<-match.arg(method)
  if(method=='swc')
    return(mouselight_fetch_swc(tracing.ids, ...))
  pb <- progress::progress_bar$new(total = length(tracing.ids),
    format = "  downloading [:bar] :percent eta: :eta")
  rr <- suppressWarnings(sapply(tracing.ids, function(y) {pb$tick();fetch_raw_tracings(y)}, simplify = F))
  ul <- unlist(lapply(rr, function(x) x$tracings), recursive = F)
  nl <- nat::nlapply(ul, process_tracing_list, OmitFailures = T)
  if(meta){
    nl[,] <- mouselight_neuron_metadata(tracing.ids = names(nl))
  }else{
    nl[,"tracing.id"] <- names(nl)
  }
  nl
}

# hidden
mouselight_fetch_swc <- function(x, timeout=length(x)*5, progress=TRUE, chunksize=5L) {
  n<-length(x)
  if(n>chunksize) {
    pb <- progress::progress_bar$new(total = n,
                                    format = "  :current/:total [:bar]  eta: :eta",
                                    show_after = 2)
    nchunks<-ceiling(n/chunksize)
    chunks<-rep(seq_len(nchunks), rep(chunksize, nchunks))[seq_len(n)]
    res <- by(x,
             chunks,
             function(chk, ...) {pb$tick(length(chk));mouselight_fetch_swc(chk, ...)},
             progress = FALSE,
             simplify = FALSE)
    nl<-do.call(c, res)
    return(nl)
  }
  bodyj <- jsonlite::toJSON(list(ids = x))
  f<-tempfile(fileext = '.zip')
  on.exit(unlink(f))
  res <- mouselight_fetch(path = 'swc',
                         body = bodyj,
                         parse.json = FALSE,
                         simplifyVector=FALSE,
                         include_headers = FALSE,
                         config = httr::content_type_json(),
                         encode='raw',
                         httr::timeout(timeout))
  res2<-httr::content(res, type = 'application/json')
  writeBin(jsonlite::base64_dec(res[[1]]), con = f)
  nat::read.neurons(f)
}

# This gets the raw tracing info for one or more ids
fetch_raw_tracings <- function(ids, baseurl='http://ml-neuronbrowser.janelia.org/tracings') {
  bodyj<-jsonlite::toJSON(list(ids=ids), auto_unbox = F) # nb auto_unbox must be F for length one queries
  res <- mouselight_fetch(path = 'tracings',
                         body = bodyj,
                         parse.json = TRUE,
                         simplifyVector=FALSE,
                         include_headers = FALSE,
                         config = httr::content_type_json(),
                         encode='raw')
  res
}

# this processes a list containing raw tracing data and converts to a neuron
process_tracing_list <- function(x) {
  if (!isTRUE(all.equal(names(x), c("id", "nodes"))))
    stop("Expect a 2 element list (id, nodes)")
  bb<-try(dplyr::bind_rows(x$nodes))
  if(inherits(bb, 'try-error')) {
    bb<-dplyr::bind_rows(lapply(x$nodes, jsonlite:::null_to_na))
  }
  rawdf2neuron(bb, id=x$id)
}

# hidden
delayedAssign('.structureIdentifiers', structureIdentifiers())
delayedAssign('.brainAreaIds', brainAreaIds())

# convert the pseudo SWC format returned by mouselight into what we need
rawdf2neuron <- function(x, ...) {
  selcols<-c("x", "y", "z", "radius", "parentNumber", "sampleNumber", "brainAreaId","structureIdentifierId")
  newcols<-c("X","Y","Z","W", "Parent", "PointNo", "brain_area","Label")
  ndf<-x[selcols]
  names(ndf)<-newcols
  ndf[['W']]=ndf[['W']]*2
  ndf[['brain_area']]<-.brainAreaIds$acronym[match(ndf[['brain_area']], .brainAreaIds$id)]
  ndf[['Label']]<-.structureIdentifiers$value[match(ndf[['Label']], .structureIdentifiers$id)]
  nat::as.neuron(ndf, ...)
}

#' @export
#' @rdname mouselight_read_neurons
mouselight_neuron_metadata <- function(tracing.ids){
  ml <-  mouselight_neuron_info(simplify=TRUE)
  selcols<- c("neuron.id", "idString", "tracing.id",
            "tracingStructure.name", "soma.x", "soma.y", "soma.z",
            "soma.radius", "soma.brainAreaId")
  newcols<- c("neuron.id","idString","tracing.id","compartment", "soma.x", "soma.y", "soma.z","soma.radius", "soma.bain_area")
  ml<- ml[selcols]
  names(ml)<- newcols
  ml <-  ml[match(tracing.ids, ml$tracing.id),]
  ml[['soma.bain_area']]=.brainAreaIds$acronym[match(ml[['soma.bain_area']], .brainAreaIds$id)]
  rownames(ml) <- ml$tracing.id
  ml
}

