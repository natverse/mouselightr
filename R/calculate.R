#' Find out how many nodes a neuron has in each brain region
#'
#' @description After reading tracgings from the MouseLight project (\code{\link{mouselight_read_brain}}),
#' find out in which neuropils it has its nodes
#'
#' @param x a \code{neuronlist} of tracings that have been read into R using \code{\link{mouselight_read_neurons}}, with \code{method = "native"}.
#' @param brain.areas a vector of brain region acronyms. See \code{\link{mouselight_brain_area_info}}.
#' @param labels a vector of label IDs, denoting neuron compartment e.g. 2 for axon, 3 for dendrite, 0 for intervening path, etc. See \code{\link{mouselight_read_neurons}}.
#' @param ... methods sent to \code{mouselight_fetch_swc}
#'
#' @return A data.frame
#' @seealso \code{\link{mouselight_read_brain}}, \code{\link{mouselight_brain_area_info}}, \code{\link{mouselight_neuron_info}}, \code{\link{mouselight_read_neurons}}
#'
#' @examples
#' \donttest{
#' ids=c("1ccbc478-88fb-4181-a4de-d0c22ed9738b",
#'   "3afa5e41-374d-45bc-a546-03f362d93649")
#' nn=mouselight_read_neurons(ids, method = "native")
#' nibr = mouselight_nodes_in_region(nn, labels = c(0, 2, 3))
#' print(nibr)
#'
#' # Counts in a specific brain area
#' cing.nodes = mouselight_nodes_in_region(nn, labels = c(0, 2, 3), brain.areas = "cing")
#' print(cing.nodes)
#'
#' }
#' @export
#' @rdname mouselight_nodes_in_region
mouselight_nodes_in_region <-function(x, brain.areas = NULL, labels = NULL) UseMethod("mouselight_nodes_in_region")

#' @export
#' @rdname mouselight_nodes_in_region
mouselight_nodes_in_region.neuron <- function(x, brain.areas = NULL, labels = NULL){
  if(!is.null(labels)){
    points = x$d[x$d$Label %in% labels,]
  }else{
    points = x$d
  }
  if(nrow(points)==0){
    return(0)
  }
  if(!is.null(brain.areas)){
    sum(points$brain_area%in%brain.areas)
  }else{
    counts = c(table(points$brain_area))
    as.data.frame((t(data.frame(counts))))
  }
}

#' @export
#' @rdname mouselight_nodes_in_region
mouselight_nodes_in_region.neuronlist <- function(x, brain.areas = NULL, labels = NULL){
  if(!is.null(brain.areas)){
    sapply(x, FUN = mouselight_nodes_in_region.neuron, brain.areas = brain.areas, labels = labels)
  }else{
    l = lapply(x, FUN = mouselight_nodes_in_region.neuron, brain.areas = brain.areas, labels = labels)
    l = nullToNA(l)
    l = do.call(plyr::rbind.fill, l)
    l[is.na(l)] = 0
    rownames(l) = names(x)
    l
  }
}
