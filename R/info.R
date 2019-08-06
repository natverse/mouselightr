#' @title Get information about the mouse brain compartments in the MouseLight project
#'
#' @description Query MouseLight's GraphQL API to retreive information about all of the brain volumes in the MouseLight project. These
#' brain regions can be sub-volumes of other brain regions. An \code{igraph} object can be generated to explore these dependencies.
#' @param ... methods passed to \code{mouselight_fetch}
#' @return Using \code{mouselight_brain_graph} will return an object of class \code{igraph} for use with the \code{igraph} R package.
#' Using \code{mouselight_brain_region_info}, a \code{data.frame} with the following values is returned:
#'\itemize{
#'  \item 	\emph{acronym}	 short name for a brain region
#'  \item 	\emph{safeName}	 long, easily computer-readble name for a brain region
#'  \item 	\emph{structureId}	 unique ID for brain region
#'  \item 	\emph{parentStructureId}	 ID for a larger brain region, to which this brain region belongs
#'  \item 	\emph{structureIdPath}	 a path of structure IDs to the 'root', the whole brain
#'}
#' @seealso \code{\link{mouselight_read_brain}}, \code{\link{mouselight_read_neurons}}
#' @examples
#' \dontrun{
#' ## First we need to download all of the neurons
#' mbr = mouselight_brain_region_info()
#'
#' ## Leet's have a look at this data
#' View(mouselight_brain_region_info)
#'
#' ## Some brain regions are actually a subcompartment of others in this set.
#' ### Hmm, so what would be good is if we could see those correspondences
#' g = mouselight_brain_graph()
#'
#' ## Let's plot these i na way we can easily see!
#' library(igraph)
#' f = igraph::layout_with_fr(g)
#' plot(g, layout = f)
#'
#' ## There are a lot of nodes here, we might want to move them about
#' tkplot(g, layout = f)
#'
#' }
#' @inherit mouselight_read_brain references
#' @export
#' @rdname mouselight_brain_info
mouselight_brain_region_info <- function(...){
  body = list(
    query = "{\n
    brainAreas {\n
    acronym\n
    safeName\n
    structureId\n
    parentStructureId\n
    structureIdPath\n
    }\n
}",
    variables = list(),
    operationName = NULL
  )
  bodyj=jsonlite::toJSON(body, null = 'null', auto_unbox = T)
  res = mouselight_fetch(path = "graphql",
                 body = bodyj,
                 parse.json = TRUE,
                 simplifyVector=FALSE,
                 include_headers = FALSE,
                 config = httr::content_type_json(),
                 encode='raw',
                 ...)
  df = data.frame(do.call(rbind, lapply(res$data$brainAreas, function(x) unlist(nullToNA(x)))))
  rownames(df) = df$structureId
  df$parentStructureId[is.na(df$parentStructureId)] = -1
  df
}

#' @export
#' @rdname mouselight_brain_info
mouselight_brain_graph <- function(...){
  bi = mouselight_brain_region_info(...)
  bi = bi[!apply(bi, 1, function(x) sum(is.na(x))>0),]
  vertices <- data.frame(name = bi$structureId,
                         label= bi$acronym)
  relations <- data.frame(from = bi$parentStructureId,
                          to= bi$structureId)
  relations = relations[relations$from != -1 & relations$to != -1,]
  igraph::graph_from_data_frame(relations, directed=TRUE, vertices=vertices)
}

#' @title Get information about the MouseLight project's GraphQL API
#'
#' @description See the API version and the number of neurons hosted by the project.
#' @param ... methods passed to \code{mouselight_fetch}
#' @return a named vector of values
#' @seealso \code{\link{mouselight_read_brain}}, \code{\link{mouselight_read_neurons}}
#' @examples
#' \dontrun{
#' ## First we need to download all of the neurons
#' mouselight_api(...)
#'
#' }
#' @inherit mouselight_read_brain references
#' @export
#' @rdname mouselight_info
mouselight_api <- function(...){
  body = list(
    query = "{\n
    systemSettings {\n
    apiVersion\n
    apiRelease\n
    neuronCount\n
    }\n
    }",
    variables = list(),
    operationName = NULL
  )
  bodyj=jsonlite::toJSON(body, null = 'null', auto_unbox = T)
  res = mouselight_fetch(path = "graphql",
                 body = bodyj,
                 parse.json = TRUE,
                 simplifyVector=FALSE,
                 include_headers = FALSE,
                 config = httr::content_type_json(),
                 encode='raw')
  unlist(res$data$systemSettings)
}

# hidden
structureIdentifiers <- function(){
  body = list(
    query = "{\n
    structureIdentifiers {\n
    id\n
    name\n
    value\n
    }\n
  }",
    variables = list(),
    operationName = NULL
  )
  bodyj=jsonlite::toJSON(body, null = 'null', auto_unbox = T)
  res = mouselight_fetch(path = "graphql",
                 body = bodyj,
                 parse.json = TRUE,
                 simplifyVector=FALSE,
                 include_headers = FALSE,
                 config = httr::content_type_json(),
                 encode='raw')
  df = do.call(rbind,res$data$structureIdentifiers)
  df = as.data.frame(t(apply(df, 1, unlist)))
  df$value = as.integer(df$value)
  df$`__typename` = "StructureIdentifier"
  rownames(df) = df$structureId
  df
}
