#' Query available neurons from mouselight site
#'
#' @param simplify Whether to simplify the query result to a simple data.frame
#'   with one row per tracing (See details).
#' @return data.frame describing the available neurons. See details for the
#'   implication of the \code{simplify} argument.
#' @details Mouselight neurons typically consist of two separate tracings, one
#'   for the axon and one for the dendrites. When \code{simplify=TRUE} (the
#'   default) the output is a \emph{flattened} data frame with one row for every
#'   tracing. When \code{simplify=FALSE} there is one row per neuron, but the
#'   data.frame contains nested data frames with per tracing information.
#'
#'   The identifier which you will need to download a neuron with
#'   \code{\link{read.neurons.mouselight}} is stored in column
#'
#' @export
#'
#' @examples
#' \donttest{
#' ndf=mouselight_list_neurons()
#' # How many tracings per neurons?
#' table(table(ndf$neuron.id))
#' }
mouselight_list_neurons <- function(simplify=TRUE) {

  body = list(
    query = "query QueryData($filters: [FilterInput!]) {\n  queryData(filters: $filters) {\n    totalCount\n    queryTime\n    nonce\n    error {\n      name\n      message\n      __typename\n    }\n    neurons {\n      id\n      idString\n      brainArea {\n        id\n        acronym\n        __typename\n      }\n      tracings {\n        id\n        tracingStructure {\n          id\n          name\n          value\n          __typename\n        }\n        soma {\n          id\n          x\n          y\n          z\n          radius\n          parentNumber\n          sampleNumber\n          brainAreaId\n          structureIdentifierId\n          __typename\n        }\n        __typename\n      }\n      __typename\n    }\n    __typename\n  }\n}\n",
    variables = list(
      filters = list(
        tracingStructureIds = list(),
        nodeStructureIds = list(),
        # operatorId = NULL,
        amount = 0L,
        brainAreaIds = list(),
        # arbCenter = list(x = NULL, y = NULL, z = NULL),
        # arbSize = NULL,
        invert = FALSE,
        composition = 2L
      )
    ),
    operationName = "QueryData"
  )

  bodyj=jsonlite::toJSON(body, null = 'null', auto_unbox = T)
  res=httr::POST(url = mouselight_url('graphql'), body = bodyj, httr::content_type_json(), encode='raw')
  httr::stop_for_status(res)
  raw_res=httr::content(res, as='text', type='application/json', encoding = 'utf8')
  parsed_res=jsonlite::fromJSON(raw_res, simplifyVector = T)
  parsed_res=parsed_res$data$queryData$neurons

  if(simplify) {
    stopifnot(isTRUE(all(
      names(parsed_res) == c("id", "idString", "brainArea", "tracings", "__typename")
    )))

    # check soma/tracingStructure columns have same number of rows
    if(!isTRUE(all(sapply(parsed_res$tracings, function(x) nrow(x$tracingStructure)==nrow(x$soma)))))
      stop("Parse Error!",
           "Unexpected mismatch between tracingStructure and soma elements of tracings column!")

    # remove nested tracings data.frames
    neurondf=parsed_res[-4]
    # flatten brainArea data.frame
    neurondf=jsonlite::flatten(neurondf)
    # first id is for the neuron (which usually has 2 associated tracings)
    names(neurondf)[1]='neuron.id'

    # flatten nested tracings data.frames
    tracedflist=sapply(parsed_res[['tracings']], jsonlite::flatten, simplify = F)
    names(tracedflist)=neurondf[['neuron.id']]
    tracedf=dplyr::bind_rows(tracedflist, .id='neuron.id')
    names(tracedf)[names(tracedf)=='id']='tracing.id'
    # join neuron and tracings data.frames, one row for each tracing
    parsed_res <- dplyr::right_join(neurondf, tracedf, by='neuron.id')
  }
  names(parsed_res) <- sub("__", "", names(parsed_res))
  parsed_res
}
