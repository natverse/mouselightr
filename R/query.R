#' Query available neurons from MouseLight site
#'
#' @param simplify whether to simplify the query result to a simple data.frame
#'   with one row per tracing (See details).
#' @param structure when searching for the brain regions containing neurons of interest,
#' the part of the neuron that should be used for the search
#' @return data.frame describing the available neurons. See details for the
#'   implication of the \code{simplify} argument.
#' @details Mouselight neurons typically consist of two separate tracings, one
#'   for the axon and one for the dendrites. When \code{simplify=TRUE} (the
#'   default) the output is a \emph{flattened} data frame with one row for every
#'   tracing. When \code{simplify=FALSE} there is one row per neuron, but the
#'   data.frame contains nested data frames with per tracing information.
#'
#'   The identifier which you will need to download a neuron with
#'   \code{\link{mouselight_read_neurons}} is stored in column
#'
#'   Note, when using \code{mouselight_neurons_by_brain_region}, the brain region
#'   returned can belong to larger brain regions, which are agglomerations of
#'   smaller ones. See \code{\link{mouselight_brain_region_info}.
#'
#' @examples
#' \donttest{
#' ndf=mouselight_neuron_info()
#' # How many tracings per neurons?
#' table(table(ndf$neuron.id))
#'
#' # In what brain regions do neurons have their end points?
#' br_search = mouselight_neurons_by_brain_region("dendrite")
#' View(br_search)
#'
#' }
#' @seealso \code{\link{mouselight_read_brain}}, \code{\link{mouselight_brain_region_info}}
#' @export
#' @rdname mouselight_neuron_info
mouselight_neuron_info <- function(simplify=TRUE) {

  body = list(
    operationName = "SearchNeurons",
    variables = list(context = list(
      scope = 6L,
      nonce = "cjyo7xu7k00033h5yrj9jfpoy",
      predicates = list(
        list(
          predicateType = 3L,
          tracingIdsOrDOIs = list("1"),
          tracingIdsOrDOIsExactMatch = FALSE,
          tracingStructureIds = list("68e76074-1777-42b6-bbf9-93a6a5f02fa4"),
          nodeStructureIds = list("c37953e1-a1e9-4b9a-847e-08d9566ced65"),
          operatorId = NULL,
          amount = 0L,
          brainAreaIds = list(),
          arbCenter = list(x = NULL, y = NULL, z = NULL),
          arbSize = NULL,
          invert = FALSE,
          composition = 3L
        )
      )
    )),
    query= "query SearchNeurons($context: SearchContext) {\n  searchNeurons(context: $context) {\n    totalCount\n    queryTime\n    nonce\n    \n    neurons {\n      id\n      idString\n      tracings {\n        id\n        tracingStructure {\n          id\n          name\n          value\n          __typename\n        }\n        soma {\n          id\n          x\n          y\n          z\n          radius\n          parentNumber\n          sampleNumber\n          brainAreaId\n          structureIdentifierId\n          __typename\n        }\n        __typename\n      }\n      __typename\n    }\n    __typename\n  }\n}\n"
  )

  bodyj=jsonlite::toJSON(body, null = 'null', auto_unbox = T)
  res=httr::POST(url = mouselight_url('graphql'), body = bodyj, httr::content_type_json(), encode='raw')
  httr::stop_for_status(res)
  raw_res=httr::content(res, as='text', type='application/json', encoding = 'utf8')
  parsed_res=jsonlite::fromJSON(raw_res, simplifyVector = T)
  parsed_res=parsed_res$data[[1]]
  if(is.null(parsed_res$neurons))
    stop("invalid return structure!")
  if(!isTRUE(nneurons <- parsed_res$totalCount>0))
    stop("No neurons returned!")
  parsed_res <- parsed_res$neurons
  if(simplify) {
    stopifnot(isTRUE(all(
      names(parsed_res) %in% c("id", "idString", "brainArea", "tracings", "__typename", "sample")
    )))

    # check soma/tracingStructure columns have same number of rows
    if(!isTRUE(all(sapply(parsed_res$tracings, function(x) nrow(x$tracingStructure)==nrow(x$soma)))))
      stop("Parse Error!",
           "Unexpected mismatch between tracingStructure and soma elements of tracings column!")

    # remove nested tracings data.frames
    neurondf=parsed_res[names(parsed_res)!='tracings']
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

#' @export
#' @rdname mouselight_neuron_info
mouselight_neuron_somata <- function(soma.info=TRUE) {
  somaq <- if(isTRUE(soma.info)){
    "tracings {\n soma {\n          x\n          y\n          z\n          radius\n}\n}\n"
  }else{
    ""
  }
  query <-  paste0("query SearchNeurons($context: SearchContext) {\n  searchNeurons(context: $context)",
                 " {\n    ",
                 "neurons {\n      id\n      idString\n ",
                 somaq,
                 "}\n}}\n\n")
  body = list(
    operationName = "SearchNeurons",
    variables = list(context = list(
      scope = 6L,
      nonce = "cjyo7xu7k00033h5yrj9jfpoy",
      predicates = list(
        list(
          predicateType = 3L,
          tracingIdsOrDOIs = list("67f04b64-80ed-4dfc-a519-7cac0605a84f"),
          tracingIdsOrDOIsExactMatch = FALSE,
          tracingStructureIds = list("68e76074-1777-42b6-bbf9-93a6a5f02fa4"),
          nodeStructureIds = list("c37953e1-a1e9-4b9a-847e-08d9566ced65"),
          operatorId = NULL,
          amount = 0L,
          brainAreaIds = list(),
          arbCenter = list(x = NULL, y = NULL, z = NULL),
          arbSize = NULL,
          invert = FALSE,
          composition = 3L
        )
      )
    )),
    query=query
  )
  bodyj=jsonlite::toJSON(body, null = 'null', auto_unbox = T)
  res <-  mouselight_fetch(path = "graphql",
                           body = bodyj,
                           parse.json = TRUE,
                           simplifyVector=TRUE,
                           include_headers = FALSE,
                           config = httr::content_type_json(),
                           encode='raw')
  som.df = data.frame(idString = res$data$searchNeurons$neurons[[2]],
                      neuron.id = res$data$searchNeurons$neurons[[1]],
                      do.call(rbind, lapply(res$data$searchNeurons$neurons[[3]], function(x) unlist(nullToNA(x)))))
  rownames(som.df) = som.df$idString
  som.df
}


# in progress
mouselight_neurons_by_brain_region <- function(structure = c("either","axon", "dendrite")){
  structure <- match.arg(structure)
  body <-  list(
    operationName = "SearchNeurons",
    variables = list(context = list(
      scope = 6L,
      nonce = "cjyzvdm9400053h5y90ywg53t",
      predicates = list(
        list(
          predicateType = 3L,
          tracingIdsOrDOIs = list("1"),
          tracingIdsOrDOIsExactMatch = FALSE,
          tracingStructureIds = list("aef2ba31-8f9b-4a47-9de0-58dab1cc06a8"), # .structureIdentifiers$id[.structureIdentifiers$name==structure]
          nodeStructureIds = list(), # end points: "c37953e1-a1e9-4b9a-847e-08d9566ced65"
          operatorId = "f191e8b3-8fb9-4151-a48c-432c1a2382cd",
          amount = 0L,
          brainAreaIds = list("825cacf6-2a60-4fd9-b749-414bd6941357"),
          arbCenter = list(x = NULL, y = NULL, z = NULL),
          arbSize = NULL,
          invert = FALSE,
          composition = 2L
        )
      )
    )),
    query= "query SearchNeurons($context: SearchContext)\n
                  {\n
                    searchNeurons(context: $context) {\n
                    neurons {\n
                    id\n
                    __typename\n
                    brainArea {\n
                              acronym\n
                              safeName\n
                              structureId\n
                            }\n
                    }\n
                  }\n
              }"
    )
  bodyj <- jsonlite::toJSON(body, null = 'null', auto_unbox = T)
  res <-  mouselight_fetch(path = "graphql",
                         body = bodyj,
                         parse.json = TRUE,
                         simplifyVector=FALSE,
                         include_headers = FALSE,
                         config = httr::content_type_json(),
                         encode='raw')
  none <-  unlist(lapply(res$data$searchNeurons$neurons, function(x) !is.null(x$brainArea)))
  df <-  data.frame(do.call(rbind, lapply(res$data$searchNeurons$neurons[none], function(x) c(id = x$id, typename = x$`__typename`, unlist(nullToNA(x$brainArea))))))
  rownames(df) <- df$id
  df
}



