ml_list_neurons <- function() {

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
  res=httr::POST(url = ml_url('graphql'), body = bodyj, httr::content_type_json(), encode='raw')
  httr::stop_for_status(res)
  raw_res=httr::content(res, as='text', type='application/json', encoding = 'utf8')
  parsed_res=jsonlite::fromJSON(raw_res, simplifyVector = T)
  parsed_res$data$queryData$neurons
}
