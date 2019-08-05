
ml_read_brain <- function(type = c("outline", "brain_areas"),
                          progress  = TRUE,
                          save.path = NULL,
                          Force  = FALSE){
  type = match.arg(type)
  # save path
  if(is.null(save.path)){
    save.path = path.package("mouselightr", quiet = FALSE)
  }
  package.path.data = paste(save.path,"inst/exdata/obj",sep="/")
  if(!dir.exists(package.path.data)){
    dir.create(package.path.data, recursive = TRUE)
  }
  body = list(
    query = "{\n
    brainAreas {\n
    acronym\n
    safeName\n
    geometryFile\n
    }\n
    }",
  variables = list(),
  operationName = NULL
  )
  bodyj=jsonlite::toJSON(body, null = 'null', auto_unbox = T)
  res = ml_fetch(path = "graphql",
                 body = bodyj,
                 parse.json = TRUE,
                 simplifyVector=FALSE,
                 include_headers = FALSE,
                 config = httr::content_type_json(),
                 encode='raw')
  df = do.call(rbind,res$data$brainAreas)
  df = as.data.frame(t(apply(df, 1, unlist)))
  df = as.data.frame(df)
  if(type=="outline"){
    df = subset(df, safeName == "Whole Brain")
  }else{
    df = subset(df, safeName != "Whole Brain")
  }
  # Now read .obj files
  urls = ml_url("static/allen/obj",df$geometryFile)
  saved.files = success = c()
  for (url in 1:length(urls)){
    message = paste("downloading", df$safeName[url],".obj file", sep = " ")
    localfile = paste0(package.path.data,"/",df$geometryFile[url])
    if(!file.exists(localfile) | Force){
      t = suppresWarnings (try( utils::download.file(urls[url], localfile, mode='wb', quiet = TRUE), silent = TRUE))
      if(inherits(t,'try-error')) {
        message = c("NOTE: unable to download ", df$safeName[url])
        next
      }
    }
    success = c(success, url)
    saved.files = c(saved.files, localfile)
    if(progress) ml_progress(url/length(urls)*100, max = 101, message = message)
  }
  objs = lapply(saved.files, function(x)
    tryCatch(readobj::read.obj(x, convert.rgl = T), error = function(e) NULL)[[1]])
  if(progress) ml_progress(url/length(urls)*100, max = 101, message = "reading .obj files")
  success = success[!sapply(objs, is.null)]
  objs = objs[!sapply(objs, is.null)]
  objs = lapply(objs, nat::as.hxsurf)
  obj.name = df$acronym
  # build single hxsurf object
  brain = list()
  brain$Vertices = data.frame()
  brain$Regions = list()
  brain$RegionList = brain$RegionColourList = c()
  class(brain) = c("hxsurf","list")
  count = 0
  structure.colors = grDevices::rainbow(length(success))
  for(i in 1:length(objs)){
    o = objs[[i]]
    v = o$Vertices
    v$PointNo = v$PointNo + count
    brain$Vertices = rbind(brain$Vertices, v)
    region.name = obj.name[success][i]
    regions = o$Regions$Interior+count
    brain$Regions[[region.name]] = regions
    brain$RegionList = c(brain$RegionList, region.name)
    brain$RegionColourList = c(brain$RegionColourList, structure.colors[success][i])
    count = count + nrow(o$Vertices)
    if(progress) ml_progress(i/length(objs)*100, max = 100, message = "assembling hxsurf object")
  }
  brain$neuropil_full_names = df$safeName[success]
  brain
}















