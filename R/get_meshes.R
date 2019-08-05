#' @title Read the brain mesh and its brain regions from the MouseLight project
#'
#' @description Save  the brain mesh and its brain regions from the MouseLight project
#' @param type whether just to retreive the outer brain mesh, or the brain constituent neuropils
#' @param progress logical, toggle progress bar
#' @param save.path if you do not want to save at \code{package_location/inst/exdata} then specify a location
#' @param Force logical, whether or not to overwrite .obj files stored at \code{save.path} or the default location
#' @param ... methods passed to \code{ml_fetch}
#' @return a \code{nat} package \code{\link[nat]{hxsurf}} object, which mimics the Amira surface format, replete with metadata that can be
#' accessed using \code{$}
#' @seealso \code{\link{mouselight_read_brain}}, \code{\link{ml_brain_region_info}}
#' @examples
#' \dontrun{
#' ## First we can quickly just plot the outer mesh for the brain
#' outline = ml_read_brain(type = "outline")
#' plot3d(outline, col = "pink", alpha = 0.3)
#'
#' ## This is cool, but maybe what we really want are its sub-divisions.
#' mousebrain = ml_read_brain(type = "brain_areas")
#' clear3d()
#' plot3d(mousebrain)
#' ### This takes a long time the first time you call this function.
#' ### By default .obj files for these brain regions are saved at the location
#' ### of this package, then following the path inst/exdata/obj and will be
#' ### read directly from there in future.
#'
#' ## What brain regions are on offer?
#' print(mousebrain$neuropil_full_names)
#'
#' ## Or if we want more information, we can get it like this:
#' mbr = ml_brain_region_info()
#' View(ml_brain_region_info)
#'
#' ## Perhaps we want to plot just the amygdala?
#' ### To do this we can do
#' amygdala.codes = mousebrain$RegionList[grepl("amygdala",
#' mousebrain$neuropil_full_names,
#' ignore.case = TRUE)]
#' plot3d(outline, col = "pink", alpha = 0.1)
#' plot3d(subset(mousebrain, amygdala.codes), alpha = 0.5)
#'
#' }
#' @references Winnubst, Johan, Erhan Bas, Tiago A. Ferreira, Zhuhao Wu, Michael N. Economo, Patrick Edson, Ben J. Arthur, et al. 2019. “Reconstruction of 1,000 Projection Neurons Reveals New Cell Types and Organization of Long-Range Connectivity in the Mouse Brain.” bioRxiv. https://doi.org/10.1101/537233.
#' Economo, Michael N., Nathan G. Clack, Luke D. Lavis, Charles R. Gerfen, Karel Svoboda, Eugene W. Myers, and Jayaram Chandrashekar. 2016. “A Platform for Brain-Wide Imaging and Reconstruction of Individual Neurons.” eLife 5 (January): e10566.
#' @export
#' @rdname ml_read_brain
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
      t = suppressWarnings (try( utils::download.file(urls[url], localfile, mode='wb', quiet = TRUE), silent = TRUE))
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

