#' Load Brazilian Fauna database
#'
#' @param data_dir (character) the same directory used to save the data
#' downloaded from Brazilian Fauna using the \link{get_faunabr} function.
#' @param data_version (character) the version of Brazilian Fauna database to
#' be loaded. It can be "latest", which will load the latest version
#' available; or another specified version, for example "1.2".
#' Default = "latest".
#' @param type (character) it determines the number of columns that will be
#' loaded. It can be "short" or "complete". Default = "short". See details.
#' @param verbose (logical) Whether to display messages during function
#' execution. Set to TRUE to enable display, or FALSE to run silently.
#' Default = TRUE.
#' @param encoding (character) the declared encodings for special characters.
#' Character strings in R can be declared to be encoded in "latin1" or "UTF-8".
#' Default: "UTF-8".
#' @details
#' The parameter type accepts two arguments. If type = short, it will load a
#' data.frame with the 19 columns needed to run the other functions of the
#' package: species, subspecies, scientificName, acceptedName, kingdom, phylum,
#' class, order, family, genus, lifeForm, habitat, states, countryCode, origin,
#' taxonomicStatus, nomenclaturalStatus, vernacularName, and taxonRank.
#' If type = complete, it will load a data.frame with all 31 variables available
#' in Brazilian Fauna database.
#'
#' @return A data.frame with the specified version (Default is the latest
#' available) of the Brazilian Fauna database. This data.frame is necessary to
#' run most of the functions of the package.
#'
#' @usage load_faunabr(data_dir, data_version = "latest",
#'                     type = "short", verbose = TRUE, encoding = "UTF-8")
#' @importFrom stats na.omit
#' @importFrom data.table fread
#' @export
#' @references
#' Brazilian Zoology Group. Catálogo Taxonômico da Fauna do Brasil. Available at:
#' https://ipt.jbrj.gov.br/jbrj/resource?r=catalogo_taxonomico_da_fauna_do_brasil
#'
#' @examples
#' \dontrun{
#' #Creating a folder in a temporary directory
#' #Replace 'file.path(tempdir(), "faunabr")' by a path folder to be create in
#' #your computer
#' my_dir <- file.path(file.path(tempdir(), "faunabr"))
#' dir.create(my_dir)
#' #Download, merge and save data
#' get_fauna(output_dir = my_dir, data_version = "latest", overwrite = TRUE,
#'             verbose = TRUE)
#' #Load data
#' df <- load_faunabr(data_dir = my_dir, data_version = "latest",
#'                    type = "short")
#' }
load_faunabr <- function(data_dir, data_version = "latest",
                         type = "short", verbose = TRUE, encoding = "UTF-8"){
  #Set folder
  if (missing(data_dir)) {
    stop("Argument data_dir is not defined")
  }

  #Check classes
  if (!is.character(data_dir)) {
    stop(paste0("Argument data_dir must be a character, not ", class(data_dir)))
  }

  if (!is.character(data_version)) {
    stop(paste0("Argument data_version must be a character, not ",
                class(data_version)))
  }

  if (!(type %in% c("short", "complete"))) {
    stop("Argument type must be 'short' or 'complete'")
  }

  #Check classes
  if (!is.character(encoding)) {
    stop(paste0("Argument encoding must be a character, not ", class(encoding)))
  }

#Set directory
  path_data <-  data_dir

  #Get latest available version if version was not set
  if(data_version == "latest") {
    #Search for directories
    all_dirs <- list.dirs(path = path_data, recursive = FALSE)
    dir_versions <- na.omit(suppressWarnings(
      as.numeric(sub(".*/([0-9.]+)$", "\\1", all_dirs))))

    #Get highest version
    if(length(dir_versions) > 0) {
      version_data <- max(dir_versions) } else {
        version_data <- 0
      }
  } else {version_data <-  data_version}

  #Check if version_data is avaliable
  check_folder <- version_data %in% list.dirs(path_data, full.names = F)

  if (!check_folder) {
    stop("The version specified - ", version_data, " - is not available in the
    specified directory. Please check the directory or run the 'get_faunabr' function
    to download the latest version of the data.")
  }

  #Load data
  if(verbose){
  message("Loading version ", version_data) }

  if(type == "complete") {
    return(data.table::fread(file.path(path_data, version_data,
                            "CompleteBrazilianFauna.gz"),
                            data.table = FALSE,
                            encoding = encoding,
                            na.strings = "")) }

  if(type == "short") {
    return(data.table::fread(file.path(path_data, version_data,
                                       "CompleteBrazilianFauna.gz"),
                             data.table = FALSE,
                             encoding = encoding,
                             select = c("species", "subspecies",
                                           "scientificName", "acceptedName",
                                           "kingdom", "phylum", "class", "order",
                                           "family", "genus", "lifeForm",
                                           "habitat", "states", "countryCode",
                                           "origin", "taxonomicStatus",
                                           "nomenclaturalStatus",
                                           "vernacularName", "taxonRank", "id"),
                             na.strings = ""))
  }
}

