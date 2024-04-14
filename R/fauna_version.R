#' Check if you have the latest version of Brazilian Flora data available
#'
#' @description
#' This function checks if you have the latest version of the Brazilian Flora
#' data available in a specified directory.
#'
#' @param data_dir the directory where the data should be located.
#'
#' @return A message informing whether you have the latest version of Brazilian
#' Flora data available in the data_dir
#' @usage check_version(data_dir)
#' @export
#'
#' @importFrom httr GET
#' @importFrom XML htmlParse xpathSApply xmlGetAttr
#'
#' @examples
#' #Check if there is a version of Brazilian Flora data available in the current
#' #directory
#' check_version(data_dir = getwd())

fauna_version <- function(data_dir) {
  #Set folder
  if (missing(data_dir)) {
    stop("Argument data_dir is not defined")
  }

  if (!is.character(data_dir)) {
    stop(paste0("Argument data_dir must be a character, not ", class(data_dir)))
  } else {
    path_data <- data_dir
  }

  #Search for directories
  all_dirs <- list.dirs(path = path_data, recursive = FALSE)
  dir_versions <- na.omit(as.numeric(gsub(data_dir, "", all_dirs,
                                          fixed = TRUE)))
  #Get highest version
  if(length(dir_versions) > 0) {
  high_version <- max(dir_versions) } else {
    high_version <- 0
  }

  #Get link of latest version
  response <- httr::GET(
    "https://ipt.jbrj.gov.br/jbrj/resource?r=catalogo_taxonomico_da_fauna_do_brasil")
  parse <- XML::htmlParse(response)
  links <- unlist(XML::xpathSApply(parse, path = "//a", XML::xmlGetAttr,
                                   "href"))
  download_pattern <- "https://ipt.jbrj.gov.br/jbrj/archive.do?r=catalogo_taxonomico_da_fauna_do_brasil&v="
  version_data <- subset(links, grepl(download_pattern, links, fixed = TRUE))
  latest_version <- as.numeric(gsub(download_pattern, "", version_data, 
                                    fixed = TRUE))
  
  #Check if you have the latest version
  is_latest <- high_version == latest_version

  #Check if final data exists in the folder
  file_exist <- file.exists(file.path(path_data, dir_versions,
                                      "CompleteBrazilianFauna.rds"))

  #Get only dir_versions with merged data
  dir_versions <- dir_versions[file_exist]

  #Check how many versions exists
  many_versions <- length(dir_versions) > 1

  #Print messages

  if(length(dir_versions) == 0) {
    message(
    "You do not have any version of Catalog of the Brazilian Fauna in this directory.
    The latest version is ", latest_version, ". Please, change the directory or
    run the function get_faunabr() to download the latest version of Catalog of
    the Brazilian Fauna.", "\n")
  }

  if(isTRUE(is_latest) & isFALSE(many_versions)) {
    message(paste("You have the latest version of the Catalog of the Brazilian Fauna Data -
              Version", latest_version, "\n"))
  }

  if(isTRUE(is_latest) & isTRUE(many_versions)) {
    message(paste("You have the following versions of the Catalog of the Brazilian Fauna:\n",
              paste(dir_versions, collapse = "\n"),
              "\n It includes the latest version: ",
              latest_version, "\n"))
  }

  if(isFALSE(is_latest) & isTRUE(many_versions)) {
    message(paste0("You have the following versions of the Catalog of the
                   Brazilian Fauna:\n",
              paste0(dir_versions, collapse = "\n"),
              "\nHowever, it does not include the latest version: ",
              latest_version,
    "\nIf you want to download the latest version, run the function
    get_faunabr() again"))
  }
}
