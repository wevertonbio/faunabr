#' Check if you have the latest version of Fauna do Brasil data available
#'
#' @description
#' This function checks if you have the latest version of the Fauna do Brasil
#' data available in a specified directory.
#'
#' @param data_dir the directory where the data should be located.
#'
#' @return A message informing whether you have the latest version of Fauna do
#' Brasil available in the data_dir
#' @usage fauna_version(data_dir)
#' @export
#'
#' @importFrom httr GET
#' @importFrom XML htmlParse xpathSApply xmlGetAttr
#'
#' @examples
#' #Check if there is a version of Fauna do Brasil data available in the
#' #current directory
#' fauna_version(data_dir = getwd())

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

  #Search for directories with data
  all_dirs <- list.files(path = path_data, recursive = TRUE,
                         pattern = "CompleteBrazilianFauna.gz", full.names = FALSE)
  dir_versions <- dirname(all_dirs)


  #Get highest version
  if(length(dir_versions) > 0) {
    high_version <- max(as.numeric(dir_versions))
    } else {
      high_version <- 0
    }

  #Get link of latest version
  response <- httr::GET(
    "https://ipt.jbrj.gov.br/jbrj/resource?r=catalogo_taxonomico_da_fauna_do_brasil")
  parse <- XML::htmlParse(response)
  links <- unlist(XML::xpathSApply(parse, path = "//a", XML::xmlGetAttr,
                                   "href"))
  download_pattern <- "https://ipt.jbrj.gov.br/jbrj/archive.do?r=catalogo_taxonomico_da_fauna_do_brasil&v="
  link_download <- subset(links, grepl(download_pattern, links,
                                       fixed = TRUE))

  #Get version
  latest_version <- as.numeric(gsub(".*catalogo_taxonomico_da_fauna_do_brasil&v=([0-9.]+).*", "\\1",
                         link_download))

  #Check if you have the latest version
  is_latest <- high_version == latest_version

  #Check how many versions exists
  many_versions <- length(dir_versions) > 1

  #Print messages

  if(length(dir_versions) == 0) {
    message(
      "You do not have any version of Fauna do Brasil in this directory.
    The latest version is ", latest_version, ". Please, change the directory or
    run the function get_faunabr() to download the latest version of fauna do
    Brasil.", "\n")
  }

  if(isTRUE(is_latest) & isFALSE(many_versions)) {
    message(paste("You have the latest version of Fauna do Brasil: Version", latest_version, "\n"))
  }

  if(isTRUE(is_latest) & isTRUE(many_versions)) {
    message(paste("You have the following versions of Fauna do Brasil:\n",
paste(dir_versions, collapse = "\n"),
"\n It includes the latest version: ",
latest_version, "\n"))
  }

  if(isFALSE(is_latest) & isTRUE(many_versions)) {
    message(paste0("You have the following versions of Fauna do Brasil:\n",
                   paste0(dir_versions, collapse = "\n"),
                   "\nHowever, it does not include the latest version: ",
                   latest_version,
                   "\nIf you want to download the latest version, run the function
    get_faunabr() again"))
  }
}
