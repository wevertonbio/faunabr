#' Download the latest version of Catálogo Taxonômico da Fauna do Brasil
#'
#' @description
#' This function downloads the latest or an older version of Catálogo Taxonômico
#' da Fauna do Brasil database, merges the information into a single data.frame,
#' and saves this data.frame in the specified directory.
#'
#' @param output_dir (character) a directory to save the data downloaded from
#' Fauna do Brasil
#' @param data_version (character) Version of the Fauna do Brasil database to
#' download. Use "latest" to get the most recent version, which is updated
#' frequently. Alternatively, specify an older version (e.g.,
#' data_version = "1.2").Default value is "latest".
#' @param solve_discrepancies Resolve inconsistencies between species and
#' subspecies  information. When set to TRUE (default), species
#' information is updated based on unique data from subspecies.
#' For example, if a subspecies occurs in a certain state, it implies that the
#' species also occurs in that state.
#' @param translate (logical) whether to translate the original dataset
#' ("lifeForm", "origin", "habitat", and "taxonRank") from Portuguese to English.
#' Default is TRUE.
#' @param overwrite (logical) If TRUE, data is overwritten. Default = TRUE.
#' @param verbose (logical) Whether to display messages during function
#' execution. Set to TRUE to enable display, or FALSE to run silently.
#' Default = TRUE.
#'
#' @returns
#' The function downloads the latest version of the Catálogo Taxonômico da Fauna
#' do Brasil database from the official source. It then merges the information
#' into a single data.frame, containing details on species, taxonomy, occurrence,
#' and other relevant data.
#' The merged data.frame is then saved as a file in the specified output
#' directory. The data is saved in a format that allows easy loading using the
#' \code{\link{load_faunabr}} function for further analysis in R.
#'
#' @usage get_faunabr(output_dir, data_version = "latest",
#'                  solve_discrepancies = TRUE, translate = TRUE,
#'                  overwrite = TRUE, verbose = TRUE)
#' @export
#'
#' @importFrom httr GET write_disk
#' @importFrom XML htmlParse xpathSApply xmlGetAttr
#' @importFrom utils unzip
#' @importFrom utils read.csv
#' @importFrom data.table fwrite
#'
#' @references
#' Brazilian Zoology Group. Catálogo Taxonômico da Fauna do Brasil. Available at:
#' https://ipt.jbrj.gov.br/jbrj/resource?r=catalogo_taxonomico_da_fauna_do_brasil
#'
#' @examples
#' \dontrun{
#' #Creating a folder in a temporary directory
#' #Replace 'file.path(tempdir(), "faunaabr")' by a path folder to be create in
#' #your computer
#' my_dir <- file.path(file.path(tempdir(), "faunabr"))
#' dir.create(my_dir)
#' #Download, merge and save data
#' get_faunabr(output_dir = my_dir)
#' }
get_faunabr <- function(output_dir, data_version = "latest",
                        solve_discrepancies = TRUE,
                        translate = TRUE,
                        overwrite = TRUE,
                        verbose = TRUE) {
  #Set folder
  if(is.null(output_dir)) {
    stop(paste("Argument output_dir is not defined, this is necessary for",
         "\n downloading and saving data"))
  }
  if (!is.character(output_dir)) {
    stop(paste0("Argument output_dir must be a character, not ",
                class(output_dir)))
  } else {
    path_data <- output_dir
  }

  if(!file.exists(output_dir)){
    dir.create(output_dir)
    warning("'output_dir' does not exists. Creating folder:\n" , output_dir)
  }

  if (!is.character(data_version)) {
    stop(paste0("Argument data_version must be a character, not ",
                class(data_version)))
  }

  if (!is.logical(solve_discrepancies)) {
    stop(paste0("Argument solve_discrepancies must be logical, not ",
                class(overwrite)))
  }

  if (!is.logical(overwrite)) {
    stop(paste0("Argument overwrite must be logical, not ",
                class(overwrite)))
  }


  #Print message
  if(verbose) {
  message("Data will be saved in ", path_data, "\n") }


  if(data_version != "latest") {
  link_download <- paste0(
    "https://ipt.jbrj.gov.br/jbrj/archive.do?r=catalogo_taxonomico_da_fauna_do_brasil&v=",
                          data_version)
  version_data <- data_version
  }


  if(data_version == "latest") {
  #Get link of latest version
  response <- httr::GET(
    "https://ipt.jbrj.gov.br/jbrj/resource?r=catalogo_taxonomico_da_fauna_do_brasil")
  parse <- XML::htmlParse(response)
  links <- unlist(XML::xpathSApply(parse, path = "//a", XML::xmlGetAttr,
                                   "href"))
  download_pattern <- "https://ipt.jbrj.gov.br/jbrj/archive.do?r=catalogo_taxonomico_da_fauna_do_brasil&v="
  version_data <- subset(links, grepl(download_pattern, links, fixed = TRUE))
  version_data <- gsub(download_pattern, "", version_data, fixed = TRUE)
  link_download <- paste0(
    "https://ipt.jbrj.gov.br/jbrj/archive.do?r=catalogo_taxonomico_da_fauna_do_brasil&v=",
    version_data)
  }

  #Print message
  if(!is.null(version_data) & verbose) {
      message("Downloading version: ", version_data, "\n")


  #Download data
  httr::GET(link_download, httr::write_disk(file.path(
    path_data,
    paste0(version_data, ".zip")),
                                      overwrite = overwrite))
  }

  #Unzip folder
  version_data_numeric <- as.numeric(version_data)
  utils::unzip(zipfile = paste0(file.path(path_data, version_data), ".zip"),
        exdir = file.path(path_data, version_data_numeric))

  #Print message
  if(verbose){
  message("Merging data. Please wait a moment...\n") }

  #Merge data
  merge_data(path_data = path_data, version_data = version_data_numeric,
             translate = translate,
             solve_discrepancies = solve_discrepancies, verbose = verbose)

  #Print final message
  if(verbose){
  message("Data downloaded and merged successfully. Final data saved in",
              file.path(path_data, version_data, "CompleteBrazilianFauna.gz"))
  }

}
