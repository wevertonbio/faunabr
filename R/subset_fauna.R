#' Extract a subset of species from Fauna do Brasil database
#' @description
#' Returns a data.frame with a subset of species from Fauna do Brasil database
#'
#' @param data (data.frame) the data.frame imported with the
#' \code{\link{load_faunabr}} function.
#' @param species (character) names of the species to be extracted from Fauna do
#' Brasil database.
#' @param include_subspecies (logical) include subspecies? Default = FALSE
#'
#' @return A data.frame with the selected species.
#' @usage subset_fauna(data, species, include_subspecies = FALSE)
#' @export
#'
#' @examples
#' data("fauna_data") #Load data example
#' #Species to extract from database
#' spp <- c("Panthera onca", "Mazama jucunda", "Subulo gouzoubira")
#' spp_subset <- subset_fauna(data = fauna_data, species = spp,
#'                       include_subspecies = FALSE)
#' spp_subset
subset_fauna <- function(data,
                         species, include_subspecies = FALSE){
  if (missing(data)) {
    stop("Argument data is not defined")
  }

  #Check classes
  if (!is.data.frame(data)) {
    stop(paste0("Argument data must be a data.frame, not ", class(data)))
  }

  if (!is.character(species)) {
    stop(paste0("Argument species must be a character, not ", class(species)))
  }

  if (!is.logical(include_subspecies)) {
    stop(paste0("Argument include_subspecies must be logical, not ",
                class(include_subspecies)))
  }


  #Get binomial names of species
  Species <- extract_binomial(species)

  #Start to filter...
  #Taxon Rank
  #Taxon Rank
  if(include_subspecies){
    ss <- "subspecies"
  } else {ss <- NULL}

  tr <- c("species", ss)

  data <- data[data$taxonRank %in% tr,]

  #Check if there is any species absent in d
  no_match <- setdiff(Species, unique(data$species))

  if(length(no_match) == length(Species)){
    stop("All species are absent of Fauna do Brasil database.
         Check the species names using the check_names() function")
  }

  if(length(no_match) > 0) {
    warning(paste("Some species are absent of Fauna do Brasil database\n",
                  "Check the species names using the check_names() function"))
  }

  return(subset(data, data$species %in% Species))

}
