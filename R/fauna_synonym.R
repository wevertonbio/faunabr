#' Retrieve synonyms for species
#'
#' @param data (data.frame) the data.frame imported with the
#' \code{\link{load_faunabr}} function
#' @param species (character) names of the species
#' @param include_subspecies (logical) include subspecies that are synonyms of
#' the species? Default = TRUE
#'
#' @return A data.frame containing unique synonyms of the specified species
#' along with relevant information on taxonomic status.
#' @usage fauna_synonym(data, species,
#'                    include_subspecies = TRUE)
#' @export
#' @examples
#' data("fauna_data") #Load Flora e Funga do Brasil data
#' #Species to extract synonyms
#' spp <- c("Panthera onca", "Mazama jucunda", "Subulo gouzoubira")
#' spp_synonyms <- fauna_synonym(data = fauna_data, species = spp,
#'                               include_subspecies = FALSE)
#' spp_synonyms
#'
fauna_synonym <- function(data, species,
                        include_subspecies = TRUE){

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

  #Taxon Rank
  if(include_subspecies){
    ss <- "subspecies"
  } else {ss <- NULL}

  tr <- c("species", ss)

  data <- data[data$taxonRank %in% tr,]


  #Check if there is any species absent in d
  no_match <- setdiff(species, unique(data$species))
  if(length(no_match) > 0 & length(no_match) < length(species)) {
    warning(paste("Some species are absent of Fauna do Brasil database\n",
                  "Check the species names using the check_names() function"))
  }
  #Get match
  order <- setdiff(species, no_match)

  res <- unique(data[which(extract_binomial(data$acceptedName) %in% species),
                     c("acceptedName", "species", "taxonomicStatus")])

  #Get species withou synonyms
  no_syn <- setdiff(species, res$acceptedName)
  if(length(no_syn) > 0){
    res_no_syn <- data[data$species %in% no_syn,
                       c("acceptedName", "species", "taxonomicStatus")]
    res_no_syn$acceptedName <- res_no_syn$species
    res_no_syn$species <- NA
    res <- rbind(res, res_no_syn)
  }


  if(nrow(res) > 0) {
    #Reorder
    res <- res[order(match(res$acceptedName, order)), ]

    #Change name of the column
    colnames(res)[2] <- "synonym"

    #Remove accepted names
    res <- subset(res, !(res$synonym %in% species))

    return(res) } else {
      warning(paste("All specified species are absent of Fauna do Brasil
                  database\n",
                    "Check the species names using the check_names() function"))
      return(NULL)
    }
}
