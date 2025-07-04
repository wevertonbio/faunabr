#' Resolve discrepancies between species and subspecies information
#'
#' @param data (data.frame) the data.frame imported with the
#' \code{\link{load_faunabr}} function.
#'
#' @return a data.frame with the discrepancies solved
#' @usage fauna_discrepancies(data)
#' @details
#' In the original dataset, discrepancies may exist between species and
#' subspecies information. An example of a discrepancy is when
#' species occurs only in one state (e.g., SP), but a subspecies or variety
#' of the same species occurs in another states (e.g., SP and RJ). This function
#' rectifies such discrepancies by considering distribution (states and countries)
#' life form, and habitat. For instance, if a subspecies is recorded in a specific
#' state, it implies that the species also occurs in that state
#'
#' @export
#'
#' @examples
#' data("fauna_data") #Load fauna e Funga do Brasil data
#' #Check if discrepancies were solved in the dataset
#' attr(fauna_data, "solved_discrepancies")
#' #Solve discrepancies
#' fauna_solved <- fauna_discrepancies(fauna_data)
#' #Check if discrepancies were solved in the dataset
#' attr(fauna_solved, "solved_discrepancies")
fauna_discrepancies <- function(data) {
  if (missing(data)) {
    stop("Argument data is not defined")
  }
  if (!inherits(data, "data.frame")) {
    stop(paste0("Argument data must be a data.frame, not ",
                class(data)))
  }

  if(isTRUE(attr(data, "fauna_discrepancies"))) {
    stop("Any discrepancies have already been resolved in this dataset")
  }

  #Get subspecies with valid names
  ind_spp <- which(data$taxonRank %in% c("subspecies", "sub_especie") &
                     data$taxonomicStatus %in% c("valid", "valido"))
  spp_var <- data[ind_spp, "species"]
  #Create temporarily column with binomial species name
  spp_var_bin <- extract_binomial(spp_var)
  data$species_bin <-  extract_binomial(data$species)
  #Get only species that exists as Species in dataframe
  spp_var_yes <- intersect(data$species_bin[which(
    data$taxonRank %in% c("species", "especie") &
      data$taxonomicStatus %in% c("valid", "valido"))],
    spp_var_bin)
  #Other species
  spp_var_no <- na.omit(setdiff(na.omit(spp_var_bin),
                                data$species_bin[which(
                                  data$taxonRank %in% c("species", "especie"))]))

  #Get dataframe to update
  d_upt <- subset(data, data$species_bin %in% spp_var_yes)

  #Update columns
  dd_updated_list <- lapply(split(d_upt, d_upt$species_bin),
                            update_columns)

  # Merge dataframes
  d_upt2 <- do.call(rbind, dd_updated_list)
  row.names(d_upt) <- NULL

  #Update final dataframe
  data_solved <- rbind(subset(data, !(data$id %in% d_upt2$id)), d_upt2)

  #Fix varieties and subspecies that does not appear as species
  if(length(spp_var_no) > 0) {
    df_no_species <- data[data$species_bin %in% na.omit(spp_var_no),]
    #Change taxonrank
    if(data$language == "en"){
    df_no_species$taxonRank <- "species"} else {
      df_no_species$taxonRank <- "especie"
    }
    #Create new id
    df_no_species$id <- sample(setdiff(1:50000, data$id), nrow(df_no_species))
    #Subset columns
    df_no_species <- df_no_species[, colnames(data)]
    #Merge data
    data_solved <- rbind(data_solved, df_no_species)
  }

  #Add attribute
  attr(data_solved, "solved_discrepancies") <- TRUE

  #Remove Unknown when there are info
  columns <- c("lifeForm", "habitat", "states", "origin")
  for(i in columns) {
    data_solved[[i]][which(
      grepl(";Unknown|Unknown;", data_solved[[i]]))] <- gsub(
        ";Unknown|Unknown;", "", data_solved[[i]][which(grepl(";Unknown|Unknown;",
                                                              data_solved[[i]]))])
  }

  #Remove column with binomial name without var or subsp
  data_solved$species_bin <- NULL

  #Return
  return(data_solved)
}
