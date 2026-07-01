#' Resolve discrepancies between species and subspecies information
#'
#' @param data (data.frame) the data.frame imported with the
#' \code{\link{load_faunabr}} function.
#'
#' @return a data.frame with the discrepancies solved
#'
#' @usage fauna_discrepancies(data)
#'
#' @details
#' In the original dataset, discrepancies may exist between species and
#' subspecies information. An example of a discrepancy is when
#' species occurs only in one state (e.g., SP), but a subspecies or variety
#' of the same species occurs in another states (e.g., SP and RJ). This function
#' rectifies such discrepancies by considering distribution (states and countries)
#' life form, and habitat. For instance, if a subspecies is recorded in a specific
#' state, it implies that the species also occurs in that state
#'
#' @importFrom stats na.omit
#' @importFrom data.table is.data.table setDT
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
#'
fauna_discrepancies <- function(data) {
  if (missing(data)) {
    stop("Argument data is not defined")
  }
  if (!inherits(data, "data.frame")) {
    stop(paste0("Argument data must be a data.frame, not ", class(data)))
  }

  # Check if it is a data.table; if it is a standard data.frame, convert to data.table
  if (!data.table::is.data.table(data)) {
    data.table::setDT(data)
  }

  if(isTRUE(attr(data, "fauna_discrepancies"))) {
    stop("Any discrepancies have already been resolved in this dataset")
  }

  # --- ERROR CORRECTION BLOCK ---
  # Filter indices for valid subspecies elements
  ind_spp <- which(data$taxonRank %in% c("subspecies", "sub_especie") &
                     data$taxonomicStatus %in% c("valid", "valido"))

  # Ensure spp_var is strictly pulled as a text vector (using $) to avoid data.table dimension issues
  spp_var <- data$species[ind_spp]

  # Temporarily generate vector containing binomial species name strings
  spp_var_bin <- extract_binomial(spp_var)

  # Ensure the target column 'species' is processed as a character vector inside the function
  data$species_bin <- extract_binomial(data$species)
  # ------------------------------

  # Get only species that exist as Species in the dataframe
  spp_var_yes <- intersect(data$species_bin[which(
    data$taxonRank %in% c("species", "especie") &
      data$taxonomicStatus %in% c("valid", "valido"))],
    spp_var_bin)

  # Identify other isolated lower-rank species
  spp_var_no <- stats::na.omit(setdiff(stats::na.omit(spp_var_bin),
                                       data$species_bin[which(
                                         data$taxonRank %in% c("species", "especie"))]))

  # Isolate dataframe subset requiring row updates
  d_upt <- subset(data, data$species_bin %in% spp_var_yes)

  # Update columns row-by-row grouping by binomial text keys
  dd_updated_list <- lapply(split(d_upt, d_upt$species_bin),
                            update_columns)

  # Merge underlying updated lists into a clean matrix data frame
  d_upt2 <- do.call(rbind, dd_updated_list)
  row.names(d_upt) <- NULL

  # Reassemble the final dataframe structures
  data_solved <- rbind(subset(data, !(data$id %in% d_upt2$id)), d_upt2)

  # Fix varieties and subspecies that do not appear explicitly as species rows
  if(length(spp_var_no) > 0) {
    df_no_species <- data[data$species_bin %in% stats::na.omit(spp_var_no),]

    # Check language safety ensuring the IF statement evaluates a single string element [1]
    if(data$language[1] == "en"){
      df_no_species$taxonRank <- "species"
    } else {
      df_no_species$taxonRank <- "especie"
    }

    # Generate new random identifier IDs avoiding existing database values
    df_no_species$id <- sample(setdiff(1:50000, data$id), nrow(df_no_species))
    # Subset matching data columns
    df_no_species <- df_no_species[, colnames(data)]
    # Bind remaining tables
    data_solved <- rbind(data_solved, df_no_species)
  }

  # Append custom metadata attribute status flag
  attr(data_solved, "solved_discrepancies") <- TRUE

  # Clean up trailing or loose "Unknown" string placeholders when valid metadata is present
  columns <- c("lifeForm", "habitat", "states", "origin")
  for(i in columns) {
    # Vectorized substitution directly on the target column matrix for top speed
    data_solved[[i]] <- gsub(";Unknown|Unknown;", "", data_solved[[i]])
  }

  # Remove temporary column containing the binomial name
  data_solved$species_bin <- NULL

  # Ensure the object returned keeps the data.table class attributes
  if (!data.table::is.data.table(data_solved)) {
    data.table::setDT(data_solved)
  }

  # Return the resolved dataset to the workspace
  return(data_solved)
}
