#' Identify records outside natural ranges according to Fauna do Brasil
#'
#' @description This function removes or flags records outside of the species'
#' natural ranges according to information provided by the Fauna do Brasil database
#'
#' @param data (data.frame) the data.frame imported with the
#' \code{\link{load_faunabr}} function.
#' @param occ (data.frame) a data.frame with the records of the species.
#' @param species (character) column name in occ with species names.
#' Default = "species"
#' @param long (character) column name in occ with longitude data. Default = "x"
#' @param lat (character) column name in occ with latitude data. Default = "y"
#' @param by_state (logical) filter records by state? Default = TRUE
#' @param buffer_state (numeric) buffer (in km) around the polygons of the
#' states of occurrence of the specie. Default = 20.
#' @param by_country (logical) filter records by country? Default = TRUE
#' @param buffer_country (numeric) buffer (in km) around the polygons of the
#' countries of occurrence of the specie. Default = 20.
#' @param value (character) Defines output values. See Value section.
#' Default = "flag&clean".
#' @param keep_columns (logical) if TRUE, keep all the original columns of the
#' input occ. If False, keep only the columns species, long and lat.
#' Default = TRUE
#' @param verbose (logical) Whether to display species being filtered during
#' function execution. Set to TRUE to enable display, or FALSE to run silently.
#' Default = TRUE.
#' @details
#' If by_state = TRUE and/or by_country = TRUE, the function takes polygons
#' representing the states and/or countrys with confirmed occurrences of the
#' specie, draws a buffer around the polygons, and tests if the records of the
#' species fall inside it.
#'
#'
#' @return Depending on the 'value' argument. If value = "flag", it returns the
#' same data.frame provided in data with additional columns indicating if the
#' record falls inside the natural range of the specie (TRUE) or outside
#' (FALSE).
#' If value = "clean", it returns a data.frame with only the records that passes
#' all the tests (TRUE for all the filters). If value = "flag&clean" (Default),
#' it returns a list with two data.frames: one with the flagged records and one
#' with the cleaned records.
#' @usage filter_faunabr(data, occ, species = "species", long = "x", lat = "y",
#'                       by_state = TRUE, buffer_state = 20, by_country = TRUE,
#'                       buffer_country = 20, value = "flag&clean",
#'                       keep_columns = TRUE, verbose = TRUE)
#' @export
#'
#' @importFrom terra aggregate subset buffer unwrap mask as.data.frame vect
#' @importFrom data.table rbindlist
#' @importFrom stats na.omit
#' @examples
#' #Test function
#' data("fauna_data") #Load fauna e Funga do Brasil data
#' data("occurrences") #Load occurrences
#' pts <- subset(occurrences, species == "Panthera onca")
#' fd <- filter_faunabr(data = fauna_data,
#'                      occ = pts, long = "x", lat = "y", species = "species",
#'                      by_state = TRUE, buffer_state = 20,
#'                      by_country = TRUE, buffer_country = 20,
#'                      value = "flag&clean", keep_columns = TRUE,
#'                      verbose = FALSE)
filter_faunabr <- function(data,
                           occ,
                           species = "species", long = "x", lat = "y",
                           by_state = TRUE, buffer_state = 20,
                           by_country = TRUE, buffer_country = 20,
                           value = "flag&clean", keep_columns = TRUE,
                           verbose = TRUE) {
  if (missing(data)) {
    stop("Argument data is not defined")
  }
  if (missing(occ)) {
    stop("Argument occ is not defined")
  }
  if (!inherits(data, "data.frame")) {
    stop(paste0("Argument data must be a data.frame, not ",
                class(data)))
  }
  if (!inherits(occ, "data.frame")) {
    stop(paste0("Argument occ must be a data.frame, not ",
                class(occ)))
  }
  if (!is.character(species)) {
    stop(paste0("Argument species must be a character, not ",
                class(species)))
  }
  if (!is.character(long)) {
    stop(paste0("Argument long must be a character, not ",
                class(long)))
  }
  if (!is.character(lat)) {
    stop(paste0("Argument lat must be a character, not ",
                class(lat)))
  }
  if (!is.logical(by_state)) {
    stop(paste0("Argument by_state must be logical, not ",
                class(by_state)))
  }
  if (!is.logical(by_country)) {
    stop(paste0("Argument by_country must be logical, not ",
                class(by_country)))
  }

  if (!is.numeric(buffer_state)) {
    stop(paste0("Argument buffer_state must be numeric, not ",
                class(buffer_state)))
  }
  if (!is.numeric(buffer_country)) {
    stop(paste0("Argument buffer_country must be numeric, not ",
                class(buffer_country)))
  }
  allowed_values <- c("flag&clean", "flag", "clean")
  if (!(value %in% allowed_values)) {
    stop("Argument value must be 'flag', 'clean' or 'flag&clean'")
  }
  if (!is.logical(keep_columns)) {
    stop(paste0("Argument keep_columns must be logical, not ",
                class(keep_columns)))
  }
  if (!is.logical(verbose)) {
    stop(paste0("Argument verbose must be logical, not ",
                class(verbose)))
  }

  #Convert colnames to lower case
  original_colnames <- colnames(data)
  colnames(data) <- tolower(colnames(data))

  if (!all(c("species", "states", "countrycode") %in%
           colnames(data))) {
    stop("Important columns are missing in data. Check if data is an object\n created by 'load_faunabr()")
  }
  if (!all(c(species, long, lat) %in% colnames(occ))) {
    stop("Important columns are missing in occurrence data. Check if correct\n         column names were set in species, long and lat")
  }
  d <- data[, c("species", "states", "countrycode")]
  occ$id_f <- seq_len(nrow(occ))
  occ_info <- occ[, c(species, long, lat, "id_f")]
  colnames(occ_info) <- c("species", "x", "y", "id_f")
  spp <- unique(occ_info$species)
  spp_out <- setdiff(spp, unique(data$species))
  if (length(spp_out) > 0) {
    stop(paste(length(spp_out), "species are not in the data. Check the species
names using the check_names() function or remove the species from
data.frame"))
  }
  d_info <- subset(d, d$species %in% unique(occ_info$species))
  d_info[d_info == ""] <- NA
  sp_info <- lapply(seq_along(spp), function(i) {
    sp <- subset(d_info, d_info$species == spp[i])
    sp$states <- paste0(na.omit(unique(sp$states)), collapse = ";")
    sp$countrycode <- paste0(na.omit(unique(sp$countrycode)), collapse = ";")
    return(sp)
  })
  sp_info <- unique(data.table::rbindlist(sp_info))
  occ_info <- merge(occ_info, sp_info, by = "species")
  occ_info <- terra::vect(occ_info, geom = c("x", "y"),
                          crs = "+init=epsg:4326")

  #Get vector of states
  states <- terra::unwrap(faunabr::states)

  #Get vector of countries
  countrys <- terra::unwrap(faunabr::world_fauna)

  #Buffer around Brazil (to select records inside Brazil)
  if(by_state){
  br_v <- terra::buffer(countrys[countrys$name == "brazil"],
                        width = buffer_state * 1000)

  #Check records inside Brazil
  occ_info$inside_br <- terra::is.related(occ_info, states, "within")
  } else {
    occ_info$inside_br <- NA
  }

  if (by_state) {
    l_state <- lapply(seq_along(spp), function(i) {
      if (verbose) {
        message("Filtering ", spp[i], " by state\n")
      }
      occ_i <- occ_info[occ_info$species == spp[i]]
      sp_i_state <- unique(gsub(";", "|", occ_i$states[1]))
      if (sp_i_state == "" | is.na(sp_i_state)) {
        if (verbose) {
          message(spp[i], "lacks info about state - Filter not applicable\n")
        }
        states_final <- occ_i
        states_final$inside_state <- "No info"
      }
      else {
        states_v <- terra::aggregate(terra::subset(states,
                                                   grepl(sp_i_state, states$abbrev_state)))
        states_v <- terra::buffer(states_v, width = buffer_state *
                                    1000)
        #Create columns
        occ_i$inside_state <- NA
        #Fill inside_state of records inside br
        occ_i$inside_state[occ_i$inside_br] <- terra::is.related(occ_i[occ_i$inside_br],
                                                                 states_v,
                                                                 "intersects")
      }
      return(occ_i)
    })
    occ_info <- do.call("rbind", l_state)
  }
  if (!by_country) {
    occ_info$inside_country <- NA
  } else {
    l_country <- lapply(seq_along(spp), function(i) {
      if (verbose) {
        message("Filtering ", spp[i], " by country\n")
      }
      occ_i <- terra::subset(occ_info, occ_info$species ==
                               spp[i])
      sp_i_country <- unique(gsub(";", "|", occ_i$countrycode[1]))
      if (sp_i_country == "" | is.na(sp_i_country)) {
        if (verbose) {
          message(spp[i], "lacks info about country - Filter not applicable\n")
        }
        countrys_final <- occ_i
        countrys_final$inside_country <- "No info"
      }
      else {
        countrys_v <- terra::aggregate(terra::subset(countrys,
                                                   grepl(sp_i_country, countrys$name)))
        countrys_v <- terra::buffer(countrys_v, width = buffer_country *
                                    1000)
        #Create columns
        occ_i$inside_country <- NA
        #Fill inside country
        occ_i$inside_country <- terra::is.related(occ_i, countrys_v, "intersects")
      }
      return(occ_i)
    })
    occ_info <- do.call("rbind", l_country)
  }

  occ_flag <- as.data.frame(occ_info)

  if (keep_columns) {
    occ_flag <- merge(occ_flag, occ, by = c("species", "id_f"))
    occ_flag$id_f <- NULL
    occ_flag <- occ_flag[, c(species, long, lat, colnames(occ_flag)[!(colnames(occ_flag) %in%
                                                                        c(species, long, lat))])]
    colnames(occ_flag)[colnames(occ_flag) %in% c(species,
                                                 long, lat)] <- c(species, long, lat)
  }
  if (!keep_columns) {
    occ_flag <- merge(occ_flag, occ[, c(species, lat, long,
                                        "id_f")], by = c("species", "id_f"))
    occ_flag$id_f <- NULL
    occ_flag <- occ_flag[, c(species, long, lat, names(occ_flag)[!(names(occ_flag) %in%
                                                                     c(species, long, lat))])]
    colnames(occ_flag)[colnames(occ_flag) %in% c(species,
                                                 long, lat)] <- c(species, long, lat)
  }

  #Remove columns
  occ_flag$inside_br <- NULL

  if (!by_state) {
    occ_flag$inside_state <- NULL
  }
  if (!by_country) {
    occ_flag$inside_country <- NULL
  }
  col_check <- intersect(c("inside_state", "inside_country",
                           "inside_br"), names(occ_flag))
  if (length(col_check) == 1) {
    occ_flag$filters_ok <- ifelse(occ_flag[, col_check] ==
                                    TRUE | is.na(occ_flag[, col_check]), TRUE, FALSE)
  }  else {
    occ_flag$filters_ok <- ifelse(rowSums(!is.na(occ_flag[,
                                                          col_check]) & occ_flag[, col_check] == FALSE) >
                                    0, FALSE, TRUE)
  }
  occ_clean <- subset(occ_flag, occ_flag$filters_ok == TRUE)
  occ_clean <- subset(occ_clean, select = setdiff(colnames(occ_clean),
                                                  c(col_check, "filters_ok")))
  if (value == "flag&clean") {
    res_final <- list(occ_flag, occ_clean)
    names(res_final) <- c("flagged", "cleaned")
    if (verbose) {
      message("Returning list with flagged and cleaned occurrences\n")
    }
  }
  if (value == "flag") {
    res_final <- occ_flag
    if (verbose) {
      message("Returning dataframe with flagged occurrences\n")
    }
  }
  if (value == "clean") {
    res_final <- occ_clean
    if (verbose) {
      message("Returning dataframe with cleaned occurrences")
    }
  }
  return(res_final)
}

# #Test function
# data("fauna_data") #Load fauna e Funga do Brasil data
# data("occurrences") #Load occurrences
# pts <- subset(occurrences, species == "Panthera onca")
# fd <- filter_faunabr(data = fauna_data,
#                      occ = pts, long = "x", lat = "y", species = "species",
#                      by_state = TRUE, buffer_state = 20,
#                      by_country = TRUE, buffer_country = 20,
#                      value = "flag&clean", keep_columns = TRUE,
#                      verbose = FALSE)

