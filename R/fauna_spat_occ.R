#' Get Spatial polygons (SpatVectors) of species based on its distribution
#' (states and countrys) according to Fauna do Brasil
#'
#' @param data (data.frame) the data.frame imported with the
#' \code{\link{load_faunabr}} function.
#' @param species (character) one or more species names (only genus and
#' specific epithet, eg. "Panthera onca")
#' @param state (logical) get SpatVector of states with occurrence of the
#' species? Default = TRUE
#' @param country (logical) get SpatVector of countrys with occurrence of the
#' species? Default = TRUE
#' @param state_and_country (character) get a Spatvector representing both states
#' and countrys with occurrence of the specie?
#' To use state_and_country = TRUE, you must define state = TRUE and country = TRUE".
#' Default = TRUE
#' @param verbose (logical) Whether to display species being filtered during
#' function execution. Set to TRUE to enable display, or FALSE to run silently.
#' Default = TRUE.
#'
#' @return A list with SpatVectors of states and/or countrys and/or both for each
#' specie.
#' @importFrom terra subset unwrap intersect mask
#' @importFrom data.table rbindlist
#' @importFrom stats na.omit
#' @export
#' @examples
#' library(terra)
#' data("fauna_data")
#' spp <- c("Panthera onca", "Mazama jucunda")
#' #Get states, countrys and intersection states-countrys of species
#' spp_spt <- fauna_spat_occ(data = fauna_data, species = spp, state = TRUE,
#'                           country = TRUE, state_and_country = TRUE,
#'                           verbose = TRUE)
#' #Plot states with confirmed occurrence of Panthera onca and Mazama jucunda
#' plot(spp_spt$`Panthera onca`$states)
#' plot(spp_spt$`Mazama jucunda`$states)
#' #Plot countries with confirmed occurrence of Panthera onca and Mazama jucunda
#' plot(spp_spt$`Panthera onca`$countries)
#' plot(spp_spt$`Mazama jucunda`$countries)
#' #Plot countries and states with confirmed occurrence of Panthera onca and Mazama jucunda
#' plot(spp_spt$`Panthera onca`$states_countries)
#' plot(spp_spt$`Mazama jucunda`$states_countries)
#'
fauna_spat_occ <- function(data, species, state = TRUE,
                           country = TRUE,
                           state_and_country = TRUE,
                           verbose = TRUE) {
  if (missing(data)) {
    stop("Argument data is not defined")
  }

  if (missing(species)) {
    stop("Argument occ is not defined")
  }

  if (!inherits(data, "data.frame")) {
    stop(paste0("Argument data must be a data.frame, not ", class(data)))
  }

  if (!is.character(species)) {
    stop(paste0("Argument species must be a character, not ", class(species)))
  }

  if (!is.logical(state)) {
    stop(paste0("Argument state must be logical, not ", class(state)))
  }

  if (!is.logical(country)) {
    stop(paste0("Argument country must be logical, not ", class(country)))
  }

  if (!is.logical(state_and_country)) {
    stop(paste0("Argument state_and_country must be logical, not ",
                class(state_and_country)))
  }

  if (!is.logical(verbose)) {
    stop(paste0("Argument verbose must be logical, not ", class(verbose)))
  }

  #Check colnames in data
  if(!all(c("species", "states", "countryCode") %in%
          colnames(data))) {
    stop("Important columns are missing in data. Check if data is an object
         created by 'load_faunabr()")
  }

  #Check if there is at least one TRUE in states or countrys
  if(!state & !country){
    stop("At least one of the parameters state or country must be TRUE")
  }
  if(state_and_country & (!state | !country)) {
    stop("To use state_and_country = TRUE, you must define state = TRUE and
         country = TRUE")
  }

  #Load data
  d <- data[,c("species", "states", "countryCode")]

  #Check if all species are in fauna do Brasil data
  spp <- extract_binomial(species_names = species)
  #Get binomial names of species
  spp_out <- setdiff(spp, unique(data$species))
  if(length(spp_out) > 0) {
    stop(paste(length(spp_out), "species are not in the data. Check the species
               names using the check_fauna_names() function"))
  }
  #Subset info
  d_info <- subset(d, d$species %in% spp)
  d_info[d_info == ""] <- NA

  #Get only one line by species, merging information of same species
  sp_info <- lapply(seq_along(spp), function(i) {
    sp <- subset(d_info, d_info$species == spp[i])
    sp$states <- paste0(stats::na.omit(unique(sp$states)),
                        collapse = ";")
    sp$country <- paste0(stats::na.omit(unique(sp$country)),
                       collapse = ";")
    return(sp)
  })
  sp_info <- unique(data.table::rbindlist(sp_info))

  #Load data
  if(state) {
    states <- terra::unwrap(faunabr::states)
  }
  if(country) {
    countrys <- terra::unwrap(faunabr::world_fauna)
  }

  #Get state and countries
  l_occ <- lapply(seq_along(spp), function(i){
    occ_i <- subset(sp_info, sp_info$species == spp[i])

    if(!state) {states_v <- NULL}

    if(state) {
      if(verbose) {
        message("Getting states of ", spp[i], "\n") }

      sp_i_state <- unique(gsub(";", "|", occ_i$states[1]))

      if(sp_i_state == "" | is.na(sp_i_state)) {
        if(verbose) {
          message(spp[i], "lacks info about state - SpatialVector not
                  returned")}
        states_v <- "No_info"
      } else {
        states_v <- terra::subset(states, grepl(sp_i_state,
                                                states$abbrev_state)) }
    }

    if(!country) {countrys_v <- NULL}

    if(country) {
      if(verbose) {
        message("Getting countries of ", spp[i], "\n") }
      sp_i_country<- unique(gsub(";", "|", occ_i$countryCode[1]))

      if(sp_i_country == "" | is.na(sp_i_country)) {
        if(verbose){
          message(spp[i], "lacks info about country - SpatialVector not
                  returned")}
        countrys_v <- "No_info"
      } else {
        countrys_v <- terra::subset(countrys, grepl(sp_i_country,
                                                countrys$name)) }
    }

    if(!state_and_country) {int_v <- NULL}

    if(state_and_country) {
      if((sp_i_country == "" | is.na(sp_i_country)) & verbose) {
        message(spp[i], "lacks info about states - Impossible to get
                  states and countries")
      }
      if((sp_i_country == "" | is.na(sp_i_country)) & verbose) {
        message(spp[i], "lacks info about countrys - Impossible to get
                  states and countries")
      }

      if(length(countrys_v[countrys_v$name != "brazil"]) > 0){
      int_v <- terra::union(countrys_v[countrys_v$name != "brazil"],
                            states_v)}
      else (int_v <- states_v)
      }

    #Save objects in a list
    final_list <- list(states_v, countrys_v, int_v)
    names(final_list) <- c("states", "countries", "states_countries")
    return(final_list)
  })
  names(l_occ) <- spp
  #Drop off null elements
  return(lapply(l_occ, function(x) x[lengths(x) > 0]))
} #End if function

# #Test function
# library(terra)
# data("fauna_data")
# spp <- c("Panthera onca", "Mazama jucunda")
# #Get states, countrys and intersection states-countrys of species
# spp_spt <- fauna_spat_occ(data = fauna_data, species = spp, state = TRUE,
#                           country = TRUE, state_and_country = TRUE,
#                           verbose = TRUE)
# #Plot states with confirmed occurrence of Panthera onca and Mazama jucunda
# plot(spp_spt$`Panthera onca`$states)
# plot(spp_spt$`Mazama jucunda`$states)
# #Plot countries with confirmed occurrence of Panthera onca and Mazama jucunda
# plot(spp_spt$`Panthera onca`$countrys)
# plot(spp_spt$`Mazama jucunda`$countrys)
# #Plot countries and states with confirmed occurrence of Panthera onca and Mazama jucunda
# plot(spp_spt$`Panthera onca`$states_countrys)
# plot(spp_spt$`Mazama jucunda`$states_countrys)
