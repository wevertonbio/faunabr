#' Selection of species based on its characteristics and distribution
#'
#' @description select_species allows filter species based on its
#' characteristics and distribution available in Brazilian Flora 2020
#'
#' @param data (data.frame) the data.frame imported with the
#' \code{\link{load_florabr}} function.
#' @param include_subspecies (logical) include subspecies?
#' Default = FALSE
#' @param include_variety (logical) include varieties of the species?
#' Default = FALSE
#' @param Kingdom (character) The Kingdom for filtering the dataset. It can be
#' "Plantae" or "Fungi". Default = "Plantae". To include both,
#' use c("Plantae", "Fungi")
#' @param Group (character) The groups for filtering the datasets. It can be
#' "Fungi", "Angiosperms", "Gymnosperms", "Ferns and Lycophytes",
#' "Bryophytes" and "Algae". To use more than one group, put the available
#' items in a vector, for example: Group = c(Angiosperms", "Gymnosperms").
#' Default = "all".
#' @param Subgroup (character) The subgroups for filtering the dataset.
#' Only available if the Group is "Fungi" or "Bryophytes". For Fungi, it can be
#' "stricto sensu" or "lato sensu". For Bryophytes, it can be "Mosses",
#' "Hornworts" and "Liverworts" . To use more than one group, put the available
#' items in a vector, for example: Subgroup = c("Mosses", "Hornworts").
#' Default = "all".
#' @param Family (character) The families for filtering the dataset. It can
#' be included more than one Family. Default = "all".
#' @param Genus (character) The genus for filtering the dataset. It can
#' be included more than one Genus. Default = "all".
#' @param lifeForm (character) The life forms for filtering the dataset. It can
#' be included more than one lifeForm. Default = "all"
#' @param filter_lifeForm (character) The type of filtering for life forms. It
#' can be "in", "only", "not_in" and "and". See details for more about this
#' argument.
#' @param habitat (character) The life habitat for filtering the dataset. It can
#' be included more than one habitat. Default = "all"
#' @param filter_habitat (character) The type of filtering for habitat. It
#' can be "in", "only", "not_in" and "and". See details for more about this
#' argument.
#' @param Biome (character) The biomes for filtering the dataset. It can
#' be included more than one biome. Default = "all"
#' @param filter_Biome (character) The type of filtering for biome. It
#' can be "in", "only", "not_in" and "and". See details for more about this
#' argument.
#' @param states (character) The states for filtering the dataset. It can
#' be included more than one state. Default = "all".
#' @param filter_state (character) The type of filtering for states. It
#' can be "in", "only", "not_in" and "and". See Details for more about this
#' argument.
#' @param VegetationType (character) The vegetation types for filtering the
#' dataset. It can be included more than one vegetation type. Default = "all".
#' @param filter_Vegetation (character) The type of filtering for
#' vegetation type. It can be "in", "only", "not_in" and "and". See details for
#' more about this argument.
#' @param Endemism (character) The endemism (endemic or non-endemic to Brazil)
#' for filtering the dataset. It can be "all", "Endemic" or "Non-endemic".
#' Default = "all".
#' @param origin (character) The origin for filtering the dataset. It can
#' be "all", "Native", "Cultivated" and "Naturalized". Default = "all".
#' @param taxonomicStatus (character) The taxonomic status for filtering the
#' dataset. It can be "all", "Accepted" or "Synonym". Default = "Accepted".
#' @param nomenclaturalStatus (character) The nomenclatural status for
#' filtering the dataset. Default = "Accepted"
#'
#' @details It's possible to choose 4 ways to filter by lifeform, by habitat,
#' by biome, by states and by vegetation type:
#' "in": selects species that have any occurrence of the determined values. It
#' allows multiple matches. For example, if Biome = c("Amazon", Cerrado" and
#' filter_Biome = "in", it will select all species that occur in the Amazon and
#' Cerrado, some of which may also occur in other biomes.
#'
#' "only": selects species that have only occurrence of the determined values.
#' It allows only single matches. For example, if Biome = c("Amazon", "Cerrado")
#' and filter_Biome = "only", it will select all species that occur exclusively
#' in both the Amazon and Cerrado biomes, without any occurrences in other
#' biomes.
#'
#' "not_in": selects species that don't have occurrence of the determined
#' values. It allows single and multiple matches. For example,
#' if Biome = c("Amazon", "Cerrado") and filter_Biome = "not_in", it will select
#' all species without occurrences in the Amazon and Cerrado biomes.
#'
#' "and": selects species that have occurrence in all determined values. It
#' allows single and multiple matches. For example,
#' if Biome = c("Amazon", "Cerrado") and filter_Biome = "and", it will select
#' all species that occurs only in both the Amazon and Cerrado biomes,
#' including species that occurs in other biomes too.
#'
#'
#'
#' To get the complete list of arguments available for Family, Genus, lifeForm,
#' habitat, Biome, state, and nomenclaturalStatus, use the function
#' \code{\link{get_attributes}}
#'
#'
#' @return A new dataframe with the filtered species.
#' @usage select_species(data,
#'                       include_subspecies = FALSE, include_variety = FALSE,
#'                       Kingdom = "Plantae", Group = "all", Subgroup = "all",
#'                       Family = "all", Genus = "all",
#'                       lifeForm = "all", filter_lifeForm = "in",
#'                       habitat = "all", filter_habitat = "in",
#'                       Biome = "all", filter_Biome = "in",
#'                       state = "all", filter_state = "in",
#'                       VegetationType = "all", filter_Vegetation = "in",
#'                       Endemism = "all", origin = "all",
#'                       taxonomicStatus = "Accepted")
#' @export
#' @references
#' Brazilian Flora 2020. Jardim Botânico do Rio de Janeiro. Available at:
#' http://floradobrasil.jbrj.gov.br/
#'
#' @examples
#' data("bf_data") #Load Brazilian Flora data
#' #'Select endemic and native species of trees with disjunct occurrence in
#' # Atlantic Forest and Amazon
#' am_af_only <- select_species(data = bf_data,
#'                              include_subspecies = FALSE,
#'                              include_variety = FALSE,
#'                              Kingdom = "Plantae",
#'                              Group = "all", Subgroup = "all",
#'                              Family = "all", Genus = "all",
#'                              lifeForm = "Tree", filter_lifeForm = "only",
#'                              habitat = "all", filter_habitat = "in",
#'                              Biome = c("Atlantic_Forest","Amazon"),
#'                              filter_Biome = "only",
#'                              state = "all", filter_state = "and",
#'                              VegetationType = "all",
#'                              filter_Vegetation = "in",
#'                              Endemism = "Endemic", origin = "Native",
#'                              taxonomicStatus = "all")

select_fauna <- function(data, include_subspecies = FALSE,
                         phylum = "all", class = "all", order = "all",
                           family = "all",
                           genus = "all",
                           lifeForm = "all", filter_lifeForm = "in",
                           habitat = "all", filter_habitat = "in",
                           states = "all", filter_states = "in",
                           origin = "all",
                           taxonomicStatus = "accepted_name") {
  if (missing(data)) {
    stop("Argument data is not defined")
  }

  #Check classes
  if (!inherits(data, "data.frame")) {
    stop(paste0("Argument data must be a data.frame, not ", class(data)))
  }


  if (!is.logical(include_subspecies)) {
    stop(paste0("Argument include_subspecies must be logical, not ",
                class(include_subspecies)))
  }

  #Correct characters, if necessary
    phylum <- firstup(trimws(phylum))
    phylum[phylum == "All"] <- "all"
    class <- firstup(trimws(class))
    class[class == "All"] <- "all"
    order <- firstup(trimws(order))
    order[order == "All"] <- "all"
    family <- firstup(trimws(family))
    family[family == "All"] <- "all"
    genus <- firstup(trimws(genus))
    genus[genus == "All"] <- "all"
    lifeForm <- tolower(trimws(lifeForm))
    habitat <- tolower(trimws(habitat))
    states <- toupper(trimws(states))
    states[states == "ALL"] <- "all"
    origin <- tolower(trimws(origin))
    taxonomicStatus <- tolower(trimws(taxonomicStatus))

  #Check available arguments#
  if(!(filter_lifeForm %in% c("in", "only", "not_in", "and"))) {
    stop(paste0("Argument filter_lifeForm must be:\n",
                "'in', 'only', 'not_in' or 'and'"))
  }
  if(!(filter_habitat %in% c("in", "only", "not_in", "and"))) {
    stop(paste0("Argument filter_habitat must be:\n",
                "'in', 'only', 'not_in' or 'and'"))
  }

  if(all(phylum != "all") & !(phylum %in% unique(data$phylum))) {
    stop(paste("phylum not valid. The phylums availables are:\n",
               paste(unique(data$phylum), collapse = ", ")))  }

  if(all(class != "all") & !(class %in% unique(data$class))) {
    stop(paste("class not valid. Check the available classes with the function
               get_fauna_att()"))  }

  if(all(order != "all") & !(order %in% unique(data$order))) {
    stop(paste("order not valid. Check the available orders with the function
               get_fauna_att()"))  }

  if(all(family != "all") & !all(family %in% unique(data$family))) {
    stop(paste("Family not valid.\n",
               "Check the available families with the function
               get_fauna_att()")
         ) }

  if(genus != "all" & !(genus %in% unique(data$genus))) {
    stop(paste("Genus not valid.\n")) }


  if(origin != "all" & !(origin %in% c('all', "native", "introduced",
                                       "cryptogenic", "domesticated"))) {
    stop(paste("origin not valid. The options availables are:\n",
               "all', 'native', 'introduced', 'cryptogenic', or 'domesticated'")
    )}

  if(taxonomicStatus != "all" &
     !(taxonomicStatus %in% c('all', 'accepted_name', 'synonym'))) {
    stop(paste("taxonomicStatus not valid. The options availables are:\n",
               "'all', 'accepted_name', or 'synonym'"))}

  #Start to filter...
    #Taxon Rank
    if(!include_subspecies) {
      d <- subset(data, data$taxonRank == "species") }
    if(include_subspecies) {
      d <- subset(data, data$taxonRank %in% c("species", "subspecies")) }

    #phylum
    if(all(phylum != "all")) {
    d <- d[which(d$phylum %in% phylum),]}

    #class
    if(all(class != "all")) {
    d <- d[which(d$class %in% class),]}

    #order
    if(all(order != "all")) {
      d <- d[which(d$order %in% order),]}

    #family
    if(all(family != "all")) {
      d <- d[which(d$family %in% family),]}

    #genus
    if(all(genus != "all")) {
      d <- d[which(d$genus %in% genus),]}


  #lifeForm ####
  if(all(lifeForm  == "all")) {
    d <- d }

  #Check if it is a valid lifeForm
  if(all(lifeForm != "all")) {
    all_lf <- unique(unlist(strsplit(d$lifeForm, split = ";")))
    newlifeForm <- gsub(" ", "", lifeForm)
    newlifeForm <- vapply(lifeForm, FUN.VALUE = character(1), function(x){
      paste(sort(gsub(" ", "", unlist(strsplit(x, split = ",")))),
            collapse = ";")
    }, USE.NAMES = FALSE)
    newlifeForm <- sort(newlifeForm)
    #Check if all lifeform exists
    newlifeForm2 <- unique(unlist(strsplit(newlifeForm, split = ";")))
    any_diff <- setdiff(newlifeForm2 , all_lf)
    if(length(any_diff) > 0) {
      warning(paste("The following life forms are not valid:\n",
                    paste(any_diff, collapse = ", ")))
    }
  }

  #Filter by lifeform
  if(all(lifeForm != "all") & filter_lifeForm == "in") {
    d <- subset(d, grepl(paste(newlifeForm, collapse = "|"),
                         d$lifeForm)) }

  if(all(lifeForm != "all") & filter_lifeForm == "only") {
    d <- subset(d, d$lifeForm == paste(newlifeForm, collapse = ";"))
  }

  if(all(lifeForm != "all") & filter_lifeForm == "not_in") {
    d <- subset(d, !grepl(paste(newlifeForm, collapse = "|"),
                          d$lifeForm))
  }

  if(all(lifeForm != "all") & filter_lifeForm == "and") {
    d <- subset(d, grepl(paste(newlifeForm, collapse = ";"), d$lifeForm ))
  }


  #habitat ####
  if(all(habitat  == "all")) {
    d <- d }

  #Check if it is a valid habitat
  if(all(habitat != "all")) {
    all_hab <- unique(unlist(strsplit(d$habitat, split = ";")))
    newhabitat <- gsub(" ", "", habitat)
    newhabitat <- vapply(habitat, FUN.VALUE = character(1), function(x){
      paste(sort(gsub(" ", "", unlist(strsplit(x, split = ",")))),
            collapse = ";")
    }, USE.NAMES = FALSE)
    newhabitat <- sort(newhabitat)
    #Check if all habitat exists
    newhabitat2 <- unique(unlist(strsplit(newhabitat, split = ";")))
    any_diff <- setdiff(newhabitat2 , all_hab)
    if(length(any_diff) > 0) {
      warning(paste("The following habitats are not valid:\n",
                    paste(any_diff, collapse = ", ")))
    }
  }

  #Filter by habitat
  if(all(habitat != "all") & filter_habitat == "in") {
    d <- subset(d, grepl(paste(newhabitat, collapse = "|"),
                         d$habitat)) }

  if(all(habitat != "all") & filter_habitat == "only") {
    d <- subset(d, d$habitat == paste(newhabitat, collapse = ";"))
  }

  if(all(habitat != "all") & filter_habitat == "not_in") {
    d <- subset(d, !grepl(paste(newhabitat, collapse = "|"),
                          d$habitat))
  }

  if(all(habitat != "all") & filter_habitat == "and") {
    d <- subset(d, grepl(paste(newhabitat, collapse = ";"), d$habitat))
  }

  #states ####
  if(all(states  == "all")) {
    d <- d}

  #Check if it is a valid state
  if(all(states != "all")) {
    all_state <- unique(unlist(strsplit(d$states, split = ";")))
    newstate <- gsub(" ", "", states)
    newstate <- vapply(states, FUN.VALUE = character(1), function(x){
      paste(sort(gsub(" ", "", unlist(strsplit(x, split = ",")))),
            collapse = ";")
    }, USE.NAMES = FALSE)
    newstate <- sort(newstate)
    #Check if all state exists
    newstate2 <- unique(unlist(strsplit(newstate, split = ";")))
    any_diff <- setdiff(newstate2 , all_state)
    if(length(any_diff) > 0) {
      warning(paste("The following states are not valid:\n",
                    paste(any_diff, collapse = ", ")))
    }
  }


  #Filter by state
  if(all(states != "all") & filter_states == "in") {
    d <- subset(d, grepl(paste(newstate, collapse = "|"),
                         d$states)) }

  if(all(states != "all") & filter_states == "only") {
    d <- subset(d, d$states == paste(newstate, collapse = ";"))
  }

  if(all(states != "all") & filter_states == "not_in") {
    d <- subset(d, !grepl(paste(newstate, collapse = "|"),
                          d$states))

  }

  if(all(states != "all") & filter_states == "and") {
    d <- subset(d, grepl(paste(newstate, collapse = ";"), d$states))
  }

  #origin ####
  if(all(origin == "all")) {
    d <- d }

  #Filter by origin
  if(all(origin != "all")) {
    origin2 <- origin
    all_origin <- unique(d$origin)
    any_diff <- setdiff(origin, all_origin)
    if(length(any_diff) > 0) {
      warning(paste("The following origins are not valid:\n",
                    paste(any_diff, collapse = ", ")))
    }
    d <- subset(d, d$origin %in% origin2) }

  #taxonomicStatus ####
  if(all(taxonomicStatus == "all")) {
    d <- d }

  #Filter by taxonomicStatus
  if(all(taxonomicStatus != "all")) {
    taxonomicStatus2 <- taxonomicStatus
    all_taxonomicStatus <- unique(d$taxonomicStatus)
    any_diff <- setdiff(taxonomicStatus, all_taxonomicStatus)
    if(length(any_diff) > 0) {
      warning(paste("The following taxonomicStatuss are not valid:\n",
                    paste(any_diff, collapse = ", ")))
    }
    d <- subset(d, d$taxonomicStatus %in% taxonomicStatus2) }
  if(nrow(d) == 0) {
    warning("Combination of characteristics return 0 species")
  }

  return(d)
} #End of function
