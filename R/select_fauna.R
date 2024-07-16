#' Selection of species based on its characteristics and distribution
#'
#' @description select_fauna allows filter species based on its
#' characteristics and distribution available in Brazilian Fauna
#'
#' @param data (data.frame) the data.frame imported with the
#' \code{\link{load_faunabr}} function.
#' @param include_subspecies (logical) include subspecies?
#' Default = FALSE
#' @param phylum (character) The phyla for filtering the dataset. It can
#' be included more than one phylum. Default = "all".
#' @param class (character) The classes for filtering the dataset. It can
#' be included more than one class. Default = "all".
#' @param order (character) The orders for filtering the dataset. It can
#' be included more than one order. Default = "all".
#' @param family (character) The families for filtering the dataset. It can
#' be included more than one family. Default = "all".
#' @param genus (character) The genus for filtering the dataset. It can
#' be included more than one genus. Default = "all".
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
#' @param states (character) The states for filtering the dataset. It can
#' be included more than one state. Default = "all".
#' @param filter_states (character) The type of filtering for states. It
#' can be "in", "only", "not_in" and "and". See Details for more about this
#' argument.
#' @param country (character) The country or countries with confirmed
#' occurrences for filtering the dataset. It can be included more than one
#' country. Default = "all".
#' @param filter_country (character) The type of filtering for country. It can
#' be "in", "only", "not_in" and "and". See details for more about this argument.
#' @param origin (character) The origin for filtering the dataset. It can
#' be "native", "introduced", "cryptogenic", "domesticaded" and "invasora".
#' Default = "all".
#' @param taxonomicStatus (character) The taxonomic status for filtering the
#' dataset. It can be "accepted", "synonym" or "all".
#' Default = "accepted".
#'
#' @details It's possible to choose 4 ways to filter by lifeform, by habitat,
#' by states and by country:
#' "in": selects species that have any occurrence of the determined values. It
#' allows multiple matches. For example, if country = c("brazil", argentina") and
#' filter_country = "in", it will select all species that occur in Brazil and/or
#' Argentina, some of which may also occur in other countries.
#'
#' "only": selects species that have only occurrence of the determined values.
#' It allows only single matches. For example, if
#' country = c("brazil", argentina") and filter_country = "in", it will select
#' all species that occur exclusively in both countries, without any occurrences
#' in other countries.
#'
#' "not_in": selects species that don't have occurrence of the determined
#' values. It allows single and multiple matches. For example,
#' if country = c("brazil", argentina") and filter_country = "not_in", it will select
#' all species without occurrences in Brazil and Argentina.
#'
#' "and": selects species that have occurrence in all determined values. It
#' allows single and multiple matches. For example,
#' if country = c("brazil", argentina") and filter_country = "and", it will select
#' all species that occurs only in both countries,including species that occurs
#' in other countries too.
#'
#' To get the complete list of arguments available for phylum, class, order,
#' family, genus, lifeForm, habitat, states, country and origins, use
#' the function \code{\link{fauna_attributes}}
#'
#' @return A new dataframe with the filtered species.
#' @usage select_fauna(data, include_subspecies = FALSE, phylum = "all",
#'                       class = "all", order = "all", family = "all",
#'                       genus = "all",
#'                       lifeForm = "all", filter_lifeForm = "in",
#'                       habitat = "all", filter_habitat = "in",
#'                       states = "all", filter_states = "in",
#'                       country = "all", filter_country = "in",
#'                       origin = "all", taxonomicStatus = "accepted")
#' @export
#' @references
#' Brazilian Zoology Group. Catálogo Taxonômico da Fauna do Brasil. Available at:
#' https://ipt.jbrj.gov.br/jbrj/resource?r=catalogo_taxonomico_da_fauna_do_brasil
#'
#' @examples
#' data("fauna_data") #Load data example
#' #Select endemic and native species of birds (Aves) with confirmed occurrence
#' #in Brazil or Argentina
#' aves_br_ar <- select_fauna(data = fauna_data, include_subspecies = FALSE,
#'                            phylum = "all", class = "Aves",
#'                            order = "all",
#'                            family = "all",
#'                            genus = "all",
#'                            lifeForm = "all", filter_lifeForm = "in",
#'                            habitat = "all", filter_habitat = "in",
#'                            states = "all", filter_states = "in",
#'                            country = c("brazil", "argentina"),
#'                            filter_country = "in",
#'                            origin = "native",
#'                            taxonomicStatus = "accepted")
select_fauna <- function(data, include_subspecies = FALSE,
                         phylum = "all", class = "all", order = "all",
                         family = "all",
                         genus = "all",
                         lifeForm = "all", filter_lifeForm = "in",
                         habitat = "all", filter_habitat = "in",
                         states = "all", filter_states = "in",
                         country = "all", filter_country = "in",
                         origin = "all",
                         taxonomicStatus = "accepted") {
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
    country <- tolower(trimws(country))
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

  if(!(filter_country %in% c("in", "only", "not_in", "and"))) {
      stop(paste0("Argument filter_country must be:\n",
                  "'in', 'only', 'not_in' or 'and'"))
  }

  if(!(filter_states %in% c("in", "only", "not_in", "and"))) {
      stop(paste0("Argument filter_country must be:\n",
                  "'in', 'only', 'not_in' or 'and'"))
  }

  if(all(phylum != "all") & !(phylum %in% unique(data$phylum))) {
    stop(paste("phylum not valid. The phylums availables are:\n",
               paste(unique(data$phylum), collapse = ", ")))  }

  # if(all(class != "all") & !(class %in% unique(data$class))) {
  #   stop(paste("class not valid. Check the available classes with the function
  #              get_fauna_att()"))  }

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


  if(all(origin != "all") & !any(origin %in% unique(data$origin))) {
    stop(paste("origin not valid. The options availables are:\n",
               paste(unique(data$origin), collapse = ", ")))
    }

  if(taxonomicStatus != "all" & !any(taxonomicStatus %in% unique(data$taxonomicStatus))) {
    stop(paste("taxonomicStatus not valid. The options availables are:\n",
               paste(unique(data$taxonomicStatus), collapse = ", ")))
    }

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
    d <- d[rowSums(sapply(newlifeForm, function(x)
      grepl(x, d$lifeForm))) == length(newlifeForm),]
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
    d <- d[rowSums(sapply(newhabitat, function(x)
      grepl(x, d$habitat))) == length(newhabitat),]
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
    d <- d[rowSums(sapply(newstate, function(x)
      grepl(x, d$states))) == length(newstate),]
    }

    #Check if it is a valid country
    if(all(country != "all")) {
      all_country <- unique(unlist(strsplit(d$countryCode, split = ";")))
      newcountry <- gsub(" ", "", country)
      newcountry <- vapply(country, FUN.VALUE = character(1), function(x){
        paste(sort(gsub(" ", "", unlist(strsplit(x, split = ",")))),
              collapse = ";")
      }, USE.NAMES = FALSE)
      newcountry <- sort(newcountry)
      #Check if all country exists
      newcountry2 <- unique(unlist(strsplit(newcountry, split = ";")))
      any_diff <- setdiff(newcountry2 , all_country)
      if(length(any_diff) > 0) {
        warning(paste("The following country/countries is/are not valid:\n",
                      paste(any_diff, collapse = ", ")))
      }
    }


    #Filter by country
    if(all(country != "all") & filter_country == "in") {
      d <- subset(d, grepl(paste(newcountry, collapse = "|"),
                           d$countryCode)) }

    if(all(country != "all") & filter_country == "only") {
      d <- subset(d, d$countryCode == paste(newcountry, collapse = ";"))
    }

    if(all(country != "all") & filter_country == "not_in") {
      d <- subset(d, !grepl(paste(newcountry, collapse = "|"),
                            d$countryCode))

    }

    if(all(country != "all") & filter_country == "and") {
      d <- d[rowSums(sapply(newcountry, function(x)
        grepl(x, d$countryCode))) == length(newcountry),]
    }

  #origin ####
  if(all(origin == "all")) {
    d <- d }

  #Filter by origin
  if(all(origin != "all")) {
    all_origin <- unique(d$origin)
    any_diff <- setdiff(origin, all_origin)
    if(length(any_diff) > 0) {
      warning(paste("The following origins are not available:\n",
                    paste(any_diff, collapse = ", ")))
    }
    d <- d[d$origin %in% origin,] }

  #taxonomicStatus ####
  if(all(taxonomicStatus == "all")) {
    d <- d }

  #Filter by taxonomicStatus
  if(all(taxonomicStatus != "all")) {
    taxonomicStatus2 <- taxonomicStatus
    all_taxonomicStatus <- unique(d$taxonomicStatus)
    any_diff <- setdiff(taxonomicStatus, all_taxonomicStatus)
    if(length(any_diff) > 0) {
      warning(paste("The following taxonomicStatus are not valid:\n",
                    paste(any_diff, collapse = ", ")))
    }
    d <- subset(d, d$taxonomicStatus %in% taxonomicStatus2) }
  if(nrow(d) == 0) {
    warning("Combination of characteristics return 0 species")
  }

  return(d)
} #End of function
