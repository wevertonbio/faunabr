#' Translate information in Brazilian Fauna database
#'
#' @description
#' This function translates information in the "lifeForm", "origin", "habitat",
#' "taxonRank", and "taxonomicStatus" columns between Portuguese and English.
#'
#' @param data (data.frame) the data.frame imported with the
#' \code{\link{load_faunabr}} function.
#' @param map_list (list) A list of data.frames used for translation. The default
#'   is NULL, which means it uses `faunabr::map_translation`. If not NULL, its
#'   structure (list names and data.frame column names) must be identical
#'   to `faunabr::map_translation`.
#' @param to (character) The target language for translation. Available options
#'   are "en" to translate from Portuguese to English, and "pt_br" to translate
#'   from English to Portuguese. The default is "en".
#'
#' @return A data.frame with the values in the "lifeForm", "origin", "habitat",
#'   "taxonRank", and "taxonomicStatus" columns translated.
#' @export
#'
#' @examples
#' data("fauna_data") #Load data example (in English)
#' #Translate to Portuguese
#' fauna_portugues <- translate_faunabr(data = fauna_data, to = "pt_br")
#' # See attributes of lifeForm in Portuguese
#' fauna_attributes(fauna_portugues, attribute = "lifeForm")
#'
translate_faunabr <- function(data, map_list = NULL, to = "en"){
  # Test data
  if (!is.data.frame(data)) {
    stop("data must be a 'data.frame', not ", class(data))
  }

  if(!(to %in% c("en", "pt_br"))){
    stop("'to' must be 'en' or 'pt_br'")
  }

  if("language" %in% colnames(data)){
    if(to == unique(data$language)){
      stop("The data is already in '", to, "'")
  }}

  if(!is.null(map_list)){
    if(!inherits(map_list, "list")){
      stop("map_list must be a 'list' with data.frames, not ", class(map_list))
    }
    names_out <- setdiff(c("lifeForm", "origin", "habitat", "taxonRank"),
                         names(map_list))
    if(length(names_out) > 0){
      stop("At least one of the data.frames within 'map_list' must be named as:
      'lifeForm', 'origin', 'habitat', or 'taxonRank'")
    }
  }

  if(is.null(map_list))
    map_list <- faunabr::map_translation

  # Iterate on 'map_list' names
  for (col_name in names(map_list)) {
    # Check if columns exists
    if (col_name %in% names(data) && !is.null(map_list[[col_name]])) {

      current_map <- map_list[[col_name]]

      # Apply function to translate
      data[[col_name]] <- translate_column_terms(
        text = data[[col_name]],
        map_df = current_map,
        to = to,
        sep = ";"
      )

      # Replace empty strings with NA
      data[[col_name]][data[[col_name]] == ""] <- NA

      #Replace NA with unknown in taxonRank
      if (col_name == "taxonRank" && to == "en") { # Assume que "unknown" é a tradução para NA em inglês
        data[[col_name]][is.na(data[[col_name]])] <- "unknown"
      } else if (col_name == "taxonRank" && to == "pt_br") { # Se traduzindo de EN para PT e tem "unknown"
        data[[col_name]][is.na(data[[col_name]])] <- "desconhecido"
      }
             }
  }

  #Update language
  data$language <- to

  return(data)
}
