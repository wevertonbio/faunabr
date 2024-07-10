#' Search for taxa using vernacular names
#'
#' @param data (data.frame) the data.frame imported with the
#' \code{\link{load_faunabr}} function or generated with the function
#' \code{\link{select_fauna}}.
#' @param names (character) vernacular name ("Nome comum") of the species to be
#' searched
#' @param exact (logic) if TRUE, the function will search only for exact
#' matches. For example, if names = "veado-mateiro" and exact = TRUE, the function
#' will return only the species popularly known as "veado-mateiro". On the other
#' hand, if names = "veado-mateiro" and exact = FALSE, the function will return
#' other results as "Veado-mateiro-pequeno". Default = FALSE
#'
#' @return a data.frame with the species with vernacular names that match the
#' input names
#' @export
#' @examples
#' data("fauna_data") #Load Fauna do Brasil data
#' #Search for species whose vernacular name is 'veado-mateiro'
#' veado_exact <- fauna_by_vernacular(data = fauna_data,
#'                                    names = "veado-mateiro",
#'                                    exact = TRUE)
#' veado_exact
#' #Search for species whose vernacular name is 'veado_mateiro', allowing non-exact
#' #matches
#' veado_not_exact <- fauna_by_vernacular(data = fauna_data,
#'                                        names = "veado-mateiro",
#'                                       exact = FALSE)
#'
fauna_by_vernacular <- function(data, names,
                                exact = FALSE){
  if (missing(data)) {
    stop("Argument data is not defined")
  }

  if (missing(names)) {
    stop("Argument names is not defined")
  }
  #Check classes
  if (!inherits(data, "data.frame")) {
    stop(paste0("Argument data must be a data.frame, not ", class(data)))
  }

  if (!is.character(names)) {
    stop(paste0("Argument names must be a character, not ", class(names)))
  }

  if (!is.logical(exact)) {
    stop(paste0("Argument exact must be logical, not ", class(exact)))
  }


  #trim and lower case
  names <- trimws(tolower(names))
  #trim and lower case
  new_names <- trimws(tolower(data$vernacularName))

  #If there is - in the pattern, use - in all names
  if(any(grepl("-", names))){
    new_names <- gsub("(?<=\\p{L}) (?=\\p{L})", "-", new_names, perl = TRUE)
  }

  #If there is space in the pattern, use space in all names
  if(any(grepl(" ", names))){
    new_names <- gsub("(?<=\\p{L})-(?=\\p{L})", " ", new_names, perl = TRUE)
  }

  #Subset
  if(!exact){
    vn_index <- grep(names, new_names, fixed = TRUE)
  } else {
    all_names <- strsplit(new_names, ", ")
    vn_index <- which(sapply(all_names, function(x) any(x == names)))
  }

  # has_word <- function(n) {
  #     palavras <- unlist(strsplit(n, ", "))
  #     any(grepl(paste0("^", names, "$"), palavras))
  #   }
  # dv <- dv[apply(dv, 1, function(x) has_word(x["vernacularName"])), ]


  #Subset original data
  dv <- data[vn_index,]

  if(nrow(dv) == 0) {
    stop(paste("There isn't any species in the Fauna do Brasil with this",
    "\nvernacular name"))
  }

  return(dv)
}

#fauna_by_vernacular(data = d, names = "pimenta", exact = T)

# #Test function
# data("fauna_data") #Load Fauna do Brasil data
# #Search for species whose vernacular name is 'veado-mateiro'
# veado_exact <- fauna_by_vernacular(data = fauna_data,
#                                     names = "veado-mateiro",
#                                     exact = TRUE)
# veado_exact
# #Search for species whose vernacular name is 'veado_mateiro', allowing non-exact
# #matches
# veado_not_exact <- fauna_by_vernacular(data = fauna_data,
#                                        names = "veado-mateiro",
#                                        exact = FALSE)


