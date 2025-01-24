#' Extract the binomial name (Genus + specific epithet) from a Scientific Name
#'
#' @param species_names (character) Scientific names to be converted to
#' binomial names
#'
#' @return A vector with the binomial names (Genus + specific epithet).
#' @usage extract_binomial(species_names)
#' @export
#'
#' @examples
#' spp <- c("Panthera onca (Linnaeus, 1758)",
#' "Zonotrichia capensis subtorquata Swainson, 1837",
#' "Paraganaspis egeria Díaz & Gallardo, 1996",
#' "Arrenurus tumulosus intercursor")
#' spp_new <- extract_binomial(species_names = spp)
#' spp_new
#'
extract_binomial <- function(species_names) {
  if (!is.character(species_names)) {
    stop(paste0("Argument species_names must be a character, not ",
                class(species_names)))
  }

  # Remove excess of whitespace between words
  species_names <- gsub("\\s+", " ", species_names)

  # Remove leading and/or trailing whitespace
  species_names <- trimws(species_names)

  selected_species_names <- vapply(species_names, FUN.VALUE = character(1), function(text) {
    # Split words
    words <- strsplit(text, " ")[[1]]
    word_count <- length(words)

    # Check if there are parentesis and if they are after Genus
    if (word_count > 2 && grepl("\\(", words[2])) {
      # Find position
      close_paren_index <- which(grepl("\\)", words))

      # If there are parentesis, keep them
        if (length(close_paren_index) > 0) {
        selected_words <- paste(words[1:close_paren_index], collapse = " ")
        # Adicionar mais uma palavra após os parênteses fechados se houver
        if (word_count >= close_paren_index + 1) {
          selected_words <- paste(selected_words, words[close_paren_index + 1])
        }
        return(selected_words)
      }
    }

    # If not, keep only first two words
    if (word_count > 2) {
      selected_words <- paste(words[1:2], collapse = " ")
      return(selected_words)
    } else {
      return(text)
    }
  })

  names(selected_species_names) <- NULL
  return(selected_species_names)
}
