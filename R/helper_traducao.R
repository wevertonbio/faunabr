translate_column_terms <- function(text, map_df, to = "pt_br", sep = ";") {

  if (!is.character(text)) {
    warning("'text' must be a character, not ", class(text))
    text <- as.character(text)
  }

  # Define columns from and to translate
  if (to == "en") {
    source_col_name <- "pt_br"
    target_col_name <- "en"
  } else if (to == "pt_br") {
    source_col_name <- "en"
    target_col_name <- "pt_br"
  }

  # Function to translate
  process_single_string <- function(single_text) {
    if (is.na(single_text) || single_text == "") {
      return(single_text)
    }

    # Split string
    terms <- unlist(strsplit(single_text, split = sep, fixed = TRUE))

    # Set to lower case
    terms_lower <- tolower(terms)

    # Translate
    translated_terms <- map_df[[target_col_name]][match(terms_lower, map_df[[source_col_name]])]

    # Keep NA when is not possible to translate
    translated_terms[is.na(translated_terms)] <- terms[is.na(translated_terms)]

    # Paste terms and collapse with ;
    return(paste(translated_terms, collapse = sep))
  }

  # Apply function to each vector
  translated_vector <- sapply(text, process_single_string, USE.NAMES = FALSE)

  return(translated_vector)
}

# #### Create dataframes with translations ####
# mapa_traducao_lifeform <- data.frame(
#   pt_br = c(
#     "vida_livre_individual", "colonial", "sessil", "herbivoro",
#     "eussocial", "predador", "endoparasitoide", "ectoparasitoide",
#     "hiperparasitoide", "ectoparasito", "endoparasito", "comensal",
#     "epibionte", "polinizador", "inquilino", "mutual",
#     "endoparasiteid", "ectoparasiteid" # Erros comuns ou variações
#   ),
#   en = c(
#     "free_living_individual", "colonial", "sessile", "herbivore",
#     "eusocial", "predator", "endoparasitoid", "ectoparasitoid",
#     "hyperparasitoid", "ectoparasite", "endoparasite", "commensal",
#     "epibiont", "polynizer", "inquiline", "mutualistic",
#     "endoparasitoid", "ectoparasitoid" # Corrigindo para os termos corretos em inglês
#   ),
#   stringsAsFactors = FALSE # Importante para evitar que strings virem fatores
# )
#
# mapa_traducao_origins <- data.frame(
#   pt_br = c(
#     "nativa",
#     "introduzida",
#     "criptogenica",
#     "domesticada",
#     "invasora"
#   ),
#   en = c(
#     "native",
#     "introduced",
#     "cryptogenic",
#     "domesticated",
#     "invasive"
#   ),
#   stringsAsFactors = FALSE
# )
#
# # Mapa de Tradução para Habitat
# mapa_traducao_habitat <- data.frame(
#   pt_br = c(
#     "agua_doce",
#     "marinho",
#     "terrestre",
#     "arboreo",
#     "fossorial",
#     "cavernicola",
#     "aguas_subterraneas",
#     "nidicola",
#     "hiporreico"
#   ),
#   en = c(
#     "freshwater",
#     "marine",
#     "terrestrial",
#     "arboreal",
#     "fossorial",
#     "cavernicolous",
#     "subterranean waters",
#     "nidicolous",
#     "hyporheic"
#   ),
#   stringsAsFactors = FALSE
# )
#
# # Mapa de Tradução para Taxon Rank
# mapa_traducao_taxonrank <- data.frame(
#   pt_br = c(
#     "filo",
#     "classe",
#     "ordem",
#     "sub_ordem",
#     "familia",
#     "sub_familia",
#     "tribo",
#     "genero",
#     "sub_genero",
#     "especie",
#     "sub_especie",
#     "sub_classe",
#     "infra_ordem",
#     "super_familia",
#     "super_classe",
#     "sub_tribo",
#     "super_ordem",
#     "infra_classe",
#     NA # Para o caso de NA -> "unknown"
#   ),
#   en = c(
#     "phylum",
#     "class",
#     "order",
#     "suborder",
#     "family",
#     "subfamily",
#     "tribe",
#     "genus",
#     "subgenus",
#     "species",
#     "subspecies",
#     "subclass",
#     "infraorder",
#     "superfamily",
#     "superclass",
#     "subtribe",
#     "superorder",
#     "infraclass",
#     "unknown" # Mapeia NA para "unknown"
#   ),
#   stringsAsFactors = FALSE
# )
#
# mapa_traducao_taxonomicStatus <- data.frame(pt_br = c("valido", "sinonimo"),
#                                             en = c("valid", "synonym")
#                                             )
#
# #Save in list
# l <- list(lifeForm = mapa_traducao_lifeform,
#           origin = mapa_traducao_origins,
#           habitat = mapa_traducao_habitat,
#           taxonRank = mapa_traducao_taxonrank,
#           taxonomicStatus = mapa_traducao_taxonomicStatus)
# sapply(l, colnames)
# map_translation <- l
#
# #Save as data
# usethis::use_data(map_translation, overwrite = TRUE)
