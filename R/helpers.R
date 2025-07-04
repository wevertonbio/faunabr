#####Helper functions####

#First letter to upper case
firstup <- function(x) {
  x <- tolower(x)
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

# #Translate origin, habitat, lifeform, taxonomicStatus and nomenclaturalStatus
# translate_lifeform <- function(text) {
#   # Aplicar gsub para traduzir e converter para letras minúsculas cada forma de vida em português
#   new_lifeform <- gsub("VIDA_LIVRE_INDIVIDUAL", "free_living_individual", text, ignore.case = TRUE)
#   new_lifeform <- gsub("COLONIAL", "colonial", new_lifeform, ignore.case = TRUE)
#   new_lifeform <- gsub("SESSIL", "sessile", new_lifeform, ignore.case = TRUE)
#   new_lifeform <- gsub("HERBIVORO", "herbivore", new_lifeform, ignore.case = TRUE)
#   new_lifeform <- gsub("EUSSOCIAL", "eusocial", new_lifeform, ignore.case = TRUE)
#   new_lifeform <- gsub("PREDADOR", "predator", new_lifeform, ignore.case = TRUE)
#   new_lifeform <- gsub("ENDOPARASITOIDE", "endoparasitoid", new_lifeform, ignore.case = TRUE)
#   new_lifeform <- gsub("ECTOPARASITOIDE", "ectoparasitoid", new_lifeform, ignore.case = TRUE)
#   new_lifeform <- gsub("HIPERPARASITOIDE", "hyperparasitoid", new_lifeform, ignore.case = TRUE)
#   new_lifeform <- gsub("ECTOPARASITO", "ectoparasite", new_lifeform, ignore.case = TRUE)
#   new_lifeform <- gsub("ENDOPARASITO", "endoparasite", new_lifeform, ignore.case = TRUE)
#   new_lifeform <- gsub("COMENSAL", "commensal", new_lifeform, ignore.case = TRUE)
#   new_lifeform <- gsub("EPIBIONTE", "epibiont", new_lifeform, ignore.case = TRUE)
#   new_lifeform <- gsub("POLINIZADOR", "polynizer", new_lifeform, ignore.case = TRUE)
#   new_lifeform <- gsub("INQUILINO", "inquiline", new_lifeform, ignore.case = TRUE)
#   new_lifeform <- gsub("MUTUAL", "mutualistic", new_lifeform, ignore.case = TRUE)
#   new_lifeform <- gsub("endoparasiteid", "endoparasitoid", new_lifeform, ignore.case = TRUE)
#   new_lifeform <- gsub("ectoparasiteid", "ectoparasitoid", new_lifeform, ignore.case = TRUE)
#   return(new_lifeform)
# }
#
# translate_origins <- function(text) {
#   # Aplicar gsub para traduzir e converter para letras minúsculas cada origem
#   text[text == ""] <- NA
#   new_origins <- gsub("NATIVA", "native", text, ignore.case = TRUE)
#   new_origins <- gsub("INTRODUZIDA", "introduced", new_origins, ignore.case = TRUE)
#   new_origins <- gsub("CRIPTOGENICA", "cryptogenic", new_origins, ignore.case = TRUE)
#   new_origins <- gsub("DOMESTICADA", "domesticated", new_origins, ignore.case = TRUE)
#   new_origins <- gsub("INVASORA", "invasive", new_origins, ignore.case = TRUE)
#   return(new_origins)
# }
#
# translate_habitat <- function(text) {
#   # Aplicar gsub para traduzir e converter para letras minúsculas cada categoria de habitat
#   text[text == ""] <- NA
#   new_habitat <- gsub("AGUA_DOCE", "freshwater", text, ignore.case = TRUE)
#   new_habitat <- gsub("MARINHO", "marine", new_habitat, ignore.case = TRUE)
#   new_habitat <- gsub("TERRESTRE", "terrestrial", new_habitat, ignore.case = TRUE)
#   new_habitat <- gsub("ARBOREO", "arboreal", new_habitat, ignore.case = TRUE)
#   new_habitat <- gsub("FOSSORIAL", "fossorial", new_habitat, ignore.case = TRUE)
#   new_habitat <- gsub("CAVERNICOLA", "cavernicolous", new_habitat, ignore.case = TRUE)
#   new_habitat <- gsub("AGUAS_SUBTERRANEAS", "subterranean waters", new_habitat, ignore.case = TRUE)
#   new_habitat <- gsub("NIDICOLA", "nidicolous", new_habitat, ignore.case = TRUE)
#   new_habitat <- gsub("HIPORREICO", "hyporheic", new_habitat, ignore.case = TRUE)
#   return(new_habitat)}
#
# translate_taxonrank <- function(rank_vector) {
#   # Converter o vetor para caracteres (strings) e tratar NA como um valor especial
#   rank_vector <- as.character(rank_vector)
#
#   # Realizar as substituições diretamente
#   rank_vector[rank_vector == "FILO"] <- "phylum"
#   rank_vector[rank_vector == "CLASSE"] <- "class"
#   rank_vector[rank_vector == "ORDEM"] <- "order"
#   rank_vector[rank_vector == "SUB_ORDEM"] <- "suborder"
#   rank_vector[rank_vector == "FAMILIA"] <- "family"
#   rank_vector[rank_vector == "SUB_FAMILIA"] <- "subfamily"
#   rank_vector[rank_vector == "TRIBO"] <- "tribe"
#   rank_vector[rank_vector == "GENERO"] <- "genus"
#   rank_vector[rank_vector == "SUB_GENERO"] <- "subgenus"
#   rank_vector[rank_vector == "ESPECIE"] <- "species"
#   rank_vector[rank_vector == "SUB_ESPECIE"] <- "subspecies"
#   rank_vector[rank_vector == "SUB_CLASSE"] <- "subclass"
#   rank_vector[rank_vector == "INFRA_ORDEM"] <- "infraorder"
#   rank_vector[rank_vector == "SUPER_FAMILIA"] <- "superfamily"
#   rank_vector[rank_vector == "SUPER_CLASSE"] <- "superclass"
#   rank_vector[rank_vector == "SUB_TRIBO"] <- "subtribe"
#   rank_vector[rank_vector == "SUPER_ORDEM"] <- "superorder"
#   rank_vector[rank_vector == "INFRA_CLASSE"] <- "infraclass"
#
#   # Substituir NA por "unknown"
#   rank_vector[is.na(rank_vector)] <- "unknown"
#
#   return(rank_vector)
# }

#Extract string between patterns
extract_between <- function(str, left, right) {
  inicio <- regexpr(left, str) + attr(regexpr(left, str), "match.length")
  str_inicio <- substring(str, first = inicio)
  fim <- regexpr(right, str_inicio)
  final <- substring(str_inicio, first = 0, last = fim - 1)
  ifelse(inicio > 0 & fim > 0, final, NA)
}


#Solve incongruences between varieties/subspecies and species
update_columns <- function(df) {
  # Get unique values of lifeForm, habitat, vegetationType, Biome e States
  unique_lifeForm <- sort(unique(unlist(strsplit(df$lifeForm, ";"))))
  unique_habitat <- sort(unique(unlist(strsplit(df$habitat, ";"))))
  unique_states <- sort(unique(unlist(strsplit(df$states, ";"))))
  unique_countries <- sort(unique(unlist(strsplit(df$countryCode, ";"))))

  # Update columns where taxonRank == "ESPECIE"
  df$lifeForm[df$taxonRank == "species"] <- paste(unique_lifeForm, collapse = ";")
  df$habitat[df$taxonRank == "species"] <- paste(unique_habitat, collapse = ";")
  df$states[df$taxonRank == "species"] <- paste(unique_states, collapse = ";")
  df$states[df$taxonRank == "species"] <- paste(unique_states, collapse = ";")
  df$countryCode[df$taxonRank == "species"] <- paste(unique_countries,
                                                     collapse = ";")

  #Return only taxonRank == "ESPECIE"
  df <- subset(df, df$taxonRank == "species")

  return(df)
}

#Extract binomial name
extract_species <- function(texto) {

  # Remove curly braces {}
  texto2 <- gsub("\\{|\\}", "", texto)

  # Remove excess of whitespace between words
  texto2 <- gsub("\\s+", " ", texto2)

  # Remove leading and/or trailing whitespace
  texto2 <- trimws(texto2)

  # Extrair two first words
  binomial <- gsub("^([[:alnum:]]+[-[:alnum:]]*(?:[[:space:]]+[[:alnum:]]+[-[:alnum:]]*)?)\\b.*",
                   "\\1", texto2)

  if (grepl("-", binomial)) {
    return(binomial)
  }

  if (!grepl("-", binomial)) {
    # Verificar se há um padrão de palavra seguida por espaço e outra palavra
    match <- regexpr("\\b\\w+(-\\w+)?\\s+\\(\\w+\\)\\s+\\w+(-\\w+)?\\b|\\b\\w+(-\\w+)?\\s+\\w+\\b", texto2, perl = TRUE)
    if (match > 0) {
      # Extrair o trecho correspondente ao padrão encontrado
      palavras <- regmatches(texto2, match)
      return(palavras)
    } else {
      # Se não encontrar nenhum padrão específico, retornar a string original
      return(texto2)
    }
  }
}

#Extract year from scientificname
extract_year <- function(sn) {
  match <- gregexpr("\\b\\d{4}\\b", sn)
  if (any(match[[1]] > 0)) {
    return(regmatches(sn, match))
  } else {
    return(NA)
  }
}


merge_data <- function(path_data, version_data, solve_discrepancies = TRUE,
                       translate = TRUE, encoding = "UTF-8", verbose = TRUE) {

  #Set folder
  if(is.null(path_data)) {
    stop(paste("Argument path_data is not defined, this is necessary for",
               "\n saving data"))
  }

  #Print message
  if(verbose) {
    message("Data will be saved in ", path_data, "\n") }

  #Get latest available version if version was not set
  if(version_data == "latest") {
    all_dirs <- list.dirs(path = path_data, recursive = FALSE,
                          full.names = FALSE)
    dir_versions <- stats::na.omit(as.numeric(all_dirs)) #Actual version
    #Get highest version
    if(length(dir_versions) > 0) {
      high_version <- max(dir_versions)
      version_data <- high_version } else {
        version_data <- 0
      } }

  #Taxon
  taxon <- utils::read.csv(file.path(path_data, version_data, "taxon.txt"),
                           header=TRUE, sep = "\t",
                           encoding = encoding, na.strings = "")
  #Remove accents
  taxon$higherClassification <- iconv(taxon$higherClassification,
                                      to="ASCII//TRANSLIT")

  #Vernacular name
  vernacular <- utils::read.csv(file.path(path_data, version_data,
                                          "vernacularname.txt"),
                                header=TRUE, sep = "\t",
                                encoding = encoding, na.strings = "")
  #Remove accents
  vernacular$vernacularName <- iconv(vernacular$vernacularName,
                                     to="ASCII//TRANSLIT")

  #Group vernacular names from same species
  grouped <- split(vernacular, vernacular$id)
  summarized <- lapply(grouped, function(group) {
    paste(group$vernacularName, collapse = ", ")
  })
  vernacular_final <- data.frame(
    id = as.numeric(names(summarized)),
    vernacularName = unlist(summarized)
  )

  ###Species Profile
  spProfile <- utils::read.csv(file.path(path_data, version_data,
                                         "speciesprofile.txt"),
                               header=TRUE, sep = "\t",
                               encoding = encoding, na.strings = "")
  #Remove accents
  spProfile$lifeForm <- iconv(spProfile$lifeForm, to="ASCII//TRANSLIT")
  spProfile$habitat <- iconv(spProfile$habitat, to="ASCII//TRANSLIT")


  ###Distribution and Location
  dist <- utils::read.csv(file.path(path_data, version_data,
                                    "distribution.txt"),
                          header=TRUE, sep = "\t",
                          encoding = encoding, na.strings = "")
  # #Remove accents
  # dist$occurrenceRemarks <- iconv(dist$occurrenceRemarks,
  #                                 to="ASCII//TRANSLIT")
  #Extrair informações para novas coluna

    #origin
  dist$origin <- dist$establishmentMeans


  #Organize information
  #Local
  Local <- dist[,c("id","locality","countryCode")]
  #Group location of same species
  grouped <- split(Local, Local$id)
  summarized <- lapply(grouped, function(group) {
    if(!is.na(group$locality)){
    paste(group$locality, collapse = ";")} else {group$locality}
    })
  Local_final <- data.frame(
    id = as.numeric(names(summarized)),
    locality = unlist(summarized)
  )
  #Merge distribution data again
  dist_final <- dist[, c("id", "countryCode", "origin")]
  dist_final <- merge(dist_final, Local_final, by = "id")
  dist_final <- unique(dist_final[,colnames(dist_final)])

  #Merge all information
  df_final1 <- merge(taxon, vernacular_final, by = "id", all = TRUE)
  df_final2 <- merge(df_final1, spProfile, by = "id", all = TRUE)
  df_final3 <- merge(df_final2, dist_final, by = "id", all = TRUE)

  #Create columns with name of the specie and accepted name
  which_is_species <- which(df_final3$taxonRank %in% c("ESPECIE", "SUB_ESPECIE"))
  which_is_subspecies <- which(df_final3$taxonRank == "SUB_ESPECIE")
  df_final3$species[which_is_species] <- paste(df_final3$genus[which_is_species],
                                                df_final3$specificEpithet[which_is_species])
  df_final3$subspecies[df_final3$taxonRank == "SUB_ESPECIE"] <- paste(df_final3$species[which_is_subspecies],
                                                                      df_final3$infraspecificEpithet[which_is_subspecies])

  #Accepted name when is synonymn
  df_final3$acceptedName <- NA
  sp_syn <- which(df_final3$taxonRank %in% c("ESPECIE" , "SUB_ESPECIE") &
                  df_final3$taxonomicStatus == "SINONIMO" &
                    !is.na(df_final3$acceptedNameUsage))
  df_final3[sp_syn, "acceptedName"] <- sapply(df_final3[sp_syn, "acceptedNameUsage"],
                                              extract_species)

  #resourcerelationship
  rr <- utils::read.csv(file.path(path_data, version_data,
                                 "resourcerelationship.txt"),
                       header=TRUE, sep = "\t",
                       encoding = encoding, na.strings = "")

  #Group resourcerelationship of same species
  grouped <- split(rr, rr$id)
  summarized_relatedResourceID <- lapply(grouped, function(group) {
    paste(group$relatedResourceID, collapse = ";")
    })
  summarized_relationshipOfResource <- lapply(grouped, function(group) {
    paste(group$relationshipOfResource, collapse = ";")
  })

  rr_final <- data.frame(
    id = as.numeric(names(summarized_relatedResourceID)),
    relatedResourceID = unlist(summarized_relatedResourceID),
    relationshipOfResource = unlist(summarized_relationshipOfResource)
  )
  df_final4 <- merge(df_final3, rr_final, by = "id", all = TRUE)


  #Extract year
  df_final4$namePublishedInYear <- sapply(df_final4$scientificName,
                                          extract_year)

  #Remove {} from acceptednameUsage usage
  df_final4$acceptedNameUsage <- gsub("\\{|\\}", "", df_final4$acceptedNameUsage)
  df_final4$acceptedNameUsage[which(df_final4$acceptedNameUsage == "")] <- NA
  df_final4$acceptedName <- gsub("\\{|\\}", "", df_final4$acceptedName)
  df_final4$acceptedName[which(df_final4$acceptedName == "")] <- NA

  #Order columns
  df_final <- df_final4[,c(c("id", "taxonID","species", "subspecies",
                             "scientificName",
                             "acceptedName",
                             "acceptedNameUsage",
                             "parentNameUsage",
                             "namePublishedInYear",
                             "higherClassification", "kingdom",
                             "phylum", "class", "order", "family", "genus",
                             "specificEpithet",
                             "infraspecificEpithet", "taxonRank",
                             "scientificNameAuthorship",
                             "taxonomicStatus", "nomenclaturalStatus",
                             "vernacularName", "lifeForm",
                             "habitat", "origin",
                             "locality",
                             "countryCode", "modified", "bibliographicCitation",
                             "relationshipOfResource"))]

  #Sort information and separe using ;
  df_final$lifeForm <- vapply(df_final$lifeForm, FUN.VALUE = character(1),
                             function(x){
                               paste(sort(unlist(strsplit(x, split = " \\| "))),collapse = ";")
                             }, USE.NAMES = FALSE)
  df_final$habitat <- vapply(df_final$habitat, FUN.VALUE = character(1),
                            function(x){
                              paste(sort(unlist(strsplit(x, split = " \\| "))),collapse = ";")
                            }, USE.NAMES = FALSE)


  #Rename columns
  colnames(df_final)[colnames(df_final) == "locality"] <- "states"

  #Fix countries
  df_final$countryCode[is.na(df_final$countryCode) & !is.na(df_final$states)] <- "BR"
  #Get index for more complex fixing
  index_fix <- !grepl("BR", df_final$countryCode) & !is.na(df_final$states)
  countries_fix <- df_final$countryCode[index_fix]
  countries_fixed <- sapply(countries_fix, function(x) {
    paste(sort(c("BR", unlist(strsplit(x, ";", fixed = TRUE)))), collapse = ";")
    }, USE.NAMES = FALSE)
  df_final$countryCode[index_fix] <- countries_fixed
  #df_final[index_fix,] %>% View()

  #Fix states
  df_final$states <- gsub("BR-", "", df_final$states)

  # #Translate and put in lower case
  # df_final$lifeForm <- translate_lifeform(df_final$lifeForm)
  # df_final$lifeForm[df_final$lifeForm == ""] <- NA
  # df_final$origin <- translate_origins(df_final$origin)
  # df_final$habitat <- translate_habitat(df_final$habitat)
  # df_final$taxonRank <- translate_taxonrank(df_final$taxonRank)
  # df_final$taxonomicStatus[df_final$taxonomicStatus=="NOME_ACEITO"] <- "accepted"
  # df_final$taxonomicStatus[df_final$taxonomicStatus=="SINONIMO"] <- "synonym"
  # df_final$nomenclaturalStatus <- tolower(df_final$nomenclaturalStatus)
  # #df_final$countryCode <- tolower(df_final$countryCode)
  # df_final$origin[df_final$origin=="EXOTICA"] <- "exotic"

  #Replace accepted_name with valid in taxonomicStatus
  df_final$taxonomicStatus[df_final$taxonomicStatus == "NOME_ACEITO"] <- "valido"

  #Change name of acceptedName to validName
  colnames(df_final)[colnames(df_final) == "acceptedName"] <- "validName"
  colnames(df_final)[colnames(df_final) == "acceptedNameUsage"] <- "validNameUsage"

  #Add language
  df_final$language <- "pt_br"

  if(translate){ #Translate
    df_final <- translate_faunabr(df_final, to = "en")} else {
      #Or put columns to lower
      df_final$lifeForm <- tolower(df_final$lifeForm)
      df_final$origin <- tolower(df_final$origin)
      df_final$habitat <- tolower(df_final$habitat)
      df_final$taxonRank <- tolower(df_final$taxonRank)
      df_final$taxonomicStatus <- tolower(df_final$taxonomicStatus)
    }


  #Solve incongruencies?
  if(solve_discrepancies){
    df_final <- fauna_discrepancies(df_final)
  }

  #Save as gzip format
  data.table::fwrite(df_final,
          file = file.path(path_data, version_data,
                           "CompleteBrazilianFauna.gz"),
          row.names = FALSE,
          compress = "gzip")

}

# ####Generate data to examples #####
# library(dplyr)
# library(data.table)
# library(terra)
# library(geobr)
#
# ####Get Fauna do Brazil dataset####
# my_dir <- "../faunabrdata/"
# dir.create(my_dir)
# get_faunabr(output_dir = my_dir, solve_discrepancies = FALSE)
#
# #Fauna do Brazil data
# df <- load_faunabr(data_dir = my_dir, type = "short")
#
# #Get only native species of chordata
# p <- df %>% filter(taxonRank == "species", origin == "native", phylum == "Chordata")
# #Get only accepted names
# pac <- p %>% filter(taxonomicStatus == "accepted")
# #Include some synonyms
# syn <- df %>% filter(acceptedName %in% c("Mazama nemorivaga", "Mazama jucunda",
#                                     "Subulo gouzoubira"))
#
# #Final data
# fauna_data <- bind_rows(pac, syn)
# usethis::use_data(fauna_data, overwrite = TRUE)

#
# ####Get species occurrences####
# library(plantR)
# library(CoordinateCleaner)
# library(pbapply)
# library(dplyr)
#
# spp <- c("Panthera onca", "Chaetomys subspinosus")
#
# oc.gbif <- pblapply(spp, function(i) {
#   rgbif2(species = i, force = TRUE, remove_na = TRUE) })
# oc.gbif <- bind_rows(oc.gbif)
#
#
# #Clean data
# library(CoordinateCleaner)
# oc_n <- oc.gbif %>% mutate(decimalLatitude = as.numeric(decimalLatitude),
#                            decimalLongitude = as.numeric(decimalLongitude))
#
# occ_f <- clean_coordinates(x = oc_n, lon = "decimalLongitude",
#                            lat = "decimalLatitude",
#                            species = "species", countries = "countryCode",
#                            tests = c("capitals", "centroids", "equal", "gbif",
#                                      "institutions","seas", "zeros"))
# #Select only valid records
# occ <- occ_f %>% filter(.summary == TRUE) %>%
#   dplyr::select(species, x = "decimalLongitude", y = "decimalLatitude",
#                 datasetKey) #To get DOI
# #Remove duplicates
# occ_dup <- cc_dupl(occ, species = "species", lon = "x", lat = "y")
# occurrences <- data.frame(occ_dup)
#
# #Generate fake data to flag
# #States in Brazil without occurrence of panthera onca
# br <- vect(geobr::read_state())
# #Pernambuco and alagoas centroid
# pe <- as.data.frame(terra::centroids(br[br$abbrev_state %in% c("PE", "AL")],
#                                      inside = TRUE),
#                     geom = "XY")
# fake_occ1 <- data.frame("species" = "Panthera onca", x = pe$x, y = pe$y)
# #States in Brazil without occurrence of "Chaetomys subspinosus"
# #DF and SP
# dfsp <- as.data.frame(terra::centroids(br[br$abbrev_state %in% c("DF", "SP")],
#                                        inside = TRUE),
#                       geom = "XY")
# fake_occ2 <- data.frame("species" = "Chaetomys subspinosus", x = dfsp$x, y = dfsp$y)
#
# #Countries without occurrence of panthera onca
# #Chile and Cuba for panthera onca
# w <- terra::unwrap(faunabr::world_fauna)
# chile_cuba <- as.data.frame(terra::centroids(w[w$name %in% c("chile", "cuba")],
#                                              inside = TRUE),
#                             geom = "XY")
# fake_occ3 <- data.frame("species" = "Panthera onca", x = chile_cuba$x, y = chile_cuba$y)
#
# #Countries without occurrence of Chaetomys subspinosus
# #Paraguai and bolivia for Chaetomys subspinosus
# pb <- as.data.frame(terra::centroids(w[w$name %in% c("paraguay", "bolivia")],
#                                              inside = TRUE),
#                             geom = "XY")
# fake_occ4 <- data.frame("species" = "Chaetomys subspinosus",
#                         x = pb$x, y = pb$y)
#
# #Join data and identify as fake data
# fake <- bind_rows(fake_occ1, fake_occ2, fake_occ3, fake_occ4)
# fake$source <- "fake_data"
# occurrences$source <- "gbif"
# occurrences <- bind_rows(occurrences, fake)

# #Plot records to see
# pts <- vect(occurrences, geom = c(x = "x",y = "y"), crs = "+init=epsg:4326")
# mapview::mapview(pts, zcol = "species", burst = T)
#
# # #Data set key
# # ds_key <- occurrences %>% count(datasetKey)
# #
# # derived_dataset(
# #   citation_data = ds_key,
# #   title = "florabr R package: Records of plant species",
# #   description="This data was downloaded using plantR::rgbif2, filtered using
# #   CoordinateCleaner::clean_coordinates and later incorported as data example in
# #   florabr R Package",
# #   source_url="https://github.com/wevertonbio/florabr/raw/main/data/occurrences.rda",
# #   gbif_download_doi = NULL,
# #   user = user, #User in GBIF
# #   pwd = pwd) #Password in GBIF
#
# #Remove datasetKey column
# occurrences <- occurrences %>% dplyr::select(-datasetKey)
# usethis::use_data(occurrences, overwrite = TRUE)

# # #### Get shape of countries ####
# w <- vect(rnaturalearth::ne_countries(scale = "large",
#                                       returnclass = "sf"))
# as.data.frame(w) %>% View()
# #Match with countrycodes in data
# cc <- bf$countryCode %>% strsplit(";") %>% unlist() %>% unique()
# country_names <- countrycode::countrycode(cc, origin = "iso2c",
#                                           destination = "country.name")
# country_names <- gsub("&", "and", country_names)
# # Create a dataframe
# country_df <- data.frame(
#   country_code = cc,
#   country_name = country_names,
#   stringsAsFactors = FALSE
# )
# #Fix some names manually
# country_df$country_name[country_df$country_code == "KOS"] <- "Kosovo"
# country_df$country_name[country_df$country_code == "PS-GZA"] <- "Palestine"
# #Saves as data names country_codes
# country_codes <- country_df %>% na.omit()
# #Match with names in w
# w$name %>% as.data.frame %>% View()
# c_matches <- florabr::match_names(species = country_df$country_name,
#                                   species_to_match = w$name_long) %>%
#   filter(!is.na(input_name))
# #Merge data
# c_matches <- c_matches %>% rename(bf_name = input_name,
#                                   map_name = Suggested_name) %>%
#   select(-Distance) %>%
#   left_join(country_codes, by = join_by("bf_name" =="country_name"))
# #Fix manually
# c_matches$map_name[c_matches$country_code == "CV"] <- "Republic of Cabo Verde"
# c_matches$map_name[c_matches$country_code == "CD"] <- "Democratic Republic of the Congo"
# c_matches$map_name[c_matches$country_code == "CG"] <- "Republic of the Congo"
# c_matches$map_name[c_matches$country_code == "VC"] <- "Saint Vincent and the Grenadines"
# c_matches$map_name[c_matches$country_code == "KR"] <- "Republic of Korea"
# c_matches$map_name[c_matches$country_code == "CZ"] <- "Czech Republic"
# c_matches$map_name[c_matches$country_code == "SH"] <- "Saint Helena"
# c_matches$map_name[c_matches$country_code == "LC"] <- "Saint Lucia"
# c_matches$map_name[c_matches$country_code == "FM"] <- 'Federated States of Micronesia'
# c_matches$map_name[c_matches$country_code == "KP"] <- 'Dem. Rep. Korea'
# c_matches$map_name[c_matches$country_code == "MM"] <- 'Myanmar'
# c_matches$map_name[c_matches$country_code == "VI"] <- 'United States Virgin Islands'
# c_matches$map_name[c_matches$country_code == "HK"] <- 'Hong Kong'
# c_matches$map_name[c_matches$country_code == "MO"] <- 'Macao'
# c_matches$map_name[c_matches$country_code == "UM"] <- 'United States Minor Outlying Islands'
# c_matches$map_name[c_matches$country_code == "TF"] <- 'French Southern and Antarctic Lands'
# c_matches$map_name[c_matches$country_code == "GS"] <- 'South Georgia and the Islands'
# c_matches$map_name[c_matches$country_code == "SZ"] <- 'Kingdom of eSwatini'
# #Save as data
# country_codes <- c_matches %>% select(-bf_names)
# usethis::use_data(country_codes, overwrite = TRUE)
#
# #Get only column with country
# w <- w[,"name_long"]
#
# #Simplify
# w2 <- terra::simplifyGeom(w)
#
# #Merge with countryCode
# w3 <- merge(w2, country_codes, by.x = "name_long", by.y = "map_name")
# world_fauna <- terra::wrap(w3)
# usethis::use_data(world_fauna, overwrite = TRUE)
# writeVector(w, "data/teste.gpkg")
#
#
# #To help fix names
# #Get all names
# n <- unique(w$name)
# #Get all countries in faunadata
# df <- load_faunabr("../faunabrdata/")
# co <- unique(unlist(strsplit(df$countryCode, ";")))
# #Get difference
# no_co <- setdiff(co, n)
# sort(no_co)
# data.frame(n) %>% View()

# # States #
# states <- terra::unwrap(florabr::states)
# crs(states) <- crs(countrys)
# states <- terra::wrap(states)
# usethis::use_data(states, overwrite = TRUE)

# states <- geobr::read_state() %>% vect()
# crs(states) <- "+init=epsg:4326"
# states <- terra::wrap(states)
# usethis::use_data(states, overwrite = TRUE)


# #### Make sticker ####
# library(hexSticker)
# library(showtext)
# ## Loading Google fonts (http://www.google.com/fonts)
# font_families_google()
# font_add_google(name = "Markazi Text")
# ## Automatically use showtext to render text for future devices
# showtext_auto()
#
# sticker(subplot = "../faunabrdata/Mapa.png", package="faunabr", p_size=22.5,
#         p_fontface = "bold", p_color = "black",
#         p_family = "Markazi Text",
#         p_y = 1.59,
#         s_x=1, s_y=.76, s_width=.67,
#         h_fill = "#008080",
#         filename="../faunabrdata/HexSticker_markazi_blue4.png",
#         spotlight = TRUE, l_alpha = 0.3)
