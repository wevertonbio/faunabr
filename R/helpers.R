#####Helper functions####

#First letter to upper case
firstup <- function(x) {
  x <- tolower(x)
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

#Translate origin, habitat, lifeform, taxonomicStatus and nomenclaturalStatus
translate_lifeform <- function(text) {
  # Aplicar gsub para traduzir e converter para letras minúsculas cada forma de vida em português
  new_lifeform <- gsub("VIDA_LIVRE_INDIVIDUAL", "free_living_individual", text, ignore.case = TRUE)
  new_lifeform <- gsub("COLONIAL", "colonial", new_lifeform, ignore.case = TRUE)
  new_lifeform <- gsub("SESSIL", "sessile", new_lifeform, ignore.case = TRUE)
  new_lifeform <- gsub("ECTOPARASITO", "ectoparasite", new_lifeform, ignore.case = TRUE)
  new_lifeform <- gsub("HERBIVORO", "herbivore", new_lifeform, ignore.case = TRUE)
  new_lifeform <- gsub("EUSSOCIAL", "eusocial", new_lifeform, ignore.case = TRUE)
  new_lifeform <- gsub("PREDADOR", "predator", new_lifeform, ignore.case = TRUE)
  new_lifeform <- gsub("ENDOPARASITOIDE", "endoparasitoid", new_lifeform, ignore.case = TRUE)
  new_lifeform <- gsub("ENDOPARASITO", "endoparasite", new_lifeform, ignore.case = TRUE)
  new_lifeform <- gsub("ECTOPARASITOIDE", "ectoparasitoid", new_lifeform, ignore.case = TRUE)
  new_lifeform <- gsub("HIPERPARASITOIDE", "hyperparasitoid", new_lifeform, ignore.case = TRUE)
  new_lifeform <- gsub("COMENSAL", "commensal", new_lifeform, ignore.case = TRUE)
  new_lifeform <- gsub("EPIBIONTE", "epibiont", new_lifeform, ignore.case = TRUE)
  new_lifeform <- gsub("POLINIZADOR", "polynizer", new_lifeform, ignore.case = TRUE)
  new_lifeform <- gsub("INQUILINO", "inquiline", new_lifeform, ignore.case = TRUE)
  new_lifeform <- gsub("MUTUAL", "mutualistic", new_lifeform, ignore.case = TRUE)
  return(new_lifeform)
}

translate_origins <- function(text) {
  # Aplicar gsub para traduzir e converter para letras minúsculas cada origem
  text[text == ""] <- NA
  new_origins <- gsub("NATIVA", "native", text, ignore.case = TRUE)
  new_origins <- gsub("INTRODUZIDA", "introduced", new_origins, ignore.case = TRUE)
  new_origins <- gsub("CRIPTOGENICA", "cryptogenic", new_origins, ignore.case = TRUE)
  new_origins <- gsub("DOMESTICADA", "domesticated", new_origins, ignore.case = TRUE)
  return(new_origins)
}

translate_habitat <- function(text) {
  # Aplicar gsub para traduzir e converter para letras minúsculas cada categoria de habitat
  text[text == ""] <- NA
  new_habitat <- gsub("AGUA_DOCE", "freshwater", text, ignore.case = TRUE)
  new_habitat <- gsub("MARINHO", "marine", new_habitat, ignore.case = TRUE)
  new_habitat <- gsub("TERRESTRE", "terrestrial", new_habitat, ignore.case = TRUE)
  new_habitat <- gsub("ARBOREO", "arboreal", new_habitat, ignore.case = TRUE)
  new_habitat <- gsub("FOSSORIAL", "fossorial", new_habitat, ignore.case = TRUE)
  new_habitat <- gsub("CAVERNICOLA", "cavernicolous", new_habitat, ignore.case = TRUE)
  new_habitat <- gsub("AGUAS_SUBTERRANEAS", "subterranean waters", new_habitat, ignore.case = TRUE)
  new_habitat <- gsub("NIDICOLA", "nidicolous", new_habitat, ignore.case = TRUE)
  new_habitat <- gsub("HIPORREICO", "hyporheic", new_habitat, ignore.case = TRUE)
  return(new_habitat)}

translate_taxonrank <- function(rank_vector) {
  # Converter o vetor para caracteres (strings) e tratar NA como um valor especial
  rank_vector <- as.character(rank_vector)

  # Realizar as substituições diretamente
  rank_vector[rank_vector == "FILO"] <- "phylum"
  rank_vector[rank_vector == "CLASSE"] <- "class"
  rank_vector[rank_vector == "ORDEM"] <- "order"
  rank_vector[rank_vector == "SUB_ORDEM"] <- "suborder"
  rank_vector[rank_vector == "FAMILIA"] <- "family"
  rank_vector[rank_vector == "SUB_FAMILIA"] <- "subfamily"
  rank_vector[rank_vector == "TRIBO"] <- "tribe"
  rank_vector[rank_vector == "GENERO"] <- "genus"
  rank_vector[rank_vector == "SUB_GENERO"] <- "subgenus"
  rank_vector[rank_vector == "ESPECIE"] <- "species"
  rank_vector[rank_vector == "SUB_ESPECIE"] <- "subspecies"
  rank_vector[rank_vector == "SUB_CLASSE"] <- "subclass"
  rank_vector[rank_vector == "INFRA_ORDEM"] <- "infraorder"
  rank_vector[rank_vector == "SUPER_FAMILIA"] <- "superfamily"
  rank_vector[rank_vector == "SUPER_CLASSE"] <- "superclass"
  rank_vector[rank_vector == "SUB_TRIBO"] <- "subtribe"
  rank_vector[rank_vector == "SUPER_ORDEM"] <- "superorder"
  rank_vector[rank_vector == "INFRA_CLASSE"] <- "infraclass"

  # Substituir NA por "unknown"
  rank_vector[is.na(rank_vector)] <- "unknown"

  return(rank_vector)
}

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


  # Update columns where taxonRank == "ESPECIE"
  df$lifeForm[df$taxonRank == "ESPECIE"] <- paste(unique_lifeForm, collapse = ";")
  df$habitat[df$taxonRank == "ESPECIE"] <- paste(unique_habitat, collapse = ";")
  df$states[df$taxonRank == "ESPECIE"] <- paste(unique_states, collapse = ";")
  df$states[df$taxonRank == "ESPECIE"] <- paste(unique_states, collapse = ";")

  #Return only taxonRank == "ESPECIE"
  df <- subset(df, df$taxonRank == "ESPECIE")

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


merge_data <- function(path_data, version_data, solve_incongruences = TRUE,
                       encoding = "UTF-8", verbose = TRUE) {

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


  # #Extract informations to new columns
  # #Life form
  # spProfile$lifeForm.new <- extract_between(spProfile$lifeForm,
  #                                           left = "lifeForm:\\[",
  #                                           right = "\\]")
  # #Habitat
  # spProfile$habitat.new <- extract_between(spProfile$lifeForm,
  #                                          left = "habitat:\\[",
  #                                          right = "\\]")
  # #Vegetation type
  # spProfile$vegetationType.new <- extract_between(spProfile$lifeForm,
  #                                                 left = "vegetationType:\\[",
  #                                                 right = "\\]")
  #
  # #Rename and select columns
  # spProfile <- spProfile[,c("id", "lifeForm.new", "habitat.new",
  #                           "vegetationType.new")]
  # colnames(spProfile) <- c("id", "lifeForm", "habitat", "vegetationType")

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


  # #Endemism
  # dist$Endemism <- ifelse(grepl("endemism:Nao endemica",
  #                               dist$occurrenceRemarks),
  #                         "Nao endemica",
  #                         ifelse(grepl("endemism:Endemica",
  #                                      dist$occurrenceRemarks),
  #                                "Endemica", NA))
  # #Phytogeographic domain
  # dist$phytogeographicDomain <- extract_between(dist$occurrenceRemarks,
  #                                               left = "phytogeographicDomain:\\[",
  #                                               right = "\\]")

  # #Deletar aspas
  # dist$phytogeographicDomain <- gsub("\"", "", dist$phytogeographicDomain)


  #Organize information
  #Local
  Local <- dist[,c("id","locality","countryCode")]
  #Group location of same species
  grouped <- split(Local, Local$id)
  summarized <- lapply(grouped, function(group) {
    paste(group$locality, collapse = ";")
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
  df_final3$species <- df_final3$specificEpithet
  df_final3$subspecies <- df_final3$infraspecificEpithet

  # #Ignore this ranks
  # all_ranks <- unique(df_final3$taxonRank)
  # ignore_rank <- setdiff(all_ranks, c("ESPECIE", "SUB_ESPECIE"))
  # df_final3$species[which(!(df_final3$taxonRank %in% ignore_rank))] <-
  #   gsub("^([[:alnum:]]+[-[:alnum:]]*(?:[[:space:]]+[[:alnum:]]+[-[:alnum:]]*)?)\\b.*",
  #        "\\1",
  #        df_final3$scientificName[which(!(df_final3$taxonRank %in%
  #                                           ignore_rank))])

  # df_final3$species[which(!(df_final3$taxonRank %in% ignore_rank))] <-
  #   gsub("^((\\w+\\W+){1}\\w+).*$","\\1",
  #        df_final3$scientificName[which(!(df_final3$taxonRank %in%
  #                                           ignore_rank))])

  #Accepted name when is synonymn
  df_final3$acceptedName <- NA
  sp_syn <- which(df_final3$taxonRank %in% c("ESPECIE" , "SUB_ESPECIE") &
                                           df_final3$taxonomicStatus == "SINONIMO")
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
  df_final$countryCode[df_final$countryCode == "" & !is.na(df_final$states)] <- "brazil"
  #Get index for more complex fixing
  index_fix <- !grepl("brazil", df_final$countryCode) & !is.na(df_final$states)
  countries_fix <- df_final$countryCode[index_fix]
  countries_fixed <- sapply(countries_fix, function(x) {
    paste(sort(c("brazil", unlist(strsplit(x, ";", fixed = TRUE)))), collapse = ";")
    }, USE.NAMES = FALSE)
  df_final$countryCode[index_fix] <- countries_fixed
  #df_final[index_fix,] %>% View()

  if(solve_incongruences){
    #Solve incongruences between species and subspecies
    #Get varieties, subspecies (and forms) with accepted names that occurs in Brazil
    spp_var <- subset(df_final,
                      df_final$taxonRank %in% c("SUB_ESPECIE") &
                        df_final$taxonomicStatus == "NOME ACEITO")[["species"]]

    #Get only species that exists as subspecies in dataframe
    spp_var_yes <- intersect(df_final$species[which(df_final$taxonRank == "SUB_ESPECIE")],
                             spp_var)
    spp_var_no <- setdiff(spp_var, df_final$species[which(df_final$taxonRank == "SUB_ESPECIE")])

    if (length(spp_var_no) > 0) {

    #Get dataframe to update
    d_upt <- subset(df_final, df_final$species %in% spp_var_yes)

    #Update columns
    dd_updated_list <- lapply(split(d_upt, d_upt$species), update_columns)

    # Merge dataframes
    d_upt <- do.call(rbind, dd_updated_list)
    row.names(d_upt) <- NULL

    #Update final dataframe
    df_final <- rbind(subset(df_final, !(df_final$id %in% d_upt$id)), d_upt)

    #Fix varieties and subspecies that does not appear as species
    df_no_species <- subset(df_final, df_final$species %in% spp_var_no)
    #Change taxonrank
    df_no_species$taxonRank <- "Species"
    #Create new id
    df_no_species$id <- sample(setdiff(1:500000, df_final$id), nrow(df_no_species))
    #Merge data
    df_final <- rbind(df_final, df_no_species) } }


  #Translate and put in lower case
  df_final$lifeForm <- translate_lifeform(df_final$lifeForm)
  df_final$origin <- translate_origins(df_final$origin)
  df_final$habitat <- translate_habitat(df_final$habitat)
  df_final$taxonRank <- translate_taxonrank(df_final$taxonRank)
  df_final$taxonomicStatus[df_final$taxonomicStatus=="NOME ACEITO"] <- "accepted_name"
  df_final$taxonomicStatus[df_final$taxonomicStatus=="SINONIMO"] <- "synonym"
  df_final$nomenclaturalStatus <- tolower(df_final$nomenclaturalStatus)
  df_final$countryCode <- tolower(df_final$countryCode)

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
# get_faunabr(output_dir = my_dir)
#
# #Fauna do Brazil data
# df <- load_faunabr(data_dir = my_dir, type = "short")
#
# #Get only native species of chordata
# p <- df %>% filter(taxonRank == "species", origin == "native", phylum == "Chordata")
# #Get only accepted names
# pac <- p %>% filter(taxonomicStatus == "accepted_name")
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

# #### Get shape of countries ####
# w <- vect(rnaturalearth::ne_countries(scale = "large",
#                                       returnclass = "sf"))
# as.data.frame(w) %>% View()
# #Get only column with country
# w <- w[,"name"]
# w$name <- tolower(w$name)
# w$name <- gsub(" ", "_", w$name)
# w$name <- gsub("is\\.", "islands", w$name)
# #Fix names manually
# w$name[w$name == "antigua_and_barb."] <- "antigua_and_barbuda"
# w$name[w$name == "bosnia_and_herz."] <- "bosnia_and_herzegovina"
# w$name[w$name == "br._indian_ocean_ter."] <- "british_indian_ocean_territory"
# w$name[w$name == "cabo_verde"] <- "cape_verde"
# w$name[w$name == "central_african_rep."] <- "central_african_republic"
# w$name[w$name == "congo"] <- "congo_republic"
# w$name[w$name == "curaçao"] <- "curacao"
# w$name[w$name == "czechia"] <- "czech_republic"
# w$name[w$name == "dominican_rep."] <- "dominican_republic"
# w$name[w$name == "eq._guinea"] <- "equatorial_guinea"
# w$name[w$name == "falkland_islands"] <- "falkland_islands_islas_malvinas"
# w$name[w$name == "faeroe_islands"] <- "faroe_islands"
# w$name[w$name == "fr._polynesia"] <- "french_polynesia"
# w$name[w$name == "fr._s._antarctic_lands"] <- "french_southern_territories"
# w$name[w$name == "guinea-bissau"] <- "guinea_bissau"
# w$name[w$name == "heard_i._and_mcdonald_islands"] <- "heard_island_and_mcdonald_islands"
# w$name[w$name == "macao"] <- "macau"
# w$name[w$name == "north_macedonia"] <- "macedonia_fyrom"
# w$name[w$name == "myanmar"] <- "myanmar_burma"
# w$name[w$name == "n._mariana_islands"] <- "northern_mariana_islands"
# w$name[w$name == "palestine"] <- "palestinian_territories"
# w$name[w$name == "st._kitts_and_nevis"] <- "saint_kitts_and_nevis"
# w$name[w$name == "st._pierre_and_miquelon"] <- "saint_pierre_and_miquelon"
# w$name[w$name == "st._vin._and_gren."] <- "saint_vincent_and_the_grenadines"
# w$name[w$name == "são_tomé_and_principe"] <- "sao_tome_and_principe"
# w$name[w$name == "s._geo._and_the_islands"] <- "south_georgia_and_the_south_sandwich_islands"
# w$name[w$name == "eswatini"] <- "swaziland"
# w$name[w$name == "timor-leste"] <- "timor_leste"
# w$name[w$name == "united_states_of_america"] <- "united_states"
# w$name[w$name == "u.s._virgin_islands"] <- "u_s_virgin_islands"
# w$name[w$name == "wallis_and_futuna_islands"] <- "wallis_and_futuna"
# w$name[w$name == "w._sahara"] <- "western_sahara"
# w$name[w$name == "côte_d'ivoire"] <- "cote_d_ivoire"
# w$name[w$name == "u.s._minor_outlying_islands"] <- "u_s_minor_outlying_islands"
# #Save
# #Simplify
# w2 <- terra::simplifyGeom(w)
# world_fauna <- terra::wrap(w2)
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
