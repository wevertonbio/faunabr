#####Helper functions####

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
  unique_States <- sort(unique(unlist(strsplit(df$States, ";"))))

  # Update columns where taxonRank == "ESPECIE"
  df$lifeForm[df$taxonRank == "ESPECIE"] <- paste(unique_lifeForm, collapse = ";")
  df$habitat[df$taxonRank == "ESPECIE"] <- paste(unique_habitat, collapse = ";")
  df$States[df$taxonRank == "ESPECIE"] <- paste(unique_States, collapse = ";")

  #Return only taxonRank == "ESPECIE"
  df <- subset(df, df$taxonRank == "ESPECIE")

  return(df)
}

#Extract binomial name
extract_species <- function(texto) {

  # Remove chaves {}
  texto2 <- gsub("\\{|\\}", "", texto)

  # Remove excesso de espaços entre palavras
  texto2 <- gsub("\\s+", " ", texto2)

  # Remove espaços extras no início e fim da string
  texto2 <- trimws(texto2)

  # Extrair as duas primeiras palavras
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
    dir_versions <- na.omit(as.numeric(all_dirs)) #Actual version
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
  #Origin
  dist$Origin <- dist$establishmentMeans

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

  # #locality - State
  # dist$locality <- gsub(".*-", "", dist$locality)

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
  dist_final <- dist[, c("id", "countryCode", "Origin")]
  dist_final <- merge(dist_final, Local_final, by = "id")
  dist_final <- unique(dist_final[,colnames(dist_final)])

  #Merge all information
  df_final1 <- merge(taxon, vernacular_final, by = "id", all = TRUE)
  df_final2 <- merge(df_final1, spProfile, by = "id", all = TRUE)
  df_final3 <- merge(df_final2, dist_final, by = "id", all = TRUE)

  #Create columns with name of the specie and accepted name
  df_final3$species <- df_final3$specificEpithet
  df_final3$sub_species <- df_final3$infraspecificEpithet
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
  df_final <- df_final4[,c(c("id", "taxonID","species", "sub_species",
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
                             "habitat", "Origin",
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
  colnames(df_final)[colnames(df_final) == "locality"] <- "States"


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


  #Save as RDS
  saveRDS(df_final,
          file = file.path(path_data, version_data,
                           "CompleteBrazilianFauna.rds"))

}
