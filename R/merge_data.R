#' Merge Taxonomic Data from Catálogo Taxonômico da Fauna do Brasil
#'
#' @description
#' Reads, processes, and merges the core components of the Catálogo Taxonômico
#' da Fauna do Brasil
#'
#' @param path_data (character) the base directory path where the downloaded
#' data is stored.
#' @param version_data (numeric) version of the Fauna do Brasil database
#' downloaded.
#' @param solve_discrepancies (logical) wheter to resolve inconsistencies
#' between species and subspecies information. When set to `TRUE` (default),
#' species information is updated based on unique data from subspecies. For
#' example, if a subspecies occurs in a certain state, it implies that the
#' species also occurs in that state.
#' @param translate (logical) whether to translate the original dataset
#'  ("lifeForm", "origin", "habitat", and "taxonRank") from Portuguese to
#'  English. Default is `TRUE.`
#' @param encoding (character) The text string encoding to pass down to
#' `data.table::fread()`. Default is `"UTF-8"`.
#' @param verbose (logical) whether to display messages during function
#' execution. Set to TRUE to enable display, or FALSE to run silently. Default
#' is `TRUE`.
#'
#' @importFrom stats na.omit
#' @importFrom data.table fread setkey setnames is.data.table setDT fwrite := .SD
#'
#' @returns
#' The final dataset is built dynamically and saved directly to the matching
#' data directory as a compressed Gzip file named `"CompleteBrazilianFauna.gz"`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' merge_data(path_data = "data", version_data = "latest",
#'             solve_discrepancies = TRUE, translate = FALSE, verbose = TRUE)
#' }
merge_data <- function(path_data, version_data, solve_discrepancies = TRUE,
                       translate = TRUE, encoding = "UTF-8", verbose = TRUE) {
  # 1. Folder and Version Configuration
  if(is.null(path_data)) {
    stop("Argument path_data is not defined, this is necessary for saving data")
  }

  if(verbose) {
    message("Data will be saved in ", path_data, "\n")
  }

  if(version_data == "latest") {
    all_dirs <- list.dirs(path = path_data, recursive = FALSE, full.names = FALSE)
    dir_versions <- stats::na.omit(as.numeric(all_dirs))
    version_data <- if(length(dir_versions) > 0) max(dir_versions) else 0
  }

  # Definition of the target directory to read files from
  target_dir <- file.path(path_data, version_data)


  # 2. Ultra-fast File Reading with fread
  if(verbose) message("Reading files...")

  taxon <- data.table::fread(file.path(target_dir, "taxon.txt"), sep = "\t",
                             encoding = encoding, na.strings = c("", "NA"))

  vernacular <- data.table::fread(file.path(target_dir, "vernacularname.txt"), sep = "\t",
                                  encoding = encoding, na.strings = c("", "NA"))

  spProfile <- data.table::fread(file.path(target_dir, "speciesprofile.txt"), sep = "\t",
                                 encoding = encoding, na.strings = c("", "NA"))

  dist <- data.table::fread(file.path(target_dir, "distribution.txt"), sep = "\t",
                            encoding = encoding, na.strings = c("", "NA"))

  rr <- data.table::fread(file.path(target_dir, "resourcerelationship.txt"), sep = "\t",
                          encoding = encoding, na.strings = c("", "NA"))


  # 3. Text Processing (Accent Removal) by Reference
  taxon[, higherClassification := iconv(higherClassification, to="ASCII//TRANSLIT")]
  vernacular[, vernacularName := iconv(vernacularName, to="ASCII//TRANSLIT")]
  spProfile[, `:=`(lifeForm = iconv(lifeForm, to="ASCII//TRANSLIT"),
                   habitat = iconv(habitat, to="ASCII//TRANSLIT"))]


  # 4. Optimized Aggregations with data.table (Replaces split + lapply)
  if(verbose) message("Aggregating secondary tables...")

  # Vernacular: Group common names by ID
  vernacular_final <- vernacular[, .(vernacularName = paste(vernacularName, collapse = ", ")), by = id]

  # Distribution: Group geographic data and origins
  dist[, origin := establishmentMeans]

  # Define location columns based on what exists in the file
  loc_col <- intersect(c("locationID", "locality"), colnames(dist))[1]

  # Fast distribution aggregation collapsing unique non-NA values
  dist_final <- dist[, .(
    states = paste(unique(stats::na.omit(.SD[[loc_col]])), collapse = ";"),
    countryCode = paste(unique(stats::na.omit(countryCode)), collapse = ";"),
    origin = paste(unique(stats::na.omit(origin)), collapse = ";")
  ), by = id]

  # Resource Relationship: Group relationships by ID
  rr_final <- rr[, .(
    relatedResourceID = paste(relatedResourceID, collapse = ";"),
    relationshipOfResource = paste(relationshipOfResource, collapse = ";")
  ), by = id]


  # 5. Efficient Table Merges
  if(verbose) message("Performing table joins (merges)...")

  # Configure primary keys to optimize joins
  data.table::setkey(taxon, id)
  data.table::setkey(vernacular_final, id)
  data.table::setkey(spProfile, id)
  data.table::setkey(dist_final, id)
  data.table::setkey(rr_final, id)

  # Sequential table merges via data.table syntax
  df_final <- vernacular_final[taxon, on = "id"]
  df_final <- spProfile[df_final, on = "id"]
  df_final <- dist_final[df_final, on = "id"]
  df_final <- rr_final[df_final, on = "id"]


  # 6. New Column Creation by Reference (No data copying)
  if(verbose) message("Processing taxonomic columns...")

  # Species and subspecies columns
  df_final[, `:=`(species = as.character(NA), subspecies = as.character(NA))]

  df_final[taxonRank %in% c("ESPECIE", "SUB_ESPECIE", "species", "subspecies"),
           species := paste(genus, specificEpithet)]

  df_final[taxonRank %in% c("SUB_ESPECIE", "subspecies"),
           subspecies := paste(species, infraspecificEpithet)]

  # Accepted name for synonyms
  df_final[, validName := as.character(NA)]
  sp_syn_idx <- df_final[, taxonRank %in% c("ESPECIE" , "SUB_ESPECIE", "species", "subspecies") &
                           taxonomicStatus %in% c("SINONIMO", "synonym") &
                           !is.na(acceptedNameUsage)]

  if(any(sp_syn_idx)) {
    # Applies custom user function: extract_species
    df_final[sp_syn_idx, validName := sapply(acceptedNameUsage, extract_species)]
  }

  # Publication year
  if("namePublishedInYear" %in% colnames(df_final)){
    df_final[, namePublishedInYear := as.numeric(namePublishedInYear)]
  } else {
    df_final[, namePublishedInYear := sapply(scientificName, extract_year)]
  }

  # Cleaning brackets {} from usage columns
  df_final[, acceptedNameUsage := gsub("\\{|\\}", "", acceptedNameUsage)]
  df_final[acceptedNameUsage == "", acceptedNameUsage := NA]
  df_final[, validName := gsub("\\{|\\}", "", validName)]
  df_final[validName == "", validName := NA]


  # 7. String Formatting and Standardization
  # Uses vectorized operations instead of slow vapply loops
  df_final[, lifeForm := sapply(strsplit(lifeForm, " \\| "), function(x) paste(sort(x), collapse = ";"))]
  df_final[, habitat := sapply(strsplit(habitat, " \\| "), function(x) paste(sort(x), collapse = ";"))]

  # Country and state adjustments
  df_final[is.na(countryCode) & states != "", countryCode := "BR"]

  # Complex country code corrections
  index_fix <- df_final[, !grepl("BR", countryCode) & states != ""]
  if(any(index_fix)) {
    df_final[index_fix, countryCode := sapply(countryCode, function(x) {
      paste(sort(c("BR", unlist(strsplit(x, ";", fixed = TRUE)))), collapse = ";")
    })]
  }

  df_final[, states := gsub("BR-", "", states)]
  df_final[taxonomicStatus %in% c("NOME_ACEITO", "accepted"), taxonomicStatus := "valido"]
  data.table::setnames(df_final, "acceptedNameUsage", "validNameUsage")

  df_final[, language := "pt_br"]


  # 8. Translations and Discrepancies
  if(translate){
    # If translate_faunabr accepts data.table, it runs directly.
    # If it strictly requires a pure data.frame, we convert back afterwards.
    df_final <- translate_faunabr(df_final, to = "en")
    if(!data.table::is.data.table(df_final)) data.table::setDT(df_final)
  } else {
    cols_to_lower <- c("lifeForm", "origin", "habitat", "taxonRank", "taxonomicStatus")
    df_final[, (cols_to_lower) := lapply(.SD, tolower), .SDcols = cols_to_lower]
  }

  if(solve_discrepancies){
    df_final <- fauna_discrepancies(df_final)
    if(!data.table::is.data.table(df_final)) data.table::setDT(df_final)
  }


  # 9. Final Column Ordering and File Saving
  select_columns <- intersect(c("id", "taxonID","species", "subspecies", "scientificName",
                                "validName", "validNameUsage", "parentNameUsage", "namePublishedInYear",
                                "higherClassification", "phylum", "class", "order", "family", "genus",
                                "specificEpithet", "infraspecificEpithet", "taxonRank", "scientificNameAuthorship",
                                "taxonomicStatus", "nomenclaturalStatus", "vernacularName", "lifeForm",
                                "habitat", "origin", "states", "countryCode", "modified", "bibliographicCitation",
                                "relationshipOfResource", "language"),
                              colnames(df_final))

  df_final <- df_final[, ..select_columns]

  if(verbose) message("Saving final compressed file...")
  data.table::fwrite(df_final,
                     file = file.path(target_dir, "CompleteBrazilianFauna.gz"),
                     row.names = FALSE,
                     compress = "gzip")

  if(verbose) message("Done!")

}
