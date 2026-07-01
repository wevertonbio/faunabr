# Define global variables to silence R CMD check NOTEs about data.table columns
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    ".", "..select_columns", ".SD", ":=", "acceptedNameUsage", "countryCode",
    "establishmentMeans", "genus", "habitat", "higherClassification", "id",
    "infraspecificEpithet", "language", "lifeForm", "namePublishedInYear", "origin",
    "relatedResourceID", "relationshipOfResource", "scientificName", "species",
    "specificEpithet", "states", "subspecies", "taxonRank", "taxonomicStatus", "validName",
    "vernacularName"
  ))
}
