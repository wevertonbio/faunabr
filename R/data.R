#' Catálogo Taxonômico da Fauna do Brasil database - Version 1.17
#'
#' @description
#'  A dataset containing a subset of the Catálogo Taxonômico da Fauna do Brasil
#'  database (version 1.17)
#'
#' @usage data(fauna_data)
#'
#' @format A \code{data.frame} with 9558 rows and 19 variables:
#' \describe{
#'   \item{species}{Species names}
#'   \item{subspecies}{Subspecies names}
#'   \item{scientificName}{Complete scientific name of the species}
#'   \item{validName}{Valid name of the species (NA when the name in
#'   species is already a valid name)}
#'   \item{kingdom}{Kingdom to which species belongs (Animalia)}
#'   \item{phylum}{Phylum to which species belongs}
#'   \item{class}{Class to which species belongs}
#'   \item{order}{Order to which species belongs}
#'   \item{family}{Family to which species belongs}
#'   \item{genus}{Genus to which species belongs}
#'   \item{lifeForm}{Life form of the species (e.g: free_living_individual,
#'   colonial, sessile, etc.)}
#'   \item{habitat}{Habitat type of the species (e.g., terrestrial, arboreal,
#'   freshwater, etc.)}
#'   \item{states}{Federal states with confirmed occurrences of the species}
#'   \item{countryCode}{Countries with confirmed occurrences of the species}
#'   \item{origin}{Indicates whether the species is native, introduced,
#'   domesticated, cryptogenic or invasive}
#'   \item{taxonomicStatus}{Indicates the level of recognition and acceptance
#'   of the species (valid or synonym)}
#'   \item{nomenclaturalStatus}{Indicates the legitimacy and validity of the
#'   species name (original_combination, changed_combination, etc.)}
#'   \item{vernacularName}{Locally or culturally used name for the species}
#'   \item{taxonRank}{Taxonomic rank (Species, Genus, Family, Order, etc). This
#'   data contains only Species}
#' }
#' @references
#' Brazilian Zoology Group. Catálogo Taxonômico da Fauna do Brasil. Available at:
#' https://ipt.jbrj.gov.br/jbrj/resource?r=catalogo_taxonomico_da_fauna_do_brasil
"fauna_data"

#' SpatVector of the federal states of Brazil
#'
#' @description
#'  A simplified and packed SpatVector of the polygons of the federal states of
#'  Brazil. The spatial data was originally obtained from
#'  \code{geobr::read_state}. Borders have been simplified by removing vertices
#'  of borders using \code{terra::simplifyGeom}. It's necessary unpack the
#'  Spatvectos using \code{terra::unwrap}
#'
#'  @usage data(states)
#'  states <- terra::unwrap(states)
#'
#' @format A \code{SpatVector} with 27 geometries and 3 attributes:
#' \describe{
#'   \item{abbrev_state}{State acronym}
#'   \item{name_state}{State's full name}
#'   \item{name_region}{The region to which the state belongs}
#'   }
"states"

#' SpatVector of the world countries
#'
#' @description
#'  A simplified and packed SpatVector of the world country polygons. The
#'  spatial data was originally obtained from \code{rnaturalearth::ne_countries}.
#'  Borders have been simplified by removing vertices of borders using
#'  \code{terra::simplifyGeom}.It's necessary unpack the Spatvectos using
#'  \code{terra::unwrap}
#'
#'  @usage data(world_fauna)
#'  biomes <- terra::unwrap(world_fauna)
#'
#' @format A \code{SpatVector} with 258 geometries and 1 attribute:
#' \describe{
#'   \item{name}{The name of the country (argentina, brazil, colombia, etc.)}
#'   }
"world_fauna"

#' Records of animal species
#'
#' @description
#'  A dataset containing records of 2 species downloaded from GBIF, with
#'  additional fake data. The records were obtained with \code{plantR::rgbif2}
#'
#' @usage data(occurrences)
#'
#' @format A \code{data.frame} with 2798 rows and 3 variables:
#' \describe{
#'   \item{species}{Species names (Panthera onca and Chaetomys subspinosus)}
#'   \item{x}{Longitude}
#'   \item{y}{Latitude}
#'   \item{source}{record downloaded from GBIF or fake data}
#' }
#' @references GBIF, 2024. florabr R package: Records of plant species. https://doi.org/10.15468/DD.QPGEB7
"occurrences"

#' Country Codes and Names
#'
#' @description
#' A dataset containing country codes used in the Catálogo Taxonômico da Fauna
#' do Brasil along with their corresponding country names, as defined in
#' \code{faunabr::world_fauna}.
#'
#' @usage data(country_codes)
#'
#' @format A \code{data.frame} with 244 rows and 2 variables:
#' \describe{
#'   \item{map_name}{Country names as defined in \code{faunabr::world_fauna}.}
#'   \item{country_code}{Country codes used in the Catálogo Taxonômico da Fauna
#'   do Brasil.}
#' }
"country_codes"

#' Helpers for translating data
#'
#' @description
#' A list of data.frames used by \code{faunabr::translate_faunabr()} function.
#' \code{faunabr::map_translation}.
#'
#' @usage data(map_translation)
#'
#' @format A \code{list} with 5 data.frames ("lifeForm", "origin", "habitat",
#' "taxonRank", and "taxonomicStatus"). Each data.frame has 2 columns:
#' \describe{
#'   \item{pt_br}{The attribute in Brazilian Portuguese.}
#'   \item{en}{The attribute in English.}
#' }
"map_translation"
