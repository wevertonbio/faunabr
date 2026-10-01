# faunabr 1.1.2 (August 2026)

* Update `fauna_version()` to notify users when the IPT is unavailable and allow version checking in this case.
* Update `get_faunabr()` to notify users when the IPT is unavailable and allow downloading a fixed version from Zenodo.


# faunabr 1.1.1 (August 2026)
* Fix a bug when merging data and solving discrepancies due to changes in the data provided by the Catálogo Taxonômico da Fauna do Brasil.

# faunabr 1.1.0 (July 2026)

* Fix citation of GBIF data in `occurrences`.
* Fix a bug when merging data due to changes in the data provided by the Catálogo Taxonômico da Fauna do Brasil.
* Optimized `merge_data()` by transitioning backend core operations to `data.table` for better performance.

# faunabr 1.0.1 (April 2026)

* Fix a bug when merging data due to changes in the data provided by the Catálogo Taxonômico da Fauna do Brasil

# faunabr 1.0.0 (October 2025)

* Initial CRAN submission.
