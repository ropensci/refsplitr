# refsplitr News


refsplitr 1.0.2 (2024-08-12)
=========================

### NEW FEATURES


  * `references_read` now extracts additional fields from Web of Science records: WE (Source Database), C3 (all author affiliations, equivalent to the Scopus `affiliations` field code), EI (eISSN), OA (Open Access), and RID (the original version of the Thomson-Reuters ResearcherID (RI); authors of some older publications might have an RID but not an RI). These are not included in the default output of `references_read`, to include them use `include_all = TRUE`.  

  * `references_read` no longer extracts some rarely used field codes: GE, LT, MC, MI, and TA
  
  * The following field codes are now returned by default when using `references_read`: DT (Document Type), ID (Keywords Plus), IS (Issue), JI (ISO abbreviated source code), and  NR (number of references cited by the article). 


refsplitr 1.0.1 (2024-07-23)
=========================

### NEW FEATURES

  * output of `plot_net_country()` now includes a list of any authors that have a lat-lon but no country (called with `products$fixable_countries`).Users can correct these and re-run the visualization to include them in the graph. 

### DEPRECATED AND DEFUNCT

  * Removed the dependency on deprecated package [maptools](https://cran.r-project.org/web/packages/maptools/index.html).    [(#90)](https://github.com/ropensci/refsplitr/issues/90)

### DOCUMENTATION FIXES

  * Updated README with citation of the _Journal of Open Source Software_ article describing refsplitr.


refsplitr 0.9.0 (2020-01-14)
=========================

### NEW FEATURES

  * Released refsplitr on rOpenSci website.  
