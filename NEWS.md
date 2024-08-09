# refsplitr News


refsplitr 1.X.X (2024-XX-XX)
=========================

### NEW FEATURES

  * `references_read` now extracts additional fields from Web of Science records: WE (Source Database), C3 (affiliations*), EI (eISSN) and RID (the original version of the Thomson-Reuters ResearcherID (RI); authors of some older publications might have an RID but not an RI). To include these in the output of`references_read` use the setting `include_all=TRUE`.  

  *a single cell with list of all affiliations, not brtoken down by author. to match scopus
  * `references_read` no longer extracts some rarely used field codes: GE, LT, MC, MI, and TA
  
  * the Document Type (DT), Keywords Plus (ID), Issue (IS), ISO abbreviated source code (JI), and number of references cited in an article (NR) are now returned by default (`include_all=FALSE`). 



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
