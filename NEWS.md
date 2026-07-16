# refsplitr News



refsplitr 1.2.2 (2026-06-16)
=========================


### MINOR IMPROVEMENTS 
  * authors_clean() would sometimes combine authors with very similar or 
  identical names in the same groupID even if they had different ORCID ID 
  numbers. A tweak in `authors_match` now eliminates these 'false positives'.


refsplitr 1.2.1 (2026-06-15)
=========================

### MINOR IMPROVEMENTS

  * modified the section of `authors_parse` that identifies corresponding author 
  and reprint address (RP field code). There are cases where there is more than 
  one corresponding author, with names separated by a ';'. This was causing both 
  authors to be ignored. The revision treats only the first name provided as the 
  corresponding author. 
  
  * email matching with author names improved. It previously compared similarity
  of author name fields (AF and AU) with emails to match them with jarow-winkler. 
  However, this was reducing the likelihood of a match because (1) AU is in 
  format "last, first", but emails don't have commas or spaces which was slightly 
  penalizing the match and (2) j-w matching favors with the same initial 
  characters, but emails are often in reverse order, e.g., AF = "last, first", 
  while email = "first.last@...". Now compares AU and AF after commas and spaces
  are removed and uses Jaccard to compare the strings to allow for reverse order
  of names in emails and AF/AU fields.
  
  * WOS records have (rare) errors as follows in the OI field :
  "Lastname, Firstname / Firstname/0000-0003-0000-3152" instead of the correct:
  "Lastname, Firstname/0000-0003-0000-3152". This was causing OIs to be deleted 
  from an authorID. Added a function to check for these errors and correct them.


### BUG FIXES
  * corrected an error in `authors_address` that was changing "USA" to "NA" when
  parsing countries.




refsplitr 1.2 (2025-04-25)
=========================

### NEW FEATURES

  * The new default service for georeferencing author institutions is the free 
  [Nominatim](https://nominatim.org/) service, which uses OpenStreetMap (OSM) data and 
  which `refsplitr` queries via the [`tidygeocoder`]((https://jessecambon.github.io/tidygeocoder/) 
  package.[`tidygeocoder`](https://jessecambon.github.io/tidygeocoder/). 
  The Google Maps API is still an option, but users should be aware that their 
  georeferencing request may exceed the lower limit of free queries.
  
  * The `authors_addresses` function has been updated and is now more efficient.
  
  * In `plot_net_address`: the deprecated function `fortify` has been replaced 
  with `sf_convert`


refsplitr 1.0.2 (2024-08-12)
=========================

### NEW FEATURES


  * `references_read` now extracts additional fields from Web of Science 
  records: WE (Source Database), C3 (all author affiliations, equivalent to the 
  Scopus `affiliations` field code), EI (eISSN), OA (Open Access), and RID 
  (the original version of the Thomson-Reuters ResearcherID (RI); authors of 
  some older publications might have an RID but not an RI). These are not 
  included in the default output of `references_read`; to include 
  them use `include_all = TRUE`.  

  * `references_read` no longer extracts some rarely used field codes: 
  GE, LT, MC, MI, and TA
  
  * The following field codes are now returned by default when using 
  `references_read`: DT (Document Type), ID (Keywords Plus), IS (Issue), 
  JI (ISO abbreviated source code), and  NR (number of references cited 
  by the article). 
  
  * corrected a bug that was 


refsplitr 1.0.1 (2024-07-23)
=========================

### NEW FEATURES

  * output of `plot_net_country()` now includes a list of any authors that have 
  a lat-lon but no country (called with `products$fixable_countries`).Users can 
  correct these and re-run the visualization to include them in the graph. 

### DEPRECATED AND DEFUNCT

  * Removed the dependency on deprecated package 
  [maptools](https://cran.r-project.org/web/packages/maptools/index.html).    
  [(#90)](https://github.com/ropensci/refsplitr/issues/90)

### DOCUMENTATION FIXES

  * Updated README with citation of the _Journal of Open Source Software_ 
  article describing refsplitr.


refsplitr 0.9.0 (2020-01-14)
=========================

### NEW FEATURES

  * Released refsplitr on rOpenSci website.  
