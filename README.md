# refnet <img src="man/figures/refnethex.png" height="200" align="right">

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)    [![Travis-CI Build Status](https://travis-ci.org/embruna/refnet.svg?branch=master)](https://travis-ci.org/embruna/refnet)    [![](https://badges.ropensci.org/256_status.svg)](https://github.com/ropensci/onboarding/issues/256)   [![Coverage status](https://coveralls.io/repos/github/embruna/refnet/badge.svg)](https://coveralls.io/r/embruna/refnet?branch=master)

refnet: an R package for processing Web of Science Records and mapping georeferenced coauthorship networks

refnet (v1.0) is an R package to read, organize, geocode, analyze, and visualize Clarivate Web of Knowledge/Web of Science, format reference data files for scientometric, social network, and Science of Science analyses. The original package development was by Forrest Stevens and Emilio M. Bruna and was on r-forge (https://r-forge.r-project.org/projects/refnet/); in December 2017 Bruna moved it to Github to facilitate updating the package and hired [Porzana Solutions](https://github.com/aurielfournier) to finalize the package and prepare it for submission to CRAN.  <b>Please make all future changes via this Github repo! Do *not* make a repo mirror of the R-forge version.</b> 

Support for the development of refnet was provided by grants from the [University of Florida Center for Latin American Studies](http://www.latam.ufl.edu/) and the [University of Florida Informatics Institute](https://informatics.institute.ufl.edu/).

## Installation

You can install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("embruna/refnet")
```

## Workflow

```{r example, eval=FALSE}
references_read()
authors_clean()
authors_refine()
authors_georef()
```

Issues, Feature Requests and Pull Requests Welcome, see here for more details on [how to contribute](https://github.com/embruna/refnet/blob/master/CONTRIBUTING.md). We expect everyone contributing to the package to abide by our [Code of Conduct](https://github.com/embruna/refnet/blob/master/CODE_OF_CONDUCT.md). 


## Contributors
* [Auriel Fournier](https://github.com/aurielfournier), Porzana Solutions
* [Forrest Stevens](http://forreststevens.com/teaching/research.html), University of Louisville
* [Matt Boone](https://github.com/birderboone), Porzana Solutions
* [Emilio Bruna](https://github.com/embruna), University of Florida

## Citation

Auriel M.V. Fournier Developer, Forrest R.
  Stevens Developer, Matthew E. Boone Developer
  and Emilio Bruna Developer (2018). refnet:
  Clarivate Web of Knowledge / Web of Science  Reference Data Tools. R package version
  0.6.
  
    @Manual{,
    title = {refnet: Thomson Reuters Web of Knowledge/Science Reference Data Tools},
    author = {Auriel M.V. Fournier Developer and Forrest R. Stevens Developer and Matthew E. Boone Developer and Emilio Bruna Developer},
    year = {2018},
    note = {R package version 0.6},
  }
  
  
