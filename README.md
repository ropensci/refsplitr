# refsplitr <img src="man/figures/refsplitrhex.png" height="200" align="right">

[![Project Status: Active The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)    [![Build Status](https://travis-ci.org/embruna/refsplitr.svg?branch=master)](https://travis-ci.org/embruna/refsplitr)    [![](https://badges.ropensci.org/256_status.svg)](https://github.com/ropensci/onboarding/issues/256)   [![Coverage Status](https://coveralls.io/repos/github/embruna/refnet/badge.svg?branch=master)](https://coveralls.io/github/embruna/refnet?branch=master)

refsplitr: an R package for processing Web of Science Records and mapping georeferenced coauthorship networks

refsplitr (v1.0) is an R package to read, organize, geocode, analyze, and visualize Clarivate Web of Knowledge/Web of Science, format reference data files for scientometric, social network, and Science of Science analyses. 

Support for the development of refsplitr was provided by grants from the [University of Florida Center for Latin American Studies](http://www.latam.ufl.edu/) and the [University of Florida Informatics Institute](https://informatics.institute.ufl.edu/).

## Installation

You can install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("embruna/refsplitr")
```

## Workflow

```{r example, eval=FALSE}
dat1 <- references_read(data = system.file("extdata", "example_data.txt", package = "refsplitr"), dir = FALSE)
dat2 <- authors_clean(references = dat1)
dat3 <- authors_refine(dat2$review, dat2$prelim)
dat4 <- authors_georef(dat3)

plot_addresses_points(dat4$addresses)
```

Issues, Feature Requests and Pull Requests Welcome, see here for more details on [how to contribute](https://github.com/embruna/refsplitr/blob/master/CONTRIBUTING.md). We expect everyone contributing to the package to abide by our [Code of Conduct](https://github.com/embruna/refsplitr/blob/master/CODE_OF_CONDUCT.md). 


## Contributors
* [Auriel Fournier](https://github.com/aurielfournier), Porzana Solutions
* [Matt Boone](https://github.com/birderboone), Porzana Solutions
* [Forrest Stevens](http://forreststevens.com/teaching/research.html), University of Louisville
* [Emilio M. Bruna](https://github.com/embruna), University of Florida

## Citation

Auriel M.V. Fournier Developer, Matthew E. Boone Developer, Forrest R.
  Stevens Developer and Emilio M. Bruna Developer (2018). refsplitr:
  Clarivate Web of Knowledge / Web of Science  Reference Data Tools. R package version
  0.6.
  
    @Manual{,
    title = {refsplitr: Thomson Reuters Web of Knowledge/Science Reference Data Tools},
    author = {Auriel M.V. Fournier Developer and Matthew E. Boone Developer and Forrest R. Stevens Developer and Emilio M. Bruna Developer},
    year = {2018},
    note = {R package version 0.6},
    }
## Note regarding package history

The early development of refsplitr - initially known as refnet - was by Forrest Stevens and Emilio M. Bruna and was on r-forge (https://r-forge.r-project.org/projects/refnet/). In December 2017 Bruna moved it to Github and hired [Porzana Solutions](https://github.com/aurielfournier) to finalize the package and prepare it for submission to CRAN.  <b>Please make all suggestions for changes via this Github repository - do *not* make a repo mirror of the R-forge version.</b> 

  
  
