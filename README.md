# refnet2

Update to refnet package for processing Web of Science Records

refnet was v1.0 of a R package to read, analyze and visualize Thomson-Reuters Web of Knowledge/Science, ISI and SCOPUS format reference data files. Social network analyses, geocoding of addresses and spatial visualization are supported. The original package development was by Forrest Stevens and Emilio Bruna and was on r-forge (https://r-forge.r-project.org/projects/refnet/), but in December 2017 Bruna moved it to github to update the package as refnet2.  <b>Please make all future changes via this Github repo! Do *not* make a repo mirror of the R-forge version.</b> 

## Installation

You can install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("embruna/refnet2",   subdir="refnet")
```

## Workflow

```{r example, eval=FALSE}
read_references()
read_authors()
refine_authors()
address_lat_long()
```

Issues, Feature Requests and Pull Requests Welcome


## Contributors
* Forest Stevens
* [Auriel Fournier](https://github.com/aurielfournier)
* [Matt Boone](https://github.com/birderboone)
* [Emilio Bruna](https://github.com/embruna)
