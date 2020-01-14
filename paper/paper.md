---
title: 'refsplitr: Author name disambiguation, author georeferencing, and mapping of coauthorship networks with Web of Science data'
tags:
  - name disambiguation
  - bibliometrics
  - coauthorship
  - collaboration
  - georeferencing
  - metascience
  - scientometrics
  - science of science
  - Web of Science
authors:
  - name: Auriel M.V. Fournier
    orcid: 0000-0002-8530-9968
    affiliation: 1
  - name: Matthew E. Boone
    orcid: 0000-0002-8070-4715
    affiliation: 1
  - name: Forrest R. Stevens
    orcid: 0000-0002-9328-3753
    affiliation: 2
  - name: Emilio M. Bruna
    orcid: 0000-0003-3381-8477
    affiliation: "3, 4"

affiliations:
 - name: Porzana Solutions, Marquette Heights, IL, 61554, USA
   index: 1
 - name: Department of Geography & Geosciences, University of Louisville, Louisville, KY, 40292, USA
   index: 2
 - name: Center for Latin American Studies, University of Florida, Gainesville, FL, 32611-5530, USA
   index: 3
 - name: Department of Wildlife Ecology & Conservation, University of Florida, Gainesville, FL, 32611-4430, USA
   index: 4
citation_author: Fournier et. al.
date: 14 January 2020
year: 2020
formatted_doi: 
bibliography: paper.bib
output: rticles::joss_article
csl: apa.csl
journal: JOSS
---

# Summary

The Science of Science (SciSci) is an emerging, trans-disciplinary approach for using large and disparate data-sets to study the emergence, dissemination, and impact of scientific research [@fortunato2018science]. Bibliometric databases such as the [Web of Science](https://clarivate.com/products/web-of-science/) are rich sources of data for SciSci studies [@sugimoto2018measuring]. In recent years the type and scope of questions addressed with data gathered from these databases has expanded tremendously [@fortunato2018science]. This is due in part to their expanding coverage and greater accessibility, but also because advances in computational power make it possible to analyze data-sets comprising millions of bibliographic records [e.g., @lariviere2013bibliometrics;@smith2014scientific].

The rapidly increasing size of bibliometric data-sets available to researchers has exacerbated two major and persistent challenges in SciSci research. The first of these is **Author Name Disambiguation**. Correctly identifying the authors of a research product is fundamental to bibliometric research, as is the ability to correctly attribute to a given author all of their scholarly output. However, this seemingly straightforward task is often extremely complicated, even when using the nominally high-quality data extracted from bibliometric databases [reviewed in @smalheiser2009author]. The most obvious case is when different authors have identical names, which can be quite common in some countries [@strotmann2012author]. However, confusion might also arise as a result of journal conventions or individual preferences for abbreviating names. For instance, one might conclude "J. C. Smith", "Jennifer C. Smith", and "J. Smith" are different authors, when in fact they are the same person. In contrast, papers by "E. Martinez" could have been written by different authors with the same last name but whose first names start with the same letter (e.g., "Enrique", "Eduardo"). Failure to disambiguate author names can seriously undermine the conclusions of some SciSci studies, but manually verifying author identity quickly becomes impractical as the number of authors or papers in a dataset increases. 

The second challenge to working with large bibliometric data-sets is correctly **parsing author addresses**. The structure of author affiliations is complex and idiosyncratic, and journals differ in the information they require authors to provide and the way in which they present it. Authors may also represent affiliations in different ways on different articles. For instance, the affiliations might be written in different ways in different journals (e.g., "Dept. of Biology", "Department of Biology", "Departamento de Biologia"). The same is true of institution names ("UC Davis", "University of California-Davis","University of California") or the country in which they are based ("USA", "United States", "United States of America"). Researchers at academic institutions might include the one or more Centers, Institutes, Colleges, Departments, or Programs in their address, and researchers working for the same institution could be based at units in geographically disparate locations (e.g., University of Florida  researchers could be based at the main campus in Gainesville or one of dozens of facilities across the state, including 12 Research and Education Centers, 5 field stations, and 67 County Extension Offices). Finally, affiliations are recorded in a single field of the reference bibliographic records, despite comprising very different types of information (e.g., city, postal code, institution). In concert, these factors can make it challenging to conduct analyses for which author affiliation or location is of particular interest.     

Package [**``refsplitr``**](https://github.com/ropensci/refsplitr) helps users of the R statistical computing environment [@Rlanguage] address these challenges. It imports and organizes the output from Web of Science searches, disambiguates the names of authors, suggests author names that would benefit from additional review to verify the disambiguation, parses author addresses, and georeferences author institutions. It then generates maps indicating the locations of authors and georeferenced coauthorship networks. Finally, the processed data-sets can be exported in tidy formats for analysis with user-written code or, after some additional formatting, packages such as [**``revtools``**](https://cran.r-project.org/package=revtools) [@westgate2018revtools] or [**``bibliometrix``**](https://CRAN.R-project.org/package=bibliometrix) [@aria2017bibliometrix].

![Map of georeferenced article coauthorships generated with ``refsplitr``.](coauthor_connections_BITR_copy.png)

# Acknowledgements

Support for the development of refsplitr was provided by grants to E. M. Bruna from the [University of Florida Center for Latin American Studies](http://www.latam.ufl.edu/) and the [University of Florida Informatics Institute](https://informatics.institute.ufl.edu/). We are extremely grateful to Bianca Kramer and Najko Jahn for the feedback they provided during the [review of `refsplitr` by rOpenSci](https://github.com/ropensci/software-review/issues/256).

# References
