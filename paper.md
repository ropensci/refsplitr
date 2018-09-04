---
title: 'Refnet: Thomson Reuters Web of Knowledge/Science and ISI Reference Data Tools'
tags:
  - tagone
  - tagtwo
  - tagthree
authors:
 - name: Auriel M.V. Fournier
   orcid: 0000-0002-8530-9968
   affiliation: 1
 - name: Forrest R. Stevens
   orcid: 0000-0002-9328-3753
   affiliation: 2
 - name: Matthew E. Boone
   orcid: 
   affiliation: 1
 - name: Emilio M. Bruna
   orcid: 0000-0003-3381-8477
   affiliation: 2
affiliations:
 - name: Porzana Solutions
   index: 1
 - name: University of Florida
   index: 2
---

# Summary


Bibliometric databases such as the [Web of Science](https://login.webofknowledge.com/error/Error?PathInfo=%2F&Error=IPError) and [SCOPUS](https://www.scopus.com/home.uri) are rich sources of data for studying factors shaping the emergence, disemination, and impact of scientific research (Sugimoto and Lariviare 2018). In recent years the type and scope of questions that researchers can address with data gathered from these databases has expanded tremendously (Forutnato et al. 2018), in part because of their expanding coverage, greater accessibility, and advances in computational power that make it possible to analyze datasets comprising millions of bibliographic records (e.g., Lariviare et al. 2013, Smith et al. 2014). Nevertheless, conducting these analyses continues to be hampered by two major and persistent challenges:

The first of these challenges is **Author Name Disambiguation (AND)**. Correctly identifying the authors of a research product is fundamental to bibliometric research, as is the ability to correctly attribute to an author all of their scholarly output. However, this seemingly simple task can often be extremely complicated, even when using the nominally high-quality data extracted from bibliometric databases (reviewed in Smalheiser and Torvik 2009). The most obvious case is when different authors have identical names, which can be quite common in some countries (Strotmann et al. 2009). However, confusion might also arise as a result of journal conventions or individual preferences for abbreviating names. For instance, one could erroneously conclude that "J.C. Smith", "Jennifer C. Smith", and "J. Smith" are different authors, while papers by "E. Gonzalez" could have been written be written by a single author or by two different ones whose first names start with the same letter (e.g., "Enrique", "Eduardo"). Failure to disambiguate author names can seriously undermine the conclusions of analyses, but verifying author identity manually quickly becomes impractical as the number of authors in a dataset increases. 

The second challenge is correctly **parsing author addresses**. The structure of author affiliations is complex and idiosyncratic, and journals differ in the information they require authors to provide as well as the way in which they present it. For instance, researchers at academic institutions might include the one or more Centers, Institutes, Colleges, Departments, or Programs in their address. The affiliations might be written in different ways in different journals (e.g., "Dept. of Biology", "Department of Biology", "Departamento de Biologia"); the same is true of the institution ("UC Davis", "University of California-Davis","University of California") or country in which it is based ("USA", "United States", "United States of America"). Researchers working for the same institution could be based at units in geographically disparate locations. Finally, these complex affiliations are all recorded in a single field of a reference's bibliographic record. In concert, these factors can make it challenging to carry out analyses for which author affiliation or location is of particular interest.     

Package **`refnet`** was written to help users of the R statistical computing environment (R Core Team 2017) address these challenges. It imports and organizes the output from Web of Science and SCOPUS searches, disambiguate author names and flags any needing additional scrutiny, parses author addresses, and georeferences author institutions. It also generates simple summary bibliometric statistics and maps author locations and coauthorships. Finally, the data can be exported in tidy formats for more in-depth analyses with user-written code or imported into packages such as 'revtools' (Westgate 2018a,b) or 'bibliometrix' (Aria & Cuccurullo 2017).


Once bibliographic records have been downloaded (for guidance see Appendix 1), `refnet`'s tools are applied in four steps:   
- (1) importing and tidying reference records,   
- (2) parsing of author addresses and author name disambiguation,  
- (3) georeferencing of author institutions, and   
- (4) data analysis and visualization (Fig 1).    

Each of these steps is described below; Appendix 2 guides users through the entire process with a sample dataset.  

<p align="center">
<img src="vignettes/RefNet_fig1_eb.jpg" height="300" /> 
</p>
<p align="center">
**Figure 1:** _refnet_ allows users to import, organize, and visualize bibliographic data in 4 simple steps.
</p>


# References


Aria, M. & Cuccurullo, C. (2017) bibliometrix: An R-tool for comprehensive science mapping analysis. Journal of Informetrics. 11(4): 959-975.

Fortunato, S., C. T. Bergstrom, K. Barner, J. A. Evans, D. Helbing, S. Milojevic, A. M. Petersen, F. Radicchi, R. Sinatra, B. Uzzi, A. Vespignani, L. Waltman, D. Wang, & A.-L. Barabasi (2018). Science of science. Science, 359:eaao0185.

Lariviare, V., Ni, C., Gingras, Y., Cronin, B., & Sugimoto, C. R. (2013). Bibliometrics: Global gender disparities in science. Nature News, 504(7479): 211-213

R Core Team (2017). R: A Language and Environment for Statistical Computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.

Smalheiser, N. R., & Torvik, V. I. (2009). Author name disambiguation. Annual Review of Information Science and Technology 43(1): 1-43.

Smith, M. J., Weinberger, C., Bruna, E. M., & Allesina, S. (2014). The scientific impact of nations: Journal placement and citation performance. PLoS One 9(10): e109195.

Strotmann, A. and Zhao, D., (2012). Author name disambiguation: What difference does it make in author based citation analysis?. Journal of the Association for Information Science and Technology. 63(9): 1820-1833.

Sugimoto CR, Lariviare V. (2018). Measuring Research: What Everyone Needs to Know®. Oxford University Press, Oxford, UK. 149 pp.

Westgate, M. J. (2018a). revtools: Tools to Support Evidence Synthesis. R package version 0.2.2.
  https://CRAN.R-project.org/package=revtools
  
Westgate, M. J. (2018b). revtools: bibliographic data visualization for evidence synthesis in R. bioRxiv:262881. doi: 10.1101/262881
