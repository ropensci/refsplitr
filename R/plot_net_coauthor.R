########################################
########################################
## 	BEGIN: plot_net_coauthor():

#' Creates a network diagram of coauthors' countries linked by reference
#'
#' \code{plot_net_coauthor} This function takes an addresses data.frame, links it to an authors_references dataset and plots a network diagram generated for co-authorship.
#'
#' @param data the `address` element from the list outputted from the `authors_georef()`` function, containing geocoded address latitude and longitude locations.

plot_net_coauthor <- function(data) {
  
  data <- data[!is.na(data$country), ]

  
  linkages <- as.matrix(Matrix::sparseMatrix(
    dims=c(length(unique(data$country)), 
           length(unique(data$UT))),
    i = as.numeric(factor(data$country)),
    j = as.numeric(factor(data$UT)),
    x = rep(1, length(data$country))))

  row.names(linkages) <- levels(factor(data$country))

  colnames(linkages) <- levels(factor(data$UT))

  ## 	Convert to a one-mode representation of countries:
  linkages_countries <- linkages %*% base::t(linkages)

  ## 	Convert to a one-mode representation of references:
  linkages_references <- t(linkages) %*% linkages

  ## 	Create an igraph object from our countries linkages:

  linkages_countries_net <- graph.adjacency(linkages_countries,
    mode = "undirected",
    weighted = TRUE
  )

  V(linkages_countries_net)$label <- V(linkages_countries_net)$name
  V(linkages_countries_net)$label.color <- rgb(0, 0, .2, .5)
  V(linkages_countries_net)$label.cex <- 0.5
  V(linkages_countries_net)$size <- 12
  V(linkages_countries_net)$frame.color <- NA
  V(linkages_countries_net)$color <- rgb(0, 0.6, 0, 0.7)

  ## 	Simplify the network edges by removing the diagonal and other half (assuming it's symmetric/undirected:
  linkages_countries_net <- simplify(linkages_countries_net)

return(plot(linkages_countries_net, 
     layout = layout.fruchterman.reingold))

}

## 	END: net_plot_coauthor():
########################################
########################################
