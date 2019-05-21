#' Creates a network diagram of coauthors' countries linked by reference
#' This function takes an addresses data.frame, 
#' links it to an authors_references dataset and plots a network diagram 
#' generated for co-authorship.
#'
#' @param data the `address` element from the list outputted from 
#' the `authors_georef()`` function, containing geocoded address 
#' latitude and longitude locations.
#' @examples 
#' data(BITR_geocode)
#' plot_net_coauthor(BITR_geocode)
#' @export plot_net_coauthor
#' 
plot_net_coauthor <- function(data) {

  data <- data[!is.na(data$country), ]

  linkages <- as.matrix(Matrix::sparseMatrix(
    dims = c(length(unique(data$country)),
    length(unique(data$UT))),
    i = as.numeric(factor(data$country)),
    j = as.numeric(factor(data$UT)),
    x = rep(1, length(data$country))))

  links <- matrix(data = linkages,
    nrow = length(unique(data$country)),
    ncol = length(unique(data$UT)))

  row.names(links) <- levels(factor(data$country))

  colnames(links) <- levels(factor(data$UT))

  ## 	Convert to a one-mode representation of countries:
  linkages_countries <- links %*% base::t(links)

  ## 	Convert to a one-mode representation of references:
  # linkages_references <- base::t(links) %*% links

  ## 	Create an igraph object from our countries linkages:

  linkages_countries_net <- igraph::graph.adjacency(linkages_countries,
    mode = "undirected",
    weighted = TRUE
  )

  igraph::V(linkages_countries_net)$label <-
          igraph::V(linkages_countries_net)$name
  igraph::V(linkages_countries_net)$label.color <- grDevices::rgb(0, 0, .2, .5)
  igraph::V(linkages_countries_net)$label.cex <- 0.5
  igraph::V(linkages_countries_net)$size <- 12
  igraph::V(linkages_countries_net)$frame.color <- NA
  igraph::V(linkages_countries_net)$color <- grDevices::rgb(0, 0.6, 0, 0.7)

  ## 	Simplify the network edges by removing the diagonal and other half
  # (assuming it's symmetric/undirected:
  linkages_countries_net <- igraph::simplify(linkages_countries_net)

  co <- igraph::layout_with_fr(linkages_countries_net)

  graphics::plot(linkages_countries_net,
    layout = co)
  return(linkages_countries_net)
}
