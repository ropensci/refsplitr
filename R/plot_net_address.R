#' Creates a network diagram of coauthors' addresses linked by reference, and
#' with nodes arranged geographically
#'
#' This function takes an addresses data.frame, links it to an
#' authors__references dataset and plots a network diagram generated for
#' individual points of co-authorship.
#'
#' @param data the `address` element from the list outputted from the
#'   `authors_georef()` function, containing geocoded address latitude and
#'   longitude locations.
#' @param mapRegion what portion of the world map to show. possible values
#'   include `"world"`, `"North America"`, `"South America"`, `"Australia"`,
#'   `"Africa"`, `"Antarctica"`, `"Eurasia"`
#' @param lineResolution the resolution of the lines drawn, higher numbers will
#'   make smoother curves default is 10.
#' @param lineAlpha transparency of the lines, fed into ggplots alpha value.
#'   Number between 0 - 1.
#' @examples 
#' ## Using the output of authors_georef (e.g., BITR_geocode)
#' data(BITR_geocode)
#' ## Plots the whole world
#' output <- plot_net_address(BITR_geocode)
#' 
#' ## Just select North America
#' output <- plot_net_address(BITR_geocode, mapRegion = 'North America')
#' 
#' ## Change the transparency of lines by modifying the lineAlpha parameter
#' output <- plot_net_address(BITR_geocode, lineAlpha = 0.2)
#'                  
#' ## Change the curvature of lines by modifying the lineResolution paramater
#' output <- plot_net_address(BITR_geocode, lineResolution = 30 )
#'                  
## With all arguments: 
#' output <- plot_net_address(BITR_geocode, mapRegion = 'North America', lineAlpha = 0.2,
#'                  lineResolution = 30)
#' 
#' 
#' @export plot_net_address

plot_net_address <- function(data,
                      mapRegion = "world",
                      lineResolution = 10,
                      lineAlpha = 0.5){

  requireNamespace(package = "ggplot2", quietly = TRUE)
  requireNamespace(package = "network", quietly = TRUE)

  data <- data[!is.na(data$country), ]
  data <- data[!is.na(data$lat), ]
  data <- data[!is.na(data$lon), ]

  ## 	Link all authors at a particular point location:
  data$latlon <- paste0(data$lat, ",", data$lon)
  test <- data.frame(latlon = unique(data$latlon))
  test$LAT <- as.numeric(lapply(strsplit(as.character(test$latlon), ","),
    function(x) x[1]))
  test$LON <- as.numeric(lapply(strsplit(as.character(test$latlon), ","),
    function(x) x[2]))
  test$latlonalpha <- paste0("a", seq_len(nrow(test)))

  test1 <- merge(test, data[, c("latlon", "refID")], by = "latlon",
    all.y = TRUE)

  linkages <- Matrix::spMatrix(
    nrow = length(unique(test1$latlonalpha)),
    ncol = length(unique(test1$refID)),
    i = as.numeric(factor(test1$latlonalpha)),
    j = as.numeric(factor(test1$refID)),
    x = rep(1, length(test1$latlonalpha))
  )

  links <- matrix(data = linkages,
    nrow = length(unique(test1$latlonalpha)),
    ncol = length(unique(test1$refID)))
  rownames(links) <- levels(factor(test1$latlonalpha))
  colnames(links) <- levels(factor(test1$refID))

  ## Convert to a one-mode representation of countries:
  linkages_points <- links %*% t(links)

  ## Convert to a one-mode representation of references:
  # linkages_references <- t(links) %*% links

  ## 	Or we might be interested in using the network package instead of igraph:
  ## 	This loads our adjacency matrix into a network object, and we
  ## 		specify directed as FALSE, and because we use the ignore.eval=FALSE
  ## 		and names.eval="value" arguments it will load our edge counts in as
  ## 		an edge attribute named "value" which we can then use as a weighting
  ## 		or plotting attribute:
  linkages_points_net <- network::network(as.matrix(linkages_points),
    directed = FALSE,
    loops = FALSE,
    ignore.eval = FALSE,
    names.eval = "value"
  )

  #vertex_names <- (linkages_points_net %v% "vertex.names")

  ## 	Get the world map from rworldmap package:
  world_map <- rworldmap::getMap()

  edgeMaker <- function(whichRow, len = 100, curved = TRUE) {
    adjacencyList$rowname <- as.character(adjacencyList$rowname)
    adjacencyList$rownamesA <- as.character(adjacencyList$rownamesA)

    fromC <- layoutCoordinates[layoutCoordinates$latlonalpha ==
        adjacencyList[whichRow, 1], 2:3 ][1, ] # Origin
    toC <- layoutCoordinates[layoutCoordinates$latlonalpha ==
        adjacencyList[whichRow, 2], 2:3 ][1, ] # Terminus

    # Add curve:
    # graphCenter <- colMeans(layoutCoordinates[, 2:3]) #Center of overall graph
    bezierMid <- as.numeric(c(fromC[1], toC[2])) # A midpoint, for bended edges
    bezierMid <- (fromC + toC + bezierMid) / 3 # Moderate the Bezier midpoint
    if (curved == FALSE) {
      bezierMid <- (fromC + toC) / 2
    } # Remove the curve
    edge <- data.frame(Hmisc::bezier(
      as.numeric(c(fromC[2], bezierMid[2], toC[2])), # Generate
      as.numeric(c(fromC[1], bezierMid[1], toC[1])), # X & y
      evaluation = len
    )) # Bezier path coordinates
    edge$Sequence <- 1:len # For size and colour weighting in plot
    edge$Group <- paste(adjacencyList[whichRow, 1:2], collapse = ">")
    return(edge)
  }

  adjacencyMatrix <- as.matrix(linkages_points)

  layoutCoordinates <- test

  adjacencydf <- data.frame(adjacencyMatrix)
  adjacencydf <- data.frame(
    rowname = rep(row.names(adjacencydf), times = nrow(adjacencydf)),
    rownamesA = rep(row.names(adjacencydf), each = nrow(adjacencydf)),
    value = c(as.matrix(adjacencydf)),
    stringsAsFactors = FALSE)

  adjacencyList <- adjacencydf[adjacencydf$value > 0, ]

  # Generate a (curved) edge path for each pair of connected nodes
  allEdges <- lapply(seq_len(nrow(adjacencyList)),
    edgeMaker,
    len = lineResolution,
    curved = TRUE
  )

  allEdges <- do.call(rbind, allEdges) # a fine-grained path ^, with bend ^

  empty_theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      line = ggplot2::element_blank(),
      #rect = ggplot2::element_blank(),
      #axis.text = ggplot2::element_blank(),
      #strip.text = ggplot2::element_blank(),
      plot.title = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      # plot.margin = structure(c(0, 0, -1, -1),
      #                         unit = "lines",
      #                         valid.unit = 3L,
      #                         class = "unit"
      # )
    )

  
  sf_convert <- function(map) {
    do.call("rbind",
            lapply(map@polygons, function(x) {
              do.call("rbind",
                      lapply(seq_along(x@Polygons), function(i) {
                        df <- setNames(as.data.frame(x@Polygons[[i]]@coords), c("long", "lat"))
                        df$order <- 1:nrow(df)
                        df$hole <- x@Polygons[[i]]@hole
                        df$piece <- i
                        df$id <- x@ID
                        df$group <- paste(x@ID, i, sep = '.')
                        df
                      }))
            })
    )
  }
  
  world_map_sub <- sf_convert(world_map) # and now this instead of fortify()
  # world_map_sub <- ggplot2::fortify(world_map) # fortify() was deprecated 
  # world_map_sub <- sf::st_as_sf(world_map) st_as_sf() is an alternative,
  # but the output includes all the polygons needed to map in a list-column.
  # This function converts the map `st` object to a data frame like fortify(). 
  
  
  if (mapRegion != "world") {
    world_map_sub <- world_map[which(world_map$continent == mapRegion &
        world_map$TYPE != "Dependency"), ]
  }
  ## 	Create the world outlines:
  world_map@data$id <- rownames(world_map@data)
  world_map.points <- ggplot2::fortify(world_map)
  world_map.df <- merge(world_map.points,
    world_map@data, by = "id", all = TRUE)
  world_map.df <- world_map.df[!is.na(world_map.df$lat), ]
  # world_map.df <- dplyr::full_join(world_map.points,
  #   world_map@data, by = "id")

  ## calculate min and max for plot
  latmin <- min(world_map.df$lat)
  latmax <- max(world_map.df$lat)
  longmin <- min(world_map.df$lon)
  longmax <- max(world_map.df$lon)
  # latmin <- world_map_sub@bbox["y", "min"]
  # latmin <- world_map_sub@bbox["y", "min"]
  # latmax <- world_map_sub@bbox["y", "max"]
  # longmin <- world_map_sub@bbox["x", "min"]
  # longmax <- world_map_sub@bbox["x", "max"]

  if (mapRegion == "Australia"){
    longmin <- 100
  }
  products <- list()
  lat <- ggplot2::quo(lat)
  long <- ggplot2::quo(long)
  group <- ggplot2::quo(group)
  x <- ggplot2::quo(x)
  y <- ggplot2::quo(y)
  Group <- ggplot2::quo(Group)
  Sequence <- ggplot2::quo(Sequence)
  LAT <- ggplot2::quo(LAT)
  LON <- ggplot2::quo(LON)
  lineAlpha <- ggplot2::enexpr(lineAlpha)
  
  products[["plot"]] <- ggplot2::ggplot() +
    ggplot2::geom_polygon(
      data = world_map.df,
      ggplot2::aes(!!long, !!lat, group = !!group),
      fill = grDevices::gray(8 / 10)
    ) +
    ggplot2::geom_path(
      data = world_map.df,
      ggplot2::aes(!!long, !!lat, group = !!group),
      
      color = grDevices::gray(6 / 10)
    ) +
    ggplot2::coord_equal(ylim = c(latmin, latmax),
      xlim = c(longmin, longmax)) +
    ggplot2::geom_path(
      data = allEdges,
      ggplot2::aes(
        x = !!x, y = !!y,
        group = !!Group, # Edges with gradient
        colour = !!Sequence,
        linewidth = !!Sequence
      ),
      alpha = lineAlpha
    ) +
    ggplot2::geom_point(
      data = data.frame(layoutCoordinates),
      ggplot2::aes(x = !!LON, y = !!LAT),
      size = 3 + 100 * sna::degree(linkages_points_net,
        cmode = "outdegree", rescale = TRUE
      ),
      pch = 21,
      colour = grDevices::rgb(8 / 10, 2 / 10, 2 / 10, alpha = 5 / 10),
      fill = grDevices::rgb(9 / 10, 6 / 10, 6 / 10, alpha = 5 / 10)
    ) +
    ggplot2::scale_colour_gradient(
      low = grDevices::rgb(8 / 10, 2 / 10, 2 / 10, alpha = 5 / 10),
      high = grDevices::rgb(8 / 10, 2 / 10, 2 / 10, alpha = 5 / 10),
      guide = "none"
    ) +
    ggplot2::scale_size(range = c(5 / 10, 5 / 10), guide = "none") +
    empty_theme+
    ggplot2::theme(legend.position="none")

  products[["data_path"]] <- allEdges
  products[["data_polygon"]] <- world_map.df
  products[["data_points"]] <- data.frame(layoutCoordinates)

  return(products)
}
