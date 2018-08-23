########################################
########################################
##	BEGIN: net_plot_coauthor_address():

#' Creates a network diagram of coauthors' addresses linked by reference, and with nodes arranged geographically
#' 
#' \code{net_plot_coauthor_address} This function takes an addresses data.frame, links it to an authors__references dataset and plots a network diagram generated for individual points of co-authorship.
#' 
#' @param addresses the `address` element from the list outputted from the `georef_authors()`` function, containing geocoded address latitude and longitude locations.
#' @param mapRegion what portion of the world map to show. possible values include ["world","North America","South America","Australia","Africa","Antarctica","Eurasia"]
#' 
net_plot_coauthor_address <- function(data,
                                      line_resolution=10,
                                      mapRegion="world") {
  
  # require(Matrix)
  # require(network)
  # require(sna)
  # require(maps)
  # require(maptools)
  
  data <- data[!is.na(data$country),]
  
  data <- data[!is.na(data$lat),]
  
  data <- data[!is.na(data$lon),]
  
  ##	Link all authors at a particular point location:
  data$latlon <- paste0(data$lat, ",", data$lon) 
  
  #data$latlonalpha <- paste0("a",1:nrow(data))
  test<-data.frame(latlon=unique(data$latlon))
  test$LAT<-as.numeric(sapply(strsplit(as.character(test$latlon),','),function(x)x[2]))
  test$LON<-as.numeric(sapply(strsplit(as.character(test$latlon),','),function(x)x[1]))
  test$latlonalpha<-paste0('a',1:nrow(test))
  
  test1<-merge(test, data[,c('latlon','refID')],by='latlon',all.y=T)
  #data$latlonalpha <- paste0("a",1:nrow(data))
  
  # linkages <- spMatrix(nrow=length(unique(data$latlonalpha)),
  #                      ncol=length(unique(data$UT)),
  #                      i = as.numeric(factor(data$latlonalpha)),
  #                      j = as.numeric(factor(data$UT)),
  #                      x = rep(1, length(data$latlonalpha)) 
  # )
  # 
  linkages <- spMatrix(nrow=length(unique(test1$latlonalpha)),
                       ncol=length(unique(test1$refID)),
                       i = as.numeric(factor(test1$latlonalpha)),
                       j = as.numeric(factor(test1$refID)),
                       x = rep(1, length(test1$latlonalpha)) 
  )
  
  rownames(linkages) <- levels(factor(test1$latlonalpha))
  colnames(linkages) <- levels(factor(test1$refID))
  
  ##	Convert to a one-mode representation of countries:
  linkages_points <- linkages %*% t(linkages)
  
  ##	Convert to a one-mode representation of references:
  linkages_references <- t(linkages) %*% linkages
  
  
  ##	Or we might be interested in using the network package instead of igraph:
  
  ##	This loads our adjacency matrix into a network object, and we 
  ##		specify directed as FALSE, and because we use the ignore.eval=FALSE
  ##		and names.eval="value" arguments it will load our edge counts in as
  ##		an edge attribute named "value" which we can then use as a weighting
  ##		or plotting attribute:
  linkages_points_net <- network(as.matrix(linkages_points),
                                 directed=FALSE, 
                                 loops=FALSE, 
                                 ignore.eval=FALSE, 
                                 names.eval="value")
  
  vertex_names <- (linkages_points_net %v% "vertex.names")
  
  ##	Get the world map from rworldmap package:
  world_map <- getMap()
  

  coords_df<-test1

edgeMaker <- function(whichRow, len = 100, curved = TRUE){
    
    adjacencyList$rowname <- as.character(adjacencyList$rowname)
    adjacencyList$rownamesA <- as.character(adjacencyList$rownamesA)
    
    fromC <- layoutCoordinates[layoutCoordinates$latlonalpha==adjacencyList[whichRow,1], 2:3 ][1,]  # Origin
    toC <- layoutCoordinates[layoutCoordinates$latlonalpha==adjacencyList[whichRow,2], 2:3 ][1,]  # Terminus
    
    # Add curve:
    graphCenter <- colMeans(layoutCoordinates[,2:3])  # Center of the overall graph
    bezierMid <- as.numeric(c(fromC[1], toC[2]))  # A midpoint, for bended edges
    bezierMid <- (fromC + toC + bezierMid) / 3  # Moderate the Bezier midpoint
    if(curved == FALSE){bezierMid <- (fromC + toC) / 2}  # Remove the curve
    edge <- data.frame(Hmisc::bezier(
      as.numeric(c(fromC[2], bezierMid[2], toC[2])),  # Generate
      as.numeric(c(fromC[1], bezierMid[1], toC[1])),  # X & y
      evaluation = len))  # Bezier path coordinates
    edge$Sequence <- 1:len  # For size and colour weighting in plot
    edge$Group <- paste(adjacencyList[whichRow, 1:2], collapse = ">")
    return(edge)
  }
  
  adjacencyMatrix <- as.matrix(linkages_points)
  
  layoutCoordinates <- test
  
  adjacencydf <- data.frame(adjacencyMatrix) %>%
    rownames_to_column() %>%
    gather(rownamesA, value, -rowname)   
  
  adjacencyList <- adjacencydf[adjacencydf$value > 0, ]
  
  
  # Generate a (curved) edge path for each pair of connected nodes
  allEdges <- lapply(1:nrow(adjacencyList), 
                     edgeMaker, 
                     len = line_resolution, 
                     curved = TRUE)
  
  
  allEdges <- do.call(rbind, allEdges)  # a fine-grained path ^, with bend ^
  
  
  empty_theme <- theme_bw()+
    theme(line = element_blank(),
          rect = element_blank(),
          axis.text = element_blank(),
          strip.text = element_blank(),
          plot.title = element_blank(),
          axis.title = element_blank(),
          plot.margin =  structure(c(0, 0, -1, -1), 
                                   unit = "lines", 
                                   valid.unit = 3L, 
                                   class = "unit"))
  
  
  if(mapRegion !="world"){
  world_map <- world_map[which(world_map$continent==mapRegion),]
  }
  
  
  
  ##	Create the world outlines:
  world_map@data$id = rownames(world_map@data)
  world_map.points <- fortify(world_map)
  world_map.df <- full_join(world_map.points, world_map@data, by="id")
  
  
products <- list()
  
  products[["plot"]] <- ggplot() + 
    geom_polygon(data=world_map.df, 
                 aes(long,lat,group=group), 
                 fill=gray(8/10)) +
    geom_path(data=world_map.df, 
              aes(long,lat,group=group), 
              color=gray(6/10)) +
    coord_equal() + 
    geom_path(data=allEdges,
              aes(x = x, y = y, 
                  group = Group,  # Edges with gradient
                  colour = Sequence, 
                  size = -Sequence), 
              alpha=0.1
    )   + 
    geom_point(data = data.frame(layoutCoordinates),
               aes(x = LON, y = LAT), 
               size = 3+100*sna::degree(linkages_points_net, 
                                   cmode="outdegree",rescale=T), 
               pch = 21,
               colour = rgb(8/10, 2/10, 2/10, alpha=5/10), 
               fill = rgb(9/10, 6/10, 6/10, alpha=5/10)
    )   + 
    scale_colour_gradient(low = rgb(8/10, 2/10, 2/10, alpha=5/10), 
                          high = rgb(8/10, 2/10, 2/10, alpha=5/10), 
                          guide = "none") + 
    scale_size(range = c(5/10, 5/10), guide = "none") + # Customize taper
    empty_theme
  
  
  products[["data_path"]] <- allEdges
  products[["data_polygon"]] <- world_map.df
  products[["data_points"]] <- data.frame(layoutCoordinates)
  
  return(products)
}

##	END: net_plot_coauthor_address():
########################################
########################################
