########################################
########################################
##	BEGIN: net_plot_coauthor_address():

#' Creates a network diagram of coauthors' addresses linked by reference, and with nodes arranged geographically
#' 
#' \code{net_plot_coauthor_address} This function takes an addresses data.frame, links it to an authors__references dataset and plots a network diagram generated for individual points of co-authorship.
#' 
#' @param addresses output from the read_addresses() function, containing geocoded address latitude and longitude locations.
#' @param authors__references output from the read_authors() function, which links author addresses together via AU_ID.

net_plot_coauthor_address <- function(addresses, authors__references) {
  references_addresses_linked <- merge(x=authors__references, y=addresses, by.x="AU_ID", by.y="AU_ID", all.x=FALSE, all.y=FALSE)
  
  ##	For now, we'll just drop any that don't have lat/long information:
  references_addresses_linked <- references_addresses_linked[complete.cases(references_addresses_linked[c("UT", "latitude")]),]
  
  ##	Link all authors at a particular point location:
  references_addresses_linked$latlon <- paste(references_addresses_linked$latitude, ",", references_addresses_linked$longitude, sep="") 
  
  
  ##	Or, we could use a sparse matrix representation:
  require(Matrix)
  
  linkages <- spMatrix(nrow=length(unique(references_addresses_linked$latlon)),
                       ncol=length(unique(references_addresses_linked$UT)),
                       i = as.numeric(factor(references_addresses_linked$latlon)),
                       j = as.numeric(factor(references_addresses_linked$UT)),
                       x = rep(1, length(references_addresses_linked$latlon)) 
  )
  rownames(linkages) <- levels(factor(references_addresses_linked$latlon))
  colnames(linkages) <- levels(factor(references_addresses_linked$UT))
  
  ##	Convert to a one-mode representation of countries:
  linkages_points <- linkages %*% t(linkages)
  
  ##	Convert to a one-mode representation of references:
  linkages_references <- t(linkages) %*% linkages
  
  
  ##	Or we might be interested in using the network package instead of igraph:
  require(network)
  require(sna)
  
  ##	This loads our adjacency matrix into a network object, and we 
  ##		specify directed as FALSE, and because we use the ignore.eval=FALSE
  ##		and names.eval="value" arguments it will load our edge counts in as
  ##		an edge attribute named "value" which we can then use as a weighting
  ##		or plotting attribute:
  linkages_points_net <- network(as.matrix(linkages_points), directed=FALSE, loops=FALSE, ignore.eval=FALSE, names.eval="value")
  #summary(linkages_points_net)
  #plot(linkages_points_net)
  
  ##	Quick way:
  #gplot(as.matrix(linkages_points))
  
  
  vertex_names <- (linkages_points_net %v% "vertex.names")
  
  
  ##	Get the world map from rworldmap package:
  world_map <- getMap()
  
  coords_df <- data.frame("latlon"=vertex_names, "LON"=as.numeric(gsub("^.*,", "", vertex_names)), "LAT"=as.numeric(gsub(",.*$", "", vertex_names)), stringsAsFactors=FALSE)
  
  
  #pdf("../output/merged_nodupe_linkages_addresses_world.pdf")
  #plot(world_map, xlim=c(-180, 180), ylim=c(-90, 90))
  #plot(linkages_countries_net, coord=coords_df[c("LON", "LAT")], displaylabels = FALSE, boxed.labels = FALSE, suppress.axes = FALSE, label.col = rgb(0., 0.5, 0.0, 0.7), edge.col=rgb(0.0,0.0,0.0,0.3), vertex.col=rgb(0.,0.7,0.,0.5), label.cex = 0.5, xlab = "Longitude", ylab = "Latitude", main="Co-Author Locations", vertex.cex=1+10*degree(linkages_countries_net, cmode="outdegree", rescale=TRUE), xlim=c(-180, 180), ylim=c(-90, 90), usecurve=TRUE, edge.curve=.75, new=FALSE)
  #dev.off()
  
  
  ##	One could also use ggplot to plot out the network geographically:
  require(maptools)
  gpclibPermit()
  require(maps)
  
  ##	From my "Plotting Social Networks With ggplot.r" code:
  doInstall <- FALSE  # Change to FALSE if you don't want packages installed.
  toInstall <- c("sna", "ggplot2", "Hmisc", "reshape2")
  if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
  lapply(toInstall, library, character.only = TRUE)
  
  
  # Function to generate paths between each connected node
  edgeMaker <- function(whichRow, len = 100, curved = TRUE){
    fromC <- layoutCoordinates[adjacencyList[whichRow, 1], ]  # Origin
    toC <- layoutCoordinates[adjacencyList[whichRow, 2], ]  # Terminus
    
    # Add curve:
    graphCenter <- colMeans(layoutCoordinates)  # Center of the overall graph
    bezierMid <- c(fromC[1], toC[2])  # A midpoint, for bended edges
    distance1 <- sum((graphCenter - bezierMid)^2)
    if(distance1 < sum((graphCenter - c(toC[1], fromC[2]))^2)){
      bezierMid <- c(toC[1], fromC[2])
    }  # To select the best Bezier midpoint
    bezierMid <- (fromC + toC + bezierMid) / 3  # Moderate the Bezier midpoint
    if(curved == FALSE){bezierMid <- (fromC + toC) / 2}  # Remove the curve
    
    edge <- data.frame(bezier(c(fromC[1], bezierMid[1], toC[1]),  # Generate
                              c(fromC[2], bezierMid[2], toC[2]),  # X & y
                              evaluation = len)
    )  # Bezier path coordinates
    edge$Sequence <- 1:len  # For size and colour weighting in plot
    edge$Group <- paste(adjacencyList[whichRow, 1:2], collapse = ">")
    return(edge)
  }
  
  
  adjacencyMatrix <- as.matrix(linkages_points)
  
  ##	Instead of getting layoutCoordinates here, we get them from LON/LAT:
  #layoutCoordinates <- gplot(adjacencyMatrix)  # Get graph layout coordinates
  layoutCoordinates <- as.matrix(coords_df[,2:3])
  
  adjacencyList <- melt(adjacencyMatrix)  # Convert to list of ties only
  adjacencyList <- adjacencyList[adjacencyList$value > 0, ]
  
  
  # Generate a (curved) edge path for each pair of connected nodes
  allEdges <- lapply(1:nrow(adjacencyList), edgeMaker, len = 500, curved = TRUE)
  allEdges <- do.call(rbind, allEdges)  # a fine-grained path ^, with bend ^
  
  
  # Empty ggplot2 theme
  new_theme_empty <- theme_bw()
  new_theme_empty$line <- element_blank()
  new_theme_empty$rect <- element_blank()
  new_theme_empty$strip.text <- element_blank()
  new_theme_empty$axis.text <- element_blank()
  new_theme_empty$plot.title <- element_blank()
  new_theme_empty$axis.title <- element_blank()
  new_theme_empty$plot.margin <- structure(c(0, 0, -1, -1), unit = "lines", valid.unit = 3L, class = "unit")
  
  
  ##	Create the world outlines:
  world_map@data$id = rownames(world_map@data)
  world_map.points <- fortify(world_map, region="id")
  world_map.df <- join(world_map.points, world_map@data, by="id")
  
  zp1 <- ggplot() + 
    geom_polygon(data=world_map.df, aes(long,lat,group=group), fill=gray(8/10)) +
    geom_path(data=world_map.df, aes(long,lat,group=group), color=gray(6/10)) +
    coord_equal()
  
  #zp1 <- ggplot()  # Pretty simple plot code
  zp1 <- zp1 + geom_path(
    data=allEdges,
    aes(x = x, y = y, group = Group,  # Edges with gradient
        colour = Sequence, size = -Sequence), alpha=0.1
  )  # and taper
  
  ##	TODO: We can change the scale from being for the paths to the points
  ##		which will allow us to provide a legend on centrality, whereas we
  ##		don't need a scale for the edges as we are not scaling by direction
  ##		as this is an undirected network...
  zp1 <- zp1 + geom_point(
    data = data.frame(layoutCoordinates),  # Add nodes
    aes(x = LON, y = LAT), size = 3+100*degree(linkages_points_net, cmode="outdegree", rescale=TRUE), pch = 21,
    #colour = gray(4/10, alpha=5/10), fill = gray(6/10, alpha=5/10)
    colour = rgb(8/10, 2/10, 2/10, alpha=5/10), fill = rgb(9/10, 6/10, 6/10, alpha=5/10)
  )  # Customize gradient v
  
  #zp1 <- zp1 + scale_shape_discrete(name  ="Prop. Centrality", breaks=c(3+100*0, 3+100*0.05, 3+100*0.1, 3+100*0.2), labels=c("0%", "5%", "10%", "20%"))
  #zp1 <- zp1 + theme(legend.justification=c(1,0), legend.position=c(1,0))
  
  
  #zp1 <- zp1 + scale_colour_gradient(low = gray(0), high = gray(9/10), guide = "none")
  #zp1 <- zp1 + scale_colour_gradient(low = gray(8/10), high = gray(8/10), guide = "none")
  #zp1 <- zp1 + scale_colour_gradient(low = gray(4/10), high = gray(4/10), guide = "none")
  zp1 <- zp1 + scale_colour_gradient(low = rgb(8/10, 2/10, 2/10, alpha=5/10), high = rgb(8/10, 2/10, 2/10, alpha=5/10), guide = "none")
  #zp1 <- zp1 + scale_size(range = c(1/10, 1), guide = "none")  # Customize taper
  zp1 <- zp1 + scale_size(range = c(5/10, 5/10), guide = "none")  # Customize taper
  
  #zp1 <- zp1 + geom_text(
  #	data = coords_df,
  #	aes(x = LON, y = LAT, label=""), size=2, color=gray(2/10)
  #)
  
  
  zp1 <- zp1 + new_theme_empty  # Clean up plot
  
  # Looks better when saved as a PNG:
  #ggsave("../output/merged_nodupe_linkages_addresses_world_ggplot.png", zp1, h = 9/2, w = 9, type = "cairo-png")
  #ggsave("../output/merged_nodupe_linkages_addresses_world_ggplot.pdf", zp1, h = 9/2, w = 9)
  
  return(zp1)
  
  
  ##	Or, we can get the fanciest and plot great circle networks:
  ##		http://flowingdata.com/2011/05/11/how-to-map-connections-with-great-circles/
}

##	END: net_plot_coauthor_address():
########################################
########################################
