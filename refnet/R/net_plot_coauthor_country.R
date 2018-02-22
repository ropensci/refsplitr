########################################
########################################
##	BEGIN: net_plot_coauthor_country():

#' Creates a network diagram of coauthors' countries linked by reference, and with nodes arranged geographically
#' 
#' \code{net_plot_coauthor_country} This function takes an addresses data.frame, links it to an authors_references dataset and plots a network diagram generated for countries of co-authorship.
#' 
#' @param addresses output from the read_addresses() function, containing geocoded address latitude and longitude locations.
#' @param authors_references output from the read_authors() function, which links author addresses together via AU_ID.


net_plot_coauthor_country <- function(
  addresses, 
  authors_references) {
  
  
  references_addresses_linked <- merge(x=authors_references, y=addresses, by.x="AU_ID", by.y="AU_ID", all.x=FALSE, all.y=FALSE)
  
  ##	For now, we'll just drop any that don't have a Country Name:
  references_addresses_linked <- references_addresses_linked[complete.cases(references_addresses_linked[c("UT", "country_name_code")]),]
  
  
  ###	Aggregate into a weighted two-mode list:
  #require(plyr)
  #linkages <- count(references_addresses_linked, vars=c("UT", "country_name_code"))
  
  
  ##	Or, we could use a sparse matrix representation:
  require(Matrix)
  
  linkages <- spMatrix(nrow=length(unique(references_addresses_linked$country_name_code)),
                       ncol=length(unique(references_addresses_linked$UT)),
                       i = as.numeric(factor(references_addresses_linked$country_name_code)),
                       j = as.numeric(factor(references_addresses_linked$UT)),
                       x = rep(1, length(references_addresses_linked$country_name_code)) 
  )
  
  rownames(linkages) <- levels(factor(references_addresses_linked$country_name_code))
  
  colnames(linkages) <- levels(factor(references_addresses_linked$UT))
  
  ##	Convert to a one-mode representation of countries:
  linkages_countries <- linkages %*% t(linkages)
  
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
  linkages_countries_net <- network(as.matrix(linkages_countries), directed=FALSE, loops=FALSE, ignore.eval=FALSE, names.eval="value")
  #summary(linkages_countries_net)
  #plot(linkages_countries_net)
  
  ##	Quick way:
  #gplot(as.matrix(linkages_countries))
  
  
  vertex_names <- (linkages_countries_net %v% "vertex.names")
  
  ##	Get the world map from rworldmap package:
  world_map <- rworldmap::getMap()

  vertexdf <- data.frame("ISO_A2"=vertex_names, stringsAsFactors=FALSE)
  
  coords_df <- merge(vertexdf, 
                     world_map[c("ISO_A2", "LON", "LAT")]@data, 
                     by="ISO_A2")
  
  ##	It seems there are two "AU" codes, so we'll aggregate and mean them:
  coords_df <- aggregate(coords_df[c("LON", "LAT")], 
                         by=list(factor(coords_df$ISO_A2)), 
                         FUN=mean)
  
  names(coords_df) <- c("ISO_A2", "LON", "LAT")
  
  
  
  ##	One could also use ggplot to plot out the network geographically:
  require(maptools)
  gpclibPermit()
  require(maps)
  
  ##	From my "Plotting Social Networks With ggplot.r" code:
  # doInstall <- FALSE  # Change to FALSE if you don't want packages installed.
  # toInstall <- c("sna", "ggplot2", "Hmisc", "reshape2")
  # if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
  # lapply(toInstall, library, character.only = TRUE)
  # 
  
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
    
    edge <- data.frame(Hmisc::bezier(c(fromC[1], bezierMid[1], toC[1]),  # Generate
                              c(fromC[2], bezierMid[2], toC[2]),  # X & y
                              evaluation = len)
    )  # Bezier path coordinates
    edge$Sequence <- 1:len  # For size and colour weighting in plot
    edge$Group <- paste(adjacencyList[whichRow, 1:2], collapse = ">")
    return(edge)
  }
  
  adjacencyMatrix <- as.matrix(linkages_countries)
  
  ##	Instead of getting layoutCoordinates here, we get them from LON/LAT:
  #layoutCoordinates <- gplot(adjacencyMatrix)  # Get graph layout coordinates
  layoutCoordinates <- as.matrix(coords_df[,2:3])
  
  ##	It seems that the melt function behaves badly when row and column
  ##		names are "NA" which happens to be a legitimate country code.  So
  ##		we'll first fix this and then convert back:
  rownames(adjacencyMatrix)[rownames(adjacencyMatrix) == "NA"] <- "NAstr"
  colnames(adjacencyMatrix)[colnames(adjacencyMatrix) == "NA"] <- "NAstr"
  
  adjacencyList <- melt(adjacencyMatrix)  # Convert to list of ties only
  adjacencyList <- adjacencyList[adjacencyList$value > 0, ]
  
  ##	Repair names:
  rownames(adjacencyMatrix)[rownames(adjacencyMatrix) == "NAstr"] <- "NA"
  colnames(adjacencyMatrix)[colnames(adjacencyMatrix) == "NAstr"] <- "NA"

  # Generate a (curved) edge path for each pair of connected nodes
  allEdges <- lapply(1:nrow(adjacencyList), 
                     edgeMaker, 
                     len = 500, 
                     curved = TRUE)
  
  allEdges <- do.call(rbind, allEdges)  # a fine-grained path ^, with bend ^
  
  
  # Empty ggplot2 theme
  new_theme_empty <- theme_bw()
  new_theme_empty$line <- element_blank()
  new_theme_empty$rect <- element_blank()
  new_theme_empty$strip.text <- element_blank()
  new_theme_empty$axis.text <- element_blank()
  new_theme_empty$plot.title <- element_blank()
  new_theme_empty$axis.title <- element_blank()
  new_theme_empty$plot.margin <- structure(c(0, 0, -1, -1), 
                                           unit = "lines", 
                                           valid.unit = 3L, 
                                           class = "unit")
  
  ##	Create the world outlines:
  world_map@data$id = rownames(world_map@data)
  
  world_map.points <- fortify(world_map)
  
  world_map.df <- full_join(world_map.points, world_map@data, by="id")
  
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
  
  zp1 <- zp1 + geom_point(
    data = data.frame(layoutCoordinates),  # Add nodes
    aes(x = LON, y = LAT), size = 5+100*sna::degree(linkages_countries_net, cmode="outdegree", rescale=TRUE), pch = 21,
    #colour = gray(4/10), fill = gray(6/10)
    colour = rgb(8/10, 2/10, 2/10, alpha=5/10), fill = rgb(9/10, 6/10, 6/10, alpha=5/10)
  )  # Customize gradient v
  
  #zp1 <- zp1 + scale_colour_gradient(low = gray(0), high = gray(9/10), guide = "none")
  #zp1 <- zp1 + scale_colour_gradient(low = gray(8/10), high = gray(8/10), guide = "none")
  #zp1 <- zp1 + scale_colour_gradient(low = gray(4/10), high = gray(4/10), guide = "none")
  zp1 <- zp1 + scale_colour_gradient(low = rgb(8/10, 2/10, 2/10, alpha=5/10), high = rgb(8/10, 2/10, 2/10, alpha=5/10), guide = "none")
  #zp1 <- zp1 + scale_size(range = c(1/10, 1), guide = "none")  # Customize taper
  zp1 <- zp1 + scale_size(range = c(1, 1), guide = "none")  # Customize taper
  
  zp1 <- zp1 + geom_text(
    data = coords_df,
    aes(x = LON, y = LAT, label=ISO_A2), size=2, color=gray(2/10)
  )
  
  zp1 <- zp1 + new_theme_empty  # Clean up plot
  
  # Looks better when saved as a PNG:
  #ggsave("../output/merged_nodupe_linkages_countries_world_ggplot.png", zp1, h = 9/2, w = 9, type = "cairo-png")
  #ggsave("../output/merged_nodupe_linkages_countries_world_ggplot.pdf", zp1, h = 9/2, w = 9)
  
  return(zp1)
  
  
  ##	Or, we can get the fanciest and plot great circle networks:
  ##		http://flowingdata.com/2011/05/11/how-to-map-connections-with-great-circles/
}

##	END: net_plot_coauthor_country():
########################################
########################################
