########################################
########################################
##	BEGIN: net_plot_coauthor_country():

#' Creates a network diagram of coauthors' countries linked by reference, and with nodes arranged geographically
#' 
#' \code{net_plot_coauthor_country} This function takes an addresses data.frame, links it to an authors_references dataset and plots a network diagram generated for countries of co-authorship.
#' 
#' @param addresses output from the read_addresses() function, containing geocoded address latitude and longitude locations.
#' @param authors_references output from the read_authors() function, which links author addresses together via AU_ID.


net_plot_coauthor_country <- function(data,line_resolution=10) {

  require(Matrix)
  require(network)
  require(sna)
  require(maps)
  require(maptools)
  
  data <- data[!is.na(data$country),]
  
  ##	Or, we could use a sparse matrix representation:
  
  linkages <- spMatrix(nrow=length(unique(data$country)),
  ncol=length(unique(data$UT)),
  i = as.numeric(factor(data$country)),
   j = as.numeric(factor(data$UT)),
   x = rep(1, length(data$country)) 
  )
  
  rownames(linkages) <- levels(factor(data$country))
  
  colnames(linkages) <- levels(factor(data$UT))
  
  ##	Convert to a one-mode representation of countries:
  linkages_countries <- linkages %*% t(linkages)
  
  ##	Convert to a one-mode representation of references:
  linkages_references <- t(linkages) %*% linkages
  

  
  ##	This loads our adjacency matrix into a network object, and we 
  ##		specify directed as FALSE, and because we use the ignore.eval=FALSE
  ##		and names.eval="value" arguments it will load our edge counts in as
  ##		an edge attribute named "value" which we can then use as a weighting
  ##		or plotting attribute:
  linkages_countries_net <- network(as.matrix(linkages_countries), 
                                    directed=FALSE, 
                                    loops=FALSE, 
                                    ignore.eval=FALSE, 
                                    names.eval="value")

  
  
  vertex_names <- (linkages_countries_net %v% "vertex.names")
  
  vertex_names <- ifelse(vertex_names=="USA","United States of America",vertex_names)
  
  ##	Get the world map from rworldmap package:
  world_map <- rworldmap::getMap()

  vertexdf <- data.frame("ISO_A2"=vertex_names, stringsAsFactors=FALSE)
  
  coords_df <- left_join(vertexdf, 
                     world_map[c("ADMIN.1", "LON", "LAT")]@data, 
                     by=c("ISO_A2"="ADMIN.1"))
  
  ##	It seems there are two "AU" codes, so we'll aggregate and mean them:
  coords_df <- aggregate(coords_df[c("LON", "LAT")], 
                         by=list(factor(coords_df$ISO_A2)), 
                         FUN=mean)
  
  names(coords_df) <- c("ISO_A2", "LON", "LAT")
  
  
  
  ##	One could also use ggplot to plot out the network geographically:

  gpclibPermit()
 
  
  # Function to generate paths between each connected node
  edgeMaker <- function(whichRow, len = 100, curved = TRUE){
    
    adjacencyList$country <- as.character(adjacencyList$country)
    adjacencyList$countryA <- as.character(adjacencyList$countryA)
    layoutCoordinates$ISO_A2 <- as.character(layoutCoordinates$ISO_A2)
    
    adjacencyList$country<- ifelse(adjacencyList$country=="USA","United States of America", adjacencyList$country)
    
    adjacencyList$countryA <- ifelse(adjacencyList$countryA=="USA","United States of America", adjacencyList$countryA)
    
    adjacencyList$country <- gsub(pattern="\\.",replacement = " ",
                                  x=adjacencyList$country)

    adjacencyList$countryA <- gsub(pattern="\\.",replacement = " ",
                                  x=adjacencyList$countryA)
        
    fromC <- layoutCoordinates[layoutCoordinates$ISO_A2==adjacencyList[whichRow,1], 2:3 ]  # Origin
    toC <- layoutCoordinates[layoutCoordinates$ISO_A2==adjacencyList[whichRow,2], 2:3 ]  # Terminus
    
    # Add curve:
    graphCenter <- colMeans(layoutCoordinates[,2:3])  # Center of the overall graph
    bezierMid <- as.numeric(c(fromC[1], toC[2]))  # A midpoint, for bended edges
    bezierMid <- (fromC + toC + bezierMid) / 3  # Moderate the Bezier midpoint
    if(curved == FALSE){bezierMid <- (fromC + toC) / 2}  # Remove the curve
    edge <- data.frame(Hmisc::bezier(
      as.numeric(c(fromC[1], bezierMid[1], toC[1])),  # Generate
      as.numeric(c(fromC[2], bezierMid[2], toC[2])),  # X & y
      evaluation = len))  # Bezier path coordinates
    edge$Sequence <- 1:len  # For size and colour weighting in plot
    edge$Group <- paste(adjacencyList[whichRow, 1:2], collapse = ">")
    return(edge)
  }
  
  
  adjacencyMatrix <- as.matrix(linkages_countries)
  
  ##	Instead of getting layoutCoordinates here, we get them from LON/LAT:
  #layoutCoordinates <- gplot(adjacencyMatrix)  # Get graph layout coordinates
  layoutCoordinates <- coords_df
  
  ##	It seems that the melt function behaves badly when row and column
  ##		names are "NA" which happens to be a legitimate country code.  So
  ##		we'll first fix this and then convert back:
  rownames(adjacencyMatrix)[rownames(adjacencyMatrix) == "NA"] <- "NAstr"
  colnames(adjacencyMatrix)[colnames(adjacencyMatrix) == "NA"] <- "NAstr"
  
  adjacencydf <- data.frame(adjacencyMatrix)
  
  adjacencydf$country <- row.names(adjacencydf)
  
  adjacencyList <- gather(data=adjacencydf, countryA, value, -country)  # Convert to list of ties only
  
  adjacencyList <- adjacencyList[adjacencyList$value > 0, ]
  
  ##	Repair names:
  rownames(adjacencydf)[rownames(adjacencydf) == "NAstr"] <- "NA"
  colnames(adjacencydf)[colnames(adjacencydf) == "NAstr"] <- "NA"

  # Generate a (curved) edge path for each pair of connected nodes
  allEdges <- lapply(1:nrow(adjacencyList), 
                     edgeMaker, len=line_resolution, 
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
  
  ##	Create the world outlines:
  world_map@data$id = rownames(world_map@data)
  
  world_map.points <- fortify(world_map)
  
  world_map.df <- full_join(world_map.points, world_map@data, by="id")
  
  products <- list()
  
  products[["plot"]] <- ggplot() + 
    geom_polygon(data=world_map.df, aes(long,lat,group=group), fill=gray(8/10)) +
    geom_path(data=world_map.df, aes(long,lat,group=group), color=gray(6/10)) +
    coord_equal() + 
    geom_path(data=allEdges,
                aes(x = x, y = y, group = Group,  # Edges with gradient
                colour = Sequence, size = -Sequence), alpha=0.1
  )  + 
    geom_point(data = data.frame(layoutCoordinates),  # Add nodes
              aes(x = LON, y = LAT), 
              size = 5+100*sna::degree(linkages_countries_net, cmode="outdegree", rescale=TRUE), pch = 21, 
              colour = rgb(8/10, 2/10, 2/10, alpha=5/10), 
              fill = rgb(9/10, 6/10, 6/10, alpha=5/10)
  )  + 
    scale_colour_gradient(low = rgb(8/10, 2/10, 2/10, alpha=5/10), high = rgb(8/10, 2/10, 2/10, alpha=5/10), guide = "none") + 
    scale_size(range = c(1, 1), guide = "none")  + 
    geom_text(
    data = coords_df,
    aes(x = LON, y = LAT, label=ISO_A2), size=2, color=gray(2/10)
  ) + empty_theme  # Clean up plot
  
  
  products[["data_path"]] <- allEdges
  products[["data_polygon"]] <- world_map.df
  products[["data_points"]] <- data.frame(layoutCoordinates)
  
  return(products)
  
  
}

##	END: net_plot_coauthor_country():
########################################
########################################
