#' Creates a network diagram of coauthors' countries linked by reference, #and
#' with nodes arranged geographically
#'
#' This function takes an addresses data.frame, links it to an
#' authors_references dataset and plots a network diagram generated for
#' countries of co-authorship.
#'
#' @param data the `address` element from the list outputted from the
#'   `authors_georef()` function, containing geocoded address latitude and
#'   longitude locations.
#' @param mapRegion what portion of the world map to show. possible values
#'   include `"world"`, `"North America"`, `"South America"`, `"Australia"`,
#'   `"Africa"`, `"Antarctica"`, and `"Eurasia"`
#' @param lineResolution the resolution of the lines drawn, higher numbers will
#'   make smoother curves default is 10.
#' @param lineAlpha transparency of the lines, fed into ggplots alpha value.
#'   Number between 0 - 1.
#' @examples 
#' ## Using the output of authors_georef (e.g., BITR_geocode)
#' data(BITR_geocode)
#' ## Plots the whole world
#' output <- plot_net_country(BITR_geocode)
#' 
#' ## Mapping only North America
#' output <- plot_net_country(BITR_geocode, mapRegion = 'North America')
#'
#' ## Change the transparency of lines by modifying the lineAlpha parameter
#' output <- plot_net_country(BITR_geocode, lineAlpha = 0.2)
#'                  
#' ## Change the curvature of lines by modifying the lineResolution paramater
#' output <- plot_net_country(BITR_geocode, lineResolution = 30 )
#'                  
#' ## With all arguments: 
#' output <- plot_net_country(BITR_geocode, mapRegion = 'North America', lineAlpha = 0.2,
#'                  lineResolution = 30)
#' 
#' 
#' 
#' @export plot_net_country
#' @importFrom network %v%

# data<-BITR_geocode
plot_net_country <- function(data,
  lineResolution = 10,
  mapRegion = "world",
  lineAlpha = 0.5) {
  

  
  fixable_countries<-data |> 
    dplyr::filter(is.na(country)==FALSE & is.na(lat)==TRUE) |> 
    dplyr::select(refID,country) |> 
    dplyr::group_by(refID,country) |> 
    dplyr::tally() |> 
    dplyr::arrange(n)
  
  
  data <- data[!is.na(data$country), ]
  
  data$country[data$country=="usa"] <- "united states of america" 
  data$country[data$country=="united states"] <- "united states of america"
  data$country[data$country=="serbia"] <- "republic of serbia"
  data$country[data$country=="peoples r china"] <- "china"
  data$country[data$country=="uk"] <- "united kingdom"
  data$country[data$country=="england"] <- "united kingdom"
  data$country[data$country=="scotland"] <- "united kingdom"
  data$country[data$country=="wales"] <- "united kingdom"
  data$country[data$country=="north ireland"] <- "united kingdom"
  data$country[data$country=="cent afr republ"] <- "central african republic"
  data$country[data$country=="cote ivoire"] <- "ivory coast"
  data$country[data$country=="papua n guinea"] <- "papua new guinea"
  data$country[data$country=="sao tome & prin"] <- "sao tome and principe"
  data$country[data$country=="tanzania"] <- "united republic of tanzania"
  data$country[data$country=="rep congo"] <- "republic of the congo"
  data$country[data$country=="bahamas"] <- "the bahamas"
  data$country[data$country=="dem rep congo"] <- "republic of the congo"
  data$country[data$country=="rep congo"] <- "democratic republic of the congo"
  data$country[data$country=="democratic republic of congo"] <- "democratic republic of the congo"
  data$country[data$country=="fr polynesia"] <- "french polynesia"
  data$country[data$country=="surinam"] <- "suriname"
  data$country[data$country=="turks & caicos"] <- "turks and caicos islands"
  data$country[data$country=="u arab emirates"] <- "united arab emirates"
  data$country[data$country=="libyan arab jamahiriya"] <- "libya"
  data$country[data$country=="rhodesia"] <- "zimbabwe"
  data$country[data$country=="russian federation"] <- "russia"
  data$country[data$country=="hong kong"] <- "hong kong sar"
  data$country[data$country=="hong kong s.a.r."] <- "hong kong sar"
  data$country[data$country=="brunei darussalam"] <- "brunei"
  data$country[data$country=="trinidade and tobago"] <- "trinidad and tobago"
  
  # to avoid fail for non-ascii characters
  data$country[data$country=="cura\u00e7ao"] <- "curacao"  
  
    ## 	we could use a sparse matrix representation:
  linkages <- Matrix::spMatrix(
    nrow = length(unique(data$country)),
    ncol = length(unique(data$UT)),
    i = as.numeric(factor(data$country)),
    j = as.numeric(factor(data$UT)),
    
    
    x = rep(1, length(data$country))
  )

  links <- matrix(data = linkages,
    nrow = length(unique(data$country)),
    ncol = length(unique(data$UT)))

  rownames(links) <- levels(factor(data$country))

  colnames(links) <- levels(factor(data$UT))

  ## 	Convert to a one-mode representation of countries:
  linkages_countries <- links %*% t(links)

  ## 	Convert to a one-mode representation of references:
  ## linkages_references <- t(links) %*% links

  ## 	This loads our adjacency matrix into a network object, and we
  ## 	specify directed as FALSE, and because we use the ignore.eval=FALSE
  ## 	and names.eval="value" arguments it will load our edge counts in as
  ##  an edge attribute named "value" which we can then use as a weighting
  ## 	or plotting attribute:
  linkages_countries_net <- network::network(as.matrix(linkages_countries),
    directed = FALSE,
    loops = FALSE,
    ignore.eval = FALSE,
    names.eval = "value"
  )

  requireNamespace(package = "network", quietly = TRUE)

  vertex_names <- (linkages_countries_net %v% "vertex.names")
  # convert to tibble to use case_when
  # vertex_names<-as_tibble(vertex_names)
  
  
  ## 	Get the world map from rworldmap package:
  world_map <- rworldmap::getMap()
  world_map$ADMIN.1 <- tolower(world_map$ADMIN.1)
  
  # rworldmap includes periods in a few country names (eg, hong king s.a.r.)
  # this was causing an error later on in the rare cases where one of those 
  # countries was in the dataset. This will remove the periods from ADMIN.1
  
  world_map$ADMIN.1 <- gsub(
    pattern = "\\.", replacement = "",
    world_map$ADMIN.1
  )
  
  vertexdf <- data.frame("ISO_A2" = vertex_names, stringsAsFactors = FALSE)
  # coords_df <- suppressWarnings(dplyr::left_join(vertexdf,
  #   world_map[c("ADMIN.1", "LON", "LAT")]@data,
  #   by = c("ISO_A2" = "ADMIN.1")
  # ))
  coords_df <- suppressWarnings(merge(vertexdf,
    world_map[c("ADMIN.1", "LON", "LAT")]@data,
    by.x = "ISO_A2", by.y = "ADMIN.1", all.x = TRUE)
  )
  
  ## 	It seems there are two "AU" codes, so we'll aggregate and mean them:
  coords_df <- stats::aggregate(coords_df[c("LON", "LAT")],
    by = list(factor(coords_df$ISO_A2)),
    FUN = mean
  )

  names(coords_df) <- c("ISO_A2", "LON", "LAT")

  # Some locations wont have coordinates in rworldmap 
  # coords_df %>% filter(is.na(LAT)==TRUE) %>% distinct(ISO_A2)
  # need to add them manually 
  
    # LAT
  
            
      coords_df$LAT <- ifelse(coords_df$ISO_A2 == "french guiana",
                              3.9339,
                              coords_df$LAT)
      
      coords_df$LAT <- ifelse(coords_df$ISO_A2 == "bonaire",
                              12.2019,
                              coords_df$LAT)
      
      coords_df$LAT <- ifelse(coords_df$ISO_A2 == "reunion",
                              -68.2624,
                              coords_df$LAT)
      
      coords_df$LAT <- ifelse(coords_df$ISO_A2 == "palestine",
                              31.9522,
                              coords_df$LAT)
      
                              
     # LON
    
      coords_df$LON <- ifelse(coords_df$ISO_A2 == "french guiana",
                              -53.1258,
                              coords_df$LON)
      
      coords_df$LON <- ifelse(coords_df$ISO_A2 == "bonaire",
                              -68.2624,
                              coords_df$LON)
      
      coords_df$LON <- ifelse(coords_df$ISO_A2 == "reunion",
                              55.5364,
                              coords_df$LON)
      
      coords_df$LON <- ifelse(coords_df$ISO_A2 == "palestine",
                              35.2332,
                              coords_df$LON)
      
  
  ## 	One could also use ggplot to plot out the network geographically:

  layoutCoordinates <- coords_df
  # layoutCoordinates$ISO_A2
  # to test for any missing latlon
  #  coords_df %>% filter(is.na(LAT)==TRUE)
  
  # Function to generate paths between each connected node
  edgeMaker <- function(whichRow, len = 100, curved = TRUE) {
    adjacencyList$country <- as.character(adjacencyList$country)
    adjacencyList$countryA <- as.character(adjacencyList$countryA)

    layoutCoordinates$ISO_A2 <- as.character(layoutCoordinates$ISO_A2)

    layoutCoordinates <- stats::na.omit(layoutCoordinates)
   
    adjacencyList<- adjacencyList |> 
      dplyr::mutate(country=dplyr::case_when(
      country == "V1" ~ NA,
      .default = as.character(country)
    ))
    
    
    
    adjacencyList<- adjacencyList |> 
      dplyr::mutate(countryA=dplyr::case_when(
        countryA == "V1" ~ NA,
        .default = as.character(countryA)
      ))
    
    
    
    # 
    # adjacencyList$country <- ifelse(adjacencyList$country == "usa",
    #   "united states of america", adjacencyList$country)
    # 
    # adjacencyList$countryA <- ifelse(adjacencyList$countryA == "usa",
    #   "united states of america", adjacencyList$countryA)

    # 
    # 
    # adjacencyList$country <- ifelse(adjacencyList$country == "rep congo",
    #                                 "republic of the congo", adjacencyList$country)
    # 
    # adjacencyList$countryA <- ifelse(adjacencyList$countryA == "rep congo",
    #                                  "republic of the congo", adjacencyList$countryA)
    # 
    # adjacencyList$country <- ifelse(adjacencyList$country == "V1",
    #   NA, adjacencyList$country)
    # 
    # adjacencyList$countryA <- ifelse(adjacencyList$countryA == "V1",
    #   NA, adjacencyList$countryA)

    adjacencyList <- stats::na.omit(adjacencyList)


    adjacencyList$country <- gsub(
      pattern = "\\.", replacement = " ",
      x = adjacencyList$country
    )

    adjacencyList$countryA <- gsub(
      pattern = "\\.", replacement = " ",
      x = adjacencyList$countryA
    )

    fromC <- layoutCoordinates[layoutCoordinates$ISO_A2 ==
        adjacencyList[whichRow, 1], 2:3 ] # Origin
    toC <- layoutCoordinates[layoutCoordinates$ISO_A2 ==
        adjacencyList[whichRow, 2], 2:3 ] # Terminus

    ## Add curve:
    # graphCenter <- colMeans(layoutCoordinates[, 2:3]) # Center of graph
    bezierMid <- as.numeric(c(fromC[1], toC[2])) # A midpoint, for bended edges
    bezierMid <- (fromC + toC + bezierMid) / 3 # Moderate the Bezier midpoint
    if (curved == FALSE) {
      bezierMid <- (fromC + toC) / 2
    } # Remove the curve
    edge <- data.frame(Hmisc::bezier(
      as.numeric(c(fromC[1], bezierMid[1], toC[1])), # Generate
      as.numeric(c(fromC[2], bezierMid[2], toC[2])), # X & y
      evaluation = len
    )) # Bezier path coordinates
    edge$Sequence <- 1:len # For size and colour weighting in plot
    edge$Group <- paste(adjacencyList[whichRow, 1:2], collapse = ">")
    return(edge)
  }

  adjacencyMatrix <- as.matrix(linkages_countries)

  rownames(adjacencyMatrix)[rownames(adjacencyMatrix) == "NA"] <- "NAstr"
  colnames(adjacencyMatrix)[colnames(adjacencyMatrix) == "NA"] <- "NAstr"

  adjacencyList <- data.frame(
    country = rep(row.names(adjacencyMatrix), each = nrow(adjacencyMatrix)),
    countryA = rep(colnames(adjacencyMatrix), times = nrow(adjacencyMatrix)),
    value = c(adjacencyMatrix)
  )
  adjacencydf <- data.frame(adjacencyMatrix)

  adjacencydf$country <- row.names(adjacencydf)

  # adjacencyList <- tidyr::gather(data = adjacencydf, key = "countryA",
  #   value = "value", -"country")

  adjacencyList <- adjacencyList[adjacencyList$value > 0, ]

  rownames(adjacencydf)[rownames(adjacencydf) == "NAstr"] <- "NA"
  colnames(adjacencydf)[colnames(adjacencydf) == "NAstr"] <- "NA"

  allEdges <- lapply(seq_len(nrow(adjacencyList)),
    edgeMaker,
    len = lineResolution,
    curved = TRUE
  )

  allEdges <- do.call(rbind, allEdges)

  requireNamespace(package = "ggplot2", quietly = TRUE)

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
  
  world_map_sub <- world_map
  if (mapRegion != "world") {
    world_map_sub <- world_map[which(world_map$continent == mapRegion &
        world_map$TYPE != "Dependency"), ]
  }
  ## 	Create the world outlines:
  world_map@data$id <- rownames(world_map@data)
  # world_map.points <- ggplot2::fortify(world_map) # fortify() was deprecated 
  # world_map.points <- sf::st_as_sf(world_map) st_as_sf() is an alternative,
  # but the output includes all the polygons needed to map in a list-column.
  # This function converts the map `st` object to a data frame like fortify(). 
  
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
  
  world_map.points <- sf_convert(world_map) # and now this instead of fortify()

  
  world_map.df <- merge(world_map.points,
                        world_map@data, by = "id", all = TRUE)
  world_map.df <- world_map.df[!is.na(world_map.df$lat), ]
  # world_map.df <- dplyr::full_join(world_map.points,
  #                                  world_map@data, by = "id")

  ## calculate min and max for plot
  latmin <- world_map_sub@bbox["y", "min"]
  latmax <- world_map_sub@bbox["y", "max"]
  longmin <- world_map_sub@bbox["x", "min"]
  longmax <- world_map_sub@bbox["x", "max"]

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
  ISO_A2 <- ggplot2::quo(ISO_A2)
  lineAlpha <- ggplot2::enexpr(lineAlpha)
  products[["plot"]] <- ggplot2::ggplot() +
    ggplot2::geom_polygon(data = world_map.df, ggplot2::aes(!!long,
      !!lat, group =!!group), fill = grDevices::gray(8 / 10)) +
    ggplot2::geom_path(data = world_map.df, ggplot2::aes(!!long,
      !!lat, group = !!group), color = grDevices::gray(6 / 10)) +
    ggplot2::coord_equal(ylim = c(latmin, latmax),
      xlim = c(longmin, longmax)) +
    ggplot2::geom_path(
      data = allEdges,
      ggplot2::aes(x = !!x, y = !!y, group = !!Group,
        colour = !!Sequence, size = !!Sequence), alpha = lineAlpha
    ) +
    ggplot2::geom_point(
      data = data.frame(layoutCoordinates), # Add nodes
      ggplot2::aes(x = !!LON, y = !!LAT),
      size = 5 + 100 * sna::degree(linkages_countries_net,
        cmode = "outdegree", rescale = TRUE), pch = 21,
      colour = grDevices::rgb(8 / 10, 2 / 10, 2 / 10, alpha = 5 / 10),
      fill = grDevices::rgb(9 / 10, 6 / 10, 6 / 10, alpha = 5 / 10)
    ) +
    ggplot2::scale_colour_gradient(low = grDevices::rgb(8 / 10, 2 / 10, 2 / 10,
      alpha = 5 / 10), high = grDevices::rgb(8 / 10, 2 / 10, 2 / 10,
        alpha = 5 / 10), guide = "none") +
    ggplot2::scale_size(range = c(1, 1), guide = "none") +
    ggplot2::geom_text(
      data = coords_df,
      ggplot2::aes(x = !!LON, y = !!LAT, label = !!ISO_A2), size = 2,
      color = grDevices::gray(2 / 10)
    ) + empty_theme # Clean up plot

  products[["data_path"]] <- allEdges
  products[["data_polygon"]] <- world_map.df
  products[["data_points"]] <- data.frame(layoutCoordinates)
  products[["fixable_countries"]] <- data.frame(fixable_countries)
  
  fix_text1<-"The dataset has entries with a 'country' for which" 
  fix_text2<-" there is not lat/lon. If you want to include them in"
  fix_text3<-" in the analysis, replace the missing lat/lon in the "
  fix_text4<-" output of `authors_georef()` that you used here."
  fix_text5<-" A list of the references is in `products$fixable_countries`"
  
  
  
  fix_test<-paste(fix_text1,fix_text2,fix_text3,fix_text4,fix_text5)
  
  if (nrow(fixable_countries) > 1){
    message (fix_test)
  } else {
    message("whew! finally done...")
    }
  

  return(products)
}

