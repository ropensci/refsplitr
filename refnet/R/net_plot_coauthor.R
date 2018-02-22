########################################
########################################
##	BEGIN: net_plot_coauthor():

#' Creates a network diagram of coauthors' countries linked by reference
#' 
#' \code{net_plot_coauthor} This function takes an addresses data.frame, links it to an authors_references dataset and plots a network diagram generated for co-authorship.
#' 
#' @param addresses output from the read_addresses() function, containing geocoded address latitude and longitude locations.
#' @param authors_references output from the read_authors() function, which links author addresses together via AU_ID.

net_plot_coauthor <- function(addresses, 
                              authors_references) {
  
  references_addresses_linked <- merge(x=authors_references, 
                                       y=addresses, 
                                       by.x="AU_ID", 
                                       by.y="AU_ID", 
                                       all.x=FALSE, 
                                       all.y=FALSE)
  
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
  
  row.names(linkages) <- levels(factor(references_addresses_linked$country_name_code))
  
  colnames(linkages) <- levels(factor(references_addresses_linked$UT))
  
  ##	Convert to a one-mode representation of countries:
  linkages_countries <- linkages %*% t(linkages)
  
  ##	Convert to a one-mode representation of references:
  linkages_references <- t(linkages) %*% linkages
  
  
  ##	Create an igraph object from our countries linkages:
  require(igraph)
  
  
  #linkages_countries_net <- graph.adjacency(linkages_countries, mode="undirected")
  linkages_countries_net <- graph.adjacency(linkages_countries, 
                                            mode="undirected", 
                                            weighted=TRUE)
  
  #summary(linkages_countries_net)
  
  V(linkages_countries_net)$label <- V(linkages_countries_net)$name
  V(linkages_countries_net)$label.color <- rgb(0,0,.2,.5)
  V(linkages_countries_net)$label.cex <- 0.5
  V(linkages_countries_net)$size <- 12
  V(linkages_countries_net)$frame.color <- NA
  V(linkages_countries_net)$color <- rgb(0, 0.6, 0, 0.7)
  
  #plot(linkages_countries_net, layout=layout.fruchterman.reingold)
  
  
  ##	This would only be necessary if we hadn't already computed weights from
  ##		above:
  #E(linkages_countries_net)$weight <- count.multiple(linkages_countries_net)
  
  ##	Simplify the network edges by removing the diagonal and other half (assuming it's symmetric/undirected:
  linkages_countries_net <- simplify(linkages_countries_net)
  #plot(linkages_countries_net, layout=layout.fruchterman.reingold)
  
  #E(linkages_countries_net)$weight
  # egam <- (log(E(linkages_countries_net)$weight)+.3)/max(log(E(linkages_countries_net)$weight)+.3)
  # 11
  # E(linkages_countries_net)$color <- rgb(0,0,0,egam)

  #pdf("../output/merged_nodupe_first1000_linkages_countries_net.pdf")
  plot(linkages_countries_net, layout=layout.fruchterman.reingold)
  #plot(linkages_countries_net, layout=layout.kamada.kawai)
  #dev.off()
  
  
  ###	Or we might be interested in using the network package instead of igraph:
  #detach(package:igraph)
  #require(network)
  #require(sna)
  
  ###	This loads our adjacency matrix into a network object, and we 
  ###		specify directed as FALSE, and because we use the ignore.eval=FALSE
  ###		and names.eval="value" arguments it will load our edge counts in as
  ###		an edge attribute named "value" which we can then use as a weighting
  ###		or plotting attribute:
  #linkages_countries_net <- network(as.matrix(linkages_countries), directed=FALSE, loops=FALSE, ignore.eval=FALSE, names.eval="value")
  #summary(linkages_countries_net)
  #plot(linkages_countries_net)
}

##	END: net_plot_coauthor():
########################################
########################################
