filter_tweets_retweets <- function(x, twitters) {
  dplyr::filter(x, is_retweet == TRUE) -> x
  dplyr::select(x, screen_name, text, created_at, retweet_screen_name) -> x
  dplyr::filter(x, retweet_screen_name %in% twitters) -> x
  dplyr::filter(x, screen_name %in% twitters) -> x
  dplyr::select(x, screen_name, retweet_screen_name) -> x
  return(x)
}
filter_tweets_replies <- function(x, twitters) {
  dplyr::filter(x, !is.na(reply_to_screen_name)) -> x
  dplyr::filter(x, screen_name %in% twitters) -> x
  dplyr::filter(x, reply_to_screen_name %in% twitters) -> x
  dplyr::filter(x, reply_to_screen_name != screen_name) -> x
  dplyr::select(x, screen_name, reply_to_screen_name) -> x
  return(x)
}
filter_tweets_quotes <- function(x, twitters) {
  dplyr::filter(x, is_quote == TRUE) -> x
  dplyr::filter(x, screen_name %in% twitters) -> x
  dplyr::filter(x, quoted_screen_name %in% twitters) -> x
  dplyr::select(x, screen_name, quoted_screen_name) -> x
  return(x)
}
#' Create PM's Network
#'
#' This function creates ipgraph object based on interactions (retweets, replies, quotes)
#' in tweets file.
#'
#' @param partia Name of party (parliament club) which network will be made
#' @param poslowie Dataframe with all PM's twitters and party/subparty affiliations
#' @param tweets Dataframe with tweets data
#' @param whole_parliament Logical indicator if function will create whole parliament network, default set to False
#' @param include_retweets Logical indicator if function will include retweets from tweets file, default set to True
#' @param include_replies Logical indicator if function will include replies from tweets file, default set to True
#' @param include_quotes Logical indicator if function will include quotes from tweets file, default set to True
#' @param only_subparty Logical indicator if partia argument is name of parliament club or party, default set to False
#' @return A igraph object representing network
#' @export
create_network <- function(partia, poslowie, tweets, whole_parliament = F, include_retweets = T, include_replies = T, include_quotes = T, only_subparty = F) {
  if (whole_parliament) {
    twitters <- poslowie$screen_name
  }
  else {
    if (only_subparty) {
      twitters <- dplyr::filter(poslowie, subparty == partia)$screen_name
    }
    else {
      twitters <- dplyr::filter(poslowie, party == partia)$screen_name
    }
  }
  if (include_retweets) {
    x <- filter_tweets_retweets(tweets, twitters)
    names(x)[names(x) == "retweet_screen_name"] <- "to"
  }
  else {

    x<-tibble::tibble(screen_name=vector(mode = "character"),to=vector(mode="character"))
  }
  if (include_replies) {
    y <- filter_tweets_replies(tweets, twitters)
    names(y)[names(y) == "reply_to_screen_name"] <- "to"
    x <- dplyr::bind_rows(x, y)
  }
  if (include_quotes) {
    z <- filter_tweets_quotes(tweets, twitters)
    names(z)[names(z) == "quoted_screen_name"] <- "to"
    x <- dplyr::bind_rows(x, z)
  }
  vertices <- union(x$screen_name, x$to)
  dplyr::inner_join(tibble::tibble(screen_name = vertices), poslowie, by = "screen_name") -> vertices
  names(vertices)[names(vertices) == "subparty"] <- "group"
  rt_g <- igraph::graph_from_data_frame(x, vertices = vertices, directed = TRUE)
  igraph::E(rt_g)$weight <- 1
  return(rt_g)
}
#' Convert network to undirected
#'
#' This function makes given network undirected, simplifies it and removes unconnected verticies
#'
#' @param network igraph object to convert
#' @return A igraph object representing undirected network
#' @export
make_undirected <- function(network) {
  network <- igraph::as.undirected(network, mode = "mutual")

  igraph::E(network)$weight <- 1
  igraph::simplify(network, remove.multiple = TRUE, edge.attr.comb = list(weight = "sum")) -> network

  igraph::delete.vertices(network, (degree(network, v = V(network), mode = "all") <= 0)) -> network
  return(network)
}
#' Summary of inter cluster connections
#'
#' This function creates summary for every verticle in network
#' showing sum of edges weights for every network .
#'
#' @param network igraph object
#' @param communities communities of network
#' @return Data frame with summary
#' @export
summary_interactions_among_clusters <- function(network, communities) {
  igraph::membership(communities) -> x

  data.frame(keyName = names(x), value = as.numeric(x), row.names = NULL) -> communities
  igraph::as_data_frame(network, what = "edges") -> edges
  extra_edges <- edges
  extra_edges$from <- edges$to
  extra_edges$to <- edges$from
  dplyr::bind_rows(edges, extra_edges) -> edges
  dplyr::left_join(edges, communities, by = c("to" = "keyName")) -> all_info
  all_info <- dplyr::group_by(all_info, from, value)
  all_info <- dplyr::summarise(all_info, weight = sum(weight))
  names(all_info)[names(all_info) == "from"] <- "verticle"
  names(all_info)[names(all_info) == "value"] <- "to_cluster"
  names(all_info)[names(all_info) == "weight"] <- "sum of weights"
  dplyr::left_join(all_info, communities, by = c("verticle" = "keyName")) -> all_info
  names(all_info)[names(all_info) == "value"] <- "cluster"
  all_info <- all_info[, c(1, 4, 2, 3)]
  return(all_info)
}

#' Summary about clusters
#' .
#'
#' This function creates summary every cluster in network
#' @param network igraph object
#' @param communities communities of network
#' @return Data frame with summary
#' @export
create_community_table<-function(network,communities){

  igraph::membership(communities ) -> x
  data.frame(keyName = names(x), value = as.numeric(x), row.names = NULL) -> communities
  igraph::as_data_frame(network,what="vertices")->vertices
  dplyr::inner_join(vertices,communities,by=c("name" = "keyName"))->x
  dplyr::select(x,name,value,group)->x
  names(x)[names(x)=='group']<-'Partia'
  names(x)[names(x)=='value']<-'Cluster'
  names(x)[names(x)=='name']<-'Imie i Nazwisko'

  return(x)
}
#' Visualize network
#' .
#'
#' This function will visualize given network using visnetwork package
#' @param network igraph object
#' @param layout igraph layout algorithm default: "layout_nicely"
#' @return visualization
#' @export
visualize_network<-function(network,layout="layout_nicely"){

  visNetwork::visIgraph(network,layout = layout,randomSeed = 155)->network
  visNetwork::visEdges( network,font ='30px arial black' ,smooth = FALSE,color = list(color = "#6D6E71", highlight = "red")) ->network
  visNetwork::visNodes(network,font = '20px arial black') -> network
  visNetwork::visOptions(network,highlightNearest = list(enabled = TRUE))->network
  visNetwork::visLegend(network,position = 'right',width = 0.3,zoom=FALSE) ->network
  return(network)

}
