# function to plot an n-graph of the subset of bureaucrats for a given replication, uses igraph

#' plot_gg_ngraph
#'
#' @param imit_obj
#' @param repl_num
#'
#' @return
#' @export
#'
#' @examples
plot_gg_ngraph <- function(imit_obj, repl_num) {
  obrecord <- imit_obj$ObstyRecord
  obrecord <- array(obrecord[obrecord[,1] == repl_num, 2:(1+imit_obj$NumBurs)], dim=c(imit_obj$NumBurs, imit_obj$NumBurs))

  obsrecord.g <- igraph::graph_from_adjacency_matrix(obrecord,diag=FALSE, mode="undirected")
  ggraph::ggraph(ggraph::create_layout(obsrecord.g, layout="circle")) +
    ggraph::geom_edge_link() + ggraph::geom_node_label(ggplot2::aes(label = 1:(imit_obj$NumBurs)))
}
