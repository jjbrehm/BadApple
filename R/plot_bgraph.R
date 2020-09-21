# function to plot an n-graph of the subset of bureaucrats for a given replication

#' Tplot_bgraph
#'
#' @param imit_obj
#' @param repl_num
#'
#' @return
#' @export
#'
#' @examples
plot_bgraph <- function(imit_obj, repl_num) {
  obrecord <- imit_obj$ObstyRecord
  obrecord <- array(obrecord[obrecord[,1] == repl_num, 2:(1+imit_obj$NumBurs)], dim=c(imit_obj$NumBurs, imit_obj$NumBurs))

  obrecord.g <- igraph::graph_from_adjacency_matrix(obrecord,diag=FALSE, mode="undirected")
  plot(obrecord.g, layout=igraph::layout_in_circle(obrecord.g))
}
