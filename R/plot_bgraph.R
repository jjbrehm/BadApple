# function to plot an n-graph of the subset of bureaucrats for a given replication

#' Tplot_bgraph
#'
#' @param imit_obj simulation object
#' @param repl_num integer
#'
#' @return plot object
#' @export
#'
plot_bgraph <- function(imit_obj, repl_num) {
  obrecord <- imit_obj$ObstyRecord
  obrecord <- array(obrecord[obrecord[,1] == repl_num, 2:(1+imit_obj$NumBurs)], dim=c(imit_obj$NumBurs, imit_obj$NumBurs))

  obrecord.g <- graph_from_adjacency_matrix(obrecord,diag=FALSE, mode="undirected")
  plot(obrecord.g, layout=layout_in_circle(obrecord.g))
}
