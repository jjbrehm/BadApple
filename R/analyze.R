#' analyze
#'
#' @param o
#'
#' @return
#' @export
#'
#' @examples analyze(obsty)
analyze <- function(o) {
  igraph::vertex_connectivity(igraph::graph_from_adjacency_matrix(o))
}

