# function rearranges simbobj$Performance to a matrix w/ (Replication, Iteration, Worker, Performance)

#' Title
#'
#' @param simobj
#'
#' @return
#' @export
#'
#' @examples
rearranger <- function(simobj) {
  pr.t <- tidyr::as_tibble(simobj$Performance)
  pr.t <- pr.t %>% tidyr::pivot_longer(cols=3:(dim(pr)[2]), names_to="Worker", values_to="Performance") %>%
            rename(Replication = V1,
                  Iteration = V2)
}