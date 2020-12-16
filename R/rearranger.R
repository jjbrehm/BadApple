# function rearranges simbobj$Performance to a matrix w/ (Replication, Iteration, Worker, Performance)

#' Title
#'
#' @param simobj simulation object
#'
#' @return matrix
#' @export
#' @import tidyverse
#'
rearranger <- function(simobj) {
  pr.t <- as_tibble(simobj$Performance)
  pr.t <- pr.t %>% pivot_longer(cols=3:(dim(pr.t)[2]), names_to="Worker", values_to="Performance") %>%
            rename(Replication = V1,
                  Iteration = V2)
}
