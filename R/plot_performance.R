# won't execute as is, but the notes

#w$Performance is the performance matrix, first col is repl, second col is iter, col 3-(end) are performance
#pr <- w$Performance  # makes a temporary copy of the performance matrix
#colnames(pr) <- c("repl", "iter", seq(1, (dim(pr)[2] - 2))) # assigns labels (but will they sort??)
#pr.t <- dplyr::as_tibble(pr) # converts to a tlbble
#rearranged.t <- pr.t %>% pivot_longer(cols=3:(dim(pr)[2]), names_to="Worker", values_to="Performance")

#' Title
#'
#' @param w
#'
#' @return
#' @export
#'
#' @examples
plot_performance <- function(w) {
  temp.t <- tidyr::as_tibble(w$TallPerformance)
  ggplot(temp.t, aes(x=Iteration, y=Performance, group=interaction(Replication, Bureaucrat))) +
    geom_line(alpha=.1) +
    theme_minimal()
}
