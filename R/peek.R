#' peek
#'
#' @param simobj simulation object
#' @param when integer
#'
#' @return ggplot object
#' @export
#' @import dplyr
#'
peek <- function(simobj, when) {
  what <- as_tibble(rearranger(simobj)) %>%
    dplyr::filter(Iteration==when) %>%
    mutate(Who=ifelse((Worker=="V3"|Worker=="V4"|Worker=="V5"), "Saboteur", "Other"))
  ggplot(what, aes(x=Performance, fill=Who)) + geom_histogram()
}
