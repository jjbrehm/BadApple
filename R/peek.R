#' peek
#'
#' @param simobj simulation object
#' @param when integer
#'
#' @return ggplot object
#' @export
#'
peek <- function(simobj, when) {
  require(tidyverse)
  what <- as_tibble(rearranger(simobj)) %>%
    filter(Iteration==when) %>%
    mutate(who=ifelse((Worker=="V3"|Worker=="V4"|Worker=="V5"), "Saboteur", "Other"))
  ggplot(what, aes(x=Performance, fill=who)) + geom_histogram()
}
