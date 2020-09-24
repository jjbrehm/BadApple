#' peek
#'
#' @param simobj
#' @param when
#'
#' @return
#' @export
#'
#' @examples peek(simobj, 1)
peek <- function(simobj, when) {
  what <- as_tibble(rearranger(simobj)) %>%
    filter(Iteration==when) %>%
    mutate(who=ifelse((Worker=="V3"|Worker=="V4"|Worker=="V5"), "Saboteur", "Other"))
  ggplot(what, aes(x=Performance, fill=who)) + geom_histogram()
}
