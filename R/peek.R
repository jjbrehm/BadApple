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
  what <- as_tibble(rearranger(foo)) %>%
    mutate(who=ifelse((Worker=="V3"|Worker=="V4"|Worker=="V5"), "Saboteur", "Other"))
    filter(Iteration==when) %>%
    ggplot(aes(x=Performance, fill=who)) + geom_histogram()
}
