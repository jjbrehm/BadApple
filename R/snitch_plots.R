#' snitch_plots
#'
#' @param simobj
#'
#' @return
#' @export
#'
#' @examples
snitch_plots <- function(simobj) {
  ns <- simobj$NumSaboteurs
  calc <- as_tibble(calc_saboteurs_vs_others(simobj))
  foo <- calc %>% select(saboteur_mean, others_mean) %>%
    pivot_longer(cols=ends_with("mean"), names_to="Who", values_to="Response", names_pattern="(.*)_.*")

  s.h <- ggplot(foo, aes(x=Response, fill=Who)) +
    geom_histogram(position="dodge", bins=30) +
    scale_fill_manual(values=c("gray70", "palegreen4")) +
    theme_minimal()

  s.d <- ggplot(foo, aes(x=Response, fill=Who)) +                     #, size=1) +
    geom_density(alpha=.5) +
    scale_fill_manual(values=c("gray70", "palegreen4")) +
    theme_minimal()

  list(s.h, s.d)

}

# I hate github
