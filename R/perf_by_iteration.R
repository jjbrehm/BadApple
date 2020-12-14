#' perf_by_iteration()
#'
#' @param pobj simulation object
#' @param title string
#' @param iteration_list vector
#' @param obj_type character string
#'
#' @return ggplot object
#' @export
#'
perf_by_iteration <- function(pobj, title=NA, iteration_list=NA, obj_type="ImitSim") {
  perf <- as_tibble(pobj$Performance)

  if (obj_type=="ImitSim") {
    perf <- perf %>%
      rename(Replication=V1, Iteration=V2) %>%
      mutate(Performance=rowMeans(select(perf, c(V3:V12)))) %>%
      select(Replication, Iteration, Performance)
  }
  else if (obj_type=="BadApple") {
    ns <- pobj$NumSaboteurs
    nb <- pobj$NumBurs

    perf <- perf %>%
      rename(Replication=V1, Iteration=V2) %>%
      mutate(saboteur_perf = rowMeans(perf[3:(2+ns)]),
             other_perf = rowMeans(perf[(3+ns):(2+nb)])) %>%
      pivot_longer(cols=ends_with("perf"), names_to="Who", values_to="Performance", names_pattern="(.*)_.*") %>%
      select(Replication, Iteration, Performance, Who)
  }


  if (!is.na(iteration_list))
    perf <- perf %>% filter(Iteration %in% iteration_list)

  if (obj_type=="ImitSim") {
    ggplot(perf, aes(x=Performance)) +
      geom_histogram() +
      facet_wrap(~ Iteration) +
      labs(x="Mean Performance", y="Count", title=title) +
      theme_minimal()
  }
  else if (obj_type=="BadApple") {
    ggplot(perf, aes(x=Performance, fill=Who)) +
      geom_histogram(position="dodge") +
      facet_wrap(~ Iteration) +
      scale_fill_manual(values = c("gray70", "palegreen4")) +
      labs(x="Mean Performance", y="Count", title=title) +
      theme_minimal()

  }
}
