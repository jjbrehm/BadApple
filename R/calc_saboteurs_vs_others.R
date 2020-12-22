#' calc_saboteurs_vs_others
#'
#' @param simobj simulation object
#'
#' @return matrix
#' @export
#' @import magrittr
#'
calc_saboteurs_vs_others <- function(simobj) {
  last_warning_setting = getOption("warn")
  options(warn=-1)

  ns <- simobj$NumSaboteurs
  perf.t <- simobj$Performance
  nc <- ncol(perf.t)

  the_names <- paste(c("Replication", "Iteration", paste0("Bureaucrat_", rep(1:(nc-2)))))
  colnames(perf.t) <- the_names

  perf.t <- as_tibble(perf.t)

  last_iter.t <- perf.t %>% dplyr::filter(Iteration==simobj$MaxIter)
  first_iter.t <- perf.t %>% dplyr::filter(Iteration==1)

  record <- last_iter.t %>%
    select(-Replication, -Iteration)
  results <- record %>%
    mutate(allmean=rowMeans(record),
           saboteur_mean=rowMeans(record[1:ns]),
           others_mean=rowMeans(record[(ns=1):ncol(record)]),
           mean_diff=others_mean-saboteur_mean) %>%
    select(allmean, saboteur_mean, others_mean, mean_diff)

  diff.t <- last_iter.t - first_iter.t
  diff.t <- diff.t %>%
    select(-Replication, -Iteration) %>%
    mutate(D_allmean=rowMeans(record),
           D_saboteur_mean=rowMeans(record[1:ns]),
           D_others_mean=rowMeans(record[(ns+1):ncol(record)]),
           D_mean_diff=D_others_mean-D_saboteur_mean) %>%
    select(D_allmean, D_saboteur_mean, D_others_mean, D_mean_diff)

  results <- cbind(results,diff.t)

  # define "convert" as when saboteurs adopt others' responses
  # and "contagion" as then others adopt saboteurs' responses
  #
  # then n_convert is number of saboteurs who imitate others
  # and n_contagion is number of others who imitate saboteurs

  first <- first_iter.t %>% select(-Replication, -Iteration)
  last <- last_iter.t %>% select(-Replication, -Iteration)

  first_sab <- first[,1:ns]
  last_sab <- last[,1:ns]

  first_others <- first[,(ns+1):ncol(first)]
  last_others <- last[,(ns+1):ncol(last)]

  # let's see if I can set the never changes to -99 and -98 and never see
  # conversions

  last_sab[last_sab == first_sab] <- -99
  last_others[last_others == first_others] <- -98

  # I wonder if I have the 1 and 2 below correct?
  is_convert <- apply(last_sab, 1, is.element, first_others)      #was 1
  is_contagion <- apply(last_others, 1, is.element, first_sab)    #was 2

  #n_contagion <- apply(is_contagion, 1, sum)

  sum_is_in <- function(a1, a2) {
    rv1 <- split(a1, row(a1))
    rv2 <- split(a2, row(a2))

    map2(rv1, rv2, ~sum(is.element(.x, .y)))
  }

  n_convert <- unlist(sum_is_in(last_sab, first_others))/ns
  n_contagion <- unlist(sum_is_in(last_others, first_sab))/(nc-ns)

  n_calcs <- data.frame(n_contagion, n_convert)

  results <- cbind(results, n_calcs)
  options(warn=last_warning_setting)
  results
}
