#' snitch_tab
#'
#' @param simobj
#'
#' @return
#' @export
#'
#' @examples
snitch_tab <- function(simobj) {
  ns <- simobj$NumSaboteurs
  calc <- as_tibble(calc_saboteurs_vs_others(simobj))

  rmean <- function(z) round(mean(z), 2)
  rsd <- function(z) round(sd(z), 2)

  rtab <- array(NA, dim=c(6,2))

  rownames(rtab) <- c("All", "Saboteurs", "Others", "Diffs (z, p)", "Converters", "Contagion")
  colnames(rtab) <- c("Mean", "SD")

  z_score <- mean(calc$others_mean - calc$saboteur_mean)/sqrt(sd(calc$others_mean)^2+sd(calc$saboteur_mean)^2)

  rtab[,1] <- c(rmean(calc$allmean), rmean(calc$saboteur_mean), rmean(calc$others_mean), round(z_score, 2),
                rmean(calc$n_convert), rmean(calc$n_contagion))
  rtab[,2] <- c(rsd(calc$allmean), rsd(calc$saboteur_mean), rsd(calc$others_mean), round(pnorm(z_score), 2), NA, NA)

  rtab
}

# I really do hate github
