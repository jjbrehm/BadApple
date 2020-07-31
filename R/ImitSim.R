# R Script to implement the Imitation model from WSS
# - Aim is to use base R functions and avoid APL/C/Java code that
#   was probably inaccessible to many people
# - Thought was also that APL is in many ways a precursor to R, so
#   that the migration should not be hard

require("igraph")
#require("xts") # only for the first()/last() functions for the old_analyze take and drop


# Call:
# ImitSim(Replications, NumBurs=10, MaxIter=10, binary=TRUE, supervision="Relative")
#   - Returns:
#     $Version: "1.1b1"
#     $NumBurs: as set, or default=10
#     $MaxIter: as set, or default=10
#     $binary: as set, or default=TRUE. Produces an error for "FALSE" since connectivity
#       measures here use the igraph:: functions, and really, it only makes sense to look at the
#       actual observed instead of "probability" to observe.
#     $supervision: "Relative" (default), or "Fixed"
#       (in which case "Tolerance" is replaced by "Std")
#     $posprefs: TRUE or FALSE (default)
#     $ExecSummary: a dataframe containing SupUtilMean, SupUtilSD, PrefMean, PrefSD,
#      RespSD, SupObsMean, SupObsSD, BurObsMean, BurObsSD,
#       Tolerance, Punishment, Connectivity, ActualResponseMean, ActualResponseSD,
#       BurUtilMean, BurUtilSD
#     $PerformanceRecord: a matrix of (repl_ct, iter, response that iteration for each bureaucrat)
#     $ObstyRecord: a matrix of (repl_ct, obsty matrix for each trial)
#     $SeenRecord:  a matrix of (repl_ct, iter, vector of whether each bureaucrat was seen that iter and trial)
#   - e.g.,
#	wss <- BigLoop1a(1000)
#     conducts a simulation with default parameters at 1000 Replications (replications)

# Defined in ImitSim
#   Replications - Number of Replications (or replications of an original simulation)
#   ResponseParms - Response Parameters (mean, sd ~ unif)
#   SupObsParms - Observability of each bureaucrat (mean, sd ~ unif)
#   BurObsParms - Observability of each bureaucrat to one another (mean, sd ~ unif)                 #Needed???
#   Obsty - Symmetric (NumBurs x NumBurs) matrix of probabilities of observability ~ unif
#   SupObsty - Vector of probabilities that supervisor sees each bureaucrat ~ normal                #? why normal??
#   PrefParms - Preferences of the policy (~unif(-1,1))
#   Tolerance - # of Std Deviations from others that supervisor permits ~ unif(-2,0)
#   Std - Minimum acceptable performance (~unif)
#   Punishment - How much punishment is levied (unif(-2,0))
#   Performance - Record of each bureaucrat's performance (MaxIter x NumBurs)
#   supervision - "Relative" (1a) or Fixed" (1b/1c/2b/2c)
#   posprefs - FALSE (default) or TRUE (prefs > 0 (2a/2b/2c))
#   omniscient - FALSE (default) or TRUE (supervisor can see everyone, 1c, 2c)

#' ImitSim
#'
#' @param Replications integer
#' @param binary logical
#' @param NumBurs integer
#' @param MaxIter integer
#' @param supervision character
#' @param posprefs logical
#' @param omniscient logical
#' @param SupObsParms vector(2)
#' @param Tolerance double
#' @param Std doublw
#' @param Punishment double
#' @param ResponseParms vector(2)
#' @param PrefParms vector(2)
#' @param BurObsParms vector(2)
#' @param tallperformance logical
#'
#' @return
#' @export
#'
#' @examples
ImitSim <- function(Replications, binary=TRUE, NumBurs=10, MaxIter=10,
                      supervision="Relative", posprefs=FALSE, omniscient=FALSE,
                      tallperformance=FALSE,
                      SupObsParms=c(-99,-99), Tolerance=-99, Std=-99, Punishment=-99,
                      ResponseParms=c(-99,-99), PrefParms=c(-99,-99), BurObsParms=c(-99,-99), debug=FALSE) {
  # This routine randomly draws the assorted parameters
  # and stores the final mean supoutc as a dependent var
  # Version 1a calls policy 1a (relative punishment) based on punrate

  cat("Running Imit Sim over", Replications, "Replications\n")
  cat(" with NumBurs", NumBurs, "\n")
  cat(" over MaxIter", MaxIter, "\n")

  cat("Other parameters:\n")
  cat(" supervision:", supervision, "\n")
  if (!posprefs) cat(" with random preferences\n")
    else cat(" with positive preferences\n")
  if (!omniscient) cat(" without omniscient supervisor\n")
    else cat(" with omniscient supervisor\n")
  if (tallperformance) cat(" with TallPerformance record (SLOW)\n")
    else cat(" without TallPerformance record\n")

  if (SupObsParms[1] != -99) cat("SupObsParms", SupObsParms, "\n")
    Init_SupObsParms <- SupObsParms
  if (Tolerance != -99) cat("Tolerance", Tolerance, "\n")
    Init_Tolerance <- Tolerance
  if (Punishment != -99) cat("Punishment", Punishment, "\n")
    Init_Punishment <- Punishment
  if (Std != -99) cat("Std", Std, "\n")
    Init_Std <- Std
  if (ResponseParms[1] != -99) cat("ResponseParms", ResponseParms, "\n")
    Init_ResponseParms <- ResponseParms
  if (PrefParms[1] != -99) cat("PrefParms", PrefParms, "\n")
    Init_PrefParms <- PrefParms
  if (BurObsParms[1] != -99) cat("BurObsParms", BurObsParms, "\n")
    Init_BurObsParms <- BurObsParms
  if (debug) cat("DEBUGGING ACTIVE\n")


  ExecSummary <- array(data=rep(Replications*17,0), dim=c(Replications, 17))
  if (supervision == "Relative")
    colnames(ExecSummary) <- c("SupUtilMean", "SupUtilSD", "PrefMean", "PrefSD", "RespMean", "RespSD",
                         "SupObsMean", "SupObsSD", "BurObsMean", "BurObsSD",
                         "Tolerance", "Punishment", "Connectivity", "ActualResponseMean", "ActualResponseSD",
                         "BurUtilMean", "BurUtilSD")
  else if (supervision == "Fixed")
    colnames(ExecSummary) <- c("SupUtilMean", "SupUtilSD", "PrefMean", "PrefSD", "RespMean", "RespSD",
                               "SupObsMean", "SupObsSD", "BurObsMean", "BurObsSD",
                               "Std", "Punishment", "Connectivity", "ActualResponseMean", "ActualResponseSD",
                               "BurUtilMean", "BurUtilSD")

  # Store the results overall in Performance                                ## Do I want Performance Record to be global?
  Performance <- array(NA, dim=c(Replications*MaxIter,2+NumBurs))
  if (tallperformance) {
    TallPerformance <- array(NA, dim=c(0, 5))
    colnames(TallPerformance) <- c("Replication", "Iteration", "Connectivity", "Bureaucrat", "Performance")
  }
  ObstyRecord <- array(NA, dim=c(0, 1+NumBurs))
  SeenRecord <- array(NA, dim=c(0, 2+NumBurs))

  # Loop starts here ("go")
  for (repl_ct in 1:Replications) {
    if (ResponseParms[1] == -99) {
      ResponseParms <- runif(2)
      ResponseParms[1] <- -1+2*ResponseParms[1]
    }

    # draw SupObsParms randomly for policy 1a/1b/2a/2b
    if (SupObsParms[1] == -99) SupObsParms <- runif(2)

    # uncomment for policy 1c/2c
    if (BurObsParms[1] == -99) BurObsParms <- runif(2)

    cat("Output BurObsParms", BurObsParms, "\n")

    ### Probably a bug from the original code: the Obsty matrix was always hyperconnected,
    ### which really shouldn't be the case.
    ###
    ### observability matrix (for bureaucrats) dist'd unif(0,1)
    ### symmetric (x sees y sees x equally), where each entry is
    ### probability of seeing
    ### (obsty has to be global for the connectivity check)
    ###Obsty <- array(runif(NumBurs^2),dim=c(NumBurs,NumBurs))               ## seems like numburs must be global??

    # observability matrix (for bureaucrats) dist'd normal(mean=BurObsParms[1], sd=BurObsBarms[2])
    # but then converted to PROBABILITIES

    # symmetric (x sees y sees x equally), where each entry is
    # probability of seeing
    # (obsty has to be global for the connectivity check)

    Obsty <- array(rnorm(NumBurs^2, mean=BurObsParms[1], sd=BurObsParms[2]),dim=c(NumBurs,NumBurs))
    Obsty <- pnorm(Obsty)
    Obsty[lower.tri(Obsty)] <- t(Obsty)[lower.tri(Obsty)]
    Obsty <- Obsty>.5
    Obsty[Obsty==0] <- NA

    diag(Obsty) <- 1

    ObstyRecord <- rbind(ObstyRecord,cbind(rep(repl_ct,NumBurs),Obsty))

    # Add to ObstyRecord
    #ObstyRecord[(repl_ct-1):(repl_ct-1)+NumBurs, ] <- Obsty

    # supervisor observability (see and be seen, here) dist'd unif(0,1)   ## I say unif in the code, but seems norm??
    SupObsty <- rnorm(NumBurs, mean=SupObsParms[1], sd=SupObsParms[2])
    SupObsty[SupObsty < 0] <- 0
    SupObsty[SupObsty > 1] <- 1

    # Set the range of Preferences (why did I call it popparms??)        ## NOT IN ORIGINAL CODE
    if (posprefs == FALSE & Init_PrefParms[1] == -99) PrefParms <- c(runif(1, min=-1, max=1), runif(1))
    else if (posprefs == TRUE & Init_PrefParms[1] == -99) PrefParms <- c(runif(1, min=0, max=1), runif(1))

    ### I'm not sure on the is.null call here. I *know* it's not null
    # "Relative" is for 1a/2a; "Fixed" is for 1b/1c/2b/2c
    if (supervision == "Relative") Tolerance <- runif(1, min = -2, max=2)
    else if (supervision == "Fixed") Std <- runif(1, min=-1, max=1)

    Punishment <- runif(1,min=0, max=2)

    if (binary) Connectivity <- analyze(Obsty)                                         ## wouldn't seem that obsty has to be global
     else Connectivity <- old_analyze(Obsty)

    # this version of the policy routine
    # assigns outcomes on the basis of dowhat * response
    # unless the saboteur is caught

    # Response - What Bureaucrats do this round (~ truncated normal ([-1,1], resp_parms))
    # Catch - Do I catch anyone? (dummy)
    # Prefs - Preferences of each Bureaucrat (~ trunc normal ([-1,1], prefs_parms))
    # BurUtil - Bureaucrats' Utility
    # SupUtil - Supervisor's Utility (mean and sd of bureaucrats' performance)
    # SupSeen - Who does supervisor see this round?
    # BurUtilSeen - Utility of Bureaucrats who are Seen by Bureaucrats
    # Envy - Which bureaucrat does each bureaucrat envy (highest utility)?


    # iter - iteration counter
    #
    # get initial conditons
    # burs respond over -1 to 1, dist'd normally
    Response <- rnorm(NumBurs, mean=ResponseParms[1], sd=ResponseParms[2])
    Response[Response < -1] <- -1
    Response[Response > 1] <- 1

    # change call below to switch policy variation

    # set counters to zero, open result matrices
    Catch <- 0
    BurUtil <- array(0, dim=c(MaxIter, NumBurs))                 ## Do I want this to be global?
    SupUtil <- array(0, dim=c(MaxIter, 2))

    #getpolicy(mnpop)                                                       ## divergence!!! scope prevents this from working
    Prefs <- rnorm(NumBurs, mean=PrefParms[1], sd=PrefParms[2])  ## whoa! was I allowing Prefs outside of -1,1???
    if (posprefs == TRUE) {
      Prefs[Prefs < 0] = 0
      Prefs[Prefs > 1] = 1
    }

    if (debug) {
      cat("\n============\nReplication:", repl_ct, "\n============\n")
      cat("Obsty:\n")
      print(Obsty)
      }

    # play: go from here
    for (iter in 1:MaxIter) {

        # Do ranges from -1 (complete defection) to 1 (complete compliance)
        Do <- Response

        # Record Performance
        Performance[(repl_ct-1)*MaxIter+iter, 1:2] <- c(repl_ct, iter)
        Performance[(repl_ct-1)*MaxIter+iter, 3:(NumBurs+2)] <- Response

        # Write to TallPerformance
        if (tallperformance)
          for (b in 1:NumBurs)
            TallPerformance <- rbind(TallPerformance, c(repl_ct, iter, Connectivity, b, Response[b]))

        # outcome equals desires times do
        if (debug) {
          cat("Prefs:\n")
          print(Prefs)
          cat("Do:\n")
          print(Do)
        }
        BurUtil[iter,] <- Prefs*Do

        if (debug) {
          cat("BurUtil (before supervision)\n")
          print(BurUtil[iter,])
        }

        # supervisor enforces against those she sees defecting
        SupUtil[iter,] <- c(mean(Do), sd(Do))

        Seen <- trunc(SupObsty+runif(1))

        if (debug) {
          cat("Seen:\n")
          print(Seen)
        }

        SeenRecord <- rbind(SeenRecord, cbind(repl_ct, iter, t(Seen)))

        # Relative tolerance => deviants are those whose behavior is below "Mean - Tolerance*SD"
        # Fixed supervision => deviants are less than Std units (on a -1,1 scale)
        if (supervision == "Relative") deviants <- Do < SupUtil[iter,1] - (Tolerance*SupUtil[iter,2])
        else if (supervision == "Fixed") deviants <- Do < Std
        if (debug){
          cat("deviants:\n")
          print(deviants)
        }

        # still have to be deviant even if omniscient
        if (!omniscient) seendeviants <- deviants * Seen
        else seendeviants <- deviants
        if (debug) {
          cat("seendeviants:\n")
          print(seendeviants)
        }

        WhoCaught <- which(as.logical(seendeviants))
        if (debug) {
          cat("WhoCaught:\n")
          print(WhoCaught)
        }

        if(length(WhoCaught)!=0) {
          Catch <- Catch + 1
          BurUtil[iter,WhoCaught] <- BurUtil[iter,WhoCaught] - Punishment
        }
        if (debug) {
          cat("---\nBurUtil (after supervision)\n")
          print(BurUtil[iter,])
        }

        # adapt:
        BurUtilAll <- array(data=BurUtil[iter,], dim=c(NumBurs, NumBurs))
        if (debug) {
          cat("BurUtilAll\n:")
          print(BurUtilAll)
        }

        BurUtilSeen <- array(NA, dim=c(NumBurs, NumBurs))

        for (i in 1:NumBurs) BurUtilSeen[i,] <- BurUtilAll[i,] * Obsty[i,]

        if (debug) {
          cat("\nBurUtilSeen:\n")
          print(BurUtilSeen)
        }

        # choose response of the person you saw who did best

        Envy <- apply(BurUtilSeen, 2, which.max)
        if (debug) {
          cat("\nEnvy:\n")
          print(Envy)
        }
        if (debug) cat("\nOld Response:\n", Do)
        Response <- Do[Envy]
        if (debug) cat("\nNew Response:\n", Response)

        # play again, unless this iteration exceeds maxiter
      }

    ExecSummary[repl_ct, 1:10] <- c(SupUtil[MaxIter,], PrefParms, ResponseParms, SupObsParms, BurObsParms)
    # will need to change punrate in line below to std for 1b/1c/2b/2c
    if (supervision == "Relative")
      ExecSummary[repl_ct, 11:15] <- c(Tolerance, Punishment, Connectivity, mean(Response), sd(Response))
    else if (supervision == "Fixed")
      ExecSummary[repl_ct, 11:15] <- c(Std, Punishment, Connectivity, mean(Response), sd(Response))
    ExecSummary[repl_ct, 16:17] <- c(mean(BurUtil[MaxIter,]), sd(BurUtil[MaxIter,]))

    if (Init_SupObsParms[1] == -99) SupObsParms <- c(-99,-99)
    if (Init_Tolerance == -99) Tolerance <- -99
    if (Init_Std == -99) Std <- -99
    if (Init_Punishment == -99) Punishment <- -99
    if (Init_PrefParms[1] == -99) PrefParms <- c(-99, -99)
    if (Init_ResponseParms[1] == -99) ResponseParms <- c(-99, -99)
    if (Init_BurObsParms[1] == -99) BurObsParms <- c(-99,-99)

  }

  if (tallperformance)
    BigList <- list("Version" = "1.1b3",
                    "NumBurs" = NumBurs,
                    "MaxIter" = MaxIter,
                    "Replications" = Replications,
                    "binary" = binary,
                    "supervision" = supervision,
                    "posprefs" = posprefs,
                    "omniscient" = omniscient,
                    "ExecSummary" = as.data.frame(ExecSummary),
                    "Performance" = Performance,
                    "TallPerformance" = TallPerformance,
                    "ObstyRecord" = ObstyRecord,
                    "SeenRecord" = SeenRecord)
  if (!tallperformance)
    BigList <- list("Version" = "1.1b3",
                    "NumBurs" = NumBurs,
                    "MaxIter" = MaxIter,
                    "Replications" = Replications,
                    "binary" = binary,
                    "supervision" = supervision,
                    "posprefs" = posprefs,
                    "omniscient" = omniscient,
                    "ExecSummary" = as.data.frame(ExecSummary),
                    "Performance" = Performance,
                    "ObstyRecord" = ObstyRecord,
                    "SeenRecord" = SeenRecord)
  BigList
}

# analyze is busted
analyze <- function(o) {
  igraph::vertex_connectivity(igraph::graph_from_adjacency_matrix(o))
}

old_analyze <- function(o) {
  cat("the original APL\11 analyze function does not work, specify 'binary=TRUE' on command instead\n")
  }
