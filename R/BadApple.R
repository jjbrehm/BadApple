# R Script to adapt the Imitation model from WSS to a BadApple problem
# - Aim is to use base R functions and avoid APL/C/Java code that
#   was probably inaccessible to many people
# - Thought was also that APL is in many ways a precursor to R, so
#   that the migration should not be hard

# R, github, are software developed by tech-dorks with minimal english language skills
# fucking github trolls
# I really really really hate github.


# Call:
# BadApple(Replications, NumBurs=10, MaxIter=10, binary=TRUE, supervision="Relative")
#   - Returns:
#     $Version: "2.0a1"
#     $NumBurs: as set, or default=10
#     $NumSaboteurs: as set [required]
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

# Defined in BadApple
#   Replications - Number of Replications (or replications of an original simulation)
#   NumBurs - Number of bureaucrats
#   NumSaboteurs - Number of saboteurs (<NumBurs)
#   ResponseParms - Response Parameters (mean, sd ~ unif)
#   SabResponseParms -Response Parameters (mean, sd, ~ unif) of the saboteures
#   SupObsParms - Observability of each bureaucrat (mean, sd ~ unif)
#   SabSupObsParms - Observability of each saboteur (mean, sd ~ unif)
#   BurObsParms - Observability of each bureaucrat to one another (mean, sd ~ unif)
#   SabBurObsParms - Observability of each saboteur _by_ others
#   Obsty - Symmetric (NumBurs x NumBurs) matrix of probabilities of observability ~ unif
#   SabObsty - Matrix of probability os observability of the saboteurs
#   SupObsty - Vector of probabilities that supervisor sees each bureaucrat ~ normal
#   SabSupObsty - Vector of probsabilities that supervisor sees each saboteur
#   PrefParms - Preferences of the policy (~unif(-1,1))
#   SabPrefParms - Saboteurs' preferences towards policy
#   Tolerance - # of Std Deviations from others that supervisor permits ~ unif(-2,0)
#   Std - Minimum acceptable performance (~unif)
#   Punishment - How much punishment is levied (unif(-2,0))
#   SabPunishment - Punishment of each Saboteur
#   Performance - Record of each bureaucrat's performance (MaxIter x NumBurs)
#   supervision - "Relative" (1a) or Fixed" (1b/1c/2b/2c)
#   posprefs - FALSE (default) or TRUE (prefs > 0 (2a/2b/2c))
#   omniscient - FALSE (default) or TRUE (supervisor can see everyone, 1c, 2c)
#   quiet - FALSE (default) or TRUE (no output)

#' BadApple
#'
#' @param Replications integer
#' @param NumSaboteurs integer
#' @param NumBurs integer
#' @param MaxIter integer
#' @param supervision character
#' @param posprefs logical
#' @param omniscient logical
#' @param Memory logical
#' @param Dismissal integer
#' @param tallperformance logical
#' @param SupObsParms vector(2)
#' @param Tolerance double
#' @param Std doublw
#' @param Punishment double
#' @param ResponseParms vector(2)
#' @param PrefParms vector(2)
#' @param BurObsParms vector(2)
#' @param SabSupObsParms vector(2)
#' @param SabPunishment double
#' @param SabResponseParms vector(2)
#' @param SabPrefParms vector(2)
#' @param SabBurObsParms vector(2)
#' @param ReplacementPrefParms vector(2)
#' @param ReplacementResponseParms vector(2)
#' @param quiet logical
#' @param debug logical
#'
#' @return simulation object
#' @export
#' @import stats
#'
BadApple <- function(Replications, NumSaboteurs=3, NumBurs=10, MaxIter=10,
                      supervision="Relative", posprefs=FALSE,
                      omniscient=FALSE,
                      Memory=FALSE, Dismissal=-99,
                      tallperformance=FALSE,
                      Tolerance=-99, Std=-99, Punishment=-99, SupObsParms=c(-99,-99),
                      ResponseParms=c(-99,-99), PrefParms=c(-99,-99), BurObsParms=c(-99,-99),
                      SabSupObsParms=c(-99,-99), SabPunishment=-99,
                      SabResponseParms=c(-99,-99), SabPrefParms=c(-99,-99), SabBurObsParms=c(-99,-99),
                      ReplacementPrefParms=c(-99, -99),
                      ReplacementResponseParms=c(-99,-99),
                      quiet=FALSE, debug=FALSE) {
  # This routine randomly draws the assorted parameters
  # and stores the final mean supoutc as a dependent var
  # Version 1a calls policy 1a (relative punishment) based on punrate

  if (!quiet) {
    cat("Running BadApple over", Replications, "Replications\n")
    cat(" with NumBurs", NumBurs, "\n")
    cat(" with NumSaboteurs", NumSaboteurs, "\n")
    if (NumSaboteurs >= NumBurs) return("NumSaboteurs must be less than NumBurs")
    cat(" over MaxIter", MaxIter, "\n")

    cat("Other parameters:\n")
    cat(" supervision:", supervision, "\n")
    if (!posprefs) cat(" with random preferences\n")
      else cat(" with positive preferences\n")
    if (!omniscient) cat(" without omniscient supervisor\n")
      else cat(" with omniscient supervisor\n")
    if (!Memory) cat(" without memory\n")
      else cat(" with memory\n")
    if (Dismissal !=99) cat(paste0(" with Dismissal threshold of", Dismissal, "\n"))
     else cat(" without Dismissal option\n")
    if (ReplacementPrefParms[1] != -99) cat(" with Replacement Pref Parms of", ReplacementPrefParms, "\n")
    if (ReplacementResponseParms[1] != -99) cat(" with Replacement Response Parms of", ReplacementResponseParms, "\n")
    if (tallperformance) cat(" with TallPerformance record (SLOW)\n")
      else cat(" without TallPerformance record\n")
  }

  # setups of parameters ----

    if (SupObsParms[1] != -99 & !quiet) cat("SupObsParms", SupObsParms, "\n")
      Init_SupObsParms <- SupObsParms
    if (SabSupObsParms[1] != -99 & !quiet) cat("SabSupObsParms", SabSupObsParms, "\n")
      Init_SabSupObsParms <- SabSupObsParms

    if (Tolerance != -99 & !quiet) cat("Tolerance", Tolerance, "\n")
      Init_Tolerance <- Tolerance
    if (Std != -99 & !quiet) cat("Std", Std, "\n")
      Init_Std <- Std

    if (Punishment != -99 & !quiet) cat("Punishment", Punishment, "\n")
      Init_Punishment <- Punishment
    if (SabPunishment != -99 & !quiet) cat("SabPunishment", SabPunishment, "\n")
      Init_SabPunishment <- SabPunishment
    if (ResponseParms[1] != -99 & !quiet) cat("ResponseParms", ResponseParms, "\n")
      Init_ResponseParms <- ResponseParms
    if (SabResponseParms[1] != -99 & !quiet) cat("SabResponseParms", SabResponseParms, "\n")
      Init_SabResponseParms <- SabResponseParms
    if (ReplacementResponseParms[1] != -99 & !quiet) cat("ReplacementResponseParms", ReplacementResponseParms, "\n")
      Init_ReplacementResponseParms <- ReplacementResponseParms

    if (PrefParms[1] != -99 & !quiet) cat("PrefParms", PrefParms, "\n")
      Init_PrefParms <- PrefParms
    if (SabPrefParms[1] != -99 & !quiet) cat("SabPrefParms", SabPrefParms, "\n")
      Init_SabPrefParms <- SabPrefParms
    if (ReplacementPrefParms[1] != -99 & !quiet) cat("ReplacementPrefParms", ReplacementPrefParms, "\n")
      Init_ReplacementPrefParms <- ReplacementPrefParms

    if (BurObsParms[1] != -99 & !quiet) cat("BurObsParms", BurObsParms, "\n")
      Init_BurObsParms <- BurObsParms
    if (SabBurObsParms[1] != -99 & !quiet) cat("SabBurObsParms", SabBurObsParms, "\n")
      Init_SabBurObsParms <- SabBurObsParms
    if (debug) cat("DEBUGGING ACTIVE\n")


  ExecSummary <- array(data=rep(Replications*20,0), dim=c(Replications, 24))
  if (supervision == "Relative")
    colnames(ExecSummary) <- c("SupUtilMean", "SupUtilSD", "PrefMean", "PrefSD", "RespMean", "RespSD",
                         "SupObsMean", "SupObsSD", "BurObsMean", "BurObsSD",
                         "Tolerance", "Punishment", "SabPunishment", "Connectivity", "ActualResponseMean", "ActualResponseSD",
                         "BurUtilMean", "BurUtilSD","NumSaboteurs","Dismissal", "ReplPrefMn", "ReplPrefSD", "ReplRespMn", "ReplRespSD")
  else if (supervision == "Fixed")
    colnames(ExecSummary) <- c("SupUtilMean", "SupUtilSD", "PrefMean", "PrefSD", "RespMean", "RespSD",
                               "SupObsMean", "SupObsSD", "BurObsMean", "BurObsSD",
                               "Std", "Punishment", "SabPunishment", "Connectivity", "ActualResponseMean", "ActualResponseSD",
                               "BurUtilMean", "BurUtilSD","NumSaboteurs","Dismissal," , "ReplPrefMn", "ReplPrefSD", "ReplRespMn", "ReplRespSD")

  # Store the results overall in Performance                                ## Do I want Performance Record to be global?
  Performance <- array(NA, dim=c(Replications*MaxIter,2+NumBurs))
  if (tallperformance) {
    TallPerformance <- array(NA, dim=c(0, 5))
    colnames(TallPerformance) <- c("Replication", "Iteration", "Connectivity", "Bureaucrat", "Performance")
  }
  ObstyRecord <- array(NA, dim=c(0, 1+NumBurs))
  SeenRecord <- array(NA, dim=c(0, 2+NumBurs))

  # Loop starts here ("go") ----
  for (repl_ct in 1:Replications) {
    # per loop setups ----
    if (ResponseParms[1] == -99) {
      ResponseParms <- runif(2)
      ResponseParms[1] <- -1+2*ResponseParms[1]
    }

    if (SabResponseParms[1] == -99) {
      SabResponseParms <- runif(2)
      SabResponseParms[1] <- -1+2*SabResponseParms[1]
    }

    if (ReplacementResponseParms[1] == -99) {
      # same as ResponseParms
      ReplacementResponseParms <- runif(2)
      ReplacementResponseParms[1] <- -1+2*ReplacementResponseParms[1]
    }

    # draw SupObsParms randomly for policy 1a/1b/2a/2b
    if (SupObsParms[1] == -99) SupObsParms <- runif(2)
    if (SabSupObsParms[1] == -99) SabSupObsParms <- runif(2)

    # uncomment for policy 1c/2c
    if (BurObsParms[1] == -99) BurObsParms <- runif(2)

    if (SabBurObsParms[1] == -99) SabBurObsParms <- runif(2)

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

    SabObsty <- array(rnorm(NumSaboteurs^2, mean=SabBurObsParms[1], sd=SabBurObsParms[2]), dim=c(NumSaboteurs, NumSaboteurs))
    SabObsty <- pnorm(SabObsty)
    SabObsty[lower.tri(SabObsty)] <- t(SabObsty)[lower.tri(SabObsty)]
    SabObsty <- SabObsty>.5
    SabObsty[SabObsty==0] <- NA
    diag(SabObsty) <- 1

    Obsty[1:NumSaboteurs, 1:NumSaboteurs] <- SabObsty

    ObstyRecord <- rbind(ObstyRecord,cbind(rep(repl_ct,NumBurs),Obsty))

    # Need to create Saboteurs
    Saboteurs <- c(rep(1,NumSaboteurs), rep(0, (NumBurs-NumSaboteurs)))

    # supervisor observability (see and be seen, here) dist'd unif(0,1)   ## I say unif in the code, but seems norm??
    SupObsty <- rnorm(NumBurs, mean=SupObsParms[1], sd=SupObsParms[2])
    SabSupObsty <- rnorm(NumSaboteurs, mean=SabSupObsParms[1], sd=SabSupObsParms[2])
    # replace the observability of the saboteurs
    SupObsty[1:NumSaboteurs] <- SabSupObsty[1:NumSaboteurs]
    # truncate
    SupObsty[SupObsty < 0] <- 0
    SupObsty[SupObsty > 1] <- 1

    #DO create subnetwork of observability _by_ super of saboteur

    # Set the range of Preferences (why did I call it popparms??)        ## NOT IN ORIGINAL CODE
    if (posprefs == FALSE & Init_PrefParms[1] == -99) PrefParms <- c(runif(1, min=-1, max=1), runif(1))
    else if (posprefs == TRUE & Init_PrefParms[1] == -99) PrefParms <- c(runif(1, min=0, max=1), runif(1))

    #DO handle prefs for saboteurs here
    if (Init_SabPrefParms[1] == -99) SabPrefParms <- c(runif(1, min=-1, max=1), runif(1))

    # handle ReplacementPrefParms here
    if(Init_ReplacementPrefParms[1] == -99) ReplacementPrefParms <- c(runif(1, min=-9, max=1), runif(1))

    ### I'm not sure on the is.null call here. I *know* it's not null
    # "Relative" is for 1a/2a; "Fixed" is for 1b/1c/2b/2c
    if (supervision == "Relative") Tolerance <- runif(1, min = -2, max=2)
    else if (supervision == "Fixed") Std <- runif(1, min=-1, max=1)

    if (Punishment == -99) Punishment <- runif(1,min=0, max=2)

    #DO handle punishment of saboteurs here: note that _default_ is to let SabPunishment *also*
    # be a random uniform number (which means that half the time, the Saboteurs will be punished
    # *less* than the others)

    if (SabPunishment == -99) SabPunishment <- runif(1,min=0, max=2)

    Connectivity <-   igraph::vertex_connectivity(igraph::graph_from_adjacency_matrix(Obsty))

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

    #DO handle saboteurs responses here
    SabResponse <- rnorm(NumSaboteurs, mean=SabResponseParms[1], sd=SabResponseParms[2])
    SabResponse[SabResponse < -1] <- -1
    SabResponse[SabResponse > 1] <- 1

    Response[1:NumSaboteurs] <- SabResponse


    # change call below to switch policy variation

    # set counters to zero, open result matrices
    Catch <- 0
    BurUtil <- array(0, dim=c(MaxIter, NumBurs))
    SupUtil <- array(0, dim=c(MaxIter, 2))

    Prefs <- rnorm(NumBurs, mean=PrefParms[1], sd=PrefParms[2])
    if (posprefs == TRUE) {
      Prefs[Prefs < 0] = 0
      Prefs[Prefs > 1] = 1
    }

    SabPrefs <- rnorm(NumSaboteurs, mean=SabPrefParms[1], sd=SabPrefParms[2])

    Prefs[1:NumSaboteurs] <- SabPrefs
    Prefs[Prefs < -1] <- -1
    Prefs[Prefs > 1] = 1


    if (debug) {
      cat("\n============\nReplication:", repl_ct, "\n============\n")
      cat("Obsty:\n")
      print(Obsty)
      cat("\nPrefs:", Prefs,"\n")
      cat("Response:", Response, "\n")
      cat("Prefs x Response:", Prefs*Response, "\n----------------\n")
    }

    # play: go from here ----

    for (iter in 1:MaxIter) {

        # Do ranges from -1 (complete defection) to 1 (complete compliance)
        Do <- Response

        # if Memory==TRUE, have to set DidWhat and PastUtil (only applies for iter>=2)

        # Record Performance
        Performance[(repl_ct-1)*MaxIter+iter, 1:2] <- c(repl_ct, iter)

        if (debug) cat("iter=", iter, "Response=", Response, "\n")
        Performance[(repl_ct-1)*MaxIter+iter, 3:(NumBurs+2)] <- Response

        # Write to TallPerformance
        if (tallperformance)
          for (b in 1:NumBurs)
            TallPerformance <- rbind(TallPerformance, c(repl_ct, iter, Connectivity, b, Response[b]))

        # outcome equals desires times do
        if (debug) {
          cat("ITERATION[",iter,"]\n")
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

        # setting the Seen to be 1 for Saboteurs w/o random add
        Seen <- trunc(SupObsty+runif(1))
        Seen[1:NumSaboteurs] <- trunc(SabSupObsty+.5)
        Seen[Seen==0] <- NA

        if (debug) {
          cat("Seen:\n")
          print(Seen)
        }

        SeenRecord <- rbind(SeenRecord, cbind(repl_ct, iter, t(Seen)))

        # Relative tolerance => deviants are those whose behavior is below "Mean - Tolerance*SD"
        # Fixed supervision => deviants are less than Std units (on a -1,1 scale)
        if (debug) {
          cat("Do < Std", Do<Std, "\n")
        }
        if (supervision == "Relative") deviants <- Do < (SupUtil[iter,1] - (Tolerance*SupUtil[iter,2]))
        else if (supervision == "Fixed") deviants <- Do < Std
        if (debug) {
          cat("deviants:\n")
          print(deviants)
        }

        if (!omniscient) seendeviants <- (deviants * as.integer(Seen))>0
        else seendeviants <- deviants>0

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
          before <- BurUtil[iter, WhoCaught]
          SabCaught <- WhoCaught[WhoCaught <= NumSaboteurs]
          OthersCaught <- WhoCaught[WhoCaught > NumSaboteurs]
          if (length(SabCaught) != 0) BurUtil[iter,SabCaught] <- BurUtil[iter, SabCaught] - SabPunishment
          if (length(OthersCaught) != 0) BurUtil[iter,OthersCaught] <- BurUtil[iter, OthersCaught] - Punishment
        }
        if (debug) {
          cat("--->>>\nBurUtil[", iter, "]\nWhoCaught:", WhoCaught, "\nPunishment:", Punishment, "\n")
          cat("---\nBurUtil (after supervision)\n")
          print(BurUtil[iter,])
        }

        # adapt: ----
        BurUtilArray <- array(data=BurUtil[iter,], dim=c(NumBurs, NumBurs))
        if (debug) {
          cat("BurUtilArray\n:")
          print(BurUtilArray)
        }

        BurUtilSeenArray <- array(NA, dim=c(NumBurs, NumBurs))

        for (i in 1:NumBurs) BurUtilSeenArray[i,] <- BurUtilArray[i,] * Obsty[i,]

        if (debug) {
          cat("\nBurUtilSeenArray:\n")
          print(BurUtilSeenArray)
        }

        # choose response of the person you saw who did best

        Envy <- apply(BurUtilSeenArray, 2, which.max)
        if (debug) {
          cat("\nEnvy:\n")
          print(Envy)
        }
        # Have to add in Memory==TRUE here: behavior should be did what if
        # new util < old util
        if (debug) cat("\nOld Response:\n", Do, "\n")

        # MEMORY
        if (debug) cat("iter=", iter, "Envy=", Envy, "\n")
        if (length(Envy)>1) Response <- Do[Envy]

        if (Memory & (iter > 1)) {
          if (debug) cat("PastUtil=",PastUtil,"\nBurUtil=",BurUtil[iter,],"\n")
          if (debug) cat("Use past response\n", (PastUtil>BurUtil[iter,]),"\n")
          remembered <- PastUtil>BurUtil[iter,]
          Response[remembered] <- DidWhat[remembered]
          }

        if (Memory) {
          DidWhat <-  Do
          PastUtil <- BurUtil[iter,]
        }

        if (debug) cat("\nNew Response:\n", Response)

        # REPLACEMENT
        # If Dismissal != -99, then if Do < Dismissal, need to replace

        # should set the comparison here to be only those who are *seen* by the supervisor
        SeenDo <- Do * Seen
        if (debug) cat("SeenDo:", SeenDo, "\n")
        if (any(SeenDo < Dismissal, na.rm=TRUE)) {
          n_replace <- sum(SeenDo<Dismissal, na.rm=TRUE)
          at_replace <- which(SeenDo<Dismissal)
          if (debug) cat("SeenDo < Dismissal: N=", n_replace,  " at ", at_replace, "\n")
          ### HMM Do I want to change Do or SeenDo? I think I should change Do
          Do[at_replace] <- rnorm(n_replace, mean=ReplacementResponseParms[1], sd=ReplacementResponseParms[2])

          if (debug) cat("ReplacementPrefParms=", ReplacementPrefParms, "\n")
          Prefs[at_replace] <- rnorm(n_replace, mean=ReplacementPrefParms[1], sd=ReplacementPrefParms[2])
        }
        # bureaucrat with a new bureaucrat (and correspondingly change the preference vector and response vector)

        # play again, unless this iteration exceeds maxiter
      }

    ExecSummary[repl_ct, 1:10] <- c(SupUtil[MaxIter,], PrefParms, ResponseParms, SupObsParms, BurObsParms)
    # will need to change punrate in line below to std for 1b/1c/2b/2c
    if (supervision == "Relative")
      ExecSummary[repl_ct, 11:15] <- c(Tolerance, Punishment, Connectivity, mean(Response), sd(Response))
    else if (supervision == "Fixed")
      ExecSummary[repl_ct, 11:15] <- c(Std, Punishment, Connectivity, mean(Response), sd(Response))
    ExecSummary[repl_ct, 16:23] <- c(mean(BurUtil[MaxIter,]), sd(BurUtil[MaxIter,]), NumSaboteurs, Dismissal,
                                     ReplacementPrefParms, ReplacementResponseParms)

    if (Init_SupObsParms[1] == -99) SupObsParms <- c(-99,-99)
    if (Init_Tolerance == -99) Tolerance <- -99
    if (Init_Std == -99) Std <- -99
    if (Init_Punishment == -99) Punishment <- -99
    if (Init_PrefParms[1] == -99) PrefParms <- c(-99, -99)
    if (Init_ResponseParms[1] == -99) ResponseParms <- c(-99, -99)
    if (Init_ReplacementPrefParms[1] == -99) ReplacmentPrefParms <- c(-99, -99)
    if (Init_BurObsParms[1] == -99) BurObsParms <- c(-99,-99)

  }

  if (tallperformance)
    BigList <- list("Version" = "2.0a2",
                    "NumBurs" = NumBurs,
                    "NumSaboteurs" = NumSaboteurs,
                    "MaxIter" = MaxIter,
                    "Replications" = Replications,
                    "supervision" = supervision,
                    "posprefs" = posprefs,
                    "omniscient" = omniscient,
                    "ExecSummary" = as.data.frame(ExecSummary),
                    "Performance" = Performance,
                    "TallPerformance" = TallPerformance,
                    "ObstyRecord" = ObstyRecord,
                    "SeenRecord" = SeenRecord,
                    "Saboteurs" = Saboteurs)
  if (!tallperformance)
    BigList <- list("Version" = "2.0a2",
                    "NumBurs" = NumBurs,
                    "NumSaboteurs" = NumSaboteurs,
                    "MaxIter" = MaxIter,
                    "Replications" = Replications,
                    "supervision" = supervision,
                    "posprefs" = posprefs,
                    "omniscient" = omniscient,
                    "ExecSummary" = as.data.frame(ExecSummary),
                    "Performance" = Performance,
                    "ObstyRecord" = ObstyRecord,
                    "SeenRecord" = SeenRecord,
                    "Saboteurs" = Saboteurs)
  BigList
}

