# R Script to implement the Imitation model from WSS
# - Aim is to use base R functions and avoid APL/C/Java code that
#   was probably inaccessible to many people
# - Thought was also that APL is in many ways a precursor to R, so
#   that the migration should not be hard

require("igraph")
#require("xts") # only for the first()/last() functions for the old_analyze take and drop

# Version 1.0: reproduced the functionality of the APL code in the appendix
#   to WSS
# Version 1.1: goals for this version are to
#   x Change "Results" to "ExecSummary" 
#   x and return as an object;
#     e.g., wss$ExecSummary will be the equivalent to v1.0
#   x Record Performance of each bureaucrat over each iteration
#   x Output Performance as a separate object
#   x Record Obsty for each iteration in ObstyRecord
#   x Output ObstyRecord as object
#   x Record Seen for each iteration as SeenRecord
#   x Output SeenRecord as object
#   x Directly implement variations from WSS for punishment by (x) standard,
#     (x) positive predispositions only, and (x) omniscience
#   ! Create entry point for utility modification (FOR THE NEXT VERSION)
#   ! Debug the old_analyze code (GAVE UP)
#   x Allow to intervene with settings for parameters
#   x Changed code to refer to Replications instead of Trials 
#     Replications is a more commonly used term in computational models, as in
#     the number of times a particular experiment is replicated.
#     So Replications are the number of times that we try a simulation, and
#     a simulation runs over so many iterations.
#   x Renamed the main function to "ImitSim" (was "BigLoop1a", which is very
#     nondescriptive)

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

ImitSim <- function(Replications, binary=TRUE, NumBurs=10, MaxIter=10, 
                      supervision="Relative", posprefs=FALSE, omniscient=FALSE,
                      SupObsParms=NULL, Tolerance=NULL, Std=NULL, Punishment=NULL,
                      ResponseParms=NULL, PrefParms=NULL, BurObsParms=NULL) {
  # This routine randomly draws the assorted parameters
  # and stores the final mean supoutc as a dependent var
  # Version 1a calls policy 1a (relative punishment) based on punrate
  
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
  ObstyRecord <- array(NA, dim=c(0, 1+NumBurs))
  SeenRecord <- array(NA, dim=c(0, 2+NumBurs))
  
  # Loop starts here ("go")
  for (repl_ct in 1:Replications) {
    if (!is.null("ResponseParms")) {
      ResponseParms <- runif(2)
      ResponseParms[1] <- -1+2*ResponseParms[1]
    }
    
    # draw SupObsParms randomly for policy 1a/1b/2a/2b
    if (!is.null("SupObsParms")) SupObsParms <- runif(2)
    
    # uncomment for policy 1c/2c
    if (!is.null("BurObsParms")) BurObsParms <- runif(2)
    
    # observability matrix (for bureaucrats) dist'd unif(0,1)
    # symmetric (x sees y sees x equally), where each entry is
    # probability of seeing
    # (obsty has to be global for the connectivity check)
    Obsty <- array(runif(NumBurs^2),dim=c(NumBurs,NumBurs))               ## seems like numburs must be global??
    
    if (binary) Obsty <- trunc(.5+Obsty)
    diag(Obsty) <- 1
    
    ObstyRecord <- rbind(ObstyRecord,cbind(rep(repl_ct,NumBurs),Obsty))
    
    # Add to ObstyRecord
    #ObstyRecord[(repl_ct-1):(repl_ct-1)+NumBurs, ] <- Obsty
    
    # supervisor observability (see and be seen, here) dist'd unif(0,1)   ## I say unif in the code, but seems norm??
    SupObsty <- rnorm(NumBurs, mean=SupObsParms[1], sd=SupObsParms[2])
    SupObsty[SupObsty < 0] <- 0
    SupObsty[SupObsty > 1] <- 1
        
    # Set the range of Preferences (why did I call it popparms??)        ## NOT IN ORIGINAL CODE
    if (posprefs == FALSE & !is.null("PrefParms")) PrefParms <- c(runif(1, min=-1, max=1), runif(1))
    else if (posprefs == TRUE & !is.null("PrefParms")) PrefParms <- c(runif(1, min=0, max=1), runif(1))
    
    # "Relative" is for 1a/2a; "Fixed" is for 1b/1c/2b/2c
    if (supervision == "Relative" & !is.null("Tolerance")) Tolerance <- runif(1, min=-2, max=0)
    else if (supervision == "Fixed" & !is.null("Std")) Std <- runif(1, min=-1, max=1)

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

    # play: go from here
    for (iter in 1:MaxIter) {

        # Do ranges from -1 (complete defection) to 1 (complete compliance)
        Do <- Response
        
        # Record Performance
        Performance[(repl_ct-1)*MaxIter+iter, 1:2] <- c(repl_ct, iter)
        Performance[(repl_ct-1)*MaxIter+iter, 3:(NumBurs+2)] <- Response

        # outcome equals desires times do
        BurUtil[iter,] <- Prefs*Do
        # supervisor enforces against those she sees defecting
        SupUtil[iter,] <- c(mean(Do), sd(Do))
        
        Seen <- trunc(SupObsty+runif(1))
        SeenRecord <- rbind(SeenRecord, cbind(repl_ct, iter, t(Seen)))
        
        if (supervision == "Relative") deviants <- (Do-SupUtil[iter,1]) < (Tolerance*SupUtil[iter,2])
        else if (supervision == "Fixed") deviants <- (Do-SupUtil[iter,1]) < Std

        # still have to be deviant even if omniscient
        if (!omniscient) seendeviants <- deviants * Seen
        else seendeviants <- deviants
        
        WhoCaught <- which(as.logical(seendeviants))
 
        if(length(WhoCaught)!=0) {
          Catch <- Catch + 1
          BurUtil[iter,WhoCaught] <- BurUtil[iter,WhoCaught] - Punishment
        }
        
        # adapt:
        BurUtilSeen <- array(data=BurUtil[iter,], dim=c(NumBurs,NumBurs)) %*% Obsty        
        
        # choose response of the person you saw who did best
        Envy <- max.col(t(BurUtilSeen))
        Response <- Do[Envy]

        # play again, unless this iteration exceeds maxiter
      }
    
    ExecSummary[repl_ct, 1:10] <- c(SupUtil[MaxIter,], PrefParms, ResponseParms, SupObsParms, BurObsParms)
    # will need to change punrate in line below to std for 1b/1c/2b/2c
    if (supervision == "Relative")
      ExecSummary[repl_ct, 11:15] <- c(Tolerance, Punishment, Connectivity, mean(Response), sd(Response))
    else if (supervision == "Fixed") 
      ExecSummary[repl_ct, 11:15] <- c(Std, Punishment, Connectivity, mean(Response), sd(Response))
    ExecSummary[repl_ct, 16:17] <- c(mean(BurUtil[MaxIter,]), sd(BurUtil[MaxIter,]))
    
  }
  
  BigList <- list("Version" = "1.1b1",
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