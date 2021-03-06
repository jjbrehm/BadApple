\name{ImitSim}
\alias{ImitSim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Simulation of Imitation by Bureaucrrats
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
ImitSim() runs an R version of the imitation simulation as published in \emph{Working, Shirking, and Sabotage} (1997). The original function was written in APL11, which hardly anyone uses. This is the core function for the simulation in the book.
}

\usage{
ImitSim(Replications, NumBurs=10, MaxIter=10, supervision="Relative", posprefs=FALSE,
omniscient=FALSE, Tolerance=-99, Std=-99, Punishment=-99, SupObsParms=c(-99,-99),
PrefParms=c(-99,-99), ResponseParms=c(-99,-99), BurObsParms=c(-99,-99),
quiet=FALSE, debug=FALSE}
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Replications}{Number (integer) of replications of each run.}
  \item{NumBurs}{Number (integer) of Bureaucrats.}
  \item{MaxIter}{Number (integer) of iterations for each replication to run.}
  \item{supervision}{"Relative" (default) for supervisor to identify deviants based on low-end deviations from responses by other bureaucrats, "Fixed" for the supervisor to identify deviants based on a floor of minimal performance.}
  \item{posprefs}{Logical, FALSE (default) if the preferences of the bureaucrats are not constrained to be positive.
    TRUE if the preferences of the bureacrats must be positive.}
  \item{omniscient}{Logical, FALSE (default) if the supervisor cannot see everyone, TRUE if she can.}
  \item{Tolerance}{Number (float) of standard deviations away from mean that the supervisor will regard as being
    deviant. Only makes sense if the "supervision is "Relative".}
  \item{Std}{Number (float) in range (-1,1) for supervisor to regard as the floor for performance. Only makes sense if
    the supervision is "Fixed".}
  \item{Punishment}{Number (float) subtracted from each deviant bureaucrat.}
  \item{SupObsParms}{Vector (mean,sd) for the observability of the supervisor. Can be redunddant with omniscient, which sets all bureaucrats as visiible (i.e., equivalent to SupObsPamrs=c(1,0)}
  \item{PrefParms}{Vector (mean,sd) for the preferences of the bureaucrat.}
  \item{ResponseParms}{Vector (mean,sd) for the responses of the bureaucrats.}
  \item{BurObsParms}{Vector (mean,sd) for the observability of each bureaucrat to one another.}
  \item{debug}{If TRUE, prints debugging information.}
  \item{quiet}{If TRUE, prints no startup information (but will print debugging information, if requested).}
%%     ~~Describe \code{x} here~~
}
\details{
This might be the place for text about the way the function will work.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
\item{ExecSummary }{Returns a (tidy-ready) dataframe that contains:  \verb{Replication, Iteration, SupUtilMean, SupUtilSD, PrefMean, PrefSD, ResponseMean, ResponseSD, SupObsMean, SupObsSD, BurObsMean, BurObsSD, Tolerance (or Std),} \verb{Punishment, Connectivity, FinalResponseMean, FinalResponseSD, BurUtilMean, BurUtilSD}}
\item{version }{2.02}
\item{NumBurs }{Number of bureaucrats (default=10)}
\item{MaxIter }{Number of iterations per replication (default=10)}
\item{supervision }{The mode of supervision ("Relative", "Fixed")}
\item{posprefs }{Are the preferences constrained to be positive?}
\item{omniscient }{Is the supervisor omniscient? (I.e., can see performance of all her subordinates?)}
\item{Performance }{Matrix of performance of each bureaucrat in each iteration of every replication, (Replications*MaxIter*Numbers) x (replication, iteration, (vector of responses)))}
\item{ObstyRecord }{Matrix of observability for each replication, (MaxIter*Numburs) x Numburss)}
\item{SeenRecord }{Matrix of observability of each bureaucrat by supervisor in each iteration of every replication, (Replication*MaxIter*NumBurs) x (replication, iteration, (vector of seen bureaucrats))}
}
\references{
%% ~put references to the literature/web site here ~
Brehm, John and Scott Gates. 1997. Working, Shirking, and Sabotage. Ann Arbor, MI
}
\author{
%%  ~~who you are~~
John Brehm
}
\note{
%%  ~~further notes~~
Makes use of the igraph library. You should have it installed.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
# To replicate Table 3.1 with relative supervision, allowing for preferences to be (-1,1)
wss <- ImitSim(1000)
hist(wss$ExecSummary$SupUtilMean)
}

