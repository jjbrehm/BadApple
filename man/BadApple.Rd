\name{BadApple}
\alias{BadApple}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Simulation of Imitation by Bureaucrrats who are very poor agents
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Builds off of the R version of ImitSim, with the eventual intention to allow for some fraction of the bureaucrats to be poor performers. These poor performers can be specified by preferences, responses, observability (to one another or to supervisor), and by sanctioning. There is a very, very long way to go.
}

\usage{
BadApple(replications, NumSaboteurs, NumBur, MaxIter, supervision, omniscient, posprefs, binary, Tolerance, Std,
Punishment, SupObsParms, PrefParms, RespParms, BurObsParms, Memory, Dismissal)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{replications}{Number (integer) of replications of each run.}
  \item{NumSaboteurs}{Number (integer) of poor performer (NumSaboteurs < NumBur), default=3.}
  \item{NumBur}{Number (integer) of Bureaucrats.}
  \item{MaxIter}{Number (integer) of iterations for each replication to run.}
  \item{supervision}{"Relative" (default) for supervisor to identify deviants based on low-end deviations from responses by other bureaucrats, "Fixed" for the supervisor to identify deviants based on a floor of minimal performance.}
  \item{omniscient}{Logical, FALSE (default) if the supervisor cannot see everyone, TRUE if she can.}
  \item{Memory}{Logical, FALSE (default) if the bureaucrat does not adapt behavior conditional on improvement from previous iteration}
  \item{Dismissal}{Number (float) that sets when the bureaucrat is dismissed, and replaced with a new bureaucrat (from what pool?)}
  \item{posprefs}{Logical, FALSE (default) if the preferences of the bureaucrats are not constrained to be positive.
    TRUE if the preferences of the bureacrats must be positive.}
  \item{binary}{TRUE (default) if the observability matrix is converted to binary for the connectivvity calculations.
    Throws an error otherwise.}
  \item{Tolerance}{Number (float) of standard deviations away from mean that the supervisor will regard as being
    deviant. Only makes sense if the "supervision is "Relative".}
  \item{Std}{Number (float) in range (-1,1) for supervisor to regard as the floor for performance. Only makes sense if
    the supervision is "Fixed".}
  \item{Punishment}{Number (float) subtracted from each deviant bureaucrat.}
  \item{SupObsParms}{Vector (mean,sd) for the observability of the bureaucrats by the supervisor. Can be redunddant with omniscient, which sets all bureaucrats as visiible (i.e., equivalent to SupObsPamrs=c(1,0)}
  \item{SabSupObsParms}{Vector (mean, sd) for the observability of the saboteurs by the supervisor.}
  \item{PrefParms}{Vector (mean,sd) for the preferences of the bureaucrat.}
  \item{SabPrefParms}{Vector (mean, sd) for the preferences of the saboteurs}
  \item{RespParms}{Vector (mean,sd) for the responses of the bureaucrats.}
  \item{SabRespParms}{Vector (mean,sd) for the responses of the saboteurs.}
  \item{BurObsParms}{Vector (mean,sd) for the observability of each bureaucrat to one another.}
  \item{SabBurObsParms}{Vector (mean,sd) for the observability of the saboteurs (of everyone. Used to set observability of the saboteurs of the group and create isolation of the netwwork.)}
  \item{ReplacementPrefParms}{Vector (mean, sd) for the preferences of the replacements.}
  \item{ReplacementResponseParms}{Vector (mean, sd) for the responses of the replacements.}
  \item{debug}{If TRUE, prints debugging information. Default is FALSE.}
  \item{quiet}{If TRUE, prints no startup information (but will print debugging information, if requested). Default is FALSE.}
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
\item{version }{1.1b3}
\item{NumBurs }{Number of bureaucrats (default=10)}
\item{NumSaboteurs}{Number of saboteurs. No default, and required.}
\item{MaxIter }{Number of iterations per replication (default=10)}
\item{binary }{Whether the connectivity of the observability is computed using a binary adjacency matrix.}
\item{supervision }{The mode of supervision ("Relative", "Fixed")}
\item{posprefs }{Are the preferences constrained to be positive?}
\item{omniscient }{Is the supervisor omniscient? (I.e., can see performance of all her subordinates?)}
\item{Memory }{Does the bureaucrat retain memory of previous behavior and success? (FALSE (default), TRUE)}
\item{Dismissal }{What is the threshold of behavior that leads to a dismissal of the bureaucrat?}
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
Makes use of the igraph library. You might need to have it installed.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
foo <- BadApple(100, NumSaboteurs=3)
}

