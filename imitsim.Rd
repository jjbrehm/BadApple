\name{ImitSim}
\alias{ImitSim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
what exactly is the title?
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
(I think this is the place).
ImitSim() runs an R version of the imitation simulation as published in Working, Shirking, and Sabotage (1997). The original function was written in APL11, which hardly anyone uses. This is the core function for the simulation in the book.
}
\usage{
ImitSim(replications)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{NumBur}{
  Number of Bureaucrats
%%     ~~Describe \code{x} here~~
}
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
\item{ExecSummary}{Returns a (tidy-ready) dataframe that contains: Replication, Iteration, ...}
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

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x) 
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
