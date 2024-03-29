\name{GKgamma}
\alias{GKgamma}
\alias{print.GKgamma}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculate Goodman-Kruskal Gamma for ordered tables}
\description{
The Goodman-Kruskal \eqn{\gamma}{gamma} statistic is a measure of
association for ordinal factors in a two-way table proposed by
Goodman and Kruskal (1954).
}
\usage{
GKgamma(x, level = 0.95)

%\method{print}{GKgamma}{x, digits = 3, ...}
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A two-way frequency table, in matrix or table form.  The rows and columns
  	are considered to be ordinal factors}
  \item{level}{Confidence level for a significance test of \eqn{\gamma \ne =}{gamma !=0}}
%  \item{digits}{Number of digits printed by the print method}
%  \item{...}{Other arguments}
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
  Returns an object of class \code{"GKgamma"} with 6 components, as follows
%  If it is a LIST, use
%\describe{
  \item{gamma}{The gamma statistic}
  \item{C}{Total number of concordant pairs in the table}
  \item{D}{Total number of discordant pairs in the table}
  \item{sigma}{Standard error of gamma }
  \item{CIlevel}{Confidence level}
  \item{CI}{Confidence interval}
%  }
}

\references{
Agresti, A. \emph{Categorical Data Analysis}. John Wiley & Sons, 2002,
pp. 57--59.

Goodman, L. A., & Kruskal, W. H. (1954). Measures of
association for cross classifications. \emph{Journal of the
American Statistical Association}, 49, 732-764.

Goodman, L. A., & Kruskal, W. H. (1963). Measures of
association for cross classifications III: Approximate
sampling theory. \emph{Journal of the American Statistical
Association}, 58, 310-364.

}

\author{Michael Friendly; original version by Laura Thompson}
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{\code{\link[vcd]{assocstats}}, \link[vcd]{Kappa}}

\examples{
data(JobSat)
GKgamma(JobSat)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{htest}
\keyword{category}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
