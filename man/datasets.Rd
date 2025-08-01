\name{datasets}
\alias{datasets}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Information on Data Sets in Packages
}
\description{
The \code{\link[utils]{data}} function is used both to load data sets from packages,
and give a display of the names and titles of data sets in one or more packages,
however it does not return a result that can be easily used to get additional
information about the nature of data sets in packages.

The \code{datasets()} function is designed to produce a more useful summary
display of data sets in one or more packages.  It extracts the \code{class}
and dimension information (\code{dim} or \code{length}) of each item,
and formats these to provide additional descriptors.
}
\usage{
datasets(package,
        allClass=FALSE,
        incPackage=length(package) > 1,
        maxTitle=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{package}{a character vector giving the package(s) to look in}
  \item{allClass}{a logical variable. Include all classes of the item (\code{TRUE}) or just the last class
       (\code{FALSE})?}
  \item{incPackage}{include the package name in result?}
  \item{maxTitle}{maximum length of data set Title}
}

\details{
The requested packages must be installed, and are silently loaded in order
to extract \code{class} and size information.
}
\value{
A \code{data.frame} whose rows correspond to data sets found in \code{package}.

The columns (for a single package) are:
  \item{Item}{data set name, a character variable}
  \item{class}{class, the object class of the data set, typically one of \code{"data.frame"}, \code{"table"}, \code{"array"} ...}
  \item{dim}{an abbreviation of the dimensions of the data set, in a form like \code{"36x3"} for a data.frame or matrix with 36 rows and 3 columns.}
  \item{Title}{data set title}
}
%\references{
%%% ~put references to the literature/web site here ~
%}
\author{
Michael Friendly, with R-help from Curt Seeliger
}
\note{
In Rmd documents, `datasets("package") |> knitr::kable()` can be used to create
a more pleasing display.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[utils]{data}}, \code{\link[knitr]{kable}}
}
\examples{
datasets("vcdExtra")
# datasets(c("vcd", "vcdExtra"))
datasets("datasets", maxTitle=50)

# just list dataset names in a package
datasets("vcdExtra")[,"Item"]
datasets("vcd")[,"Item"]

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{package}
\keyword{data}% __ONLY ONE__ keyword per line
