\name{vcdExtra-package}
\alias{vcdExtra-package}
\alias{vcdExtra}
\docType{package}
\title{
Extensions and additions to vcd: Visualizing Categorical Data
}
\description{
% \if{html}{\figure{man/figures/logo.png}{options: align='right' alt='logo' width='100'}}

This package provides additional data sets, documentation, and
a few functions designed to extend the \code{vcd} package for Visualizing Categorical Data
and the \code{gnm} package for Generalized Nonlinear Models.
In particular, vcdExtra extends mosaic, assoc and sieve plots from vcd to handle glm() and gnm() models and
adds a 3D version in \code{\link{mosaic3d}}.

This package is also a support package for the book, \emph{Discrete Data Analysis with R} by
Michael Friendly and David Meyer, Chapman & Hall/CRC, 2016,
\url{https://www.routledge.com/Discrete-Data-Analysis-with-R-Visualization-and-Modeling-Techniques-for-Categorical-and-Count-Data/Friendly-Meyer/p/book/9781498725835}
with a number of additional data sets,
and functions.
The web site for the book is \url{http://ddar.datavis.ca}.

In addition, I teach a course, \emph{Psy 6136: Categorical Data Analysis},
\url{https://friendly.github.io/psy6136/} using this package.
}

\details{
The main purpose of this package is to serve as a sandbox for
introducing extensions of
mosaic plots and related graphical methods
that apply to loglinear models fitted using
\code{glm()} and related, generalized nonlinear models fitted
with \code{gnm()} in the \code{\link[gnm]{gnm-package}} package.
A related purpose is to fill in some holes in the analysis of
categorical data in R, not provided in base R, the \pkg{vcd},
or other commonly used packages.

The method \code{\link{mosaic.glm}}
extends the \code{\link[vcd]{mosaic.loglm}} method in the \pkg{vcd}
package to this wider class of models.  This method also works for
the generalized nonlinear models fit with the \code{\link[gnm]{gnm-package}} package,
including models for square tables and models with multiplicative associations.

\code{\link{mosaic3d}}
introduces a 3D generalization of mosaic displays using the
\pkg{rgl} package.

In addition, there are several new data sets, a tutorial vignette,
\describe{
  \item{vcd-tutorial}{Working with categorical data with R and the vcd package, \code{vignette("vcd-tutorial", package = "vcdExtra") }}
}
and a few functions for manipulating categorical data sets and working with models for
categorical data.

A new class, \code{\link{glmlist}}, is introduced for working with
collections of \code{glm} objects, e.g., \code{\link{Kway}} for fitting
all K-way models from a basic marginal model, and \code{\link{LRstats}}
for brief statistical summaries of goodness-of-fit for a collection of
models.

For square tables with ordered factors, \code{\link{Crossings}} supplements the
specification of terms in model formulas using
\code{\link[gnm]{Symm}},
\code{\link[gnm]{Diag}},
\code{\link[gnm]{Topo}}, etc. in the \code{\link[gnm]{gnm-package}}.



Some of these extensions may be migrated into \pkg{vcd} or \pkg{gnm}.

A collection of demos is included to illustrate fitting and visualizing a wide variety of models:
\describe{
  \item{mental-glm}{Mental health data: mosaics for glm() and gnm() models}
  \item{occStatus}{Occupational status data: Compare mosaic using expected= to mosaic.glm}
  \item{ucb-glm}{UCBAdmissions data: Conditional independence via loglm() and glm()}
  \item{vision-quasi}{VisualAcuity data: Quasi- and Symmetry models}
  \item{yaish-unidiff}{Yaish data: Unidiff model for 3-way table}
  \item{Wong2-3}{Political views and support for women to work (U, R, C, R+C and RC(1) models)}
  \item{Wong3-1}{Political views, support for women to work and national welfare spending (3-way, marginal, and conditional independence models)}
  \item{housing}{Visualize glm(), multinom() and polr() models from \code{example(housing, package="MASS")}}
}
Use \code{ demo(package="vcdExtra")} for a complete current list.

The \pkg{vcdExtra} package now contains a large number of data sets illustrating various forms of categorical data analysis
and related visualizations, from simple to advanced. Use \code{data(package="vcdExtra")} for a
complete list, or \code{datasets(package="vcdExtra")} for an annotated one showing the \code{class} and
\code{dim} for each data set.
}
\author{
Michael Friendly

Maintainer: Michael Friendly <friendly AT yorku.ca> || (\href{https://orcid.org/0000-0002-3237-0941}{ORCID})
}
\references{
Friendly, M. \emph{Visualizing Categorical Data}, Cary NC: SAS Institute, 2000.
Web materials: \url{http://www.datavis.ca/books/vcd/}.

Friendly, M. and Meyer, D. (2016).
\emph{Discrete Data Analysis with R: Visualization and Modeling Techniques for Categorical and Count Data}.
Boca Raton, FL: Chapman & Hall/CRC. \url{http://ddar.datavis.ca}.

Meyer, D.; Zeileis, A. & Hornik, K.
The Strucplot Framework: Visualizing Multi-way Contingency Tables with vcd
\emph{Journal of Statistical Software}, 2006, \bold{17}, 1-48.
Available in R via \code{vignette("strucplot", package = "vcd")}

Turner, H. and Firth, D. \emph{Generalized nonlinear models in R: An overview of the gnm package},
2007,
\url{http://eprints.ncrm.ac.uk/472/}. Available in R via \code{vignette("gnmOverview", package = "gnm")}.

}
\keyword{ package }
\seealso{
\code{\link[gnm]{gnm-package}}, for an extended range of models for contingency tables

\code{\link[vcd]{mosaic}} for details on mosaic displays within the strucplot framework.
}
\examples{
example(mosaic.glm)

demo("mental-glm")
}
