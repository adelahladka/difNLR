% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/difORD.R
\name{logLik.difORD}
\alias{logLik.difORD}
\alias{AIC.difORD}
\alias{BIC.difORD}
\title{Log-likelihood and information criteria for an object of
\code{"difORD"} class.}
\usage{
\method{logLik}{difORD}(object, item = "all", ...)

\method{AIC}{difORD}(object, item = "all", ...)

\method{BIC}{difORD}(object, item = "all", ...)
}
\arguments{
\item{object}{an object of \code{"difORD"} class.}

\item{item}{numeric or character: either character \code{"all"} to
apply for all converged items (default), or a vector of item
names (column names of \code{Data}), or item identifiers
(integers specifying the column number).}

\item{...}{other generic parameters for S3 methods.}
}
\description{
S3 methods for extracting log-likelihood, Akaike's
  information criterion (AIC) and Schwarz's Bayesian criterion
  (BIC) for an object of \code{"difORD"} class.
}
\examples{
\dontrun{
# loading data
data(Anxiety, package = "ShinyItemAnalysis")
Data <- Anxiety[, paste0("R", 1:29)] # items
group <- Anxiety[, "gender"] # group membership variable

# testing both DIF effects with adjacent category logit model
(x <- difORD(Data, group, focal.name = 1, model = "adjacent"))

# AIC, BIC, log-likelihood
AIC(x)
BIC(x)
logLik(x)

# AIC, BIC, log-likelihood for the first item
AIC(x, item = 1)
BIC(x, item = 1)
logLik(x, item = 1)
}
}
\seealso{
\code{\link[difNLR]{difORD}} for DIF detection among
ordinal data. \cr \code{\link[stats]{logLik}} for generic function
extracting log-likelihood. \cr \code{\link[stats]{AIC}} for generic
function calculating AIC and BIC.
}
\author{
Adela Hladka (nee Drabinova) \cr
Institute of Computer Science of the Czech Academy of Sciences \cr
Faculty of Mathematics and Physics, Charles University \cr
\email{hladka@cs.cas.cz} \cr

Patricia Martinkova \cr
Institute of Computer Science of the Czech Academy of Sciences \cr
\email{martinkova@cs.cas.cz} \cr
}
