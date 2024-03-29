% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ddfMLR.R
\name{logLik.ddfMLR}
\alias{logLik.ddfMLR}
\alias{AIC.ddfMLR}
\alias{BIC.ddfMLR}
\title{Log-likelihood and information criteria for an object of \code{"ddfMLR"} class.}
\usage{
\method{logLik}{ddfMLR}(object, item = "all", ...)

\method{AIC}{ddfMLR}(object, item = "all", ...)

\method{BIC}{ddfMLR}(object, item = "all", ...)
}
\arguments{
\item{object}{an object of \code{"ddfMLR"} class.}

\item{item}{numeric or character: either character \code{"all"} to
apply for all converged items (default), or a vector of item
names (column names of \code{Data}), or item identifiers
(integers specifying the column number).}

\item{...}{other generic parameters for S3 methods.}
}
\description{
S3 methods for extracting log-likelihood, Akaike's
  information criterion (AIC) and Schwarz's Bayesian criterion
  (BIC) for an object of \code{"ddfMLR"} class.
}
\examples{
\dontrun{
# loading data
data(GMATtest, GMATkey)
Data <- GMATtest[, 1:20] # items
group <- GMATtest[, "group"] # group membership variable
key <- GMATkey # correct answers

# testing both DDF effects
(x <- ddfMLR(Data, group, focal.name = 1, key))

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
\code{\link[difNLR]{ddfMLR}} for DDF detection among nominal data. \cr
\code{\link[stats]{logLik}} for generic function extracting log-likelihood. \cr
\code{\link[stats]{AIC}} for generic function calculating AIC and BIC.
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
