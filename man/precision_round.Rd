% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/precision.R
\name{precision_round}
\alias{precision_round}
\alias{precision_round_}
\title{Suggested precision for rounded significant digit formats}
\source{
The \code{d3-format} function \href{https://github.com/d3/d3-format/blob/master/src/precisionRound.js}{precisionRound}
}
\usage{
precision_round(x)

precision_round_(step, xmax)
}
\arguments{
\item{x}{A numeric vector of values to be formatted.}

\item{step}{Numeric: Minimum absolute difference between values that will be formatted.}

\item{xmax}{Numeric: Maximum absolute value of the values to be formatted.}
}
\value{
An integer vector of suggest precisions. For \code{precision_fixed},
   this is length one, for \code{precision_fixed_} it is the same length
   as \code{step}.
}
\description{
The function \code{precision_round_} takes the maximum absolute distance
between values, (\code{step}), and the maximum absolute value, (\code{value}),
of the values to be formatted, and returns the suggested precision for
formats that round to significant digits (e.g. \code{"g"}).
\code{precision_round} calculates this precision given a numeric vector of
values to format, \code{x}.
}
\details{
The suggested precision, \eqn{p}, for values in a vector, \eqn{x}, is
\eqn{
  p = \max \left(0,\lfloor\log_{10}(|\max_{i} x|) \rfloor - \lfloor \log_{10} d \rfloor \right)
}{max(0, floor(log10(max(abs(x)) - d)) - floor(log10(d)))},
where \eqn{d} the maximum absolute distance between values of
\eqn{x}.

For the exponential format, \code{"e"}, substract use \code{p - 1} for the
precision.
}
\examples{
# For these, the step size is 0.01 and suggested precision is 3
x <- c(0.99, 1, 1.01)
p <- precision_round(x)
fmt(x, paste0(".", p, "r"))
# For these, the step size is 0.1 and suggested precision is 2
x2 <- c(0.9, 1.0, 1.1)
p2 <- precision_round(x2)
fmt(x2, paste0(".", p2, "r"))
# For the e format type subtract one
fmt(x, paste0(".", p - 1, "e"))
}
\seealso{
\code{\link{precision_fixed}} for the suggested precision to
  use with formats that use a fixed number of digits after the decimal point,
  and \code{\link{precision_prefix}} for a suggested SI prefix to use.
}
