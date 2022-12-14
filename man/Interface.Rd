% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/base_classes.R
\name{Interface}
\alias{Interface}
\title{Interface Class}
\usage{
Interface()
}
\description{
Initialize an empty abstract class.
}
\details{
\link[=inheritance]{Inherit} from \code{Interface} to define the
argument signatures and return value types
of abstract methods.
}
\examples{
BinaryOperation = function() {
  self = Interface()
  self$operate = function(x = numeric(1L), y = numeric(1L)) return(numeric(1L))
  return_object(self, "BinaryOperation")
}

}
