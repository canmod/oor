% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/base_classes.R
\name{Base}
\alias{Base}
\title{Base Class}
\usage{
Base(starting_environment = emptyenv())
}
\arguments{
\item{starting_environment}{An environment to enclose the empty object.
This enclosing environment is useful for making things other than \code{self}
available to methods. I have found that it is usually best to ignore this
possibility, but it might indeed be useful from time-to-time.}
}
\description{
Initialize an empty object.
}
\details{
\link[=inheritance]{Inherit} from \code{Base} if you want to start
from an empty class
}
\examples{
empty_object = Base()
print(empty_object)
names(empty_object)

Printer = function(x) {
  self = Base()
  self$.x = x
  self$print = function() print(self$.x)
  return_object(self, "Printer")
}
printer = Printer("something to print")
printer$print()

SupportivePrinter = function(x) {
  self = Printer(x)
  self$print = function() {
    print(paste(sQuote(self$.x), "is a very nice thing to say"))
  }
  return_object(self, "Supportive")
}
supportive_printer = SupportivePrinter("something to print")
supportive_printer$print()

}
