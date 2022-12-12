# Lightweight Tools for Object Oriented Programming

[![R-CMD-check](https://github.com/canmod/oor/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/canmod/oor/actions/workflows/R-CMD-check.yaml)

## Why?

Many object oriented programming frameworks exist for R, so why another? I wanted minimal magic, to develop a very solid understanding and use of basic R concepts (e.g. environments), and to have no dependencies on third-party packages. 


## Installation

To install the development version please use this.
```
remotes::install_github("canmod/oor")
```

To install the stable version please use this.
```
install.packages("oor", repos = "https://canmod.github.io/drat", type = "source")
```


## Roadmap

We intend for this to be a very stable and simple package. To get to this point, the roadmap consists of the following two items that we hope will never change.

1. Remove or simplify the complexities of traits and other fancy types of inheritance.
2. Test


## Hello world

```
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
```
