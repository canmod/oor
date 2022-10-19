#' Abstract Class Testing Objects
#'
#' @return Object with an \code{apply} method that takes a single
#' argument, \code{x}, and returns a length-one \code{\link{logical}} vector.
#'
#' @export
TestSteps = function() {
  self = Base()
  self$apply = function(x) logical(1L)
  return_object(self, "TestSteps")
}

# Dunder methods to make sure that functions, Valid, and Test objects
# can all be used interchangably as test_functions

#' @rdname TestSteps
#' @export
as.function.TestSteps = function(x, ...) x$apply

#' Basic Test
#'
#' @param basic_tester An object that can be converted to a function
#'
#' @return Object of class \code{\link{TestSteps}} that evaluates
#' the \code{basic_tester}.
#' @export
TestBasic = function(basic_tester) {
  self = TestSteps()
  self$.basic_tester = as.function(basic_tester)
  self$apply = function(x) {
    self$.basic_tester(x)
  }
  return_object(self, "TestBasic")
}

#' Not
#'
#' @param basic_tester An object that can be converted to a function
#'
#' @return Object of class \code{\link{TestSteps}} that evaluates the
#' complement of \code{basic_tester}.
#'
#' @examples
#' Not(is.numeric)$apply(1) # FALSE
#' Not(is.numeric)$apply("1") # TRUE
#'
#' @export
Not = function(basic_tester) {
  self = TestBasic(basic_tester)
  self$apply = function(x) {
    !isTRUE(self$.basic_tester(x))
  }
  return_object(self, "Not")
}

#' Test Pipeline
#'
#' @param ... Objects of class \code{\link{TestSteps}} or \code{\link{function}}.
#' The final object in this list must be either a \code{TestSteps} object
#' with an \code{apply} method that returns a length-one logical vector
#' or a function that does so.
#' @return Object inheriting from \code{\link{TestSteps}}
#'
#' @examples
#' is_matrix = TestPipeline(
#'   Summarizer(dim, length),
#'   TestRange(0, 2)
#' )
#' is_matrix$apply(array("a", c(1))) # TRUE
#' is_matrix$apply(array(1, c(1, 2, 3))) # FALSE
#'
#' each_is_matrix = TestPipeline(
#'   MappedSummarizer(dim, length),
#'   All(TestRange(0, 2))
#' )
#' each_is_matrix$apply(list(1, matrix(1, 2, 3), "a")) # TRUE
#' each_is_matrix$apply(list(1, array(1, c(2, 3, 4), "a"))) # FALSE
#'
#' @export
TestPipeline = function(...) {
  self = TestSteps()
  self$.stages = list(...)
  self$apply = function(x) {
    for (stage in self$.stages) {
      x = stage$apply(x)
    }
    if (!(is.logical(x) & length(x) == 1L)) {
      stop("object testing pipeline failed to return boolean")
    }
    return(x)
  }
  return_object(self, "TestPipeline")
}

#' Summarizer
#'
#' Summarize an object to be tested, so that the test is applied to the
#' summary and not the object itself (e.g. \code{length(dim(object)) == 2L}).
#' \code{Summarizer}s are typically included in \code{\link{TestPipeline}}s.
#'
#' @param ... A list of summarizing functions.
#'
#' @return Object of class \code{\link{TestSteps}} that summarizes objects
#' to test.
#' @export
Summarizer = function(...) {
  self = TestSteps()
  self$.summary_function_list = lapply(list(...), as.function)
  self$apply = function(x) {
    for (f in self$.summary_function_list) x = f(x)
    return(x)
  }
  return_object(self, "Summarizer")
}

#' Mapped Summarizer
#'
#' Apply a \code{\link{Summarizer}} to each element of a list, in order
#' to test that a particular summary of each lists item meets a certain
#' criterion. \code{MappedSummarizer}s are typically included in
#' \code{\link{TestPipeline}}s.
#'
#' @param ... A list of summarizing functions.
#'
#' @return Object of class \code{\link{TestSteps}} that summarizes each
#' element of objects to test.
#' @export
MappedSummarizer = function(...) {
  self = Summarizer(...)
  self$apply = function(x) {
    for (f in self$.summary_function_list) x = lapply(x, f)
    return(x)
  }
  return_object(self, "MappedSummarizer")
}

#' Multi Test
#'
#' Assess several criteria.
#'
#' @param test_function_list List of objects of class \code{\link{TestSteps}}
#' or \code{\link{function}}.
#' @param boolean_aggregator A function that summarizes a \code{\link{logical}}
#' vector.
#'
#' @return Object of class \code{\link{TestSteps}} that tests several
#' criteria at the same time.
#'
#' @examples
#' is_matrix = All(
#'  is.numeric,
#'  TestPipeline(
#'    Summarizer(dim, length),
#'    TestRange(0, 2)
#'  )
#' )
#' is_matrix$apply(array("a", c(1))) # FALSE
#' is_matrix$apply(array("a", c(1, 1, 2))) # FALSE
#' is_matrix$apply(array(1, c(1, 1, 2))) # FALSE
#' is_matrix$apply(array(1, c(1, 2))) # TRUE
#' is_matrix$apply(1) # TRUE
#'
#' @export
MultiTest = function(test_function_list, boolean_aggregator) {
  self = TestSteps()
  self$.test_function_list = lapply(test_function_list, as.function)
  self$.boolean_aggregator = as.function(boolean_aggregator)
  self$apply = function(x) {
    test_results = lapply(self$.test_function_list, do.call, list(x))
    self$.boolean_aggregator(vapply(test_results, isTRUE, logical(1L)))
  }
  return_object(self, "MultiTest")
}

#' @describeIn MultiTest Test that all of the criteria are met.
#' @export
All = function(...) MultiTest(list(...), all)

#' @describeIn MultiTest Test that any of the criteria are met.
#' @export
Any = function(...) MultiTest(list(...), any)

#' Test for Homogeneity
#'
#' Test that all elements in an object are identical.
#'
#' @return Object of class \code{\link{TestSteps}} that tests that all
#' elements in an object are identical.
TestHomo = function() {
  self = TestSteps()
  self$apply = function(x) {
    length(unique(x)) == 1L
  }
  return_object(self, "TestHomo")
}

#' Range Test
#'
#' Test that all elements in an object greater than or equal to \code{lower}
#' and less than or equal to \code{upper}.
#'
#' @param lower Lower bound
#' @param upper Upper bound
#'
#' @return Object of class \code{\link{TestSteps}} that tests that all
#' elements in an object numerically on a particular range.
#' @export
TestRange = function(lower, upper) {
  self = TestSteps()
  self$.lower = lower
  self$.upper = upper
  self$apply = function(x) {
    all(x >= self$.lower) & all(x <= self$.upper)
  }
  return_object(self, "TestRange")
}

#' Subset Test
#'
#' Test that all elements in an object are in \code{set}
#'
#' @param set Universe of possibilities.
#'
#' @return Object of class \code{\link{TestSteps}} that tests that all
#' elements in an object are in a particular set.
#' @export
TestSubset = function(set) {
  self = TestSteps()
  self$.set = set
  self$apply = function(x) {
    all(x %in% self$.set)
  }
  return_object(self, "TestSubset")
}
