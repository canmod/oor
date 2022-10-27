#' Validity Messager
#'
#' Couple a test function with a failure message
#'
#' \code{ValidityMessager} objects have an \code{assert} method
#' with one argument, \code{x}. If the test function evaluates to
#' \code{\link{TRUE}} then the argument, \code{x}, is returned. If
#' it does not return \code{\link{TRUE}} then the failure message is given.
#'
#' @param test_function Object that is coercible to a \code{\link{function}},
#' typically a function that will return a length-1 \code{\link{logical}}
#' vector. Often this object will inherit from \code{\link{TestSteps}},
#' which provides a way to compose object tests.
#' @param ... Length-1 \code{\link{character}} vectors to display
#' if \code{test_function} does not return \code{TRUE}.
#'
#' @return Object of class \code{ValidityMessager} containing a \code{check}
#' method that will return \code{TRUE} or fail with \code{fail_message}.
#'
#' @examples
#' is_numeric = ValidityMessager(is.numeric, "not numeric")
#'
#' is_numeric$check("1")
#'
#' HoldANumber = function(x) {
#'   self = Base()
#'   self$x =  is_numeric$assert(x)
#'   return_object(self, "HoldANumber")
#' }
#' try(HoldANumber("a")) ## error message
#' HoldANumber(1) ## success
#'
#' @export
ValidityMessager = function(test_function, ...) {
  self = Base()
  self$.test_function = as.function(test_function)
  self$.fail_message = pasten(...)
  stopifnot(length(formals(args(self$.test_function))) == 1L)
  self$.error = function(test_result) inherits(test_result, "try-error")
  self$.test_error_message = function(test_result) {
    paste(""
      ,self$.fail_message
      ,"but validity could not be checked because:"
      ,as.character(test_result)
      ,sep = "\n"
    )
  }
  self$.test_inconclusive_message = function(test_result) {
    paste(""
      ,self$.fail_message
      ,"but test was inconclusive returning neither TRUE nor FALSE but rather:"
      ,toString(capture.output(test_result), getOption("width"))
      ,sep = "\n"
    )
  }
  self$assert = function(x) {
    result = try(self$.test_function(x), silent = TRUE)
    if (isTRUE(result)) return(x)
    if (isFALSE(result)) stop(self$.fail_message)
    if (self$.error(result)) stop(self$.test_error_message(result))
    stop(self$.test_inconclusive_message(result))
  }
  self$check = function(x) {
    result = try(self$.test_function(x), silent = TRUE)
    if (isTRUE(result)) return(TRUE)
    if (isFALSE(result)) stop(self$.fail_message)
    if (self$.error(result)) stop(self$.test_error_message(result))
    stop(self$.test_inconclusive_message(result))
  }
  self$is_true = function(x) {
    return(isTRUE(try(self$.test_function(x), silent = TRUE)))
  }
  return_object(self, "ValidityMessager")
}


#
# is_matrix = All(
#  is.numeric,
#  TestPipeline(
#    Summarizer(dim, length),
#    TestRange(0, 2)
#  )
# )
# xx = ValidityTestMessager(is_matrix, "object is not a matrix")
# xx$valid(array(1, c(1, 2, 3)))
#
# G = function() {
#   self = inherit_from(Testable, ValidityTestMessager)
# }
#
#
# # Interface Class for handing validity results
# #
# # @return object with a check(x) function, which returns TRUE
# # if x is valid or throws an error with the text in fail_message
# ValidityMessagerInterface = function(
#     fail_message = "this validity check is not implemented"
#   ) {
#   self = Interface()
#   self$.fail_message = fail_message
#   self$check = function(x = Base()) stop(self$.fail_message)
#   return_object(self, "ValidityMessagerInterface")
# }
#
# # Implements ValidityMessagerInterface for single-argument test functions that
# # return TRUE, FALSE, anything else (inconclusive), or fail entirely
# ValidityTestMessager = function(test_function, fail_message) {
#   self = implements(ValidityMessagerInterface)
#   self$.test_function = as.function(test_function)
#   stopifnot(length(formals(args(self$.test_function))) == 1L)
#   self$.error = function(test_result) inherits(test_result, "try-error")
#   self$.test_error_message = function(test_result) {
#     paste(""
#       ,self$.fail_message
#       ,"but validity could not be checked because:"
#       ,as.character(test_result)
#       ,sep = "\n"
#     )
#   }
#   self$.test_inconclusive_message = function(test_result) {
#     paste(""
#       ,self$.fail_message
#       ,"but test was inconclusive returning neither TRUE nor FALSE but rather:"
#       ,toString(capture.output(test_result), getOption("width"))
#       ,sep = "\n"
#     )
#   }
#   self$check = function(x = Base()) {
#     result = try(self$.test_function(x), silent = TRUE)
#     if (isTRUE(result)) return(TRUE)
#     if (isFALSE(result)) return(self$.fail_message)
#     if (self$.error(result)) stop(self$.test_error_message(result))
#     stop(self$.test_inconclusive_message(result))
#   }
#   return_object(self, "ValidityTestMessager")
# }
#
# is_numeric = ValidityTestMessager(is.numeric, "not numeric")
# is_numeric
# is_numeric$check(2)
# is_numeric$check("a")
# is_numeric_maybe = ValidityTestMessager(function(x) {
#   if (is.integer(x)) return(rnorm(1000))
#   if (is.call(x)) stop("not even close")
#   is.numeric(x)
# }, "not numeric")
# is_numeric_maybe$valid(1)
# is_numeric_maybe$valid("a")
# is_numeric_maybe$valid(1L)
# is_numeric_maybe$valid(a ~ b)
#
# as.function.BaseValid = function(x, ...) x$.test_function
#
#
#
#
# # Trait for forwarding the `valid` method
# #
# # Traits are just regular expressions that assume that they will be
# # evaluated in a class definition
# #
# # @param test_function function that could possibly return TRUE,
# # indicating validity
# # @param fail_message length-one character vector giving the message to
# # use to communicate lack of validity to the user
#
# return_valid_object = function(self, class, test_function, fail_message) {
#   self$valid = function(x) self$.validity$valid(x) # method forwarding
#   self$.validity = ValidityTestMessager(test_function, fail_message)
#   clean_object(parent.frame())
#   structure(self, class = c(class, class(self)))
# }
#
# # Implements simple validity checking
# Valid = function(test_function, fail_message) {
#   self = Base()
#   return_valid_object(self, "Valid", test_function, fail_message)
# }
#
# is_numeric = Valid(is.numeric, "do not know")
# is_numeric$valid(1)
#
# SummaryValid = function(basic_tester, summary_function_list, fail_message) {
#   self = Base()
#   test_function = TestPipeline(list(
#     Summarizer(summary_function_list),
#     TestBasic(basic_tester)
#   ))
#   return_valid_object(self, "Valid", test_function, fail_message)
# }
#
# xx = SummaryValid(TestRange(0, 2), list(dim, length), "wrong number of dimensions")
# xx$valid(0)
# xx$valid(array(0, c(1, 2, 3)))
#
# EachValid = function(basic_tester, fail_message) {
#   self = Base()
#   test_function = TestEach(basic_tester)
#   return_valid_object(self, "Valid", test_function, fail_message)
# }
#
# xx = EachValid(is.numeric, "not each numeric")
# xx$valid(list(0, 2, "a"))
#
# MultiValid = function(tester_list, boolean_aggregator, fail_message) {
#   self = Base()
#   test_function = MultiTest(tester_list, boolean_aggregator)
#   return_valid_object(self, "Valid", test_function, fail_message)
# }
#
# xx = MultiValid(list(is.numeric, is.character), any, "not simple")
# xx$valid(new.env())
#
# EachSummaryValid = function(basic_tester, summary_function_list, fail_message) {
#   self = Base()
#   test_function = TestPipeline(list(
#     MappedSummarizer(summary_function_list),
#     TestEach(basic_tester)
#   ))
#   return_valid_object(self, "Valid", test_function, fail_message)
# }
#
# HomoValid = function(summary_function_list, fail_message) {
#   self = Base()
#   test_function = TestPipeline(list(
#     MappedSummarizer(summary_function_list),
#     TestHomo()
#   ))
#   return_valid_object(self, "Valid", test_function, fail_message)
# }
#
#
# xx = list(
#   matrix(0, 2, 3),
#   0,
#   1:10,
#   #"french"
#  array(0, c(5, 4, 3))
# )
# ff = MultiValid(
#   list(
#     TestPipeline(list(
#       MappedSummarizer(list(dim, length)),
#       TestRange(0, 2)
#     )),
#     TestEach(is.numeric)
#   ),
#   all, "not all valid numeric matrices"
# )
# ff$valid(xx)
# TestEach(is.numeric)$apply(xx)
