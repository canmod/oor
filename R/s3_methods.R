# S3 Methods -----------------------
# these are like python-style __dunder__ methods
# such as __str__, __eq__

#' @export
print.Base = function(x, ...) {
  str(x)
  print(lsf.str(envir = x))
  invisible(x)
}

#' @export
`$.Base` = function(x, name) {
  ## make sure that you get an error if
  ## a name cannot be found in self
  get(name, envir = x)
}

## Base objects are not testable ...
validate_object.Base = function(object) {}
## ... but Testable objects obviously are
validate_object.Testable = function(object) {
  # S4-style validity checking
  v = try(object$valid())
  if (!isTRUE(v)) {
    msg = try(as.character(v))
    if (inherits(msg, "try-error")) {
      stop("object construction has failed, and a reason is unavailable")
    }
    stop(v)
  }
}
## Implementation and Interface objects get some special
## validity checking before the standard stuff that gets
## checked using the $valid method of the object.
validate_object.Implementation = function(object) {
  object$.stopifnot_valid_implementation()
  validate_object.Testable(object)  ## assess object using $valid method
}
validate_object.Interface = function(object) {
  object$.stopifnot_valid_interface()
  validate_object.Testable(object)  ## assess object using $valid method
}
