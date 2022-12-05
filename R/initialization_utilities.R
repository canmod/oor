#' Clean Method Environment
#'
#' Clean the environment of a method (or methods) so that
#' they contain a single object -- self -- which is the
#' environment defining an object
#'
#' @param e Environment containing methods in an object
#' @return There is no return value. The function is called for
#' its side-effect of cleaning a method environment.
#' @export
clean_method_environment = function(e) {
  # remove everything from the initializing
  # environment except self. this is a convenience
  # so that you don't need to worry about cleaning
  # up intermediate results from the class definition
  if (!is.environment(e)) stop("e must be an environment")
  if (identical(e, globalenv())) stop("e cannot be the global environment")
  if (environmentIsLocked(e)) stop("e is locked")
  if (!"self" %in% ls(e)) stop("e must contain self")
  if (!is.environment(e$self)) stop("self must be an environment")
  if (identical(e$self, globalenv())) stop("self cannot be the global environment")
  if (environmentIsLocked(e$self)) stop("self is locked")
  all_objects = ls(envir = e)  # clean objects starting with a dot as well?
  objects_to_keep = "self"
  objects_to_clean = setdiff(all_objects, objects_to_keep)
  rm(list = objects_to_clean, envir = e)
}

#' Validate Object
#'
#' S3 generic for checking the validity of a constructed
#' object. should either return nothing or trigger an error.
#'
#' @param object Object to be validated.
#' @return TODO -- check $valid methods
#' @export
validate_object = function(object) {
  UseMethod("validate_object", object)
}

#' Return Object
#'
#' This should be the final function called in a class
#' definition. Think of it like return(...)
#'
#' @param self New object.
#' @param class String giving the class name.
#' @return New object of class given by \code{class}.
#' @export
return_object = function(self, class) {
  UseMethod("return_object", self)
}

#' Inheritance
#'
#' Inherit methods and fields from other classes.
#'
#' There are three ways to inherit from other classes:
#' (1) directly
#' (2) using the \code{inherit_from} function
#' (3) using the \code{implements} function
#'
#' Each of these ways works by adding a line that creates an object called
#' \code{self} at the beginning of a class definition. The object \code{self}
#' is an object of class \code{ParentClass} and you are free to add new fields
#' and methods to this object.
#'
#' @section Direct Inheritance:
#'
#' \code{self = ParentClass(...)}
#'
#' Here \code{ParentClass} is the name of the parent class being inherited from.
#' In most cases, direct inheritance is the most useful approach. The other
#' two are for more advanced use.
#'
#' @section Inherit From:
#'
#' \code{self = inherit_from(ParentClass, list_of_trait_classes)}
#'
#' Here \code{ParentClass} is the class being directly inherited from
#' and \code{list_of_trait_classes} is a list of \code{\link{Trait}}
#' classes containing methods to be forwarded to \code{self}.
#'
#' @section Implementations:
#'
#' \code{self = implements(ParentInterface)}
#'
#' Here \code{ParentInterface} is an abstract set of method signatures.
#' Following this initialization, concrete definitions of these abstract
#' method need to be added to \code{self}. This process is referred to as
#' implementing an interface.
#'
#' @name inheritance
NULL

#' @param parent A single class from which to inherit methods and fields.
#' @param traits A vector of \code{\link{Trait}} classes from which to
#' forward methods (trait classes are like mixin class in Python).
#' @param ... Arguments to pass to the initialization of the \code{parent}
#' class.
#' @rdname inheritance
#' @export
inherit_from = function(parent, traits, ...) {
  self = parent(...)
  for (trait in traits) {
    trait_object = trait()
    trait_object$forward_all(self)
  }
  return(self)
}

#' @param interface Class definition that inherits from \code{\link{Interface}}
#' @rdname inheritance
#' @export
implements = function(interface) {
  object = Implementation()
  object$.implements(interface)
  return(object)
}

#' @export
return_facade = function(self, private, class) {
  assign("self", self, envir = private)
  f_nms = names(which(unlist(eapply(self, is.function))))
  for (f_nm in f_nms) {
    environment(self[[f_nm]]) = private
  }
  structure(self, class = c(class, unique(class(self))))
}
