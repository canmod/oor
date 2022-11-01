# Basal Classes ---------------
# Base: Testable: Implementation
#                 Trait
#                 Interface

#' Base Class
#'
#' Initialize an empty object.
#'
#' \link[=inheritance]{Inherit} from \code{Base} if you want to start
#' from an empty class
#'
#' @examples
#' empty_object = Base()
#' print(empty_object)
#' names(empty_object)
#'
#' Printer = function(x) {
#'   self = Base()
#'   self$.x = x
#'   self$print = function() print(self$.x)
#'   return_object(self, "Printer")
#' }
#' printer = Printer("something to print")
#' printer$print()
#'
#' @export
Base = function() {
  self = new.env(parent = emptyenv())
  structure(self, class = "Base")
}

#' @export
BaseBase = function() {
  self = new.env(parent = baseenv())
  structure(self, class = "Base")
}

#' @export
Unclean = function() {
  self = BaseBase()
  return_object(self, "Unclean")
}

#' Testable Class
#'
#' Initialize an object with functionality for validating objects
#'
#' \link[=inheritance]{Inherit} from \code{Testable} if you
#' would like your class to provide a validity
#' check. Validity checking often requires differentiating between public
#' versus private members, as well as fields versus methods.
#'
#' @examples
#'
#' Printer = function(x) {
#'   self = Testable()
#'   self$.x = x
#'   self$valid = function() {
#'      if (!is.character(self$.x)) {
#'        return("can only print character strings")
#'      }
#'      if (length(self$.x) != 1L) {
#'        return("can only print length-1 character vectors")
#'      }
#'      return(TRUE)
#'   }
#'   self$print = function() print(self$.x)
#'   return_object(self, "Printer")
#' }
#' printer = Printer("something to print")
#' printer$print()
#' try(Printer(0)) ## error
#'
#' @export
Testable = function() {
  self = Base()
  self$valid = function() TRUE
  self$.field_names = function() {
    not_function = function(x) !is.function(x)
    names(which(unlist(eapply(self, not_function))))
  }
  self$.method_names = function() {
    names(which(unlist(eapply(self, is.function))))
  }
  self$.public = function(names) names[!startsWith(names, ".")]
  self$.private = function(names) names[startsWith(names, ".")]
  self$.public_method_names = function() {
    self$.public(self$.method_names())
  }
  self$.private_method_names = function() {
    self$.private(self$.method_names())
  }
  self$.public_field_names = function() {
    self$.public(self$.method_names())
  }
  self$.private_field_names = function() {
    self$.private(self$.method_names())
  }
  return_object(self, "Testable")
}

#' Trait Class
#'
#' Initialize an object with methods that are intended to be forwarded
#' to other classes.
#'
#' \link[=inheritance]{Inherit} from \code{Trait} if you want to use
#' your class to forward public methods to other classes without direct
#' inheritance.
#'
#' @examples
#' Print = function(x) {
#'   self = Testable()
#'   self$.x = x
#'   return_object(self, "Print")
#' }
#' Printer = function() {
#'   self = Trait()
#'   self$print = function() print(self$.x)
#'   return_object(self, "PrinterTrait")
#' }
#' PrintString = function(x) {
#'   self = inherit_from(Print, list(PrinterTrait), x)
#'   self$valid = function() {
#'      if (!is.character(self$.x)) return("can only print character strings")
#'      if (length(self$.x) != 1L) return("can only print length-1 character vectors")
#'      return(TRUE)
#'   }
#'   return_object(self, "PrintString")
#' }
#' PrintNumber = function(x) {
#'   self = inherit_from(Print, list(PrinterTrait), x)
#'   self$valid = function() {
#'      if (!is.numeric(self$.x)) return("can only print character strings")
#'      return(TRUE)
#'   }
#'   return_object(self, "PrintNumber")
#' }
#' PrintString("something to print")$print()
#' PrintNumber(pi)$print()
#' try(PrintNumber("not a number")) ## error
#'
#' @export
Trait = function() {
  self = Testable()
  self$.forwardable_method_names = function() {
    setdiff(self$.public_method_names(), c("forward_all", "forward_method"))
  }
  self$forward_method = function(focal_object, method) {
    # forward the method from the trait (i.e. self) to focal_object
    focal_object[[method]] = self[[method]]
    # make sure that the environment of the forwarded method
    # contains a single object called self, which is the
    # focal_object itself
    environment(focal_object[[method]]) = list2env(list(self = focal_object))
  }
  self$forward_all = function(focal_object) {
    methods = self$.forwardable_method_names()
    for (method in methods) {
      self$forward_method(focal_object, method)
    }
  }
  self$valid = function() {
    if (length(self$.field_names()) != 0L)
    return("traits cannot contain data, only methods")
    TRUE
  }
  return_object(self, "Trait")
}

#' Interface Class
#'
#' Initialize an empty abstract class.
#'
#' \link[=inheritance]{Inherit} from \code{Interface} to define the
#' argument signatures and return value types
#' of abstract methods.
#'
#' @examples
#' BinaryOperation = function() {
#'   self = Interface()
#'   self$operate = function(x = numeric(1L), y = numeric(1L)) return(numeric(1L))
#'   return_object(self, "BinaryOperation")
#' }
#'
#' @export
Interface = function() {
  self = Testable()
  self$.stopifnot_valid_interface = function() {
    method_nms = self$.public_method_names()
    for (nm in method_nms) {
      ## assumption: arguments in the pairlist are of type `name` if they
      ## have no default -- this seems fragile but i can't seem to find the
      ## canonical/explicit approach here
      if (any(vapply(formals(self[[nm]]), is.name, logical(1L)))) {
        stop("all public methods in interfaces must have defaults")
      }
    }
  }
  return_object(self, "Interface")
}

#' Implementation Class
#'
#' Initialize an object with concrete implementations of abstract method
#' definitions.
#'
#' \link[=inheritance]{Inherit}
#' from \code{Implementation} (using the
#' \code{\link{implements}} utility function) if you want
#' your class to implement an \code{\link{Interface}}.
#'
#' @examples
#' BinaryOperation = function() {
#'   self = Interface()
#'   self$operate = function(x = numeric(1L), y = numeric(1L)) return(numeric(1L))
#'   return_object(self, "BinaryOperation")
#' }
#' Add = function() {
#'   self = implements(BinaryOperation)
#'   self$operate = function(x = numeric(1L), y = numeric(1L)) return(x + y)
#'   return_object(self, "Add")
#' }
#' Multiply = function() {
#'   self = implements(BinaryOperation)
#'   self$operate = function(x = numeric(1L), y = numeric(1L)) return(x * y)
#'   return_object(self, "Multiply")
#' }
#' Add()$operate(1, 1)
#' Multiply()$operate(2, 2)
#'
#' @export
Implementation = function() {
  self = Testable()
  self$interface = Interface()
  self$.implements = function(interface_class) {
    self$interface = interface_class()
  }
  self$.stopifnot_valid_implementation = function() {
    interface = self$interface
    method_nms = interface$.public_method_names()
    for (nm in method_nms) {
      if (!identical(formals(self[[nm]]), formals(interface[[nm]]))) {
        stop("method arguments do not have identical signatures")
      }
      self_return_type = typeof(try(self[[nm]](), silent = TRUE))
      interface_return_type = typeof(try(interface[[nm]](), silent = TRUE))
      if (!identical(self_return_type, interface_return_type)) {
        stop("default return value must be of the same type")
      }
    }
  }
  return_object(self, "Implementation")
}
