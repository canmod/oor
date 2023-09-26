DebugList = function() {
  self = new.env(parent = new.env())
  self$.methods = character()
  self$.classes = character()

  self$flag = function(method, class = "Base") {
    self$.methods = c(self$.methods, method[1L])
    self$.classes = c(self$.classes, class[1L])
  }
  self$unflag = function(method) {
    meth_to_rm = self$.methods == method[1L]
    self$.methods = self$.methods[!meth_to_rm]
    self$.classes = self$.classes[!meth_to_rm]
    suppressWarnings(self$.objects$.undebug(method))
    self$.objects$.remove(method)
  }
  self$list_flagged = function() {
    data.frame(method = self$.methods, class = self$.classes)
  }
  self$undebug = function(method) self$.objects$.undebug(method)
  self$debug = function(method) self$.objects$.debug(method)
  self$list_objects = function() self$.objects$.objects

  self$.objects = DebugObjects()
  structure(self, class = c("DebugList", "Base"))
}

DebugObjects = function() {
  self = new.env(parent = new.env())

  ## named list of lists. names refer to methods and
  ## inner lists give objects with each method being
  ## debugged.
  self$.objects = list()

  ## track all objects with methods flagged for debugging
  self$.add = function(methods, object) {
    for (m in methods) {
      self$.objects[[m]] = unique(c(self$.objects[[m]], object))
    }
  }

  ## remove tracking of objects for a particular method
  self$.remove = function(method) self$.objects[[method]] = NULL

  self$.is_method = function(method, object) isTRUE(try(is.function(object[[method]])))

  ## undebug a particular method in all objects being tracked
  self$.undebug = function(method) {
    if (method %in% names(self$.objects)) {
      for (obj in self$.objects[[method]]) {
        if (self$.is_method(method, obj)) undebug(obj[[method]])
      }
    }
  }

  ## debug a particular method in all objects being tracked
  self$.debug = function(method) {
    if (method %in% names(self$.objects)) {
      for (obj in self$.objects[[method]]) {
        if (self$.is_method(method, obj)) debug(obj[[method]])
      }
    }
  }

  structure(self, class = c("DebugObjects", "Base"))
}

#' Debug Methods
#'
#' List of methods that will be flagged for debugging when they are created.
#'
#' Because oor methods are not created until the object within which they
#' exist is created, users cannot as easily use the \code{\link{debug}}
#' function. By adding method-class pairs to this \code{oor_debug} object,
#' users can control what methods get debugged before they are created.
#' Note that once a method that is flagged for debugging is created, it cannot
#' be \code{\link{undebug}}ed. To stop debugging particular methods, the
#' associated objects must be recreated and this is often best done by just
#' reruning the script.
#'
#' @format This list is an object of class \code{DebugList} with the following
#' methods.
#'
#' * `$add(method, class)` -- Add a `method` in a particular `class` to be
#' debugged. By default `class == "Base"`, which will typically debug a method
#' called `method` in any class because typically all classes will inherit
#' from `Base`.
#' * `$remove(method)` -- Stop debugging all `method`s with a particular name.
#' * `$list()` -- List all `method`-`class` pairs that would be debugged if
#' called.
#' * `$undebug(method)` -- Stop debugging a method in all objects.
#'
#' @export
oor_debug = DebugList()


.onLoad <- function(lib, pkg) {
}

.onUnload <- function(libpath) {
}
