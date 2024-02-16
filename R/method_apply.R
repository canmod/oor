#' Method Apply
#'
#' Call a method for each item in a list of objects.
#'
#' @param objects List of objects.
#' @param method_name Character string giving the name of the method.
#' @param ... Arguments to pass to the method.
#' @importFrom stats setNames
#' @export
method_apply = function(objects, method_name, ...) {
  output_objects = list()
  for (i in seq_along(objects)) {
    output_objects[[i]] = objects[[i]][[method_name]](...)
  }
  setNames(output_objects, names(objects))
}
