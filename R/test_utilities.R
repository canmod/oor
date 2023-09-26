dot_names = function(...) {
  vapply(substitute(list(...)), deparse, character(1), USE.NAMES = FALSE)[-1]
}

as_test_object = function(x) {
  f = as.function(x)
  self = environment(f)$self
  # if (is.null(self)) {
  #   self = TestBasic(f)
  # } else if (!inherits(self, "Test")) {
  #   stop("Cannot convert to a test object")
  # }
  self
}

collapse_class = function(x, sep = "-") {
  paste0(class(x), collapse = sep)
}

name_of_names = function(nm, nms, sep, indent = 0) {
  sep = paste0("\n", paste0(rep(" ", indent), collapse = ""), sep)
  paste(
    nm, "(\n",
    paste0(nms, collapse = sep),
    "\n)",
    sep = ""
  )
}
#cat(name_of_names("make", c("a" ,"b"), " |> ", 3))
