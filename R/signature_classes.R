Signature = function() {
  self = Base()
  self$check_arguments = function() {
    ValidityMessager(TestPlaceholder(), "")
  }
  self$check_return_value = function() {
    ValidityMessager(TestPlaceholder(), "")
  }
  return_object(self, "Signature")
}
