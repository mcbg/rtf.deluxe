# list filter -------------------------------------------------------------

list_filter = \(input_list, expr) {
  captured_expression = substitute(expr)
  matching_entries = sapply(input_list, \(x) {
    eval(captured_expression, envir = x)
  })
  input_list[matching_entries]
}
