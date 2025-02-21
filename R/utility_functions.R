# list filter -------------------------------------------------------------

list_filter = \(input_list, expr) {
  captured_expression = substitute(expr)
  matching_entries = sapply(input_list, \(x) {
    eval(captured_expression, envir = x)
  })
  input_list[matching_entries]
}

# functions ---------------------------------------------------------------
# this are pipe friendly versions of base R functions, where text
# is the first argument.

deluxe_sub = \(text, regex, replace) sub(regex, replace, text, perl=TRUE)
deluxe_gsub = \(text, regex, replace) gsub(regex, replace, text, perl=TRUE)
deluxe_split = \(text, sep) strsplit(text, split=sep)[[1]]
deluxe_substr = \(text, i_start, i_end) substr(text, i_start, i_end)[[1]]
