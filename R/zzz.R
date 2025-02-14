.onLoad <- function(libname, pkgname) {
  default_options <- list(
    rtf_deluxe.table_of_contents_entries_per_page = 39,
    rtf_deluxe.max_rows_per_page = 20,
    rtf_deluxe.cm_per_character = 5 / 25 # this is based on how many S's in pt 10 Consolas I could fit in a 5cm column
  )
  missing_options <- !(names(default_options) %in% names(options()))
  options(default_options[missing_options])
}

