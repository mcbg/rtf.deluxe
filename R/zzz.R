.onLoad <- function(libname, pkgname) {
  default_options <- list(
    rtf.deluxe.margin_top_cm = 1.27,
    rtf.deluxe.margin_bottom_cm = 1.27,
    rtf.deluxe.margin_left_cm = 2.00,
    rtf.deluxe.margin_right_cm = 1.27,
    rtf.deluxe.page_dims = c(8.5, 11), # US letter format
    rtf.deluxe.page_dims_unit = 'inch',
    rtf.deluxe.table_of_contents_entries_per_page = 39,
    rtf.deluxe.max_rows_per_page = 20,
    rtf.deluxe.cm_per_character = 5 / 25 # this is based on how many S's in pt 10 Consolas I could fit in a 5cm column
  )
  missing_options <- !(names(default_options) %in% names(options()))
  options(default_options[missing_options])
}

