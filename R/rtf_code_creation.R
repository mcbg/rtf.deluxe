# Functions that create elements of a RTF-file
#
# Bugs:
# - ignores multirow header and cell merging in flextables
# - the table of contents and header use \tx13000 which hardcodes how far to the right the page number is in twip
#   could be fixed with global option
# - no easy way to update font styling

# functions, derive RTF ---------------------------------------------------------------


rtf_create_bookmark = \(code) sprintf('{\\*\\bkmkstart %s}{\\*\\bkmkend %s}', code, code)

rtf_create_header = \(text) {
  # \outlinelevelN indicates headers
  text_rtf = sprintf("{\\pard\\outlinelevel0\\fs24\\b\\qc %s \\par}", text)
  paste0(text_rtf)
}

rtf_create_link = \(text, reference, page_number, fixed_width = 100) {
  sprintf('{\\pard {\\field{\\*\\fldinst{HYPERLINK "#%s"}}{\\fldrslt{\\tldot\\tqr %s %s \\tab %d\\par}}}}',
    reference, get_tx_value(), text, page_number)
}

dataset_to_row_list  = \(dataset) {
  N = dataset |> nrow()
  seq_len(N) |> lapply(\(i) {
    dataset[i,] |> sapply(as.character)
  })
}

rtf_create_png_rtf = \(file_name, width_scale = 100, height_scale = 100) {
  png_binary = readBin(file_name, what = 'raw', n = 100000000) |> paste(collapse = '') # 100000000 bytes is 100MB
  png_rtf = sprintf("{\\pard\\qc{\\pict\\pngblip\\picscalex%s\\picscaley%s %s} \\par}", width_scale, height_scale, png_binary)
  return(png_rtf)
}

# rtf_create_table returns a list of rtf code, where each page
# corresponds to an entry of the list.
rtf_create_table = \(table_input,
    margin_cm=0.25, max_rows_per_page = getOption('rtf.deluxe.max_rows_per_page')) {

  # extra data from table_input
  if ('data.frame' %in% class(table_input)) {
    header = names(table_input)
    dataset = table_input
  }
  else if ('flextable' %in% class(table_input)) {
    dataset = table_input$body$dataset
    header = table_input$header$content$data |>
      tail(1) |> # ignore multiple row headers (TEMPORARY FIX)
      sapply(\(x) x$txt)
  }
  else {
    stop('invalid class ', class(dataset))
  }

  # check
  check_table(dataset)

  # derive cellx control words
  largest_nchar_rows = sapply(dataset, character_count_largest_word)
  largest_nchar_header = header |>
    deluxe_sub('\t.*', '') |>
    sapply(character_count_largest_word)
  largest_nchar = pmax(largest_nchar_rows, largest_nchar_header)
  cell_width_cm = largest_nchar |>
    character_to_cm() |>
    sapply(max, 2)

  # create header
  header_rtf = rtf_create_hierarchical_header(header, cell_width_cm) |> unlist()

  # prepare rows
  table_rows = dataset_to_row_list(dataset)
  table_rows_length = length(table_rows)
  table_rows_indices = seq_len(table_rows_length)
  table_rows_page_index = 1 + table_rows_indices %/% max_rows_per_page

  # prepare pages
  number_of_pages = local({
    extra_pages = ifelse(table_rows_length %% max_rows_per_page == 0, 0, 1)
    (table_rows_length %/% max_rows_per_page) + extra_pages
  })

  # if a table cant fit on 1 page, the table will be split into
  # sub-tables that each have their own page
  sub_tables = lapply(seq_len(number_of_pages), \(page_index) {
    sub_table_row_index = which(page_index == table_rows_page_index)
    sub_table_contents = table_rows[sub_table_row_index]
    sub_table_length = length(sub_table_contents)

    # split last row to add border
    contents_excl_last = sub_table_contents[-sub_table_length]
    contents_last = sub_table_contents[sub_table_length]

    # create sub-table for page
    rows_rtf = sapply(contents_excl_last, rtf_create_row, cell_width_cm) |> unlist() # is empty it return list() if unlist() is not used
    last_row_rtf = rtf_create_row(contents_last, border_control_words = '\\clbrdrb\\brdrs',
      cell_width_cm)

    # paragraph is to ensure that \\page works
    c('{\\pard\\fs24\\par}', '{', header_rtf, rows_rtf, last_row_rtf, '}')
  })
  return(sub_tables)
}

rtf_create_row = \(rows, cell_width_cm, bold = FALSE, border_control_words = '') {
  # derive cellx
  cell_width_twips = (cell_width_cm) |>
    cm_to_twip()
  cellx_values = cell_width_twips |>
    cumsum() |>
    ceiling()
  cellx_control_words = paste0('\\cellx', cellx_values)
  cell_control_words = c(
    ifelse(bold, '\\b', '\\b0'),
    ' '
  ) |> paste(collapse = '')
  # derive cells
  cells = sapply(rows, \(cell) {
    paste0('\\pard\\qc', cell_control_words, cell, '\\intbl\\cell')
  })

  cell_height_cm = 0.5
  cell_height_twip = cell_height_cm |> cm_to_twip()

  # answer
  ans = c('\\trowd',
    paste0('\\clvertalc', border_control_words, cellx_control_words),
    cells,
    paste0('\\trrh', cell_height_twip),
    '\\trqc\\row') # trqc centers the table
  return(ans)
}

rtf_create_text = \(text) paste0('{\\pard\\qc\\fs24 ', text, '\\par}')

# functions, column width -------------------------------------------------

character_count_largest_word = \(x, header=NULL) {
  if (x |> nchar() |> max() == 0) {
    return(0)
  }
  x |>
    as.character() |>
    c(header) |>
    strsplit(split = c(' ', '\n')) |>
    unlist() |>
    nchar() |>
    max()
}

character_to_cm = \(x, cm_per_character = getOption('rtf.deluxe.cm_per_character')) cm_per_character * x

inch_to_twip = \(x_inch) round(x_inch * 1440)
cm_to_twip = \(x_cm) {
  x_inches = x_cm / 2.54
  x_twip = inch_to_twip(x_inches)
  return(x_twip)
}

# create page -------------------------------------------------------------

clean_utf8 = \(text) {
  rtf_full_nonbreak_spaces_fix = gsub('\u00A0', '\\\\~', text)
  rtf_full_dash_fix = gsub('\u2011', '-', rtf_full_nonbreak_spaces_fix)
  rtf_full_final = gsub('\u00B1', '\\\\u177? ', rtf_full_dash_fix) # minus
  return(rtf_full_final)
}

rtf_create_page = \(rtf_content, rtf_title, rtf_subtitle, rtf_footnote){
  # combine
  rtf_separator = '{\\pard\\par}'
  rtf_full = c(rtf_title, rtf_subtitle, rtf_separator, rtf_content, rtf_separator, rtf_footnote, '\\page')

  # BUG: only fixes limited number of symbols
  #      fix all by using utf8ToInt and \'XX control words
  # fix unicode character
  rtf_full_final = rtf_full |> clean_utf8()
  return(rtf_full_final)
}

check_table = \(tfl_table) {
      if ('data.table' %in% class(tfl_table) & !exists('data.table'))
        stop('table with class `data.table` but data.table is not loaded')
      if (tfl_table |> nrow() == 0)
        stop('table with 0 rows')
}

rtf_create_output_by_metadata = \(output_metadata, output_directory, reference) {
  with(output_metadata, {
    # title
    type_format = c('figure' = 'Figure', 'table' = 'Table', 'listing' = 'Listing')
    full_title = paste(type_format[type], numbering, title)
    rtf_title = c(
      rtf_create_bookmark(reference),
      rtf_create_header(full_title)
    )
    rtf_title_without_bookmark = c(
      rtf_create_header(full_title)
    )

    # subtitle & footnote
    rtf_subtitle = rtf_create_text(subtitle)
    rtf_footnote = footnotes |> sapply(rtf_create_text)

    # content
    if (type == 'table' | type == 'listing') {
      filename = paste0(name, '.rds')
      tfl_table = file.path(output_directory, filename) |>  readRDS()

      # table list (entry per page)
      if ('list' %in% class(tfl_table)) {
        # check
        sapply(tfl_table, check_table)

        # create pages
        rtf_pages_contents = tfl_table |> sapply(rtf_create_table)

      }
      # single table
      else {
        # create pages
        rtf_pages_contents = rtf_create_table(tfl_table)
      }
    }
    else if (type == 'figure') {
      filename = file.path(output_directory, paste0(name, '.png'))
      rtf_pages_contents = rtf_create_png_rtf(filename) |> list()
    }
    else {
      stop('invalid type')
    }

    # check all entries are character
    rtf_pages_contents |> sapply(class) |> sapply(\(class_vector) 'character' == class_vector) |> all() |> stopifnot()

    # only adds title and subtitle to first subtable
    rtf_pages_first = rtf_create_page(rtf_pages_contents[[1]],
      rtf_title=rtf_title, rtf_subtitle=rtf_subtitle, rtf_footnote=rtf_footnote)
    rtf_pages_rest = lapply(rtf_pages_contents[-1], rtf_create_page,
      rtf_title=rtf_title_without_bookmark, rtf_subtitle=rtf_subtitle, rtf_footnote=rtf_footnote) |>
      unlist()
    rtf_pages = c(rtf_pages_first, rtf_pages_rest)

    # checks
    if (class(rtf_pages)[1] != 'character')
      sprintf('should return charactor vector not %s', class(rtf_pages)) |> stop()

    return(rtf_pages)
  })
}

# assemble doc ------------------------------------------------------------

derive_page_number = \(number_of_pages, page_offset) {
  cumulated_pages = cumsum(number_of_pages)
  result = c(0, cumulated_pages[-length(cumulated_pages)]) + 1 + page_offset
  return(result)
}

# rtf_content_list contain 1 output per entry
# the table of contents will have 1 entry per output
#' @export
create_table_of_contents = \(rtf_content_list, output_titles, references) {
  # get options
  table_of_contents_entries_per_page = getOption('rtf.deluxe.table_of_contents_entries_per_page')

  # asserts
  if (length(rtf_content_list) != length(output_titles)) {
    stop('must have 1 title or each entry of rtf_content_list')
  }

  # processing
  content_page_count = sapply(rtf_content_list, \(rtf_line) grep('\\page', rtf_line) |> length())
  number_of_output = length(rtf_content_list)

  page_offset = (number_of_output %/% table_of_contents_entries_per_page) + 1
  page_numbers = derive_page_number(content_page_count, page_offset)

  indices = seq_along(rtf_content_list)
  rtf_toc = sapply(indices, \(i) {
    clean_title = output_titles[i] |> clean_utf8()
    rtf_create_link(clean_title, reference = references[i], page_numbers[i])
  })
  return(rtf_toc)
}

get_tx_value = \() {
  page_width_twips = get_page_dims_twips()[1]
  left_twips = c('rtf.deluxe.margin_left_cm') |> getOption() |> cm_to_twip()
  right_twips = c('rtf.deluxe.margin_right_cm') |> getOption() |> cm_to_twip()
  tx_twips = page_width_twips - left_twips - right_twips
  return(paste0('\\tx', tx_twips))
}

get_page_dims_twips = \() {
  page_dims = getOption('rtf.deluxe.page_dims')
  dims_unit = getOption('rtf.deluxe.page_dims_unit')
  page_dims_twip = switch(dims_unit,
    cm = cm_to_twip(page_dims),
    inch = inch_to_twip(page_dims),
    stop('invalid value of rtf.deluxe.page_dims_unit, must be "cm" or "inch"')
  )
  return(page_dims_twip)
}

#' @export
rtf_add_head_and_tail = \(rtf_contents, header_text) {
  # compute page dims in twip

  page_dims_twip = get_page_dims_twips()

  # header format
  header_text_from_top = cm_to_twip(getOption('rtf.deluxe.margin_top_cm')) / 2
  rtf_headery = '\\headery' |> paste0(header_text_from_top)


  # create header
  rtf_paper_dims = paste0(c('\\paperw', '\\paperh'), page_dims_twip)
  margin_values_twip = c('rtf.deluxe.margin_top_cm', 'rtf.deluxe.margin_bottom_cm',  'rtf.deluxe.margin_left_cm', 'rtf.deluxe.margin_right_cm') |>
    sapply(getOption) |>
    sapply(cm_to_twip)
  margin_control_words = c('\\margt', '\\margb', '\\margl', '\\margr')
  rtf_margins = paste0(margin_control_words, margin_values_twip)
  rtf_tx = get_tx_value()

  rtf_header = c(
    '{\\rtf1\\ansi\\deff0{\\fonttbl{\\f0 \\fmodern TimesNewRoman;}}',
    '\\notabind', # remove hanging indent
    rtf_headery,
    rtf_paper_dims,
    rtf_margins,
    '\\fs20',
    '{\\header{\\pard\\tqr\\fs20', rtf_tx, header_text, '\\tab Page \\chpgn  of {\\field{\\*\\fldinst NUMPAGES}}\\par}}',
    '{\\footer{\\qc\\fs20 \\par}}'
  )
  full_document = c(rtf_header, rtf_contents, '}')
  return(full_document)
}
