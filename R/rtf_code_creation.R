# Author: Michael Galanakis
# Bugs:
# - ignores multirow header and cell merging in flextables
# - the following variables are hardcoded and depend on font size:
#   cm_per_character (used to compute a tables column width based on the number of characters)
# - the table of contents and header use \tx13000 which hardcodes how far to the right the page number is in twip
# - no easy way to update font styling
# many bugs could be handled by adding global options

# functions, derive RTF ---------------------------------------------------------------


rtf_create_bookmark = \(code) sprintf('{\\*\\bkmkstart %s}{\\*\\bkmkend %s}', code, code)

rtf_create_header = \(text) {
  reference = gsub('\\W', '_', text)
  bm = rtf_create_bookmark(reference)
  # \outlinelevelN indicates headers
  text_rtf = sprintf("{\\pard\\outlinelevel0\\fs24\\b\\qc %s \\par}", text)
  paste0(bm, text_rtf)
}

rtf_create_link = \(text, reference, page_number, fixed_width = 100) {
  dot_length = fixed_width - nchar(text) - nchar(page_number)
  dots_text = rep('.', dot_length) |> paste(collapse = '')
  sprintf('{\\pard {\\field{\\*\\fldinst{HYPERLINK "#%s"}}{\\fldrslt{\\tldot\\tqr\\tx13000 %s \\tab %d\\par}}}}',
    reference, text, page_number)
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
rtf_create_table = \(table_input, margin_cm=0.25, max_rows_per_page = getOption('rtf_deluxe.max_rows_per_page')) {

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

  # derive cellx control words
  largest_nchar_rows = sapply(dataset, character_count_largest_word)
  largest_nchar_header = sapply(header, character_count_largest_word)
  largest_nchar = pmax(largest_nchar_rows, largest_nchar_header)
  cell_width_cm = largest_nchar |>
    character_to_cm() |>
    sapply(max, 2)
  cell_width_twips = (cell_width_cm + margin_cm) |>
    cm_to_twip()
  cellx_values = cell_width_twips |>
    cumsum() |>
    ceiling()
  cellx_control_words = paste0('\\cellx', cellx_values)

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
    header_rtf = header |> rtf_create_row(cellx_control_words, bold=TRUE, border_control_words = '\\clbrdrt\\brdrs\\clbrdrb\\brdrs')
    rows_rtf = sapply(sub_table_contents, rtf_create_row, cellx_control_words=cellx_control_words)
    last_row_rtf = rtf_create_row(contents_last, border_control_words = '\\clbrdrb\\brdrs', cellx_control_words = cellx_control_words)
    # paragraph is to ensure that \\page works
    c('{\\pard\\fs24\\par}', '{', header_rtf, rows_rtf, last_row_rtf, '}')
  })
  return(sub_tables)
}

rtf_create_row = \(rows, cellx_control_words, bold = FALSE, border_control_words = '') {
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
  x |>
    as.character() |>
    c(header) |>
    strsplit(split = c(' ', '\n')) |>
    unlist() |>
    nchar() |>
    max()
}

character_to_cm = \(x, cm_per_character = getOption('rtf_deluxe.cm_per_character')) cm_per_character * x

inch_to_twip = \(x_inch) round(x_inch * 1440)
cm_to_twip = \(x_cm) {
  x_inches = x_cm / 2.54
  x_twip = inch_to_twip(x_inches)
  return(x_twip)
}

# create page -------------------------------------------------------------

rtf_create_page = \(rtf_content, rtf_title, rtf_subtitle, rtf_footnote){
  # combine
  rtf_separator = '{\\pard\\par}'
  rtf_full = c(rtf_title, rtf_subtitle, rtf_separator, rtf_content, rtf_separator, rtf_footnote, '\\page')

  # BUG: only fixes limited number of symbols
  #      fix all by using utf8ToInt and \'XX control words
  # fix unicode character
  rtf_full_nonbreak_spaces_fix = gsub('\u00A0', '\\\\~', rtf_full)
  rtf_full_dash_fix = gsub('\u2011', '-', rtf_full_nonbreak_spaces_fix)
  rtf_full_final = gsub('\u00B1', '\\\\u00B1', rtf_full_dash_fix)
  return(rtf_full_final)
}

rtf_create_output = \(output, type) {
    if (type == 'table' | type == 'listing') {
      rtf_pages_contents = rtf_create_table(output)
    }
    # BUG:
    # else if (type == 'figure') {
    #  rtf_pages_contents = rtf_create_png_rtf(filename) |> list()
    #}
    else {
      stop('invalid type')
    }
}

rtf_create_output_by_metadata = \(output_metadata, output_directory, id_lookup) {
  with(output_metadata, {
    # title
    type_format = c('figure' = 'Figure', 'table' = 'Table', 'listing' = 'Listing')
    full_title = paste(type_format[type], original_numbering, title)
    rtf_title = rtf_create_header(full_title)

    # subtitle & footnote
    rtf_subtitle = rtf_create_text(subtitle)
    rtf_footnote = footnotes |> sapply(rtf_create_text)

    # content
    if (type == 'table' | type == 'listing') {
      filename = paste0(name, '.rds')
      tfl_table = file.path(output_directory, filename) |>  readRDS()
      rtf_pages_contents = rtf_create_table(tfl_table)
    }
    else if (type == 'figure') {
      filename = file.path(output_directory, paste0(name, '.png'))
      rtf_pages_contents = rtf_create_png_rtf(filename) |> list()
    }
    else {
      stop('invalid type')
    }

    # only adds title and subtitle to first subtable
    rtf_pages_first = rtf_create_page(rtf_pages_contents[[1]],
      rtf_title=rtf_title, rtf_subtitle=rtf_subtitle, rtf_footnote=rtf_footnote)
    # BUG: doesn't handle case with only one input
    rtf_pages_rest = lapply(rtf_pages_contents[-1], rtf_create_page,
      rtf_title=NULL, rtf_subtitle=NULL, rtf_footnote=rtf_footnote) |>
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

#' rtf_content_list contain 1 output per entry
#' the table of contents will have 1 entry per output
#' @export
create_table_of_contents = \(rtf_content_list, output_titles) {
  # get options
  table_of_contents_entries_per_page = getOption('rtf_deluxe.table_of_contents_entries_per_page')

  # asserts
  if (length(rtf_content_list) != length(output_titles)) {
    stop('must have 1 title or each entry of rtf_content_list')
  }

  # processing
  content_page_count = sapply(rtf_content_list, \(rtf_line) grep('\\page', rtf_line) |> length())
  number_of_output = length(rtf_content_list)

  page_offset = (number_of_output %/% table_of_contents_entries_per_page) + 1
  page_numbers = derive_page_number(content_page_count, page_offset)
  references = gsub('\\W', '_', output_titles) # used to link table of contents entries to output headers

  indices = seq_along(rtf_content_list)
  rtf_toc = sapply(indices, \(i) {
    rtf_create_link(output_titles[i], reference = references[i], page_numbers[i])
  })
  return(rtf_toc)
}

#' @export
rtf_add_head_and_tail = \(rtf_contents, header_text) {
  # BUG: doesnt add margins + footery / headery
  letter_dims_inch = c(8.5, 11) |> rev()
  rtf_paper_dims = paste0(c('\\paperw', '\\paperh'), letter_dims_inch |> inch_to_twip())
  rtf_header = c(
    '{\\rtf1\\ansi\\deff0{\\fonttbl{\\f0 \\fmodern TimesNewRoman;}}',
    rtf_paper_dims,
    '\\widowctrl\\ftnbj\\fet0\\sectd\\lndscpsxn\\linex0', # landscape
    '\\fs20',
    '{\\header{\\pard\\tqr\\tx13000\\fs20 ', header_text, '\\tab Page \\chpgn  of {\\field{\\*\\fldinst NUMPAGES}}\\par}}',
    '{\\footer{\\qc\\fs20 \\par}}'
  )
  full_document = c(rtf_header, rtf_contents, '}')
  return(full_document)
}
