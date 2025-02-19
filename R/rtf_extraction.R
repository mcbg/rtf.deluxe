deluxe_sub = \(text, regex, replace) sub(regex, replace, text, perl=TRUE)
deluxe_gsub = \(text, regex, replace) gsub(regex, replace, text, perl=TRUE)
deluxe_split = \(text, sep) strsplit(text, split=sep)[[1]]
deluxe_substr = \(text, i_start, i_end) substr(text, i_start, i_end)[[1]]

extract_page = \(single_page) {
  result = single_page |>
    remove_braces_by_match('\\{\\\\header') |>
    remove_braces_by_match('\\{\\\\footer') |>
    deluxe_gsub('(?s)\\\\paper[wh]\\d+', '') |>
    deluxe_gsub('(?s)\\\\marg\\w\\d+', '') |>
    deluxe_gsub('\\\\headery\\d+', '') |>
    deluxe_gsub('\\\\footery\\d+', '') |>
    deluxe_gsub('(?s)\\\\marg\\w\\d+', '') |>
    deluxe_split(sep='\n') |>
    c('\\page')
  return(result)
}

# takes a standalone RTF-file and return contents without header and final curly brace
#' @export
extract_file = \(rtf_lines) {
  # BUG: adds a final blank page
  rtf_text = paste(rtf_lines, collapse = '\n')
  pages = rtf_text |> deluxe_split('\\page')
  pages_extracted = lapply(pages, extract_page)
  pages_extracted_lines = pages_extracted |> unlist()
  result_text = pages_extracted_lines |>
    paste(collapse = '\n') |>
    deluxe_sub('(?s)(.*?)(?=\\{\\\\pard)', '') |>
    deluxe_sub('(?s)(.*)\\}', '\\1')
  result_lines = result_text |>
    deluxe_split('\n')
  return(result_lines)
}


get_index_of_closing_brace = \(text) {
  # BUG: {{}} doesnt remove last brace
  open_brace = gregexpr('\\{', text)[[1]]
  closed_brace = gregexpr('\\}', text)[[1]]
  i = 1
  j = 0
  unclosed = 1
  N = open_brace |> length()
  M = closed_brace |> length()
  while(j <= M) {
    i_peak = i + 1
    j_peak = j + 1
    i_confirmed = i
    j_confirmed = j
    if (i_confirmed == j_confirmed) {
      return (closed_brace[j_confirmed])
    }
    else if (i == N | open_brace[i_peak] > closed_brace[j_peak]) {
      j = j + 1
    }
    else if (open_brace[i_peak] < closed_brace[j_peak]) {
      i = i + 1
    }
    else {
      stop('??')
    }
  }
  stop('closing not found')
}

remove_braces = \(text, starting_brace_index) {
  # BUG: doesnt work if starting_brace_index = 1
  text_before_starting_brace = deluxe_substr(text, 1, starting_brace_index - 1)
  text_after_starting_brace = substr(text, starting_brace_index, nchar(text))
  closing_bracket_index = get_index_of_closing_brace(text_after_starting_brace)
  # BUG: doesnt work if closing bracket is last character in string
  text_after_closing_brace = deluxe_substr(text_after_starting_brace, closing_bracket_index + 1, nchar(text_after_starting_brace))
  result = paste0(text_before_starting_brace, text_after_closing_brace)

  return(result)
}

extract_braces = \(text, starting_brace_index) {
  text_after_starting_brace = substr(text, starting_brace_index, nchar(text))
  closing_bracket_index = get_index_of_closing_brace(text_after_starting_brace)
  result = deluxe_substr(text_after_starting_brace, 1, closing_bracket_index)
  return(result)
}

remove_braces_by_match = \(text, regex) {
  i = gregexec(regex, text)[[1]] |> min()
  if (i == -1) return(text)
  remove_braces(text, i)
}
