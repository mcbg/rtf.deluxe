deluxe_sub = \(text, regex, replace) sub(regex, replace, text, perl=TRUE)
deluxe_gsub = \(text, regex, replace) gsub(regex, replace, text, perl=TRUE)
deluxe_split = \(text, sep) strsplit(text, split=sep)[[1]]
deluxe_substr = \(text, i_start, i_end) substr(text, i_start, i_end)[[1]]


#' takes a standalone RTF-file and return contents without header and final curly brace
#' @export
extract_contents = \(rtf_lines) {
  rtf_text = paste(rtf_lines, collapse = '\n')
  no_header = rtf_text |>
    remove_braces_by_match('\\{\\\\header')
  no_footer = no_header |>
    remove_braces_by_match('\\{\\\\footer')
  no_preamble = no_footer |>
    deluxe_sub('(?s)(.*?)(?=\\{\\\\pard)', '')
  no_final_brace = no_preamble |>
    deluxe_sub('(?s)(.*)\\}', '\\1')

  result = no_final_brace |>
    deluxe_split(sep='\n')

  return(result)
}

#' @export
assemble_rtfs_files = \(file_names, output_titles) {
  name = basename(file_names)
  rtf_content_list = lapply(seq_along(file_names), \(i) {
    single_file_name = file_names[i]
    single_title = output_titles[i]
    rtf_content = readLines(single_file_name)
    reference = gsub('\\W', '_', single_title) # used to link table of contents entries to output headers
    bookmark = rtf_create_bookmark(reference)
    c(bookmark, extract_contents(rtf_content), '\\page')
  })
  rtf_toc = create_table_of_contents(rtf_content_list, output_titles)
  rtf_all_content = c(
    rtf_toc,'\\page',
    rtf_content_list |> unlist()
  )
  full_document = rtf_add_head_and_tail(rtf_all_content, header_text = '')
  return(full_document)
}

get_index_of_closing_brace = \(text) {
  # BUG: {{}} doesnt remove last brace
  open_br = gregexpr('\\{', text)[[1]]
  closed_br = gregexpr('\\}', text)[[1]]
  i = 2
  j = 1
  unclosed = 1
  N = open_br |> length()
  M = closed_br |> length()
  while(j <= M) {
    if (unclosed == 0) {
      return (closed_br[j])
    }
    else if (i == N | open_br[i] > closed_br[j]) {
      if (unclosed == 1) {
        return(closed_br[j])
      }
      else {
        j = j + 1
        unclosed = unclosed - 1
      }
    }
    else if (open_br[i] == closed_br[j]) {
      stop('two symbols at same index???')
    }
    else if (open_br[i] < closed_br[j]) {
      i = i + 1
      unclosed = unclosed + 1
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
