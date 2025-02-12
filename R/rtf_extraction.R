deluxe_sub = \(text, regex, replace) sub(regex, replace, text, perl=TRUE)
deluxe_gsub = \(text, regex, replace) gsub(regex, replace, text, perl=TRUE)
deluxe_split = \(text, sep) strsplit(text, split=sep)[[1]]

#' takes a standalone RTF-file and return contents without header and final curly brace
#' @export
extract_contents = \(rtf_lines) {
  rtf_text = paste(rtf_lines, collapse = '\n')
  rtf_text_stripped = rtf_text |>
    deluxe_sub('\\{\\\\header.*\\}', '') |>
    deluxe_sub('\\{\\\\footer.*\\}', '') |>
    deluxe_split(sep='\n')

  # remove preamble
  body_start = grep('\\\\pard\\b', rtf_text_stripped) |> min()
  result = rtf_text_stripped[body_start:length(rtf_text_stripped)]

  # remove final curly brace
  final_curly_brace_index = grep('\\}', result) |> max()
  result[final_curly_brace_index] = sub('\\}\\s*$', '', result[final_curly_brace_index])

  return(result)
}

#' @export
assemble_rtfs_files = \(path_to_rtf_files, output_titles) {
  file_names = list.files(path_to_rtf_files, pattern = '*.rtf', recursive = TRUE, full.names = TRUE)
  name = basename(file_names)
  rtf_content_list = lapply(file_names, \(single_file_name) {
    rtf_content = readLines(single_file_name)
    extract_contents(rtf_content)
  })
  rtf_toc = create_table_of_contents(rtf_content_list, output_titles)
  rtf_all_content = c(
    rtf_toc,
    rtf_content_list |> unlist()
  )
  full_document = rtf_add_head_and_tail(rtf_all_content, header_text = '')
  return(full_document)
}


rtf_content_list[1] |>
  assemble_rtfs_files('aslkdj') |>
  cat(file='test.rtf', sep='\n')

path_to_rtf_files = 'C:/Users/MichaelGalanakis/omicronbycims/eSystems by CIMS - C - Workspace/2 Sponsors/NBCD/APPA-P2-2/current/04_TLF/tlfs_R/qs'
output_titles = name
assemble_rtfs_files(path_to_rtf_files, output_titles) |>
  cat(file='test.rtf', sep='\n')

'{\\header alskdjlaksjd{} } alskjd }' |>
deluxe_sub('\\{\\\\header.*\\}', '')

test = '{ {} {} {{}} } {}'

get_index_of_closing_brace = \(text) {
  open_br = gregexpr('\\{', text)[[1]]
  closed_br = gregexpr('\\}', text)[[1]]
  i = 2
  j = 1
  N = open_br |> length()
  M = closed_br |> length()
  while(j <= M + 1) {
    if (i == j) {
      return (closed_br[j - 1])
    }
    else if (i >= N | open_br[i] > closed_br[j]) {
      j = j + 1
    }
    else if (open_br[i] == closed_br[j]) {
      stop('two symbols at same index???')
    }
    else if (open_br[i] < closed_br[j]) {
      i = i + 1
    }
    else {
      stop('??')
    }
  }
  stop('closing not found')
}

get_index_of_closing_brace('{{}{}}{}')
