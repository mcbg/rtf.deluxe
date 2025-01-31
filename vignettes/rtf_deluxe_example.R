# Author: Michael Galanakis

tfl.path = '...'
metadata = tfl.path |> list.files(pattern = '*.json', full.names = TRUE) |>
  lapply(jsonlite::read_json, simplifyVector = TRUE)

header_text = '\\b Sponsor: \\b0 Deluxe Pharma \\b Study: \\b0 Deluxe Trial'
rtf_content = metadata |>
  list_filter(type %in% c('listing')) |>
  #list_filter(type %in% c('table', 'figure')) |>
  assemble_tfl_document(header_text = header_text)

rtf_content |> cat(sep='\n', file='test.rtf')
