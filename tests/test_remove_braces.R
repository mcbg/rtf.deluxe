path = 'C:/Users/MichaelGalanakis/omicronbycims/eSystems by CIMS - C - Workspace/2 Sponsors/NBCD/APPA-P2-2/current/04_TLF/tlfs_R/dm'
files = path |> list.files(full.names = TRUE, pattern='*.rtf')
output_titles = basename(files)
assemble_rtfs_files(files, output_titles) |>
  cat(file = 'test.rtf', sep='\n')

undebug(remove_braces)
debug(get_index_of_closing_brace)
debug(extract_file)


'{{}}' |> remove_braces(1)

'{}{}' |> remove_braces(1)
'{}{{}}{}' |> remove_braces(3)

'{}{{}}'|> remove_braces(3)
'{}{{}}'|> remove_braces(4)
'{{}}'|> remove_braces(1)
'{{}'|> remove_braces(1)
'{{}{}}' |> remove_braces(1)
'{{}{}{}}' |> remove_braces(1)



test = '{
{1}
{\\header
alsk
alskdj
{}
}
2
}'

test |>
  remove_braces_by_match('\\{\\\\header') |>
  cat(sep='\n')
'xa132 xb2 xc123' |>
  deluxe_gsub('x[ab]\\d+', 'no')
