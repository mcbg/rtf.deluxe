# Author: Michael Galanakis

output_directory = tempdir()

# define, example table, summary of iris ---------------------------------------------------

example_table1 = aggregate(Sepal.Width ~ Species, iris, mean) |> as.data.frame()
example_table2 = aggregate(Sepal.Length ~ Species, iris, mean) |> as.data.frame()

rtf.deluxe::save_output_and_metadata |> debugonce()
rtf.deluxe::save_output_and_metadata(example_table1, path = output_directory, name='sepal-width-mean', numbering = '1.1', title = 'Mean of sepal width', type = 'table', program_name = 'vignette.R')
rtf.deluxe::save_output_and_metadata(example_table2, path = output_directory, name='sepal-length-mean', numbering = '1.2', title = 'Mean of sepal length', type = 'table', program_name = 'vignette.R')

example_table1

rtf_content = metadata |>
  list_filter(type %in% c('listing')) |>
  #list_filter(type %in% c('table', 'figure')) |>
  assemble_tfl_document(header_text = header_text)

rtf_content |> cat(sep='\n', file='test.rtf')
