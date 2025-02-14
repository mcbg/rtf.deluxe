
create_standalone_rtf_file(iris) |>
  cat(file='test.rtf', sep='\n')
yo=options()
yo$rtf_deluxe.max_rows_per_page

tb_list = rtf_create_table(iris)
rtf_create_text('')
names(iris) = iris |> names() |> paste(c('Sepal', 'Sepal', 'Petal', 'Petal'), sep='\t')
tb = create_standalone_rtf_file(iris)
tb |>  cat(file='test.rtf', sep='\n')
tb |> writeClipboard()
