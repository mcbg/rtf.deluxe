devtools::load_all()
create_standalone_rtf_file(iris) |>
  cat(file='test.rtf', sep='\n')
yo=options()
yo$rtf_deluxe.max_rows_per_page

ds=datasets::iris
names(ds) = ds |> names() |> paste(c('Sepal', 'Sepal', 'Petal', 'Petal'), sep='\t')
tb = create_standalone_rtf_file(ds)
tb |>  cat(file='test.rtf', sep='\n')
tb |> writeClipboard()

ll = c('c' , 'x\ty', 'z\ty', 'a') |> data.table::tstrsplit(split = '\t')
ll = c('c' , 'x\ty', 'z\ty', 'a') |> strsplit(split = '\t')
sapply(ll, `[`, 1)
base_width = 1:3
values = sapply(ll, `[`, 2)
#values[is.na(values)] = ''

# TRACER FOR HEADER HIERARCHY FUNCTIONALITY
res =
for (i in seq_alone(values)) {
  x = values[i]
  if (is.na(x)) {
    res = rep(0, length(values))
    res[i] = 1
    return(res)
  }
  else {
  values == x
  }
}
M = sapply(values, \(x) x == values) |>
  unique()
as.numeric(M %*% base_width)
