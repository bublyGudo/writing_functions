nsduh_table_format = function (table_num, table_name) {
  out_table =
    nsduh_html |> 
    html_table() |> 
    nth(table_num) |> 
    slice (-1) |> 
    mutate (name = table_name)
  
  return (out_table)
}