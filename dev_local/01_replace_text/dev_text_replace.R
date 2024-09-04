
root <- "dev_local/01_replace_text"
file_in <- file.path(root, "example_01.pptx")
file_out <- file_in |> stringr::str_replace(".pptx", "_CHANGED.pptx")


x <- officer::read_pptx(file_in)
pptx_text_replace(x, "@" = "<<>>", "{1}" = "ö")
print(x, target = file_out)
file_open(file_out)


