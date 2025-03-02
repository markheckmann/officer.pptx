# Some example file

file_in <- example_pptx("text_replace")
file_out <- tempfile(fileext = ".pptx")

# file_open(file_in)

x <- pptx_read(file_in)
x <- pptx_text_replace(x, "@" = ">>>", "{1}" = ":)")
print(x, target = file_out)

# file_open(file_out)
