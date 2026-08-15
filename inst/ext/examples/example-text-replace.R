file_in <- example_pptx("text_replace")
file_out <- tempfile(fileext = ".pptx")

# file_open(file_in)

x <- read_pptx(file_in)
x <- text_replace(x, "{1}" = ">>>", "@" = ":)")
print(x, target = file_out)

# file_open(file_out)
