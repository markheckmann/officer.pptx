file_in <- example_pptx("text_replace")
x <- read_pptx(file_in)
x <- text_replace(x, "{1}" = ">>>", "@" = ":)", verbose = 0)

# Retrieve the replacement log
text_replace_log(x)
