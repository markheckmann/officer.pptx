file_in <- example_pptx("text_replace")
x <- read_pptx(file_in)
x <- text_replace(x, "{1}" = ">>>", "@" = ":)", verbose = 0)

# Assert that "{1}" was replaced at least once
x |> text_replace_expect("{1}", min = 1)

# Assert exact count on a specific slide
x |> text_replace_expect("@", min = 1, slide_idx = 1)
