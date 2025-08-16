file_pptx <- tempfile(fileext = ".pptx")
file_png <- tempfile(fileext = ".png")

# sample image
png(file_png, bg = "transparent")
  plot(1:10, col = "red", pch = 16, cex = 3)
  title("PNG")
dev.off()

x <- read_pptx() |> layout_default("Title and Content")

# default settings
x <- x |> add_slide(title = "img() - defaults", body = img(file_png))

# img() settings
x <- x |> add_slide(
  title = "img() - with settings",
  body = img(file_png, h_just = 0, line = "blue", background = "lightblue")
)

# settings via ph_with()
x <- x |>
  add_slide(title = "img() - with settings") |>
  ph_with(img(file_png), "body",
    line = sp_line(lwd = 4, color = "red", lty = "dash"),
    background = "#ffdddd", rotation = 90
  )

# via phs_with() ---
x <- x |>
  add_slide() |>
  phs_with(fullsize = img(file_png, scale = .5, v_just = 1, h_just = 0)) |>
  phs_with(fullsize = img(file_png, scale = .5, v_just = 0, h_just = 0))

# More img() options
x <- x |> add_slide(body = img(file_png, fit = "inside", scale = .65, rotation = 45))

# possible to add mutiple times at same ph position
x <- x |> add_slide() |> phs_with(
  body = img(file_png, fit = "inside", scale = .5, x_offset = -1),
  body = img(file_png, fit = "inside", scale = .5),
  body = img(file_png, fit = "inside", scale = .5, x_offset = 1)
)

print(x, file_pptx)
file_open(file_pptx) # may not work on all systems
