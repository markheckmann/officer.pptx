devtools::load_all()

library(officer)
library(grid)
library(ggplot2)
library(ggtext)
library(stringr)

# ggplot2 --------------------------------------------------------

path <- "/Users/markheckmann/Library/CloudStorage/SynologyDrive-DPG-Modellvorhaben/02 Arbeitspakete Modellvorhaben/05 Arbeitspaket 5_Strukturaufbau und Bedarfsanalyse/02 Bedarfsanalyse Beschäftigte/Mitarbeiterbefragung/Auswertungen/Präventionsrat/Präventionsrat V09 2024-05-27.pptx"
path <- "/Users/markheckmann/Library/CloudStorage/SynologyDrive-DPG-Modellvorhaben/02 Arbeitspakete Modellvorhaben/05 Arbeitspaket 5_Strukturaufbau und Bedarfsanalyse/02 Bedarfsanalyse Beschäftigte/Mitarbeiterbefragung/Auswertungen/Präventionsrat/Vorlage Präventionsrat V08.pptx"
# path <- test_file("testdata_03_draw")
# annotate_base(path, output_file = "dev_local/03_slide_draw/test.pptx")
x <- pptx_read(path)
pptx_slide_draw(x, 1)

# file_open(path)
# plot_layout_properties(x, "Title Slide", "Office Theme")
#
# s <- slide_size(x)
# h <- s$height
# w <- s$width
#
# layout <- "Title Slide"
# master <- "Office Theme"
# dat <- layout_properties(x, layout, master)

# 1. get all shapes (todo: images, placeholders) on slide
# 2. draw bouding boxes and add text (with name, id etc.)
pptx_slide_draw <- function(x, slide_idx, highlight = NULL) {

  df_shapes <- pptx_shapes_info(x, slide_idx = slide_idx)
  # df_images <- pptx_images_info(x, slide_idx = slide_idx)

  df <- bind_rows(df_shapes) #df_images
  # layout_summary(x)
  # df <- df |> dplyr::filter(slide_idx == slide_idx)
  s <- slide_size(x)
  h <- s$height
  w <- s$width
  ratio <- h / w

  df <- df |> mutate(
    text_cleaned = str_trim(text) |> str_remove(fixed("+")),
    width_npc = unit(width / w, "npc"),
    height_npc = unit(height / h, "npc")
  )

  p <- ggplot(df) +
    geom_rect(aes(xmin = left, xmax = left + width, ymin = h - (top + height), ymax = h - top),
              colour = "grey", linetype = "dashed", fill = "white") +
    coord_fixed(ratio = 1) +
    scale_x_continuous(limits = c(0, w), expand = expansion(), breaks = scales::pretty_breaks()) +
    scale_y_continuous(limits = c(0, h), expand = expansion()) +
    theme(panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
          plot.title = element_text(colour = "grey")) +
    labs(x=NULL, y =NULL) +
    ggtitle(paste("Slide", slide_idx))


  # one textbox for each shape
  df_w_coords <- df #|> filter(!ph)
  rows <- seq_len(nrow(df_w_coords))
  geoms <- list()
  for (row in rows) {
    ss <- df_w_coords[row, ]
    geoms[[row]] <- geom_textbox(aes(label = paste(name, ":", text), x = left, y = h - top), data = ss,
                                 hjust =0, vjust =1,  width = ss$width_npc, height = ss$height_npc, box.r = unit(0, "mm"))
  }
  for (geom in geoms) {
    p <- p + geom
  }
  p
}




#
# s <- slide_size(x)
# h <- s$height
# w <- s$width
#
# layout <- "Title Slide"
# master <- "Office Theme"
# dat <- layout_properties(x, layout, master)
#
# dat |> dplyr::select()
# library(ggplot2)
# ratio <- s$height / s$width
# ggplot(dat) +
#   geom_rect(aes(xmin = w - offx, ymin = h - offy, xmax = w - (offx + cx), ymax = h- (offy + cy))) +
#   coord_fixed(ratio = ratio) +
#   scale_x_continuous(limits = c(0, w), expand = expansion()) +
#   scale_y_continuous(limits = c(0, h), expand = expansion())
#
#
#
# plot_layout_properties <- function (x, layout = NULL, master = NULL, labels = TRUE) {
#   dat <- layout_properties(x, layout = layout, master = master)
#   if (length(unique(dat$name)) != 1) {
#     stop("one single layout need to be choosen")
#   }
#   # vp_main <- viewport()
#   # s <- slide_size(x)
#   # hu <- unit(s$height, "inches")
#   # wu <- unit(s$width, "inches")
#   # vp <- viewport(width = wu, height = hu, )
#   # box <- rectGrob(vp = vp, gp = gpar(col = "blue", lwd = 3))
#   # grid.newpage()
#   # grid.draw(box)
#
#
#
#   h <- s$height
#   w <- s$width
#   offx <- dat$offx
#   offy <- dat$offy
#   cx <- dat$cx
#   cy <- dat$cy
#   if (labels)
#     labels <- dat$ph_label
#   else {
#     labels <- dat$type[order(as.integer(dat$id))]
#     rle_ <- rle(labels)
#     labels <- sprintf("type: '%s' - id: %.0f", labels, unlist(lapply(rle_$lengths, seq_len)))
#   }
#   plot(x = c(0, w), y = -c(0, h), asp = 1, type = "n", axes = FALSE,
#        xlab = "x (inches)", ylab = "y (inches)")
#   rect(xleft = offx, xright = offx + cx, ybottom = -offy, ytop = -(offy + cy))
#   rect()
#   text(x = offx + cx/2, y = -(offy + cy/2), labels = labels,
#        cex = 0.5, col = "red")
#   box()
#
#
#
#
#
#
#
# }

