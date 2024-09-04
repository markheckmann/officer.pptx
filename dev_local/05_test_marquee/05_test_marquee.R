library(ggplot2)
library(marquee)

g <- ggplot(mtcars, aes(disp, mpg, label = rownames(mtcars))) +
  geom_marquee(size = 8) +
  geom_text(size = 8, nudge_y = -.2, color="red") +
  coord_cartesian(xlim = c(125, 180), ylim= c(15, 25))

png("dev_local/05_test_marquee/test_02.png", )
print(g)
dev.off()
ggsave("dev_local/05_test_marquee/test_01.png", g)
