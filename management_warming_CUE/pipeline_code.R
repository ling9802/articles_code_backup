# Shared plotting objects used by code.R.
# Unused analysis functions and helper objects were removed.

cols1 <- c("royalblue4", "red4")

bg.cols <- c(
  "Conventional" = "grey",
  "Conservation" = "#a76825"
)

my_theme <- theme_cowplot() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    strip.text = element_text(size = 14)
  )

background_data <- data.frame(
  Tillage = c("Conventional", "Conservation"),
  xmin = c(0.5, 1.5),
  xmax = c(1.5, 2.5),
  fill_color = c("grey", "#a76825")
)
