library(data.table)
library(ggplot2)

font <- "Alegreya Sans"
# sysfonts::font_add_google(font, font, regular.wt = 300)
# showtext::showtext_auto()
source(here::here("R", "ggplot2-mc.R"))
ggplot2::theme_set(theme_mc(base_size = 11, base_family = font))

ggplot2::theme_update(
  plot.title.position = "plot",
  plot.caption.position = "plot",
  plot.title = ggplot2::element_text(),
  plot.subtitle = ggplot2::element_text(face = "italic"),
  plot.caption = ggplot2::element_text(face = "italic"),
  axis.title.x = ggplot2::element_text(),
  axis.text.x = ggplot2::element_text(),
  axis.text.x.top = ggplot2::element_text(),
  axis.title.y = ggplot2::element_text(),
  axis.text.y = ggplot2::element_text(),
  panel.grid = ggplot2::element_blank(),
  panel.border = ggplot2::element_blank()
)

options(
  ggplot2.discrete.colour = function(...) ggplot2::scale_colour_viridis_d(..., begin = 0.15, end = 0.85),
  ggplot2.discrete.fill = function(...) ggplot2::scale_fill_viridis_d(..., begin = 0.15, end = 0.85),
  ggplot2.continuous.colour = function(...) ggplot2::scale_colour_viridis_c(..., begin = 0.15, end = 0.85),
  ggplot2.continuous.fill = function(...) ggplot2::scale_fill_viridis_c(..., begin = 0.15, end = 0.85)
)

update_geom_defaults("point", list(colour = "#FAFAFA"))
update_geom_defaults("boxplot", list(colour = "#FAFAFA", fill = "#333333"))

dt <- merge(
  x = fread("data/ratings.csv"),
  y = fread("data/theatres.csv"),
  by.x = "Const",
  by.y = "imdb_id",
  all.y = TRUE
)

## Boxplot Ratings
ggplot(
  data = dt[
    j = strsplit(Genres, ", "),
    by = setdiff(names(dt), "Genres")
  ][
    !is.na(V1)
  ][
    j = median := median(`IMDb Rating`, na.rm = TRUE),
    by = "V1"
  ][
    j = V1 := factor(
      x = V1,
      levels = unique(V1[order(median)])
    )
  ]
) +
 aes(x = `IMDb Rating`, y = V1) +
 geom_boxplot() +
 scale_x_continuous(limits = c(0, 10), breaks = 0:10, expand = c(0, 0))

ggplot(
  data = dt[
    theatre == "LILLE"
  ][
    j = median := median(`IMDb Rating`, na.rm = TRUE),
    by = "room"
  ][
    j = room := factor(
      x = sprintf("%02d", room),
      levels = unique(sprintf("%02d", room)[order(median)])
    )
  ]
) +
  aes(
    x = `IMDb Rating`,
    y = room
  ) +
  geom_boxplot() +
  scale_x_continuous(limits = c(0, 10), breaks = 0:10, expand = c(0, 0))

## Word Cloud
library(ggwordcloud)
update_geom_defaults(ggwordcloud:::GeomTextWordcloud, list(colour = "#FAFAFA"))
ggplot(
  data = dt[
    j = strsplit(Genres, ", "),
    by = setdiff(names(dt), "Genres")
  ][
    !is.na(V1)
  ][
    j = V2 := .N,
    by = "V1"
  ][j = unique(.SD), .SDcols = c("V1", "V2")]
) +
  aes(label = V1, size = V2) +
  geom_text_wordcloud()

ggplot(
  data = dt[
    i = theatre %in% "LILLE",
    j = .N,
    by = "room"
  ][]
) +
  aes(label = room, size = N, colour = N) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 10)

## Animate
library(gganimate)
library(rayshader)
p <- ggplot(data = dt[between(Year, 2012, 2022)]) +
  aes(x = `Runtime (mins)`, y = `IMDb Rating`) +
  stat_density_2d(
    mapping = aes(fill = stat(nlevel)), 
    geom = "polygon",
    bins = 50,
    contour = TRUE
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 10), breaks = 0:10, expand = c(0, 0)) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank()
  )
plot_gg(p, width = 5, height = 5, raytrace = FALSE, preview = TRUE)


ibrary(gganimate)
p <- ggplot(data = dt[between(Year, 2012, 2022)]) +
  aes(x = `Runtime (mins)`, y = `IMDb Rating`) +
  stat_density_2d(
    mapping = aes(fill = after_stat(density)),
    geom = "raster",
    # alpha = 0.5,
    contour = FALSE,
    n = 500
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 10), breaks = 0:10, expand = c(0, 0)) +
  scale_fill_gradient(
    low = "#333333",
    high = "#fafafa",
    na.value = "#333333",
    limits = c(0, NA)
  ) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank()
  )

animate(
  plot = p + transition_time(Year),
  renderer = gifski_renderer(file = "media/animation.gif", width = 1600, height = 1200),
  device = "ragg_png"
)


## 3D
library(rayshader)
library(lubridate)
p <- tar_read(all_years_streak_plot) +
  facet_grid(rows = vars(year))
plot_gg(
  ggobj = p,
  width = 8,
  height = 12,
  raytrace = FALSE,
  preview = FALSE,
  # zoom = 0.55,
  phi = 30,
  invert = TRUE
)
render_snapshot()
