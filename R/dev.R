library(data.table)
library(ggplot2)
library(gganimate)

font <- "Alegreya Sans"
# sysfonts::font_add_google(font, font, regular.wt = 300)
# showtext::showtext_auto()
source(here::here("R", "ggplot2-mc.R"))
ggplot2::theme_set(theme_mc(base_size = 11, base_family = font))

ggplot2::theme_update(
  plot.title.position = "plot",
  plot.caption.position = "plot",
  plot.title = ggtext::element_markdown(),
  plot.subtitle = ggtext::element_markdown(face = "italic"),
  plot.caption = ggtext::element_markdown(face = "italic"),
  axis.title.x = ggtext::element_markdown(),
  axis.text.x = ggtext::element_markdown(),
  axis.text.x.top = ggtext::element_markdown(),
  axis.title.y = ggtext::element_markdown(),
  axis.text.y = ggtext::element_markdown()
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

p <- ggplot(data = dt[between(Year, 2012, 2022)]) +
  aes(x = `Runtime (mins)`, y = `IMDb Rating`) +
  geom_density2d_filled(alpha = 0.5) +
  # geom_point(size = 0.25) +
  # geom_smooth(method = "lm", colour = "white", linetype = 2) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 10), breaks = 0:10, expand = c(0, 0)) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank()
  ) +
  transition_time(Year)

animate(p, renderer = gifski_renderer(file = "media/animation.gif", width = 1600, height = 1200))

tar_read(all_years_streak_plot)
