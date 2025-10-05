library(here)
library(data.table)
library(ggplot2)
library(marquee)
library(scales)
library(lubridate)
library(systemfonts)
# library(ragg)
library(svglite)
library(rvest)

Sys.setlocale("LC_TIME", "en_US.UTF-8")

fig_caption <- NULL # "&copy; Micka&euml;l '<i style='color:#21908CFF;'>Coeos</i>' Canouil"

height_count_plot <- 8
height_streak_plot <- 13.5

theme_coeos <- function(
  base_size = 11,
  base_family = "Alegreya Sans",
  header_family = NULL,
  base_line_size = base_size / 22,
  base_rect_size = base_size / 22,
  ink = "#fafafa",
  paper = "#333333",
  accent = "#7f7f7f"
) {
  systemfonts::require_font(base_family)

  half_line <- base_size / 2

  # Use header_family if specified, otherwise use base_family
  if (is.null(header_family)) {
    header_family <- base_family
  }

  ggplot2::theme(
    line = ggplot2::element_line(
      colour = ink,
      linewidth = base_line_size,
      linetype = 1,
      lineend = "butt",
      linejoin = "round"
    ),
    rect = ggplot2::element_rect(
      fill = paper,
      colour = ink,
      linewidth = base_rect_size,
      linetype = 1,
      linejoin = "round"
    ),
    text = ggplot2::element_text(
      family = base_family,
      face = "plain",
      colour = ink,
      size = base_size,
      lineheight = 0.9,
      hjust = 0.5,
      vjust = 0.5,
      angle = 0,
      margin = ggplot2::margin(),
      debug = FALSE
    ),
    title = ggplot2::element_text(family = header_family),
    spacing = ggplot2::unit(half_line, "pt"),
    margins = ggplot2::margin_auto(half_line),
    point = ggplot2::element_point(
      colour = ink,
      shape = 19,
      fill = paper,
      size = (base_size / 11) * 1.5,
      stroke = base_line_size
    ),
    polygon = ggplot2::element_polygon(
      fill = paper,
      colour = ink,
      linewidth = base_rect_size,
      linetype = 1,
      linejoin = "round"
    ),
    geom = element_geom(
      ink = ink,
      paper = paper,
      accent = accent,
      linewidth = base_line_size,
      borderwidth = base_line_size,
      linetype = 1L,
      bordertype = 1L,
      family = base_family,
      fontsize = base_size,
      pointsize = (base_size / 11) * 1.5,
      pointshape = 19
    ),
    axis.line = ggplot2::element_blank(),
    axis.line.x = NULL,
    axis.line.x.top = NULL,
    axis.line.x.bottom = NULL,
    axis.line.y = NULL,
    axis.line.y.left = NULL,
    axis.line.y.right = NULL,
    axis.text = ggplot2::element_text(size = ggplot2::rel(0.8), colour = ink),
    axis.text.x = ggplot2::element_text(
      margin = ggplot2::margin(t = 0.8 * half_line / 2),
      vjust = 1
    ),
    axis.text.x.top = ggplot2::element_text(
      margin = ggplot2::margin(b = 0.8 * half_line / 2),
      vjust = 0
    ),
    axis.text.x.bottom = NULL,
    axis.text.y = ggplot2::element_text(
      margin = ggplot2::margin(r = 0.8 * half_line / 2),
      hjust = 1
    ),
    axis.text.y.left = NULL,
    axis.text.y.right = ggplot2::element_text(
      margin = ggplot2::margin(l = 0.8 * half_line / 2),
      hjust = 0
    ),
    axis.text.r = ggplot2::element_text(
      margin = ggplot2::margin(l = 0.8 * half_line / 2),
      hjust = 0.5
    ),
    axis.ticks = ggplot2::element_line(colour = ink),
    axis.ticks.x = NULL,
    axis.ticks.x.top = NULL,
    axis.ticks.x.bottom = NULL,
    axis.ticks.y = NULL,
    axis.ticks.y.left = NULL,
    axis.ticks.y.right = NULL,
    axis.ticks.length = ggplot2::rel(0.5),
    axis.ticks.length.x = NULL,
    axis.ticks.length.x.top = NULL,
    axis.ticks.length.x.bottom = NULL,
    axis.ticks.length.y = NULL,
    axis.ticks.length.y.left = NULL,
    axis.ticks.length.y.right = NULL,
    axis.minor.ticks.length = ggplot2::rel(0.75),
    axis.title = NULL,
    axis.title.x = ggplot2::element_text(
      margin = ggplot2::margin(t = half_line),
      vjust = 1
    ),
    axis.title.x.top = ggplot2::element_text(
      margin = ggplot2::margin(b = half_line),
      vjust = 0
    ),
    axis.title.x.bottom = NULL,
    axis.title.y = ggplot2::element_text(
      angle = 90,
      margin = ggplot2::margin(r = half_line),
      vjust = 1
    ),
    axis.title.y.left = NULL,
    axis.title.y.right = ggplot2::element_text(
      angle = -90,
      margin = ggplot2::margin(l = half_line),
      vjust = 0
    ),
    legend.background = ggplot2::element_rect(fill = paper, colour = NA),
    legend.spacing = ggplot2::rel(2),
    legend.spacing.x = NULL,
    legend.spacing.y = NULL,
    legend.margin = ggplot2::margin_auto(half_line),
    legend.key = ggplot2::element_rect(fill = paper, colour = ink),
    legend.key.size = ggplot2::unit(1.2, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.key.spacing = ggplot2::unit(half_line / 4, "pt"),
    legend.text = ggplot2::element_text(size = ggplot2::rel(0.8)),
    legend.text.align = NULL,
    legend.title = ggplot2::element_text(hjust = 0),
    legend.title.align = NULL,
    legend.ticks.length = ggplot2::rel(0.2),
    legend.position = "right",
    legend.direction = NULL,
    legend.justification = "center",
    legend.box = NULL,
    legend.box.just = NULL,
    legend.box.margin = ggplot2::margin_auto(0),
    legend.box.background = ggplot2::element_blank(),
    legend.box.spacing = ggplot2::unit(2 * half_line, "pt"),
    panel.background = ggplot2::element_rect(fill = paper, colour = NA),
    panel.border = ggplot2::element_rect(
      fill = NA,
      colour = ink,
      linewidth = 0.5,
      linetype = "solid"
    ),
    panel.spacing = ggplot2::unit(half_line, "pt"),
    panel.spacing.x = NULL,
    panel.spacing.y = NULL,
    panel.grid = ggplot2::element_line(colour = accent),
    panel.grid.major = ggplot2::element_line(linewidth = ggplot2::rel(0.60)),
    panel.grid.minor = ggplot2::element_line(linewidth = ggplot2::rel(0.30)),
    panel.grid.major.x = NULL,
    panel.grid.major.y = NULL,
    panel.grid.minor.x = NULL,
    panel.grid.minor.y = NULL,
    panel.ontop = FALSE,
    strip.background = ggplot2::element_rect(fill = paper, colour = ink),
    strip.background.x = NULL,
    strip.background.y = NULL,
    strip.clip = "inherit",
    strip.placement = "inside",
    strip.placement.x = NULL,
    strip.placement.y = NULL,
    strip.text = ggplot2::element_text(
      colour = ink,
      size = ggplot2::rel(0.8),
      margin = ggplot2::margin_auto(0.8 * half_line)
    ),
    strip.text.x = NULL,
    strip.text.y = ggplot2::element_text(angle = -90),
    strip.text.y.left = ggplot2::element_text(angle = 90),
    strip.switch.pad.grid = ggplot2::unit(half_line / 2, "pt"),
    strip.switch.pad.wrap = ggplot2::unit(half_line / 2, "pt"),
    plot.background = ggplot2::element_rect(colour = paper),
    plot.title = ggplot2::element_text(
      family = header_family,
      size = ggplot2::rel(1.25),
      face = "bold",
      hjust = 0,
      vjust = 1,
      margin = ggplot2::margin(b = half_line)
    ),
    plot.title.position = "plot",
    plot.subtitle = ggplot2::element_text(
      family = header_family,
      size = ggplot2::rel(1),
      face = "italic",
      hjust = 0,
      vjust = 1,
      margin = ggplot2::margin(b = half_line)
    ),
    plot.caption = ggplot2::element_text(
      size = ggplot2::rel(0.75),
      face = "italic",
      hjust = 1,
      vjust = 1,
      margin = ggplot2::margin(t = half_line)
    ),
    plot.caption.position = "plot",
    plot.tag = ggplot2::element_text(
      size = ggplot2::rel(1.25),
      hjust = 0.5,
      vjust = 0.5
    ),
    plot.tag.position = "topleft",
    plot.margin = ggplot2::margin_auto(half_line),

    palette.colour.discrete = function(...) {
      ggplot2::scale_colour_viridis_d(..., begin = 0.15, end = 0.85)
    },
    palette.fill.discrete = function(...) {
      ggplot2::scale_fill_viridis_d(..., begin = 0.15, end = 0.85)
    },
    palette.colour.continuous = function(...) {
      ggplot2::scale_colour_viridis_c(..., begin = 0.15, end = 0.85)
    },
    palette.fill.continuous = function(...) {
      ggplot2::scale_fill_viridis_c(..., begin = 0.15, end = 0.85)
    },

    complete = TRUE
  )
}

set_theme(theme_coeos(base_size = 11))

update_theme(
  plot.title.position = "plot",
  plot.caption.position = "plot",
  plot.title = element_marquee(),
  plot.subtitle = element_marquee(style = classic_style(italic = TRUE)),
  plot.caption = element_marquee(style = classic_style(italic = TRUE)),
  axis.title.x = element_marquee(),
  axis.text.x = element_marquee(),
  axis.text.x.top = element_marquee(),
  axis.title.y = element_marquee(),
  axis.text.y = element_marquee()
)

theatres_raw_data <- fread(file = here("data", "theatres.csv"))

theatres_complete_data <- theatres_raw_data[
  j = date := as.IDate(date_time)
][
  J(date = seq(min(date), today(), by = "1 day")),
  on = "date"
][
  j = c("month", "year", "wday") := list(
    as.character(month(date, label = TRUE, abbr = FALSE)),
    year(date),
    wday(date, label = TRUE, abbr = FALSE, week_start = 1)
  )
][
  j = year_complete := uniqueN(date) >= 365,
  by = "year"
]

all_years_streak_data <- theatres_complete_data[
  j = list(
    count = sum(!is.na(theatre)),
    month = sub(
      "May.",
      "May",
      sprintf("%s.", as.character(month(date, label = TRUE, abbr = TRUE)))
    ),
    year = year,
    month_num = month(date),
    wday = wday
  ),
  by = "date"
][
  j = week := fifelse(
    isoweek(date) == 1 & month_num == 12,
    max(isoweek(date)) + 1,
    fifelse(isoweek(date) > 5 & month_num == 1, 0, isoweek(date))
  ),
  by = "year"
][
  i = year %in% unique(year[week == 0]),
  j = week := week + 1
][
  j = week := factor(sprintf("%02d", week), ordered = TRUE)
][
  j = unique(.SD)
][
  j = week := as.numeric(week)
]

all_week_month_breaks <- all_years_streak_data[
  i = wday == "Monday" & year > 2013,
  j = list(week = week, n = .N),
  by = c("month_num", "month")
][
  j = month_start := n == max(n) &
    as.numeric(week) == max(sort(unique(as.numeric(week)))[1:2]),
  by = c("month_num", "month")
][
  i = (month_start),
  j = unique(.SD),
  .SDcols = c("month_num", "month", "week", "n")
][
  order(month_num, week)
][
  j = week := as.numeric(week)
]

streak_geoms <- list(
  geom_tile(
    data = ~ .x[month_num %% 2 == 0],
    show.legend = FALSE,
    colour = "#fafafa66",
    fill = "white",
    alpha = 0.05,
    width = 0.8,
    height = 0.8
  ),
  geom_tile(
    data = ~ .x[month_num %% 2 == 1],
    show.legend = FALSE,
    colour = "#fafafa66",
    fill = NA,
    alpha = 0.05,
    width = 0.8,
    height = 0.8
  ),
  geom_tile(
    data = ~ .x[
      between(date, ymd("2020-03-16"), ymd("2020-06-21")) |
        between(date, ymd("2020-11-02"), ymd("2021-05-18"))
    ],
    fill = "#21908cff",
    alpha = 0.3
  ),
  geom_marquee(
    data = ~ .x[count != 0],
    colour = "#fafafa",
    na.rm = TRUE,
    style = classic_style(weight = "bold", baseline = -1),
    size = 2
  ),
  scale_y_discrete(
    expand = expansion(add = 0.5),
    labels = function(x) sub("([[:alpha:]]{3}).*", "\\1.", x)
  ),
  scale_colour_viridis_c(name = "Count"),
  scale_fill_viridis_c(
    name = "Count",
    begin = 0.25,
    end = 1,
    limits = c(1, NA),
    na.value = NA
  ),
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    plot.caption = element_marquee(
      style = classic_style(italic = TRUE),
      size = rel(0.90)
    )
  )
)

all_years_streak_plot <- ggplot(data = all_years_streak_data) +
  aes(
    x = week,
    y = factor(wday, levels = rev(levels(wday))),
    label = count
  ) +
  streak_geoms +
  scale_x_continuous(
    expand = expansion(add = 0.25),
    breaks = all_week_month_breaks[["week"]],
    labels = all_week_month_breaks[["month"]]
  ) +
  labs(
    title = sprintf(
      "Streak of Movies Seen in a Theatre (%s)",
      format(sum(all_years_streak_data[["count"]]), big.mark = ",")
    ),
    caption = fig_caption,
    x = NULL, # "Week Number",
    y = NULL # "Day"
  ) +
  facet_grid(rows = vars(year))

# Streak of movies seen in a movie theatre per week and years.
svglite(
  filename = "media/streak-years.svg",
  width = 8,
  height = height_streak_plot
)
print(all_years_streak_plot)
invisible(dev.off())

streak_data <- theatres_raw_data[
  j = date := as.IDate(date_time)
][
  i = J(date = seq(min(date), today(), by = "1 day")),
  on = "date"
][
  between(
    date,
    today() - days(as.numeric(grepl("02-29$", today()))) - months(12),
    today()
  )
][
  date %in% head(sort(unique(date), decreasing = TRUE), 365)
][
  j = list(
    count = sum(!is.na(theatre)),
    month = sub(
      "May.",
      "May",
      sprintf("%s.", as.character(month(date, label = TRUE, abbr = TRUE)))
    ),
    year = year(date),
    month_num = month(date),
    wday = wday(date, label = TRUE, abbr = FALSE, week_start = 1)
  ),
  by = "date"
][
  i = order(date)
][
  i = wday %in% "Monday",
  j = x_week := seq_len(.N)
][
  j = x_week := nafill(x = x_week, type = "locf")
][
  i = order(date),
  j = `:=`(
    "x" = fifelse(test = is.na(x_week), 0, x_week),
    "y" = factor(wday, levels = rev(levels(wday)))
  )
]

week_month_breaks <- streak_data[
  j = .SD[.N == 7],
  by = c("year", "month", "x")
][
  j = list(week = max(unique(x)[1:2], na.rm = TRUE)),
  by = c("year", "month")
]

streak_plot <- ggplot(data = streak_data) +
  aes(x = x, y = y, label = count) +
  streak_geoms +
  scale_x_continuous(
    expand = expansion(add = 0.25),
    breaks = week_month_breaks[["week"]],
    labels = week_month_breaks[["month"]],
    position = "top"
  ) +
  labs(
    caption = sprintf(
      "**%s movies seen** in a movie theatre **in the last year**.",
      format(sum(streak_data[["count"]]), nsmall = 0, big.mark = ",")
    )
  )

# Streak of movies seen in a movie theatre per week and days for the last year.
svglite(filename = "media/streak.svg", width = 8, height = 1.75)
print(streak_plot)
invisible(dev.off())

count_data <- theatres_raw_data[
  j = .N,
  by = list(
    "year" = year(date_time),
    month = month(date_time, label = TRUE, abbr = FALSE)
  )
][
  i = J(year = rep(unique(year), each = 12), month = unique(month)),
  on = c("year", "month")
][
  i = is.na(N),
  j = N := 0
][
  j = year := factor(year, levels = rev(unique(year)))
]
count_plot <- ggplot(data = count_data) +
  aes(x = month, y = year, label = N) +
  geom_tile(
    mapping = aes(fill = N),
    alpha = 0.3,
    colour = "#fafafa66",
    linewidth = 0.15,
    width = 0.8,
    height = 0.8,
    linejoin = "round"
  ) +
  geom_tile(
    data = ~ .x[which.max(N)],
    colour = "#fafafa66",
    fill = NA,
    linewidth = 1,
    width = 0.8,
    height = 0.8,
    linejoin = "round"
  ) +
  geom_marquee(
    colour = "#fafafa",
    na.rm = TRUE,
    style = classic_style(weight = "bold", baseline = -1),
    size = 3.5
  ) +
  scale_x_discrete(
    labels = function(x) {
      ifelse(
        x %in% unique(count_data[which.max(N), month]),
        sprintf("**%s**", x),
        x
      )
    },
    expand = expansion(add = 0.5),
    position = "top"
  ) +
  scale_y_discrete(
    labels = function(x) {
      ifelse(
        x %in% unique(count_data[which.max(N), year]),
        sprintf("**%s**", x),
        x
      )
    },
    expand = expansion(add = 0.5)
  ) +
  scale_fill_viridis_c(
    begin = 0,
    end = 0.80,
    limits = c(1, NA),
    na.value = NA
  ) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    plot.caption = element_marquee(
      style = classic_style(italic = TRUE),
      size = rel(0.90)
    )
  ) +
  labs(
    caption = sprintf(
      "**%s movies seen** in **a movie theatre**.",
      format(sum(count_data[["N"]]), nsmall = 0, big.mark = ",")
    )
  )

# Counts of movies seen in a movie theatre per month and year.
svglite(filename = "media/counts.svg", width = 8, height = height_count_plot)
print(count_plot)
invisible(dev.off())

rvest::read_html("https://www.imdb.com/user/ur56341222/ratings") |>
  rvest::html_elements(css = "li.ipc-inline-list__item") |>
  rvest::html_text() |>
  (function(x) {
    x <- grep(" titles", x, value = TRUE)
    n <- format(as.numeric(gsub("([0-9]+) .*", "\\1", x)), big.mark = ",")
    sprintf(
      paste(
        '<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" width="37" height="20" role="img" aria-label="%s">',
        '<title>%s</title>',
        '<linearGradient id="s" x2="0" y2="100%%">',
        '<stop offset="0" stop-color="#bbb" stop-opacity=".1"/><stop offset="1" stop-opacity=".1"/>',
        '</linearGradient>',
        '<clipPath id="r"><rect width="37" height="20" rx="3" fill="#fff"/></clipPath>',
        '<g clip-path="url(#r)">',
        '<rect width="0" height="20" fill="#009e73"/>',
        '<rect x="0" width="37" height="20" fill="#009e73"/>',
        '<rect width="37" height="20" fill="url(#s)"/>',
        '</g>',
        '<g fill="#fff" text-anchor="middle" font-family="Verdana,Geneva,DejaVu Sans,sans-serif" text-rendering="geometricPrecision" font-size="110">',
        '<text aria-hidden="true" x="185" y="150" fill="#010101" fill-opacity=".3" transform="scale(.1)" textLength="270">%s</text>',
        '<text x="185" y="140" transform="scale(.1)" fill="#fff" textLength="270">%s</text>',
        '</g>',
        '</svg>'
      ),
      n,
      n,
      n,
      n
    )
  })() |>
  writeLines(con = "media/imdb.svg")
