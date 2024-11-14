library(here)
library(data.table)
library(ggplot2)
library(ggtext)
library(scales)
library(lubridate)
library(showtext)
# library(ragg)
library(svglite)
library(rvest)

Sys.setlocale("LC_TIME", "en_US.UTF-8")

fig_caption <- NULL # "&copy; Micka&euml;l '<i style='color:#21908CFF;'>Coeos</i>' Canouil"

font <- "Alegreya Sans"
font_add_google(font, font, regular.wt = 300)
showtext_auto()

theme_mc <- function(
  base_size = 11,
  base_family = "",
  base_line_size = base_size / 22,
  base_rect_size = base_size / 22,
  theme = "mc"
) {
  bc <- c("#333333", "#7F7F7F", "#FAFAFA")
  half_line <- base_size / 2
  ggplot2::theme(
    line = ggplot2::element_line(
      colour = bc[3],
      linewidth = base_line_size,
      linetype = 1,
      lineend = "butt"
    ),
    rect = ggplot2::element_rect(
      fill = bc[1],
      colour = bc[3],
      linewidth = base_rect_size,
      linetype = 1
    ),
    text = ggplot2::element_text(
      family = base_family,
      face = "plain",
      colour = bc[3],
      size = base_size,
      lineheight = 0.9,
      hjust = 0.5,
      vjust = 0.5,
      angle = 0,
      margin = ggplot2::margin(),
      debug = FALSE
    ),
    title = NULL,
    aspect.ratio = NULL,

    axis.title = NULL,
    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = half_line), vjust = 1),
    axis.title.x.top = ggplot2::element_text(margin = ggplot2::margin(b = half_line), vjust = 0),
    axis.title.x.bottom = NULL,
    axis.title.y = ggplot2::element_text(angle = 90, margin = ggplot2::margin(r = half_line), vjust = 1),
    axis.title.y.left = NULL,
    axis.title.y.right = ggplot2::element_text(angle = -90, margin = ggplot2::margin(l = half_line), vjust = 0),
    axis.text = ggplot2::element_text(size = ggplot2::rel(0.8), colour = bc[3]),
    axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 0.8 * half_line / 2), vjust = 1),
    axis.text.x.top = ggplot2::element_text(margin = ggplot2::margin(b = 0.8 * half_line / 2), vjust = 0),
    axis.text.x.bottom = NULL,
    axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 0.8 * half_line / 2), hjust = 1),
    axis.text.y.left = NULL,
    axis.text.y.right = ggplot2::element_text(margin = ggplot2::margin(l = 0.8 * half_line / 2), hjust = 0),
    axis.ticks = ggplot2::element_line(colour = bc[3]),
    axis.ticks.x = NULL,
    axis.ticks.x.top = NULL,
    axis.ticks.x.bottom = NULL,
    axis.ticks.y = NULL,
    axis.ticks.y.left = NULL,
    axis.ticks.y.right = NULL,
    axis.ticks.length = ggplot2::unit(half_line / 2, "pt"),
    axis.ticks.length.x = NULL,
    axis.ticks.length.x.top = NULL,
    axis.ticks.length.x.bottom = NULL,
    axis.ticks.length.y = NULL,
    axis.ticks.length.y.left = NULL,
    axis.ticks.length.y.right = NULL,
    axis.line = ggplot2::element_blank(),
    axis.line.x = NULL,
    axis.line.x.top = NULL,
    axis.line.x.bottom = NULL,
    axis.line.y = NULL,
    axis.line.y.left = NULL,
    axis.line.y.right = NULL,

    legend.background = ggplot2::element_rect(fill = bc[1], colour = NA),
    legend.margin = ggplot2::margin(half_line, half_line, half_line, half_line),
    legend.spacing = ggplot2::unit(2 * half_line, "pt"),
    legend.spacing.x = NULL,
    legend.spacing.y = NULL,
    legend.key = ggplot2::element_rect(fill = bc[1], colour = bc[3]),
    legend.key.size = ggplot2::unit(1.2, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.text = ggplot2::element_text(size = ggplot2::rel(0.8)),
    legend.text.align = NULL,
    legend.title = ggplot2::element_text(hjust = 0),
    legend.title.align = NULL,
    legend.position = "right",
    legend.direction = NULL,
    legend.justification = "center",
    legend.box = NULL,
    legend.box.just = NULL,
    legend.box.margin = ggplot2::margin(0, 0, 0, 0, "cm"),
    legend.box.background = ggplot2::element_blank(),
    legend.box.spacing = ggplot2::unit(2 * half_line, "pt"),

    panel.background = ggplot2::element_rect(fill = bc[1], colour = NA),
    panel.border = ggplot2::element_rect(fill = NA, colour = bc[3], linewidth = 0.5, linetype = "solid"),
    panel.spacing = ggplot2::unit(half_line, "pt"),
    panel.spacing.x = NULL,
    panel.spacing.y = NULL,
    panel.grid = ggplot2::element_line(colour = bc[2]),
    panel.grid.major = ggplot2::element_line(colour = bc[2]),
    panel.grid.minor = ggplot2::element_line(colour = bc[2], linewidth = ggplot2::rel(0.5)),
    panel.grid.major.x = NULL,
    panel.grid.major.y = NULL,
    panel.grid.minor.x = NULL,
    panel.grid.minor.y = NULL,
    panel.ontop = FALSE,

    plot.background = ggplot2::element_rect(colour = bc[1]),
    plot.title = ggplot2::element_text(
      size = ggplot2::rel(1.25),
      face = "bold",
      hjust = 0,
      vjust = 1,
      margin = ggplot2::margin(b = half_line)
    ),
    plot.title.position = "plot",
    plot.subtitle = ggplot2::element_text(
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
    plot.tag = ggplot2::element_text(size = ggplot2::rel(1.25), hjust = 0.5, vjust = 0.5),
    plot.tag.position = "topleft",
    plot.margin = ggplot2::margin(half_line, half_line, half_line, half_line),

    strip.background = ggplot2::element_rect(fill = bc[1], colour = bc[3]),
    strip.background.x = NULL,
    strip.background.y = NULL,
    strip.placement = "inside",
    strip.placement.x = NULL,
    strip.placement.y = NULL,
    strip.text = ggplot2::element_text(
      colour = bc[3],
      size = ggplot2::rel(0.8),
      margin = ggplot2::margin(0.8 * half_line, 0.8 * half_line, 0.8 * half_line, 0.8 * half_line)
    ),
    strip.text.x = NULL,
    strip.text.y = ggplot2::element_text(angle = -90),
    strip.switch.pad.grid = ggplot2::unit(half_line / 2, "pt"),
    strip.switch.pad.wrap = ggplot2::unit(half_line / 2, "pt"),

    complete = TRUE
  )
}
theme_set(theme_mc(base_size = 11, base_family = font))
theme_update(
  plot.title.position = "plot",
  plot.caption.position = "plot",
  plot.title = element_text(), # element_markdown(),
  plot.subtitle = element_markdown(face = "italic"),
  plot.caption = element_markdown(face = "italic"),
  axis.title.x = element_markdown(),
  axis.text.x = element_markdown(),
  axis.text.x.top = element_markdown(),
  axis.title.y = element_markdown(),
  axis.text.y = element_markdown()
)
options(
  ggplot2.discrete.colour = function(...) ggplot2::scale_colour_viridis_d(..., begin = 0.15, end = 0.85),
  ggplot2.discrete.fill = function(...) ggplot2::scale_fill_viridis_d(..., begin = 0.15, end = 0.85),
  ggplot2.continuous.colour = function(...) ggplot2::scale_colour_viridis_c(..., begin = 0.15, end = 0.85),
  ggplot2.continuous.fill = function(...) ggplot2::scale_fill_viridis_c(..., begin = 0.15, end = 0.85)
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
    month = sub("May.", "May", sprintf("%s.", as.character(month(date, label = TRUE, abbr = TRUE)))),
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
  j = month_start := n == max(n) & as.numeric(week) == max(sort(unique(as.numeric(week)))[1:2]),
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
    colour = "#FAFAFA66",
    fill = "white",
    alpha = 0.05,
    width = 0.8,
    height = 0.8
  ),
  geom_tile(
    data = ~ .x[month_num %% 2 == 1],
    show.legend = FALSE,
    colour = "#FAFAFA66",
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
    fill = "#21908CFF",
    alpha = 0.3
  ),
  geom_richtext(
    data = ~ .x[count != 0],
    colour = "#FAFAFA",
    na.rm = TRUE,
    family = font,
    fontface = "bold",
    size = 3.5,
    fill = NA,
    label.colour = NA
  ),
  scale_y_discrete(
    expand = expansion(add = 0.5),
    labels = function(x) sub("([[:alpha:]]{3}).*", "\\1.", x)
  ),
  scale_colour_viridis_c(),
  scale_fill_viridis_c(
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
    plot.caption = element_markdown(face = "italic", size = rel(0.90))
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
    colour = "Count",
    fill = "Count",
    x = NULL, # "Week Number",
    y = NULL # "Day"
  ) +
  facet_grid(rows = vars(year))

# Streak of movies seen in a movie theatre per week and years.
svglite(filename = "media/streak-years.svg", width = 8, height = 12)
print(all_years_streak_plot)
invisible(dev.off())

streak_data <- theatres_raw_data[
  j = date := as.IDate(date_time)
][
  i = J(date = seq(min(date), today(), by = "1 day")),
  on = "date"
][
  between(date, today() - days(as.numeric(grepl("02-29$", today()))) - months(12), today())
][
  date %in% head(sort(unique(date), decreasing = TRUE), 365)
][
  j = list(
    count = sum(!is.na(theatre)),
    month = sub("May.", "May", sprintf("%s.", as.character(month(date, label = TRUE, abbr = TRUE)))),
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
      "<b>%s movies seen</b> in a movie theatre <b>in the last year</b>.",
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
    colour = "#FAFAFA66",
    linewidth = 0.15,
    width = 0.8,
    height = 0.8,
    linejoin = "round"
  ) +
  geom_tile(
    data = ~ .x[which.max(N)],
    colour = "#FAFAFA66",
    fill = NA,
    linewidth = 1,
    width = 0.8,
    height = 0.8,
    linejoin = "round"
  ) +
  geom_richtext(
    colour = "#FAFAFA",
    na.rm = TRUE,
    family = font,
    fontface = "bold",
    size = 3.5,
    fill = NA,
    label.colour = NA
  ) +
  scale_x_discrete(
    labels = function(x) {
      ifelse(
        x %in% unique(count_data[which.max(N), month]),
        sprintf("<b>%s</b>", x),
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
        sprintf("<b>%s</b>", x),
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
    plot.caption = element_markdown(face = "italic", size = rel(0.90))
  ) +
  labs(
    caption = sprintf(
      "<b>%s movies seen</b> in <b>a movie theatre</b>.",
      format(sum(count_data[["N"]]), nsmall = 0, big.mark = ",")
    )
  )

# Counts of movies seen in a movie theatre per month and year.
svglite(filename = "media/counts.svg", width = 8, height = 6.5)
print(count_plot)
invisible(dev.off())

rvest::read_html("https://www.imdb.com/user/ur56341222/ratings") |>
  rvest::html_element(css = "li.ipc-inline-list__item") |>
  rvest::html_text() |>
  (function(x) {
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
      n, n, n, n
    )
  })() |>
  writeLines(con = "media/imdb.svg")
