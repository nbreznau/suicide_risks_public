library(ggplot2)

# Set default colour scales -----------------------------------------------

options(ggplot2.discrete.colour = unname(palette.colors()[2:8]), # Okabe-Ito palette
        ggplot2.discrete.fill = unname(palette.colors()[2:8]),
        ggplot2.continuous.colour = function(...) {
          scale_colour_viridis_c(option = "magma", ...)
        },
        ggplot2.continuous.fill = function(...) {
          scale_fill_viridis_c(option = "magma", ...)
        })

# Set theme ---------------------------------------------------------------

theme_fira <- function(base_size = 11.5) {
  theme_minimal(
    base_size = base_size,
    base_family = "Fira Code"
  ) +
    theme(
      axis.text = element_text(size = rel(.9)),
      axis.title = element_text(
        family = "Fira Sans Medium",
        hjust = 1
      ),
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.title = element_text(family = "Fira Sans"),
      legend.margin = margin(base_size / 3, base_size / 3, base_size / 3, base_size / 3),
      legend.position = "top",
      panel.grid = element_line(colour = "#cccccc", size = .2),
      panel.grid.minor = element_blank(),
      panel.spacing = unit(base_size * 1.3, "pt"),
      plot.title = element_text(
        family = "Fira Sans Medium",
        size = rel(1.5),
        lineheight = 1.15,
        margin = unit(c(0, 0, base_size * 1.3, 0), "pt"),
      ),
      plot.subtitle = element_text(
        family = "Fira Sans",
        size = rel(1.05),
        lineheight = 1.15,
        margin = unit(c(-base_size * 0.44, 0, base_size * 1.3, 0), "pt")
      ),
      plot.caption = element_text(
        family = "Fira Sans",
        lineheight = 1.15,
        margin = unit(c(base_size * 1.3, 0, 0, 0), "pt")
      ),
      plot.tag = element_text(family = "Fira Sans"),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.margin = margin(30, 30, 30, 30),
      strip.text = element_text(
        family = "Fira Sans Medium",
        size = rel(1.05),
        hjust = 0
      )
    )
}

theme_set(theme_fira())

# Function to specify grid lines ------------------------------------------

panel_grid <- function(grid = "XY", on_top = FALSE,
                       grid_colour = "#cccccc", grid_colour_top = "#ffffff") {
  ret <- theme(panel.ontop = on_top)
  if (grid == TRUE || is.character(grid)) {
    if (on_top == TRUE)
      grid_col <- grid_colour_top
    else
      grid_col <- grid_colour
    ret <- ret + theme(panel.grid = element_line(colour = grid_col,
                                                 size = .2))
    ret <- ret + theme(panel.grid.major = element_line(colour = grid_col,
                                                       size = .2))
    ret <- ret + theme(panel.grid.major.x = element_line(colour = grid_col,
                                                         size = .2))
    ret <- ret + theme(panel.grid.major.y = element_line(colour = grid_col,
                                                         size = .2))
    ret <- ret + theme(panel.grid.minor = element_line(colour = grid_col,
                                                       size = .2))
    ret <- ret + theme(panel.grid.minor.x = element_line(colour = grid_col,
                                                         size = .2))
    ret <- ret + theme(panel.grid.minor.y = element_line(colour = grid_col,
                                                         size = .2))
    if (is.character(grid)) {
      if (!grepl("X", grid))
        ret <- ret + theme(panel.grid.major.x = element_blank())
      if (!grepl("Y", grid))
        ret <- ret + theme(panel.grid.major.y = element_blank())
      if (!grepl("x", grid))
        ret <- ret + theme(panel.grid.minor.x = element_blank())
      if (!grepl("y", grid))
        ret <- ret + theme(panel.grid.minor.y = element_blank())
      if (grid != "ticks") {
        ret <- ret + theme(axis.ticks = element_blank())
        ret <- ret + theme(axis.ticks.x = element_blank())
        ret <- ret + theme(axis.ticks.y = element_blank())
      } else {
        ret <- ret + theme(axis.ticks = element_line(size = .2))
        ret <- ret + theme(axis.ticks.x = element_line(size = .2))
        ret <- ret + theme(axis.ticks.y = element_line(size = .2))
        ret <- ret + theme(axis.ticks.length = grid::unit(4, "pt"))
      }
    }
  } else {
    ret <- theme(panel.ontop = FALSE)
    ret <- ret + theme(panel.grid = element_blank())
    ret <- ret + theme(panel.grid.major = element_blank())
    ret <- ret + theme(panel.grid.major.x = element_blank())
    ret <- ret + theme(panel.grid.major.y = element_blank())
    ret <- ret + theme(panel.grid.minor = element_blank())
    ret <- ret + theme(panel.grid.minor.x = element_blank())
    ret <- ret + theme(panel.grid.minor.y = element_blank())
  }
  ret
}

# Function to show unit only for top axis label ---------------------------

label_x_unit <- function(x, unit) {
  if_else(x == x[which.max(x)],
          paste0(x, unit),
          as.character(x))
}

label_y_unit <- function(y, unit) {
  if_else(y == y[which.max(y)],
          paste0(y, unit),
          paste0(y, strrep(" ", nchar(unit))))
}
