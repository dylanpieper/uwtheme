#' @importFrom shiny tags div p
#' @importFrom bslib bs_theme bs_add_rules font_google page_navbar nav_panel
#' @importFrom sass sass_file
#' @importFrom base64enc base64encode
NULL

# Internal: Read and encode SVG as base64
read_svg_as_base64 <- function(filepath) {
  if (file.exists(filepath)) {
    svg_raw <- readBin(filepath, "raw", file.info(filepath)$size)
    paste0("data:image/svg+xml;base64,", base64enc::base64encode(svg_raw))
  } else {
    NULL
  }
}

#' UW-Madison Theme
#'
#' A bslib theme with UW-Madison branding including Badger Red accents
#' and Red Hat typography.
#'
#' @return A bslib theme object
#' @export
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(bslib)
#'   ui <- page_navbar(
#'     title = "My App",
#'     theme = uw_theme()
#'   )
#' }
uw_theme <- function() {
  scss_path <- system.file("www/uw-theme.scss", package = "uwtheme")

  theme <- bslib::bs_theme(
    version = 5,
    bg = "#ffffff",
    fg = "#282728",
    primary = "#c5050c",
    base_font = bslib::font_google("Red Hat Text"),
    heading_font = bslib::font_google("Red Hat Display")
  )

  if (nzchar(scss_path)) {
    theme <- bslib::bs_add_rules(theme, sass::sass_file(scss_path))
  }

  theme
}

#' Create UW-Madison navbar title with logo
#'
#' @param title Character string for the title text
#' @param logo_height Height of the logo (e.g., "55px")
#' @return A tags$span element with logo and title
#' @export
#' @examples
#' if (interactive()) {
#'   uw_navbar_title("My UW App")
#' }
uw_navbar_title <- function(
  title = "University of Wisconsin\u2013Madison",
  logo_height = "55px"
) {
  logo_path <- system.file(
    "www/uw-crest-color-web-digital.svg",
    package = "uwtheme"
  )

  shiny::tags$span(
    style = "display: inline-flex; align-items: center; gap: 12px;",
    shiny::tags$img(
      src = if (nzchar(logo_path)) read_svg_as_base64(logo_path) else "",
      height = logo_height
    ),
    title
  )
}

#' Create UW-Madison footer
#'
#' @param copyright_year Year for copyright notice (defaults to current year)
#' @param additional_content Optional additional content to include in footer
#' @return A div element with footer styling
#' @export
#' @examples
#' if (interactive()) {
#'   uw_footer()
#'   uw_footer(additional_content = shiny::p("Custom content"))
#' }
uw_footer <- function(
  copyright_year = format(Sys.Date(), "%Y"),
  additional_content = NULL
) {
  shiny::div(
    style = paste(
      "background-color: #282728;",
      "color: #ffffff;",
      "padding: 30px 20px;",
      "text-align: center;",
      "margin-top: 50px;"
    ),
    additional_content,
    shiny::p(
      style = "margin: 15px 0 5px 0; font-size: 14px; color: #999;",
      paste0(
        "\u00A9 ",
        copyright_year,
        " Board of Regents of the University of Wisconsin System"
      )
    )
  )
}

#' Create content container with UW styling
#'
#' @param ... Content to include in the container
#' @param max_width Maximum width of the container (default: "1200px")
#' @param padding Padding for the container (default: "40px 20px")
#' @return A div element with container styling
#' @export
#' @examples
#' if (interactive()) {
#'   uw_container(
#'     shiny::h1("Title"),
#'     shiny::p("Content")
#'   )
#' }
uw_container <- function(..., max_width = "1200px", padding = "40px 20px") {
  shiny::div(
    style = paste0(
      "max-width: ",
      max_width,
      "; ",
      "margin: 0 auto; ",
      "padding: ",
      padding,
      ";"
    ),
    ...
  )
}

#' Create UW-styled page navbar
#'
#' @param ... Navigation panels to include
#' @param title Navbar title (if NULL, uses uw_navbar_title())
#' @param theme bslib theme to use (defaults to uw_theme())
#' @param footer Footer element (if NULL, uses uw_footer())
#' @return A page_navbar element
#' @export
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   uw_page_navbar(
#'     uw_nav_panel("Home", h1("Welcome"))
#'   )
#' }
uw_page_navbar <- function(..., title = NULL, theme = NULL, footer = NULL) {
  if (is.null(title)) {
    title <- uw_navbar_title()
  }

  if (is.null(theme)) {
    theme <- uw_theme()
  }

  if (is.null(footer)) {
    footer <- uw_footer()
  }

  bslib::page_navbar(
    title = title,
    theme = theme,
    ...,
    footer = footer
  )
}

#' Create a standard UW content panel
#'
#' @param title Panel title
#' @param ... Content to include in the panel
#' @return A nav_panel element with UW styling
#' @export
#' @examples
#' if (interactive()) {
#'   uw_nav_panel("Home", shiny::h1("Welcome"))
#' }
uw_nav_panel <- function(title, ...) {
  bslib::nav_panel(
    title,
    uw_container(...)
  )
}

#' Run example UW-Madison Shiny app
#'
#' Launches an example Shiny application demonstrating the UW-Madison theme.
#'
#' @return A Shiny app object (run interactively)
#' @export
#' @examples
#' if (interactive()) {
#'   run_example()
#' }
run_example <- function() {
  ui <- uw_page_navbar(
    uw_nav_panel(
      "Home",
      shiny::h1("Welcome to UW\u2013Madison"),
      shiny::p(
        "Since its founding in 1848, this campus has been a catalyst for the extraordinary. As a public land-grant university and major research institution, our students, staff, and faculty engage in a world-class education while solving real-world problems."
      ),
      shiny::h2("The Wisconsin Idea"),
      shiny::p(
        "UW\u2013Madison's longest and proudest tradition is the Wisconsin Idea: the principle that our work should improve people's lives beyond the boundaries of campus. This century-old guiding philosophy applies to our academics, research, and outreach."
      ),
      shiny::h2("Academic Excellence"),
      shiny::p(
        "At UW\u2013Madison, we drive change by pushing beyond boundaries. From life-saving medical advances to barrier-breaking social movements, our campus continues to be home to history in the making."
      )
    ),
    uw_nav_panel(
      "About",
      shiny::h1("About This Theme"),
      shiny::p(
        "This Shiny application demonstrates the WiscWeb 2.0 theme design with Badger Red accents, Red Hat typography, and professional UW-Madison branding."
      ),
      shiny::h2("Design Elements"),
      shiny::tags$ul(
        shiny::tags$li("Badger Red (#C5050C) as primary color"),
        shiny::tags$li("Red Hat Display and Red Hat Text fonts"),
        shiny::tags$li("Clean, accessible design"),
        shiny::tags$li("Responsive layout")
      )
    )
  )

  server <- function(input, output, session) {}

  shiny::shinyApp(ui, server)
}
