# uwtheme

uwtheme provides a UW-Madison branded theme for Shiny applications using bslib. Includes the Badger Red color, Red Hat font, and favicon and logo branding.

## Installation

You can install the development version of uwtheme from GitHub:

```r
# install.packages("pak")
pak::pak("dylanpieper/uwtheme")
```

## Usage

```r
library(shiny)
library(uwtheme)

ui <- uw_page_navbar(
 uw_nav_panel(
   "Home",
   h1("Welcome"),
   p("Your content here.")
 )
)

server <- function(input, output, session) {}

shinyApp(ui, server)
```

Or use individual components with any bslib page:

```r
library(shiny)
library(bslib)
library(uwtheme)

ui <- page_navbar(
 title = uw_navbar_title("My App"),
 theme = uw_theme(),
 nav_panel("Home", p("Content")),
 footer = uw_footer()
)
```

Run `run_example()` to see a demo.
