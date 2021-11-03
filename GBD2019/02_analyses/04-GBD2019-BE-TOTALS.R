#' ---
#' title: GBD2019 BELGIUM / TOTALS
#' output:
#'   html_document:
#'     toc: true
#' ---

#' # Settings

## required packages
library(knitr)


#' # Data

x <- read.csv("../01_data/IHME-GBD_2019_DATA-a2387e1e-1.csv")
x$age <- x$metric <- x$cause <- x$location <- NULL


#' # All causes

col <- c("measure", "sex", "year", "val", "lower", "upper")

kable(subset(x, year == 1990)[, col], row.names = FALSE)
kable(subset(x, year == 2019)[, col], row.names = FALSE)

##rmarkdown::render("GBD2019-BE-TOTALS.R")