#' ---
#' title: GBD2019 BELGIUM
#' output:
#'   github_document:
#'     toc: true
#' ---

#' # Settings

## required packages
library(knitr)

## define EU-15 countries
EU15 <- c("Austria", "Denmark", "Finland", "France", "Germany",
          "Greece", "Ireland", "Italy", "Luxembourg", "Netherlands",
          "Portugal", "Spain", "Sweden", "United Kingdom")

## helper functions
tab_sort <-
function(.year, .measure, .sex) {
  y <-
  subset(
    x,
    location == "Belgium" &
      year == .year &
      measure == .measure &
      sex == .sex)
  y <- y[order(y$val, decreasing = T), c("cause", "val", "lower", "upper")]
  rownames(y) <- seq(nrow(y))
  kable(y, row.names = TRUE)
}

tab_trends <-
function(year1, year2, .measure, .sex) {
  y1 <-
  subset(
    x,
    location == "Belgium" &
      year == year1 &
      measure == .measure &
      sex == .sex)
  y2 <-
  subset(
    x,
    location == "Belgium" &
      year == year2 &
      measure == .measure &
      sex == .sex)
  yy <-
  data.frame(
    cause = y1[order(y1$cause), "cause"],
    diff = y2[order(y2$cause), "val"] - y1[order(y1$cause), "val"])

  tab_diff <- t(c(sum(y1$val), sum(y2$val), sum(yy$diff)))
  colnames(tab_diff) <- c(1990, 2019, "difference")
  print(kable(tab_diff))

  kable(
    yy[order(yy$diff, decreasing = FALSE), ],
    row.names = FALSE)
}

tab_eu15_mean <-
function(.year, .measure, .sex) {
  ybe <-
  subset(
    x,
    location == "Belgium" &
      year == .year &
      measure == .measure &
      sex == .sex)
  y15 <-
  subset(
    x,
    year == .year &
      measure == .measure &
      sex == .sex)
  y15m <- with(y15, tapply(val, cause, mean, na.rm = TRUE))
  y15m <- na.omit(data.frame(cause = names(y15m), val = y15m))

  y2 <- merge(ybe, y15m, by = "cause")

  yy <-
  data.frame(
    cause = y2$cause,
    diff = y2$val.x - y2$val.y,
    EU15 = y2$val.y,
    BE = y2$val.x,
    BE_lwr = y2$lower,
    BE_upr = y2$upper,
    SIG = ifelse(y2$val.y < y2$lower | y2$val.y > y2$upper, "***", ""))

  tab_diff <- t(c(sum(y15m$val), sum(ybe$val), sum(yy$diff)))
  colnames(tab_diff) <- c("EU15", "Belgium", "difference")
  print(kable(tab_diff))

  kable(
    yy[order(yy$diff, decreasing = TRUE), ],
    row.names = FALSE)
}

tab_eu15_countries <-
function(.year1, .year2, .measure, .sex) {
  tab_diff_head <- NULL
  tab_diff_tail <- NULL

  y0 <-
  subset(
    x,
      year == .year1 &
      measure == .measure &
      sex == .sex)
  y15 <- with(y0, tapply(val, location, sum))
  y15 <- data.frame(location = names(y15), val = y15)
  y15 <- y15[order(y15$val), ]
  y15$diff <- y15$val - subset(y15, location == "Belgium")$val
  rownames(y15) <- 1:15

  print(
    kable(
      y15,
      row.names = TRUE,
      caption = sprintf("%s per 100,000 in %s, %s",
                        .measure, .year1, .sex)))

  y <-
  subset(
    x,
      year == .year2 &
      measure == .measure &
      sex == .sex)
  y15 <- with(y, tapply(val, location, sum))
  y15 <- data.frame(location = names(y15), val = y15)
  y15 <- y15[order(y15$val), ]
  y15$diff <- y15$val - subset(y15, location == "Belgium")$val
  rownames(y15) <- 1:15

  print(
    kable(
      y15,
      row.names = TRUE,
      caption = sprintf("%s per 100,000 in %s, %s",
                        .measure, .year2, .sex)))

  ybe <-
  subset(
    x,
    location == "Belgium" &
      year == .year2 &
      measure == .measure &
      sex == .sex)

  for (.location in EU15) {
    yxx <-
    subset(
      x,
      location == .location &
        year == .year2 &
        measure == .measure &
        sex == .sex)
    y2 <- merge(ybe, yxx, by = "cause")

    yy <-
    data.frame(
      cause = y2$cause,
      diff = y2$val.x - y2$val.y)
  
    tab_diff_head <-
      rbind(
        tab_diff_head,
        cbind(location = .location,
              head(yy[order(yy$diff, decreasing = TRUE), ], 1)))
    tab_diff_tail <-
      rbind(
        tab_diff_tail,
        cbind(location = .location,
              tail(yy[order(yy$diff, decreasing = TRUE), ], 1)))
  }

  tab_diff_head <- droplevels(tab_diff_head)
  tab_diff_tail <- droplevels(tab_diff_tail)

  print(kable(tab_diff_head, row.names = FALSE))
  print(kable(sort(table(tab_diff_head$cause), decreasing = TRUE)))

  print(kable(tab_diff_tail, row.names = FALSE))
  print(kable(sort(table(tab_diff_tail$cause), decreasing = TRUE)))
}


#' # Data

## load results
x <- readRDS("GBD2019.rds")


#' # Mortality

#' ## TOP 2019
#+ results='asis'

tab_sort(2019, "Deaths", "Male")
tab_sort(2019, "Deaths", "Female")
tab_sort(2019, "Deaths", "Both")

#' ## TRENDS 1990-2019
#+ results='asis'

tab_trends(1990, 2019, "Deaths", "Male")
tab_trends(1990, 2019, "Deaths", "Female")
tab_trends(1990, 2019, "Deaths", "Both")

#' ## BENCHMARK EU-15 MEAN
#+ results='asis'

tab_eu15_mean(2019, "Deaths", "Male")
tab_eu15_mean(2019, "Deaths", "Female")
tab_eu15_mean(2019, "Deaths", "Both")

#' ## BENCHMARK EU-15 COUNTRIES
#+ results='asis'

tab_eu15_countries(1990, 2019, "Deaths", "Male")
tab_eu15_countries(1990, 2019, "Deaths", "Female")
tab_eu15_countries(1990, 2019, "Deaths", "Both")


#' # Years Lived with Disability

#' ## TOP 2019
#+ results='asis'

tab_sort(2019, "YLDs (Years Lived with Disability)", "Male")
tab_sort(2019, "YLDs (Years Lived with Disability)", "Female")
tab_sort(2019, "YLDs (Years Lived with Disability)", "Both")

#' ## TRENDS 1990-2019
#+ results='asis'

tab_trends(1990, 2019, "YLDs (Years Lived with Disability)", "Male")
tab_trends(1990, 2019, "YLDs (Years Lived with Disability)", "Female")
tab_trends(1990, 2019, "YLDs (Years Lived with Disability)", "Both")

#' ## BENCHMARK EU-15 MEAN
#+ results='asis'

tab_eu15_mean(2019, "YLDs (Years Lived with Disability)", "Male")
tab_eu15_mean(2019, "YLDs (Years Lived with Disability)", "Female")
tab_eu15_mean(2019, "YLDs (Years Lived with Disability)", "Both")

#' ## BENCHMARK EU-15 COUNTRIES
#+ results='asis'

tab_eu15_countries(1990, 2019, "YLDs (Years Lived with Disability)", "Male")
tab_eu15_countries(1990, 2019, "YLDs (Years Lived with Disability)", "Female")
tab_eu15_countries(1990, 2019, "YLDs (Years Lived with Disability)", "Both")


#' # Years of Life Lost

#' ## TOP 2019
#+ results='asis'

tab_sort(2019, "YLLs (Years of Life Lost)", "Male")
tab_sort(2019, "YLLs (Years of Life Lost)", "Female")
tab_sort(2019, "YLLs (Years of Life Lost)", "Both")

#' ## TRENDS 1990-2019
#+ results='asis'

tab_trends(1990, 2019, "YLLs (Years of Life Lost)", "Male")
tab_trends(1990, 2019, "YLLs (Years of Life Lost)", "Female")
tab_trends(1990, 2019, "YLLs (Years of Life Lost)", "Both")

#' ## BENCHMARK EU-15 MEAN
#+ results='asis'

tab_eu15_mean(2019, "YLLs (Years of Life Lost)", "Male")
tab_eu15_mean(2019, "YLLs (Years of Life Lost)", "Female")
tab_eu15_mean(2019, "YLLs (Years of Life Lost)", "Both")

#' ## BENCHMARK EU-15 COUNTRIES
#+ results='asis'

tab_eu15_countries(1990, 2019, "YLLs (Years of Life Lost)", "Male")
tab_eu15_countries(1990, 2019, "YLLs (Years of Life Lost)", "Female")
tab_eu15_countries(1990, 2019, "YLLs (Years of Life Lost)", "Both")


#' # Disability-Adjusted Life Years

#' ## TOP 2019
#+ results='asis'

tab_sort(2019, "DALYs (Disability-Adjusted Life Years)", "Male")
tab_sort(2019, "DALYs (Disability-Adjusted Life Years)", "Female")
tab_sort(2019, "DALYs (Disability-Adjusted Life Years)", "Both")

#' ## TRENDS 1990-2019
#+ results='asis'

tab_trends(1990, 2019, "DALYs (Disability-Adjusted Life Years)", "Male")
tab_trends(1990, 2019, "DALYs (Disability-Adjusted Life Years)", "Female")
tab_trends(1990, 2019, "DALYs (Disability-Adjusted Life Years)", "Both")

#' ## BENCHMARK EU-15 MEAN
#+ results='asis'

tab_eu15_mean(2019, "DALYs (Disability-Adjusted Life Years)", "Male")
tab_eu15_mean(2019, "DALYs (Disability-Adjusted Life Years)", "Female")
tab_eu15_mean(2019, "DALYs (Disability-Adjusted Life Years)", "Both")

#' ## BENCHMARK EU-15 COUNTRIES
#+ results='asis'

tab_eu15_countries(
  1990, 2019, "DALYs (Disability-Adjusted Life Years)", "Male")
tab_eu15_countries(
  1990, 2019, "DALYs (Disability-Adjusted Life Years)", "Female")
tab_eu15_countries(
  1990, 2019, "DALYs (Disability-Adjusted Life Years)", "Both")


##rmarkdown::render("01-GBD2019-BE.R")