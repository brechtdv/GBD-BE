#' ---
#' title: GBD2019 BELGIUM / TOTALS // PLOT
#' output:
#'   github_document:
#'     html_preview: false
#'     toc: true
#' ---

#' # Settings

## knitr settings
knitr::opts_chunk$set(fig.width=12)

## required packages
library(ggplot2)

## helper functions
plot_evolution <-
function(.measure) {
  xlim <- seq(min(x$year), max(x$year), 5)
  p <-
  ggplot(
    subset(x,
      measure == .measure &
      sex != "Both"),
    aes(x = year, y = val)) +
    geom_line(aes(colour = sex)) +
    theme_bw() +
    scale_colour_discrete(breaks = c("Male", "Female")) +
    scale_x_continuous(NULL, breaks = xlim) +
    scale_y_continuous(.measure, labels = scales::comma)

  tiff(sprintf("fig/evolution-%s-%s-%s.tiff",
               .measure, min(x$year), max(x$year)),
       10, 4, units = "in", res = 300, compress = "lzw")
  print(p)
  dev.off()
}


#' # Data

x <- read.csv("../01_data/IHME-GBD_2019_DATA-a2387e1e-1.csv")
x$age <- x$metric <- x$cause <- x$location <- NULL


#' # Evolution

plot_evolution("Deaths")
plot_evolution("YLLs (Years of Life Lost)")
plot_evolution("YLDs (Years Lived with Disability)")
plot_evolution("DALYs (Disability-Adjusted Life Years)")

#' # Comparison

xlim <- seq(min(x$year), max(x$year), 5)
ggplot(
  subset(x,
    measure %in%
      c("YLLs (Years of Life Lost)", "YLDs (Years Lived with Disability)")&
    sex != "Both"),
  aes(x = year, y = val)) +
  geom_line(aes(colour = sex, linetype = measure)) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper,
        colour = sex, fill = sex, linetype = measure),
    alpha = 0.5) +
  theme_bw() +
  scale_fill_discrete(breaks = c("Male", "Female")) +
  scale_colour_discrete(breaks = c("Male", "Female")) +
  scale_x_continuous(NULL, breaks = xlim) +
  scale_y_continuous(NULL, labels = scales::comma) +
  theme(legend.position = c(0.85, 0.82))

tiff("fig/evolution-all.tiff", 10, 5, units = "in", res = 300, compress = "lzw")
ggplot(
  subset(x,
    measure %in%
      c("YLLs (Years of Life Lost)", "YLDs (Years Lived with Disability)")&
    sex != "Both"),
  aes(x = year, y = val)) +
  geom_line(aes(colour = sex, linetype = measure)) +
  theme_bw() +
  scale_colour_discrete(breaks = c("Male", "Female")) +
  scale_x_continuous(NULL, breaks = xlim) +
  scale_y_continuous(NULL, limits = c(5000, 20000), labels = scales::comma) +
  theme(legend.position = c(0.87, 0.8))
dev.off()

##rmarkdown::render("05-GBD2019-BE-TOTALS-plot.R")