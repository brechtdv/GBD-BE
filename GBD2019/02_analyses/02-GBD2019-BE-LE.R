#' ---
#' title: GBD2019 BELGIUM / LIFE EXPECTANCY
#' output:
#'   github_document:
#'     html_preview: false
#'     toc: true
#' ---

#' # Settings

## required packages
library(cowplot)
library(ggplot2)
library(knitr)

## define EU-15 countries
EU15 <- c("Austria", "Denmark", "Finland", "France", "Germany",
          "Greece", "Ireland", "Italy", "Luxembourg", "Netherlands",
          "Portugal", "Spain", "Sweden", "United Kingdom")


## helper functions
tab_eu15_mean <-
function(.year1, .year2, .sex) {
  y15a <-
  subset(
    x,
    year == .year1 &
      sex == .sex)
  y15b <-
  subset(
    x,
    year == .year2 &
      sex == .sex)
  
  ## BE vs EU15 mean | time period
  tab <-
    rbind(
      cbind(
        EU15 = mean(y15a$val),
        subset(y15a, location == "Belgium")[, c("val", "lower", "upper")]),
      cbind(
        EU15 = mean(y15b$val),
        subset(y15b, location == "Belgium")[, c("val", "lower", "upper")]))
  tab <-
    rbind(tab,
          tab[2, ] - tab[1, ])
  rownames(tab) <- c(.year1, .year2, "DIFF")
  print(
    kable(
      tab,
      row.names = TRUE,
      caption = sprintf("EU15 life expectancy for %s, %s", .sex, .year1)))

  ## LE by country | time period
  tab1 <-
    y15a[order(y15a$val, decreasing = TRUE),
         c("location", "val", "lower", "upper")]
  rownames(tab1) <- seq(nrow(tab1))
  print(
    kable(
      tab1,
      row.names = TRUE,
      caption = sprintf("Country life expectancy for %s, %s", .sex, .year1)))

  tab2 <-
    y15b[order(y15b$val, decreasing = TRUE),
         c("location", "val", "lower", "upper")]
  rownames(tab2) <- seq(nrow(tab2))
  print(
    kable(
      tab2,
      row.names = TRUE,
      caption = sprintf("Country life expectancy for %s, %s", .sex, .year2)))
}


#' # Data

x <- read.csv("../01_data/IHME-GBD_2019_DATA-f72da28d-1.csv")
x$age <- x$metric <- x$measure <- NULL
x <- droplevels(subset(x, location %in% c("Belgium", EU15)))
if (length(unique(x$location)) != 15)
  stop("Not all EU15 countries located.")

#' # Life Expectancy

#' ## Benchmark tables
#+ results='asis'

tab_eu15_mean(1990, 2019, "Male")
tab_eu15_mean(1990, 2019, "Female")
tab_eu15_mean(1990, 2019, "Both")

#' ## Benchmark plot

x1 <- subset(x, year %in% range(x$year) & sex == "Male")
x1 <- x1[order(x1$location), ]
x1$val2 <- x1$val
x1[x1$year == max(x$year), ]$val2 <-
  subset(x1, year == max(x$year))$val - subset(x1, year == min(x$year))$val

x1 <- x1[order(x1$val), ]
x1$location <-
  factor(x1$location, subset(x1, year == max(x$year))$location)
x1$year <- factor(x1$year, range(x$year))
x1$sex <- "Men"

p1 <-
ggplot(x1, aes(x = val2, y = location, fill = year)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  facet_grid(~sex) +
  scale_fill_brewer() +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "Life expectancy", y = NULL, fill = NULL)

x2 <- subset(x, year %in% range(x$year) & sex == "Female")
x2 <- x2[order(x2$location), ]
x2$val2 <- x2$val
x2[x2$year == max(x$year), ]$val2 <-
  subset(x2, year == max(x$year))$val - subset(x2, year == min(x$year))$val

x2 <- x2[order(x2$val), ]
x2$location <-
  factor(x2$location, subset(x2, year == max(x$year))$location)
x2$year <- factor(x2$year, range(x$year))
x2$sex <- "Women"

p2 <-
ggplot(x2, aes(x = val2, y = location, fill = year)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  facet_grid(~sex) +
  scale_fill_brewer() +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "Life expectancy", y = NULL, fill = NULL)

plot_grid(p1, p2)

tiff("Fig1.tiff", 10, 4, units = "in", res = 300, compress = "lzw")
plot_grid(p1, p2)
dev.off()

##rmarkdown::render("02-GBD2019-BE-LE.R")