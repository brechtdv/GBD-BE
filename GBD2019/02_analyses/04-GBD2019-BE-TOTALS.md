GBD2019 BELGIUM / TOTALS
================
BrDe394
2021-11-03

  - [Settings](#settings)
  - [Data](#data)
  - [All causes](#all-causes)

# Settings

``` r
## required packages
library(knitr)
```

# Data

``` r
x <- read.csv("../01_data/IHME-GBD_2019_DATA-a2387e1e-1.csv")
x$age <- x$metric <- x$cause <- x$location <- NULL
```

# All causes

``` r
col <- c("measure", "sex", "year", "val", "lower", "upper")

kable(subset(x, year == 1990)[, col], row.names = FALSE)
```

| measure                                | sex    | year |        val |      lower |      upper |
| :------------------------------------- | :----- | ---: | ---------: | ---------: | ---------: |
| Deaths                                 | Male   | 1990 |   923.8085 |   915.7559 |   931.4719 |
| Deaths                                 | Female | 1990 |   540.7042 |   535.5769 |   545.9511 |
| Deaths                                 | Both   | 1990 |   695.2393 |   690.7088 |   699.5382 |
| YLLs (Years of Life Lost)              | Male   | 1990 | 20493.3476 | 20295.6544 | 20692.9337 |
| YLLs (Years of Life Lost)              | Female | 1990 | 11395.8332 | 11270.9264 | 11524.4908 |
| YLLs (Years of Life Lost)              | Both   | 1990 | 15489.3326 | 15368.8881 | 15612.7111 |
| YLDs (Years Lived with Disability)     | Male   | 1990 |  9725.6581 |  7191.6955 | 12668.9781 |
| YLDs (Years Lived with Disability)     | Female | 1990 | 11807.5176 |  8622.5018 | 15447.5861 |
| YLDs (Years Lived with Disability)     | Both   | 1990 | 10760.4848 |  7904.6683 | 14044.9569 |
| DALYs (Disability-Adjusted Life Years) | Male   | 1990 | 30219.0047 | 27638.1620 | 33091.6010 |
| DALYs (Disability-Adjusted Life Years) | Female | 1990 | 23203.3495 | 20050.7512 | 26847.6034 |
| DALYs (Disability-Adjusted Life Years) | Both   | 1990 | 26249.8162 | 23357.1149 | 29484.5790 |

``` r
kable(subset(x, year == 2019)[, col], row.names = FALSE)
```

| measure                                | sex    | year |        val |      lower |      upper |
| :------------------------------------- | :----- | ---: | ---------: | ---------: | ---------: |
| Deaths                                 | Male   | 2019 |   558.6511 |   546.3762 |   571.8147 |
| Deaths                                 | Female | 2019 |   362.1466 |   354.3695 |   370.5249 |
| Deaths                                 | Both   | 2019 |   449.5291 |   439.6344 |   460.1688 |
| YLLs (Years of Life Lost)              | Male   | 2019 | 11526.0328 | 11160.9624 | 11930.0968 |
| YLLs (Years of Life Lost)              | Female | 2019 |  6961.5307 |  6739.7746 |  7207.7103 |
| YLLs (Years of Life Lost)              | Both   | 2019 |  9129.1640 |  8837.4002 |  9453.0517 |
| YLDs (Years Lived with Disability)     | Male   | 2019 |  9900.7770 |  7332.3068 | 12889.1024 |
| YLDs (Years Lived with Disability)     | Female | 2019 | 12178.1757 |  8886.0042 | 15796.8113 |
| YLDs (Years Lived with Disability)     | Both   | 2019 | 11041.2580 |  8083.0749 | 14321.2950 |
| DALYs (Disability-Adjusted Life Years) | Male   | 2019 | 21426.8088 | 18812.1433 | 24409.1605 |
| DALYs (Disability-Adjusted Life Years) | Female | 2019 | 19139.7051 | 15835.4204 | 22728.1403 |
| DALYs (Disability-Adjusted Life Years) | Both   | 2019 | 20170.4208 | 17229.8475 | 23434.5722 |

``` r
##rmarkdown::render("04-GBD2019-BE-TOTALS.R")
```
