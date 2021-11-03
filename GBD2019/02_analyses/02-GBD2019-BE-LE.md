GBD2019 BELGIUM / LIFE EXPECTANCY
================
BrDe394
2021-11-03

  - [Settings](#settings)
  - [Data](#data)
  - [Life Expectancy](#life-expectancy)
      - [BENCHMARK EU-15 MEAN](#benchmark-eu-15-mean)

# Settings

``` r
## required packages
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
```

# Data

``` r
x <- read.csv("../01_data/IHME-GBD_2019_DATA-f72da28d-1.csv")
x$age <- x$metric <- x$measure <- NULL
x <- droplevels(subset(x, location %in% c("Belgium", EU15)))
if (length(unique(x$location)) != 15)
  stop("Not all EU15 countries located.")
```

# Life Expectancy

## BENCHMARK EU-15 MEAN

``` r
tab_eu15_mean(1990, 2019, "Male")
```

|      |      EU15 |       val |     lower |     upper |
| :--- | --------: | --------: | --------: | --------: |
| 1990 | 72.870416 | 72.788182 | 72.677105 | 72.901604 |
| 2019 | 79.706431 | 78.992025 | 78.731530 | 79.239212 |
| DIFF |  6.836015 |  6.203844 |  6.054426 |  6.337608 |

EU15 life expectancy for Male, 1990

|    | location       |      val |    lower |    upper |
| :- | :------------- | -------: | -------: | -------: |
| 1  | Sweden         | 75.09184 | 74.97239 | 75.21346 |
| 2  | Greece         | 74.85305 | 74.71774 | 74.97875 |
| 3  | Netherlands    | 73.92706 | 73.83068 | 74.03128 |
| 4  | Italy          | 73.81889 | 73.76100 | 73.87171 |
| 5  | Spain          | 73.36368 | 73.29111 | 73.43727 |
| 6  | France         | 73.10379 | 73.04164 | 73.16198 |
| 7  | United Kingdom | 73.01858 | 72.96233 | 73.07069 |
| 8  | Belgium        | 72.78818 | 72.67710 | 72.90160 |
| 9  | Austria        | 72.45203 | 72.32385 | 72.58689 |
| 10 | Denmark        | 72.41152 | 72.25413 | 72.56121 |
| 11 | Ireland        | 72.32977 | 72.16180 | 72.50131 |
| 12 | Germany        | 72.22054 | 72.17102 | 72.27002 |
| 13 | Luxembourg     | 71.63730 | 71.24692 | 72.06244 |
| 14 | Finland        | 71.25518 | 71.09523 | 71.40939 |
| 15 | Portugal       | 70.78483 | 70.66386 | 70.90130 |

Country life expectancy for Male, 1990

|    | location       |      val |    lower |    upper |
| :- | :------------- | -------: | -------: | -------: |
| 1  | Sweden         | 81.09171 | 80.89758 | 81.27304 |
| 2  | Italy          | 80.80018 | 80.66947 | 80.93278 |
| 3  | Luxembourg     | 80.79452 | 79.62265 | 81.86451 |
| 4  | Spain          | 80.35433 | 80.14521 | 80.55404 |
| 5  | Ireland        | 80.15536 | 79.77837 | 80.51236 |
| 6  | Netherlands    | 80.04524 | 79.78033 | 80.29575 |
| 7  | France         | 79.88232 | 79.66444 | 80.09237 |
| 8  | Austria        | 79.78120 | 79.56607 | 79.98411 |
| 9  | United Kingdom | 79.23800 | 79.10080 | 79.37114 |
| 10 | Denmark        | 79.16703 | 78.83165 | 79.48479 |
| 11 | Finland        | 79.07969 | 78.67513 | 79.46206 |
| 12 | Belgium        | 78.99203 | 78.73153 | 79.23921 |
| 13 | Germany        | 78.92010 | 78.70049 | 79.10910 |
| 14 | Portugal       | 78.74862 | 78.45887 | 79.02278 |
| 15 | Greece         | 78.54613 | 78.23613 | 78.83642 |

Country life expectancy for Male, 2019

``` r
tab_eu15_mean(1990, 2019, "Female")
```

|      |      EU15 |       val |     lower |     upper |
| :--- | --------: | --------: | --------: | --------: |
| 1990 | 79.371339 | 79.427184 | 79.314215 | 79.533768 |
| 2019 | 84.253147 | 83.776199 | 83.564887 | 83.975513 |
| DIFF |  4.881808 |  4.349015 |  4.250671 |  4.441745 |

EU15 life expectancy for Female, 1990

|    | location       |      val |    lower |    upper |
| :- | :------------- | -------: | -------: | -------: |
| 1  | France         | 81.30909 | 81.25229 | 81.36785 |
| 2  | Sweden         | 80.82075 | 80.70441 | 80.93492 |
| 3  | Spain          | 80.54859 | 80.47683 | 80.61788 |
| 4  | Italy          | 80.41056 | 80.35663 | 80.46060 |
| 5  | Netherlands    | 80.27657 | 80.17816 | 80.37552 |
| 6  | Greece         | 79.61783 | 79.50799 | 79.71893 |
| 7  | Finland        | 79.43993 | 79.27968 | 79.59924 |
| 8  | Belgium        | 79.42718 | 79.31422 | 79.53377 |
| 9  | Austria        | 79.10946 | 79.00863 | 79.20950 |
| 10 | Luxembourg     | 78.75673 | 78.40515 | 79.12226 |
| 11 | Germany        | 78.72971 | 78.67966 | 78.77996 |
| 12 | United Kingdom | 78.54218 | 78.48689 | 78.59686 |
| 13 | Denmark        | 78.04310 | 77.88805 | 78.19574 |
| 14 | Ireland        | 77.77018 | 77.58816 | 77.94548 |
| 15 | Portugal       | 77.76825 | 77.65547 | 77.88666 |

Country life expectancy for Female, 1990

|    | location       |      val |    lower |    upper |
| :- | :------------- | -------: | -------: | -------: |
| 1  | Spain          | 85.74280 | 85.58445 | 85.89300 |
| 2  | France         | 85.73586 | 85.56846 | 85.89617 |
| 3  | Italy          | 85.26113 | 85.15048 | 85.36728 |
| 4  | Luxembourg     | 84.98531 | 83.97887 | 85.87891 |
| 5  | Finland        | 84.66488 | 84.35426 | 84.95734 |
| 6  | Sweden         | 84.58207 | 84.41699 | 84.73524 |
| 7  | Portugal       | 84.52809 | 84.30991 | 84.73428 |
| 8  | Austria        | 84.46287 | 84.29403 | 84.62298 |
| 9  | Ireland        | 83.87868 | 83.56639 | 84.17407 |
| 10 | Belgium        | 83.77620 | 83.56489 | 83.97551 |
| 11 | Germany        | 83.47407 | 83.29937 | 83.62209 |
| 12 | Netherlands    | 83.39128 | 83.16420 | 83.60649 |
| 13 | Greece         | 83.34168 | 83.12466 | 83.54405 |
| 14 | Denmark        | 83.09228 | 82.80229 | 83.36776 |
| 15 | United Kingdom | 82.88002 | 82.75908 | 82.99559 |

Country life expectancy for Female, 2019

``` r
tab_eu15_mean(1990, 2019, "Both")
```

|      |      EU15 |       val |     lower |     upper |
| :--- | --------: | --------: | --------: | --------: |
| 1990 | 76.164004 | 76.149097 | 76.070951 | 76.231861 |
| 2019 | 82.003746 | 81.405313 | 81.163795 | 81.633899 |
| DIFF |  5.839742 |  5.256216 |  5.092843 |  5.402038 |

EU15 life expectancy for Both, 1990

|    | location       |      val |    lower |    upper |
| :- | :------------- | -------: | -------: | -------: |
| 1  | Sweden         | 77.94902 | 77.86546 | 78.03229 |
| 2  | Greece         | 77.22877 | 77.13702 | 77.31926 |
| 3  | France         | 77.20881 | 77.16585 | 77.25587 |
| 4  | Italy          | 77.16393 | 77.12440 | 77.20323 |
| 5  | Netherlands    | 77.15587 | 77.08191 | 77.23254 |
| 6  | Spain          | 76.95951 | 76.90604 | 77.01389 |
| 7  | Belgium        | 76.14910 | 76.07095 | 76.23186 |
| 8  | Austria        | 75.96697 | 75.87079 | 76.06513 |
| 9  | United Kingdom | 75.84562 | 75.80323 | 75.88622 |
| 10 | Germany        | 75.65803 | 75.61796 | 75.69502 |
| 11 | Finland        | 75.42946 | 75.27441 | 75.57352 |
| 12 | Luxembourg     | 75.24969 | 74.96305 | 75.52687 |
| 13 | Denmark        | 75.22717 | 75.12087 | 75.34119 |
| 14 | Ireland        | 74.98401 | 74.85856 | 75.11863 |
| 15 | Portugal       | 74.28408 | 74.19573 | 74.36909 |

Country life expectancy for Both, 1990

|    | location       |      val |    lower |    upper |
| :- | :------------- | -------: | -------: | -------: |
| 1  | Italy          | 83.09883 | 83.01104 | 83.18570 |
| 2  | Spain          | 83.08902 | 82.90002 | 83.26897 |
| 3  | Luxembourg     | 82.91936 | 81.80577 | 83.92283 |
| 4  | France         | 82.86674 | 82.66888 | 83.05699 |
| 5  | Sweden         | 82.82941 | 82.69988 | 82.95249 |
| 6  | Austria        | 82.16446 | 81.96825 | 82.34994 |
| 7  | Ireland        | 82.01906 | 81.66849 | 82.35088 |
| 8  | Finland        | 81.86531 | 81.49771 | 82.21205 |
| 9  | Netherlands    | 81.74461 | 81.49395 | 81.98187 |
| 10 | Portugal       | 81.71806 | 81.45814 | 81.96381 |
| 11 | Belgium        | 81.40531 | 81.16379 | 81.63390 |
| 12 | Germany        | 81.19662 | 80.99503 | 81.36900 |
| 13 | Denmark        | 81.13389 | 80.81543 | 81.43602 |
| 14 | United Kingdom | 81.06964 | 80.97521 | 81.15991 |
| 15 | Greece         | 80.93586 | 80.66784 | 81.18624 |

Country life expectancy for Both, 2019

``` r
##rmarkdown::render("02-GBD2019-BE-LE.R")
```
