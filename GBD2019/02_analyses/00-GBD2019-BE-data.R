### GBD 2019 // DATA

## define EU-15 countries
EU15 <- c("Austria", "Denmark", "Finland", "France", "Germany",
          "Greece", "Ireland", "Italy", "Luxembourg", "Netherlands",
          "Portugal", "Spain", "Sweden", "United Kingdom")

## import data
x <-
rbind(
  read.csv("../01_data/IHME-GBD_2019_DATA-d2c90045-1.csv"),
  read.csv("../01_data/IHME-GBD_2019_DATA-d2c90045-2.csv"),
  read.csv("../01_data/IHME-GBD_2019_DATA-d2c90045-3.csv"),
  read.csv("../01_data/IHME-GBD_2019_DATA-d2c90045-4.csv"),
  read.csv("../01_data/IHME-GBD_2019_DATA-d2c90045-5.csv"),
  read.csv("../01_data/IHME-GBD_2019_DATA-d2c90045-6.csv"),
  read.csv("../01_data/IHME-GBD_2019_DATA-d2c90045-7.csv"),
  read.csv("../01_data/IHME-GBD_2019_DATA-d2c90045-8.csv"),
  read.csv("../01_data/IHME-GBD_2019_DATA-d2c90045-9.csv"),
  read.csv("../01_data/IHME-GBD_2019_DATA-d2c90045-10.csv"),
  read.csv("../01_data/IHME-GBD_2019_DATA-d2c90045-11.csv"),
  read.csv("../01_data/IHME-GBD_2019_DATA-d2c90045-12.csv"),
  read.csv("../01_data/IHME-GBD_2019_DATA-d2c90045-13.csv"),
  read.csv("../01_data/IHME-GBD_2019_DATA-d2c90045-14.csv"),
  read.csv("../01_data/IHME-GBD_2019_DATA-d2c90045-15.csv"),
  read.csv("../01_data/IHME-GBD_2019_DATA-d2c90045-16.csv"),
  read.csv("../01_data/IHME-GBD_2019_DATA-d2c90045-17.csv"),
  read.csv("../01_data/IHME-GBD_2019_DATA-d2c90045-18.csv"),
  read.csv("../01_data/IHME-GBD_2019_DATA-d2c90045-19.csv"),
  read.csv("../01_data/IHME-GBD_2019_DATA-d2c90045-20.csv"),
  read.csv("../01_data/IHME-GBD_2019_DATA-d2c90045-21.csv"),
  read.csv("../01_data/IHME-GBD_2019_DATA-d2c90045-22.csv"))

x$age <- x$metric <- NULL

x <- droplevels(subset(x, location %in% c("Belgium", EU15)))
if (length(unique(x$location)) != 15)
  stop("Not all locations identified.")

## save results
saveRDS(x, file = "GBD2019.rds")
