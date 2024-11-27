# DF_632-HW1
Repository for HW 1 for biostats631 course
{r setup}
library(tidyverse)
library(readxl)
library(testthat)
library(assertthat)
library(devtools)
#knitr::opts_chunk$set(include = FALSE)
#r setup
# load csv file est
est <- read_csv("est.csv")
#plot_cp_hw1 runs function from 531 HW1
plot_cp_hw1 <- function(est, iso_code) {
  est2 <- est %>%
    filter(iso == iso_code)

  est2 %>%
    ggplot(aes(x = Year, y = Median)) +
    geom_line() +
    geom_smooth(
      stat = "identity",
      aes(ymax = U95, ymin = L95)
    ) +
    labs(x = "Time", y = "Modern use (%)", title = est2$`Country or area`[1])
}
plot_cp_hw1(est, iso_code = 4)