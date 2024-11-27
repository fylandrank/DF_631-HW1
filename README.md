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

#load new csv to expand data
cp_use <- read_csv("contraceptive_use.csv")

#reformat data set
dat <- cp_use %>% 
  select(division_numeric_code, contraceptive_use_modern, is_in_union, start_date , end_date) %>% 
  rename(iso = division_numeric_code, obs_mCPR = contraceptive_use_modern, mar_status = is_in_union) %>% 
  mutate(year = (start_date + end_date)/2, cp = (obs_mCPR*100)) %>% 
  filter(mar_status == "Y")
  
#write function to plot with info from cp_use and est
plot_cp <- function(dat, est, iso_code, CI = "95") {
  
  dat2 <- dat %>% 
    filter(iso == iso_code)
  est2 <- est %>% 
    filter(iso == iso_code)
  if (is.na(CI)) {
    CI <- "none"
  }
  combo_plot <- ggplot(est2, aes(x = Year, y = Median)) +
    geom_line()
if (CI == "95") {
    combo_plot <- combo_plot + 
      geom_smooth(
      stat = "identity",
      aes(ymax = U95, ymin = L95)
    )}
  else if(CI == "80") {
    combo_plot <- combo_plot + 
      geom_smooth(
      stat = "identity",
      aes(ymax = U80, ymin = L80)
    )}
  combo_plot <- combo_plot + 
    geom_point(data = dat2, aes(year, cp)) +
    labs(x = "Time", y = "Modern use (%)", title = est2$`Country or area`[1])
  return(combo_plot)
}

#run test conditions
plot_cp(dat, est, iso_code = 4)
plot_cp(dat, est, iso_code = 4, CI = NA)
plot_cp(dat, est, iso_code = 404, CI = 80)