library(fs)
library(tidyverse)

x <- read_csv("mt_2_results.csv")


download.file(url = "https://goo.gl/ZRCBda",
              destfile = "master.zip",
              quiet = TRUE,
              mode = "wb")

unzip("master.zip")

my_list <- dir_ls("2018-live-poll-results-master/data/")

x <- map_dfr(my_list, read_csv, .id = "name") %>% 
  filter(str_detect(name, "sen"))

