rm(list=ls())

library(dplyr)
library(tidyr)
library(stringr)
library(tidyverse)
library(extrafont)


library(cowplot)
library(egg)
library(ggplot2)



source("./R/info.R")
####### Process data to RDS
scen <- list.files(path = "./scenario", all.files = FALSE, recursive = FALSE)
#scenpath <- list.dirs(path = "./scenario", full.names = TRUE, recursive = F)
dir.create("./scenario/RDS", showWarnings = FALSE)

for (s in setdiff(scen, "RDS")) {
  n = paste0("./scenario/",s)
    source("./R/csv_to_rds.R")
} 
source("./R/bindRDS.R")


####### Read RDS
setwd("./scenario/RDS/RDS_binded") 
querynames = substr(list.files(pattern = "*.rds"), 1, nchar(list.files(pattern = "*.rds"))-4)
for (x in querynames){assign(paste0(x),readRDS(paste0(x,".rds")))}
setwd(dirname(dirname(dirname(getwd()))))
#######


source("./R/rds_to_visualdata.R")



