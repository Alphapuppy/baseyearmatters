
setwd("./scenario/RDS")

querynames = substr(list.files(pattern = "*.rds"), 1, nchar(list.files(pattern = "*.rds"))-4)
querynames1 <- unique(substr(querynames,1,nchar(querynames)-8))

dir.create("./RDS_binded", showWarnings = FALSE)

for (x in querynames1) {
query2 <- list.files(pattern = "*.rds")[str_detect(list.files(pattern = "*.rds"),x) == T]
assign(x, query2 %>% map(readRDS) %>% bind_rows())
saveRDS(get(x),file = paste0("./RDS_binded/",x,".rds"))
}

setwd(dirname(dirname(getwd()))) #home



