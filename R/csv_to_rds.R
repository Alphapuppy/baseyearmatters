
setwd(n)
Para =s
rds = "../RDS/"
#################################################  
  quary = "Detailedland"
  Filenames = list.files(pattern = paste0(quary, "*"))
  for (file in Filenames) {
    assign(file, gather(read.csv(textConnection(readLines(file[1])[-1]), header = TRUE, stringsAsFactors = FALSE), 
                        "year", "value", -c(scenario, region, LandLeaf, Units, X)) %>% mutate(scenario = file))
  }
  
  assign(quary, do.call(rbind, lapply(Filenames, get)))
  rm(list = Filenames)
  
  Detailedland1 <- get(quary) %>%
    within(rm("X")) %>% mutate(year = substr(year,2,5)) %>% rename(landleaf = LandLeaf, unit = Units) %>%
    mutate(result = quary, scenario = substr(scenario, nchar(quary)+2, nchar(scenario)-4),para= Para)
  # separate(col = scenario, into = c("result","exp","scen","csv")) %>% unite(scenario, c(exp,scen), sep = "_")
  
  Detailedland2 <- Detailedland1 %>%
    filter(str_detect(landleaf, paste(C_1, collapse = "|"))) %>%
    separate(col = landleaf, into = c("crop","basin")) %>%
    mutate(water = "NA", tech = "NA") %>%
    dplyr::select(scenario,result, para, region, basin, crop, unit,crop,year, area =value, water,tech) %>%
    bind_rows(Detailedland1 %>%
            filter(str_detect(landleaf, paste(C_2, collapse = "|"))) %>%
            separate(col = landleaf, into = c("crop","basin","water","tech")) %>%
            dplyr::select(scenario,result, para, region,basin, crop,unit,crop,year, area =value, water,tech))%>%
    bind_rows(Detailedland1 %>%
            filter(str_detect(landleaf, paste(C_3, collapse = "|"))) %>%
            separate(col = landleaf, into = c("Crop1", "Crop2","basin","water","tech")) %>%
            unite(crop, c(Crop1,Crop2), remove = T) %>%
            dplyr::select(scenario, result, para,region,basin, crop,unit,crop,year, area =value, water,tech)) %>%
    filter(crop %notin% Land_fixed, year %notin% nonyear)
  
  ##save
  saveRDS(Detailedland2, file = paste0(rds,quary,Para,".rds"))
  
  rm(Detailedland, Detailedland1, Detailedland2)
  
  ###############################################
  quary = "Agrental"
  Filenames = list.files(pattern = paste0(quary, "*"))
  
  for (file in Filenames) {
    df0<- read.csv(textConnection(readLines(file[1])[-1]), header = TRUE, stringsAsFactors = FALSE)
    assign(file,  gather(df0,"year", "value", -c(scenario, region, LandLeaf, Units, X)) %>% mutate(scenario = file))
  }
  assign(quary, do.call(rbind, lapply(Filenames, get)))
  
  rm(list = Filenames)
  
  Agrental1 <- get(quary) %>%
    within(rm("X")) %>% mutate(year = substr(year,2,5)) %>% rename(landleaf = LandLeaf, unit = Units) %>%
    mutate(result = quary, scenario = substr(scenario, nchar(quary)+2, nchar(scenario)-4),para= Para)
  # separate(col = scenario, into = c("result","exp","scen","csv")) %>% unite(scenario, c(exp,scen), sep = "_")
  
  Agrental2 <- Agrental1 %>%
    filter(str_detect(landleaf, paste(C_1, collapse = "|"))) %>%
    separate(col = landleaf, into = c("crop","basin")) %>%
    mutate(water = "NA", tech = "NA") %>%
    dplyr::select(scenario,result,para, region, basin, crop, unit,crop,year, rental =value, water,tech) %>%
    bind_rows(Agrental1 %>%
            filter(str_detect(landleaf, paste(C_2, collapse = "|"))) %>%
            separate(col = landleaf, into = c("crop","basin","water","tech")) %>%
              dplyr::select(scenario,result,para, region,basin, crop,unit,crop,year, rental =value, water,tech))%>%
    bind_rows(Agrental1 %>%
            filter(str_detect(landleaf, paste(C_3, collapse = "|"))) %>%
            separate(col = landleaf, into = c("Crop1", "Crop2","basin","water","tech")) %>%
            unite(crop, c(Crop1,Crop2), remove = T) %>%
              dplyr::select(scenario,result,para, region,basin, crop,unit,crop,year, rental =value, water,tech)) %>%
    filter(crop %notin% Land_fixed, year %notin% nonyear)
  
  saveRDS(Agrental2, file = paste0(rds,quary,Para,".rds"))
  rm(Agrental, Agrental1, Agrental2)
  #########################################
  quary = "Agprod"
  Filenames = list.files(pattern = paste0(quary, "*"))
  
  for (file in Filenames) {
    df0<- read.csv(textConnection(readLines(file[1])[-1]), header = TRUE, stringsAsFactors = FALSE)
    assign(file,  gather(df0,"year", "value", -c(scenario, region, sector, subsector, output, technology, Units, X)) %>% mutate(scenario = file))
  }
  assign(quary, do.call(rbind, lapply(Filenames, get)))
  #assign(quary, do.call(rbind, sapply(Filenames, read.csv, simplify = FALSE,USE.NAMES = TRUE)))
  rm(list = Filenames)
  Agprod1 <- get(quary) %>%
    within(rm("X")) %>% mutate(year = substr(year,2,5)) %>% rename(landleaf = technology, unit = Units) %>%
    mutate(result = quary, scenario = substr(scenario, nchar(quary)+2, nchar(scenario)-4),para= Para)
  # separate(col = scenario, into = c("result","exp","scen","csv")) %>% unite(scenario, c(exp,scen), sep = "_")
  
  Agprod2 <- Agprod1 %>%
    filter(str_detect(landleaf, paste(C_1, collapse = "|"))) %>%
    separate(col = landleaf, into = c("crop","basin")) %>%
    mutate(water = "NA", tech = "NA") %>%
    dplyr::select(scenario,result,para, region, basin, crop, unit, crop,year, prod =value, water,tech) %>%
    bind_rows(Agprod1 %>%
            filter(str_detect(landleaf, paste(C_2, collapse = "|"))) %>%
            separate(col = landleaf, into = c("crop","basin","water","tech")) %>%
              dplyr::select(scenario,result,para, region,basin, crop,unit,crop,year, prod =value, water,tech))%>%
    bind_rows(Agprod1 %>%
            filter(str_detect(landleaf, paste(C_3, collapse = "|"))) %>%
            separate(col = landleaf, into = c("Crop1", "Crop2","basin","water","tech")) %>%
            unite(crop, c(Crop1,Crop2), remove = T) %>%
              dplyr::select(scenario,result,para, region,basin, crop,unit,crop,year, prod =value, water,tech)) %>%
    filter(crop %notin% Land_fixed, year %notin% nonyear)
  
  saveRDS(Agprod2, file = paste0(rds,quary,Para,".rds"))
  rm(Agprod,Agprod1,Agprod2)
  #########################################
  quary = "Agdemand"
  Filenames = list.files(pattern = paste0(quary, "*"))
  for (file in Filenames) {
    df0<- read.csv(textConnection(readLines(file[1])[-1]), header = TRUE, stringsAsFactors = FALSE)
    assign(file,  gather(df0,"year", "value", -c(scenario, region, input, sector, Units, X)) %>% mutate(scenario = file))
  }
  assign(quary, do.call(rbind, lapply(Filenames, get)))
  rm(list = Filenames)
  Agdemand1 <- get(quary) %>%
    within(rm("X")) %>% mutate(year = substr(year,2,5)) %>% rename(unit = Units, crop = input) %>%
    mutate(result = quary, scenario = substr(scenario, nchar(quary)+2, nchar(scenario)-4),para= Para) %>%
    filter(year %notin% nonyear) %>%
    mutate(crop = as.character(crop)) %>%
    mutate(crop = if_else(str_detect(crop, "regional"),tolower(substr(crop,nchar("regional")+2, nchar(crop))), tolower(crop)))
  
  saveRDS(Agdemand1, file = paste0(rds,quary,Para,".rds"))
  rm(Agdemand, Agdemand1)
  ###############################################
  quary = "Agprice"
  Filenames = list.files(pattern = paste0(quary, "*"))
  for (file in Filenames) {
    df0<- read.csv(textConnection(readLines(file[1])[-1]), header = TRUE, stringsAsFactors = FALSE)
    assign(file,  gather(df0,"year", "value", -c(scenario, region, sector, Units, X)) %>% mutate(scenario = file))
  }
  assign(quary, do.call(rbind, lapply(Filenames, get)))
  #assign(quary, do.call(rbind, sapply(Filenames, read.csv, simplify = FALSE,USE.NAMES = TRUE)))
  rm(list = Filenames)
  Agprice1 <- get(quary) %>%
    within(rm("X")) %>% mutate(year = substr(year,2,5)) %>% rename(unit = Units, crop = sector,price = value) %>%
    mutate(result = quary, scenario = substr(scenario, nchar(quary)+2, nchar(scenario)-4),para= Para) %>%
    filter(year %notin% nonyear)
  
  saveRDS(Agprice1, file = paste0(rds,quary,Para,".rds"))
  rm(Agprice, Agprice1)
  ###############################################
  quary = "RegAgsource"
  Filenames = list.files(pattern = paste0(quary, "*"))
  for (file in Filenames) {
    df0<- read.csv(textConnection(readLines(file[1])[-1]), header = TRUE, stringsAsFactors = FALSE)
    assign(file,  gather(df0,"year", "value", -c(scenario, region, sector, subsector, input, Units, X)) %>% mutate(scenario = file))
  }
  assign(quary, do.call(rbind, lapply(Filenames, get)))
  rm(list = Filenames)
  RegAgsource1 <- RegAgsource %>%
    within(rm("X")) %>% mutate(year = substr(year,2,5)) %>% rename(unit = Units, crop = sector) %>%
    mutate(result = quary, scenario = substr(scenario, nchar(quary)+2, nchar(scenario)-4),para= Para) %>%
    filter(year %notin% nonyear) %>%
    mutate(crop = as.character(crop))%>%
    mutate(crop = if_else(str_detect(crop, "regional"),tolower(substr(crop,nchar("regional")+2, nchar(crop))), tolower(crop))) %>%
    filter(crop %notin% noncrop) %>%
    mutate(source = if_else(str_detect(subsector, "domestic"), "domestic", "imported")) %>%
    dplyr::select(scenario,result, para, region, year, unit, crop, source, value)
  
  saveRDS(RegAgsource1, file = paste0(rds,quary,Para,".rds"))
  rm(RegAgsource, RegAgsource1)
  ################################################
  quary = "GDP"
  Filenames = list.files(pattern = paste0(quary, "*"))
  for (file in Filenames) {
    df0<- read.csv(textConnection(readLines(file[1])[-1]), header = TRUE, stringsAsFactors = FALSE)
    assign(file,  gather(df0,"year", "value", -c(scenario, region, Units, X)) %>% mutate(scenario = file))
  }
  assign(quary, do.call(rbind, lapply(Filenames, get)))
  rm(list = Filenames)
  
  GDP1 <- GDP %>%
    within(rm("X")) %>% mutate(year = substr(year,2,5)) %>% rename(unit = Units) %>%
    mutate(result = quary, scenario = substr(scenario, nchar(quary)+2, nchar(scenario)-4),para= Para) %>%
    filter(year %notin% nonyear) %>%
    dplyr::select(scenario,result,para, region, year, unit, value)
  
  saveRDS(GDP1, file = paste0(rds,quary,Para,".rds"))
  rm(GDP, GDP1)
  ###############################################
  quary = "POP"
  Filenames = list.files(pattern = paste0(quary, "*"))
  for (file in Filenames) {
    df0<- read.csv(textConnection(readLines(file[1])[-1]), header = TRUE, stringsAsFactors = FALSE)
    assign(file,  gather(df0,"year", "value", -c(scenario, region, Units, X)) %>% mutate(scenario = file))
  }
  assign(quary, do.call(rbind, lapply(Filenames, get)))
  rm(list = Filenames)
  
  POP1 <- POP %>%
    within(rm("X")) %>% mutate(year = substr(year,2,5)) %>% rename(unit = Units) %>%
    mutate(result = quary, scenario = substr(scenario, nchar(quary)+2, nchar(scenario)-4),para= Para) %>%
    filter(year %notin% nonyear) %>%
    dplyr::select(scenario, region, year, unit, value)
  
  saveRDS(POP1, file = paste0(rds,quary,Para,".rds"))
  rm(POP, POP1)
  ###############################################
  
  setwd(dirname(dirname(getwd()))) #home

