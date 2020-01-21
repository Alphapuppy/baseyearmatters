crop10 <- unique(Detailedland$crop)
scen1 <- setdiff(scen, "RDS")

#crop10 <- setdiff(crop12, c("FodderGrass", "FodderHerb"))

#production area yield
Agprod1_1 <-  Agprod %>%
  filter(para %in% scen1, crop %in% crop10) %>% 
  group_by(para,region, crop, year) %>%
  summarise(prod = sum(prod)) %>%  ungroup() %>%
  left_join(Detailedland %>% filter(para %in% scen1, crop %in% crop10) %>%
              group_by(para,region, crop, year) %>%
              summarise(area = sum(area)/10)%>%  ungroup())%>% #change unit to Mha
  dplyr::select(para, region, crop, year, prod, area) %>%
  group_by(para,region, crop,year) %>%
  summarise(prod = sum(prod), area = sum(area)) %>% ungroup()%>% 
  mutate(yield = prod/area, year = as.numeric(year), crop = tolower(crop))
Agprod1_2 <- Agprod1_1 %>% 
  bind_rows(Agprod1_1 %>% group_by(para,year) %>% 
              summarise(prod = sum(prod), area = sum(area), yield = prod/area) %>% 
              ungroup()%>% mutate(crop = "Aggregated", region = "World")) %>% 
  gather(variable,"area","prod", "yield", value = "value")%>%
  mutate(year = as.numeric(year), para = as.character(para)) %>% 
  mutate(RC = interaction(region, tolower(crop)))

#290 combinations!!!  no zero

Agprice1_1 <- Agprice %>% filter(para %in% scen1) %>%  
  bind_rows(Agprice %>% filter(crop == "biomass") %>% mutate(crop = "biomass_tree")) %>%
  bind_rows(Agprice %>% filter(crop == "biomass") %>% mutate(crop = "biomass_grass")) %>%
  filter(crop %in% crop10) %>% dplyr::select(para, region, crop, year, price) %>%
  mutate(year = as.numeric(year), crop = tolower(crop))

Agprice1_2 <- Agprice1_1 %>% 
  bind_rows(Agprod1_1 %>% left_join(Agprice1_1) %>% group_by(para,year) %>% 
              summarise(price = weighted.mean(price,prod)) %>% 
              ungroup()%>% mutate(crop = "Aggregated", region = "World")) %>% 
  gather(variable,"price", value = "value")%>%
  mutate(year = as.numeric(year), para = as.character(para)) %>% 
  mutate(RC = interaction(region, tolower(crop)))
df00 <- Agprice1_2 %>% filter(para == "Base2010",crop %in%  tolower(crop10), year == 2010)
# 31 * 10 = 310
#ggplot(Agprice1_2, aes(x = RC, y =  price, color = crop)) + geom_point() + coord_flip()
#20 crops not produced but imported!!!!!   So prices for nondomesticly produced good are not changing...
rmprice = setdiff(unique(Agprice1_2$RC), unique(Agprod1_2$RC))

Agprice1_2 %>% filter(RC == "Indonesia.wheat")

#consume
consume1_1 <- 
  Agdemand %>% filter(para %in% scen1,crop %in% tolower(crop10)) %>% rename(consume = value) %>% 
  group_by(para, region, crop, year) %>%  summarise(consume = sum(consume)) %>% ungroup() %>% bind_rows(
    Agdemand %>% filter(para %in% scen1,crop %in% tolower(crop10)) %>% rename(consume = value) %>% 
      group_by(para, year) %>%  summarise(consume = sum(consume)) %>% ungroup() %>% mutate(crop ="Aggregated", region = "World")) %>%
  gather(variable,"consume", value = "value") %>% 
  mutate(year = as.numeric(year), para = as.character(para))%>% 
  mutate(RC = interaction(region, tolower(crop)))
df00 <- consume1_1 %>% filter(para == "Base2010",crop %in%  tolower(crop10), year == 2010)
# 31 * 10 = 310 !  no zero 
#ggplot(consume1_1 %>% filter(crop != "Aggregated"), aes(x = RC, y =  value, color = crop)) + geom_point() + coord_flip()
df <- consume1_1 %>% filter(crop %in%  tolower("palmfruit"), region == "EU-15")

#export and import
export1_1 <- RegAgsource  %>%  filter(para %in% scen1, source == "imported") %>% rename(imported = value) %>% dplyr::select(para, region,crop, year, imported) %>%
  left_join(Agprod %>% 
              mutate(crop=tolower(crop)) %>% 
              filter(para %in% scen1,crop %in% unique(RegAgsource$crop))%>% group_by(para, region, crop, year) %>% summarise(prod = sum(prod))
            , by = c("para", "region", "crop", "year")) %>% 
  left_join(Agdemand %>% 
              filter(para %in% scen1,crop %in% unique(RegAgsource$crop)) %>% 
              rename(consume = value) %>% 
              group_by(para, region, crop, year) %>%
              summarise(consume = sum(consume))) %>%
  mutate(prod = replace_na(prod, 0), imported = replace_na(imported, 0)) %>%
  mutate(export = prod + imported - consume, netexport = export - imported) 
export1_2 <- export1_1 %>% bind_rows(export1_1 %>% group_by(para, year) %>% 
                                       summarise(imported = sum(imported), export=sum(export),netexport =sum(netexport)) %>% 
                                       ungroup() %>% mutate(crop = "Aggregated", region ="World")) %>% 
  dplyr::select(para, region, crop, year, import = imported,export, netexport) %>% 
  gather(variable,"import","export","netexport", value = "value")  %>% 
  mutate(year = as.numeric(year), para = as.character(para)) %>% 
  mutate(RC = interaction(region, tolower(crop)))
df00 <- export1_2 %>% filter(para == "Base2010",crop %in%  tolower(crop10), year == 2010)
# 31 * 10 = 310

df0 <- export1_2 %>% filter(value <=0, variable =="export"); unique(df0$RC)
df1 <- export1_2 %>% filter(value <=0, variable =="import"); unique(df1$RC)

setdiff(setdiff(unique(Agprice1_2$RC), unique(Agprod1_2$RC)), unique(df0$RC))
setdiff(unique(df0$RC),setdiff(unique(Agprice1_2$RC), unique(Agprod1_2$RC)))
rmexport = union(unique(df0$RC),setdiff(unique(Agprice1_2$RC), unique(Agprod1_2$RC)))
rmimport = unique(df1$RC)

Fig1_1 <- bind_rows(Agprod1_2) %>% 
  bind_rows(Agprice1_2 %>% filter(RC %notin% rmprice)) %>%
  bind_rows(consume1_1) %>% 
  bind_rows(export1_2 %>% filter(variable == "export", RC %notin% rmexport)) %>% 
  bind_rows(export1_2 %>% filter(variable == "import", RC %notin% rmimport)) %>% 
  dplyr::select(para, region, crop,variable,year,value, RC)
unique(Fig1_1$variable) 
unique(Fig1_1$region) 
df <- Fig1_1 %>% filter(crop != "Aggregate", para == "Base2010") %>% dplyr::select(region) %>% unique()
Fig1_1 %>% filter(crop != "Aggregate", para == "Base2015") %>% dplyr::select(region) %>% unique()

df <- Fig1_1 %>% filter(crop =="fibercrop", region == "Canada")

Fig1_1 %>% filter(year == 2010, para == "Base2015", variable == "price")
Fig1_1 %>% filter(year == 2010, para == "Base2010", variable == "price")


Fig1_2 <- Fig1_1 %>% 
  group_by(para, region, crop, variable) %>% 
  mutate(value = value/first(value)) %>% ungroup()  #2010 =1 for all variables
