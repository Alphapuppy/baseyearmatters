df1 <- Detailedland %>% filter(para %in% scen1) %>%
  group_by(para,region, crop, year) %>%
  summarise(area = sum(area)/10)%>%  ungroup()%>% #change unit to Mha
  dplyr::select(para, region, crop, year, area) %>%
  filter(year == 2100)

df2 <- df1 %>% group_by(para, year) %>%
  summarise(area = sum(area))

unique(Detailedland$crop)


Cropmap <- read.csv("./data/Cropmapping.csv", header=TRUE, sep=",",comment.char = "#")
Regionmap <- read.csv("./data/Regmapping.csv", header=TRUE, sep=",",comment.char = "#")

df1 <- Detailedland %>% filter(para %in% scen1) %>%
  group_by(para,region, crop, year) %>%
  summarise(area = sum(area)/10)%>%  ungroup()%>% #change unit to Mha
  dplyr::select(para, region, crop, year, area) %>% left_join(Cropmap) %>%
  left_join(Regionmap) %>%
  group_by(para, region = REG, crop =crop1, year ) %>%
  summarise(area = sum(area)) %>% filter(year!= 1990)

df2 <- df1 %>% group_by(para, crop, year) %>% 
  summarise(area = sum(area)) %>%
  spread(para, area) %>% mutate(Diff = Base2015 - Base2010)

ggplot(df2) +
  geom_bar(aes(x = year, y = Diff, fill = crop),
           position="stack", stat="identity", colour="black", size = 0.01, alpha = 0.7) +
  theme_bw() + theme0

ggplot(df1 %>% spread(para, area) %>% mutate(Diff = Base2015 - Base2010)) +
  facet_wrap(vars(region)) +
  geom_bar(aes(x = year, y = Diff, fill = crop),
           position="stack", stat="identity", alpha = 0.7) +
  theme_bw() + theme0 #+ theme(legend.position = c(0.1, 0.2))




df1 <- Detailedland %>% filter(para %in% scen1) %>%
  group_by(para,region, crop, year) %>%
  summarise(area = sum(area)/10)%>%  ungroup()%>% #change unit to Mha
  dplyr::select(para, region, crop, year, area) %>% left_join(Cropmap) %>%
  left_join(Regionmap) %>%
  group_by(para, region = REG, crop, crop1, year ) %>%
  summarise(area = sum(area)) %>% filter(year!= 1990, crop1 %in% c("Cropland", "Biomass"))

df2 <- df1 %>% group_by(para, crop, year) %>% 
  summarise(area = sum(area)) %>%
  spread(para, area) %>% mutate(Diff = Base2015 - Base2010)

ggplot(df2) +
  geom_bar(aes(x = year, y = Diff, fill = crop),
           position="stack", stat="identity", colour="black", size = 0.01, alpha = 0.7) +
  theme_bw() + theme0 +
  geom_hline(yintercept=0,  color = "black") +
  geom_vline(xintercept= which(df2$year == '2015'),  color = "black")

ggplot(df1 %>% spread(para, area) %>% mutate(Diff = Base2015 - Base2010)) +
  facet_wrap(vars(region)) +
  geom_bar(aes(x = year, y = Diff, fill = crop),
           position="stack", stat="identity", alpha = 0.7) +
  theme_bw() + theme0  +
  geom_hline(yintercept=0,  color = "black")+
  geom_vline(xintercept=  which(df2$year == '2015'),  color = "black")




ggplot(GDP) + facet_wrap(vars(region)) +
  geom_line(aes(x = year, y = value, group = para, color = para))

ggplot(GDP %>% group_by(para, year) %>% summarise(value = sum(value))) + 
  geom_line(aes(x = year, y = value, group = para, color = para))


ggplot(GDP %>% spread(para, value) %>% mutate(Diff = Base2015 - Base2010)) + facet_wrap(vars(region)) +
  geom_line(aes(x = year, y = Diff, group = result))



ggplot(POP %>% group_by(para, year) %>% summarise(value = sum(value))) + 
  geom_line(aes(x = year, y = value, group = para, color = para))

ggplot(POP) + facet_wrap(vars(region)) +
  geom_line(aes(x = year, y = value, group = para, color = para))


ggplot(POP %>% spread(para, value) %>% mutate(Diff = Base2015 - Base2010)) + facet_wrap(vars(region)) +
  geom_line(aes(x = year, y = Diff, group = result))




df1 <- Detailedland %>% filter(para %in% scen1) %>%
  group_by(para,region, crop, year) %>%
  summarise(area = sum(area)/10)%>%  ungroup()%>% #change unit to Mha
  dplyr::select(para, region, crop, year, area) %>%
  left_join(Regionmap) %>%
  group_by(para, region = REG, crop, year ) %>%
  summarise(area = sum(area)) %>% 
  group_by(para, crop, year) %>% 
  summarise(area = sum(area)) %>%
  spread(para, area) %>% filter(year == 2100)
