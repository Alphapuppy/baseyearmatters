prod <- Agprod %>%
  filter(para %in% scen1, crop %in% crop10) %>% 
  group_by(para,region, crop, year) %>%
  summarise(prod = sum(prod)) %>%  ungroup() %>%
  left_join(Detailedland %>% filter(para %in% scen1, crop %in% crop10) %>%
              group_by(para,region, crop, year) %>%
              summarise(area = sum(area)/10)%>%  ungroup())%>% #change unit to Mha
  dplyr::select(para, region, crop, year, prod, area) %>%
  group_by(para, crop, year) %>%
  summarise(prod = sum(prod), area = sum(area)) %>% ungroup() %>% 
  mutate(yield = prod/area, year = as.numeric(year), crop = tolower(crop)) %>% 
  gather(variable,"area","prod", "yield", value = "value")

group = "para"
  
ggplot(prod %>% filter(variable == "area")) + facet_wrap(vars(crop), scales = "free") +
  geom_line(aes(x = year, y = value, group = get(group), color = get(group), linetype = get(group))) +
  theme_bw() + theme0 + theme_leg 
  
ggplot(prod %>% filter(variable == "prod")) + facet_wrap(vars(crop), scales = "free") +
  geom_line(aes(x = year, y = value, group = get(group), color = get(group), linetype = get(group))) +
  theme_bw() + theme0 + theme_leg

ggplot(prod %>% filter(variable == "yield")) + facet_wrap(vars(crop), scales = "free") +
  geom_line(aes(x = year, y = value, group = get(group), color = get(group), linetype = get(group))) +
  theme_bw() + theme0 + theme_leg


Agprice1_1 <- Agprice %>% filter(para %in% scen1, scenario == "pf_d") %>%  
  bind_rows(Agprice %>% filter(crop == "biomass") %>% mutate(crop = "biomass_tree")) %>%
  bind_rows(Agprice %>% filter(crop == "biomass") %>% mutate(crop = "biomass_grass")) %>%
  dplyr::select(para, region, crop, year, price) %>%
  mutate(year = as.numeric(year))

region0 <- unique(Agprice1_1$region)

ggplot(Agprice1_1 %>% filter(region == region0[31]) ) + facet_wrap(vars(crop), scales = "free") +
  geom_line(aes(x = year, y = price, group = get(group), color = get(group), linetype = get(group))) +
  theme_bw() + theme0 + theme_leg 

ggplot(Agprice1_1 %>% filter(region == region0[7]) ) + facet_wrap(vars(crop), scales = "free") +
  geom_line(aes(x = year, y = price, group = get(group), color = get(group), linetype = get(group))) +
  theme_bw() + theme0 + theme_leg 

ggplot(Agprice1_1 %>% filter(region == region0[11]) ) + facet_wrap(vars(crop), scales = "free") +
  geom_line(aes(x = year, y = price, group = get(group), color = get(group), linetype = get(group))) +
  theme_bw() + theme0 + theme_leg 

ggplot(Agprice1_1 %>% filter(region == region0[19]) ) + facet_wrap(vars(crop), scales = "free") +
  geom_line(aes(x = year, y = price, group = get(group), color = get(group), linetype = get(group))) +
  theme_bw() + theme0 + theme_leg 

Agprice1_1 %>% filter(region == "USA", para == "Base2015") %>% spread(year, price )
