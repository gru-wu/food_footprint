# Processing results FABIO
# Starting date: 23/01/2020

base::rm(list = ls())   # cleaning workspace

# Packages for data processing
library(tidyverse)
library(openxlsx)
library(showtext)
suppressPackageStartupMessages(library(data.table))

# Load data ---------
base::load("H:/fabio_ghg/data/Classifications.RData")

prob.countries <- c("BHR","BDI","LBY","PNG","PRI","ERI",
                    "QAT","SGP","SOM","SYR","COD", "SSD")

territorial_results <- data.table::fread("results/2022-07-01.territorial.results.csv") %>% 
  filter(!Category %in%  c("1 years", "5 years" ,"10 years","20 years","50 years" ),
         !ISO %in% prob.countries) 

footprint_results <- data.table::fread("results/2022-06-16.footprint.results.csv") %>% 
  filter(!Category %in%  c("1 years", "5 years" ,"10 years", "20 years","50 years" ),
         !ISO %in% prob.countries)

trade_results <- data.table::fread("results/2022-06-28.trade.results.csv") %>% 
  filter(!Category %in%  c("5 years" ,"10 years","20 years","50 years", "1`years" ),
         !`EX ISO` %in% prob.countries,
         !`IM ISO` %in% prob.countries)

input_results <- data.table::fread("results/2022-06-28.input.results.csv") %>% 
  filter(!Category %in%  c("5 years" ,"10 years","20 years","50 years","1 years" ),
         !ISO %in% prob.countries)

footprint.origin_results <- data.table::fread("results/2022-07-01.footprint.origin.results.csv") %>% 
  filter(!Category %in%  c("5 years" ,"10 years","20 years","50 years", "1 years" ),
         !ISO %in% prob.countries)

roe <- readRDS("H:/food_footprint_ppm/results/roe.rds") 

biomass <- readRDS("H:/food_footprint_ppm/results/biomass.rds") 

population <- readr::read_csv("data/a999f80e-8914-49fe-a0a1-a6c6a8b5f31c_Data.csv") %>% 
  tidyr::gather(Year, Pop, -c(1:4)) %>%
  dplyr::filter(!is.na(Pop) & Pop != "..") %>% 
  dplyr::mutate(Year = as.integer(substr(Year, 1, 4)),
                Pop = as.numeric(Pop)) %>% 
  rename(ISO = `Country Code`)

population <- population %>% 
  bind_rows(data.frame(ISO = "SUN", Year = 1986, 
                       Pop = sum(population %>% filter(ISO %in% ex_USSR & Year == 1986) %>% 
                                   pull(Pop))),
            data.frame(ISO = "YUG", Year = 1986, 
                       Pop = sum(population %>% filter(ISO %in% ex_Yug & Year == 1986) %>% 
                                   pull(Pop))))

# Correction iso regions
countries.europe <- countries %>% select(ISO, Continent) %>% 
  filter(Continent %in% c("EU", "EUR"))

iso_regions <- read_csv("data/iso_regions.csv") %>% 
  mutate(region = if_else(ISO %in% countries.europe$ISO, "Europe", region))

# Graphical features
font_add("lato", "Lato-Regular.ttf")
showtext_auto()

palette_OkabeIto <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")

common.plot <- list(cowplot::theme_half_open(font_family = "lato") + cowplot::background_grid() +
                      theme(axis.text = element_text(size = 10),
                        axis.title.x = element_blank(),
                        legend.spacing.y = unit(-0.1, "cm"),
                        legend.position =  "bottom"))

# Figure 1 ---------
trade_to_total <-  trade_results %>%
  group_by(Year) %>% summarise(Trade_Mg = sum(Value)/1000) %>% 
  left_join(footprint_results %>% 
              group_by(Year) %>% summarise(Total = sum(Value)/1000)) %>% 
  ungroup() %>% 
  mutate(total =  (Total/Total[Year == 1986]),
          foreign = (Trade_Mg/Trade_Mg[Year == 1986]),
         domestic = ((Total-Trade_Mg)/(Total[Year == 1986]-Trade_Mg[Year == 1986]))) %>% 
  select(-Trade_Mg, -Total) %>% 
  gather(category, value, total:domestic)

trade_total_roe <- roe %>% data.table::rbindlist() %>% 
  filter(!`EX ISO` %in% prob.countries,
         !`IM ISO` %in% prob.countries) %>% 
  mutate(category = ifelse(`EX ISO` == `IM ISO`, "domestic", "foreign")) %>% 
  group_by(Year, category) %>% 
  summarise(Gg = sum(Value)/1000) 

trade_total_roe <- trade_total_roe %>%
  bind_rows(trade_total_roe %>% group_by(Year) %>% 
              summarise(Gg = sum(Gg)) %>% 
              ungroup() %>% mutate(category = "total")) %>% 
  group_by(category) %>% 
  mutate(value = (Gg/Gg[Year == 1995])) %>% 
  select(-Gg)

trade_total_biomass <- biomass %>% data.table::rbindlist() %>% 
  filter(!is.na(`EX ISO`)) %>% 
  filter(!`EX ISO` %in% prob.countries,
         !`IM ISO` %in% prob.countries) %>% 
  mutate(category = ifelse(`EX ISO` == `IM ISO`, "domestic", "foreign")) %>% 
  group_by(Year, category) %>% 
  summarise(Gg = sum(Value)/1000) 

trade_total_biomass <- trade_total_biomass %>% 
  bind_rows(trade_total_biomass %>% group_by(Year) %>% 
      summarise(Gg = sum(Gg)) %>% 
      ungroup() %>% mutate(category = "total")) %>% 
  group_by(category) %>% 
  mutate(value = (Gg/Gg[Year == 1995])) %>% 
  select(-Gg)

data.plot <- trade_to_total %>% mutate(System = "Agro-food system") %>% 
  bind_rows(trade_total_biomass %>% mutate(System = "Textile and other land activities"),
            trade_total_roe %>% mutate(System = "Rest of the economy")) %>% 
  mutate(System = factor(System, c("Agro-food system", "Textile and other land activities",
                                   "Rest of the economy")))

p1 <- ggplot(data.plot, aes(x=Year, y=value, group=category)) +
  geom_point(size=1.5, aes(colour=category)) +
  geom_line(size=0.5, aes(colour=category)) +
  scale_x_continuous(breaks = c(1985, 1990, 1995, 2000, 2005, 2010), limits = c(1985, 2013)) +
  scale_y_continuous( breaks=c(0.80, 1.00, 1.2,1.4, 1.6, 1.80), limits = c(0.8, 1.80), labels = scales::label_percent()) +
  scale_color_manual(values = palette_OkabeIto[c(2,4,1)]) +
  common.plot + theme(strip.text = element_text(size = 12),
                      legend.title = element_blank(),
                      axis.title.y = element_text(size = 10, margin = margin(t = 0, r = 10, b = 0, l = 0)))+
  cowplot::background_grid() +
  theme(axis.title.x=element_blank(), panel.spacing.x = unit(2, "lines"))+
  labs(y="% change in GHG emissions") +
  facet_wrap(~ System, nrow = 1, scales="free") +
  guides(fill = guide_legend(nrow = 1, title = ""))

plot(p1)

# Figure Prod vs Cons ---------
eu_28 <- filter(countries, Continent == "EU") %>% pull(ISO)

years <- c(1986:2013)

obtainFig1 <- function(country){
  
  results <- territorial_results %>% 
    filter(ISO %in% country) %>% 
    group_by(Year) %>% 
    summarise(TE = sum(Value)/1000000) %>% 
    left_join(footprint_results %>% 
                filter(ISO %in% country) %>% 
                group_by(Year) %>% 
                summarise(FT = sum(Value)/1000000)) %>% 
                mutate(`Net balance` = FT-TE) %>% 
    mutate(Flow = ifelse(`Net balance` >0 , "net imports", "net exports"),
           `Net balance` = abs(`Net balance`),
           ISO = ifelse(length(country)== 1, country, "EU28")) 
  
  return(results)
  
}

data.fig1 <- lapply(list("BRA", "USA", "IND", eu_28, "CHN"), obtainFig1) %>% 
  rbindlist()

labels_countries <- c("BRA" = "Brazil",
                      "USA" = "USA",
                      "CHN" = "China",
                      "EU28" = "EU-28",
                      "IND" = "India",
                      "IDN" = "Indonesia")

data.fig1 <- data.fig1 %>% 
  mutate(Max = ifelse(FT > TE, FT, TE),
         Min = ifelse(FT < TE, FT, TE)) %>% 
  reshape2::melt(-c(2:4)) %>% 
  mutate(Flow = ifelse(ISO == "IND" & Year %in% c(1998, 1999), "net exports", Flow)) 

pop_sel <- population %>% 
  filter(ISO %in% c(eu_28, "BRA", "CHN", "USA", "IND", "IDN")) %>% 
  mutate(ISO = ifelse(ISO %in% eu_28, "EU28", ISO)) %>% 
  group_by(Year, ISO) %>%  summarise(Pop = sum(Pop)) %>% ungroup()

data.fig1b <- data.fig1 %>% 
  filter(variable %in% c("FT", "TE")) %>% 
  select(-Flow, -Max, -Min) %>% 
  left_join(pop_sel, by = c("ISO", "Year")) %>% 
  mutate(value = value*1000000000/Pop) %>% 
  select(-Pop) %>% 
  spread(variable, value) %>% 
  mutate(Max = ifelse(FT > TE, FT, TE),
         Min = ifelse(FT < TE, FT, TE),
         Flow = ifelse((FT-TE) > 0, "net imports", "net exports"),
         Net = abs(FT-TE)) %>% 
  reshape2::melt(-c(3,4,8))

p2 <- ggplot(data = data.fig1 %>% filter(variable == "FT")) +
  geom_line(aes(x = Year, y = value, linetype = variable), colour = "black", size = 1) +
  geom_line(data = (data.fig1 %>% dplyr::filter(variable == "TE")),
            aes(x = Year, y = value, linetype = variable), colour = "black", size = 1)+
  geom_ribbon(data = (data.fig1 %>%dplyr::filter(variable == "Net balance") %>% select(-variable,-value)),
              aes(x = Year, ymax = Max, ymin = Min, fill = Flow), alpha = .6) +
  common.plot + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), strip.text = element_text(size = 12),
                      axis.title.y = element_text(size = 10),
                      legend.key.width= unit(1, 'cm'))+
  facet_wrap(~ ISO, nrow = 1, labeller = labeller(ISO = labels_countries), scales="free") +
    guides(fill = guide_legend(nrow = 1, title = ""), linetype = guide_legend(nrow = 1, title = "")) +
  scale_fill_manual(values = palette_OkabeIto[c(3,6)]) +
  scale_y_continuous(limits = c(0, 2), labels = scales::number_format(accuracy = 0.1))+ 
  scale_linetype_manual(labels=c("consumption-based", "production-based"),
                        values = c("dashed", "solid")) +
  scale_x_continuous(breaks=c(1990, 2000, 2010))+
  labs(y=expression(GtCO[2]~e/yr))

plot(p2)

p3 <- ggplot(data = data.fig1b %>% dplyr::filter(variable == "FT")) +
  geom_line(aes(x = Year, y = value, linetype = variable), colour = "black", size = 1) +
  geom_line(data = (data.fig1b %>%  dplyr::filter(variable == "TE")),
            aes(x = Year,  y = value, linetype = variable),  colour = "black", size = 1)+
  geom_ribbon(data = (data.fig1b %>%   dplyr::filter(variable == "Net") %>% select(-variable,-value)),
              aes(x = Year, ymax = Max,  ymin = Min,  fill = Flow),  alpha = .6) +
  geom_hline(yintercept = 5, linetype = "dashed", color = "red",
              size= 1) +
  common.plot +  theme(strip.text = element_blank(), axis.title.y = element_text(size = 10))+
  facet_wrap(~ ISO, nrow = 1, labeller = labeller(ISO = labels_countries),scales="free") +
  guides(linetype = guide_legend(nrow = 1),fill = guide_legend(nrow = 1, title = "")) +
  scale_fill_manual(values = palette_OkabeIto[c(3,6)]) +
  scale_y_continuous(limits = function(Capita) {if(max(Capita) > 10){c(0, 11)} else{ c(0,5)}}, labels = scales::number_format(accuracy = 0.1)) + 
  scale_x_continuous(breaks=c(1990, 2000, 2010))+
  scale_linetype_manual(labels=c("consumption-based", "production-based"),  values = c("dashed", "solid")) +
  labs(y=expression(tCO[2]~e/yr/cap))

plot(p3)

Fig1 <- cowplot::plot_grid(p1, cowplot::plot_grid(cowplot::plot_grid(p2+theme(legend.position="none"), 
                                                                     p3+theme(legend.position="none"), ncol = 1, 
                                        align = 'v', axis = "t"), cowplot::get_legend(p2), ncol = 1, rel_heights = c(1, .1), align = 'h', axis = "none"), 
                           ncol = 1, align = 'v', rel_heights = c(0.6, 1), labels = c("a", "b"))
plot(Fig1)

ggsave( file = "Fig1_final.png", path="results/",
        plot = Fig1,
        dpi = 96,
        width = 25,
        height = 18,
        units = "cm" )

# Figure SI ---------
territorial_results <- read_csv("results/2022-06-10.territorial.results.csv") %>% 
  #data.table::fread("results emissions included/2022-05-14.territorial.results.csv") #%>% 
  filter(Source ==  c("Land use change"))

footprint_results <-  read_csv("results/2022-06-07.footprint.results.csv") %>% 
  filter(Source ==  c("Land use change"))

luc <- territorial_results %>% 
  group_by(Category, Year) %>% summarise(Value = sum(Value)/1000000) %>% 
  ungroup() %>% 
  mutate(Category = ifelse(Category == "1 years", "1 year", Category),
    Category = factor(Category, levels = c("1 year", "5 years", "10 years",
                                                "20 years", "50 years",
                                                "100 years")))

luc.total <- territorial_results %>% 
  group_by(Category) %>% summarise(Value = sum(Value)/1000000)

p <- ggplot(data = luc, aes(x = Year, y = Value)) +
  geom_point(size=1.5, aes(colour=Category)) +
  geom_line(size=0.5, aes(colour=Category)) +
  scale_x_continuous(breaks = c(1985, 1990, 1995, 2000, 2005, 2010), limits = c(1985, 2013)) +
  #scale_y_continuous( breaks=c(0.80, 1.00, 1.2,1.4, 1.6, 1.80), limits = c(0.8, 1.80), labels = scales::label_percent()) +
  scale_color_manual(values = palette_OkabeIto) +
   common.plot + theme(strip.text = element_text(size = 12),
                      axis.title.y = element_text(size = 10),
                      legend.title = element_blank())+
    guides(fill = guide_legend(nrow = 2, title = ""), linetype = guide_legend(nrow =2, title = "")) +
  #scale_y_continuous(limits = c(0, 2.1), labels = scales::number_format(accuracy = 0.1))+ 
  scale_y_continuous(limits = c(0, 6), labels = scales::number_format(big.mark = " "))+
  # scale_linetype_manual(labels=c("Consumption-based", "Production-based"),
  #                       values = c("dashed", "solid")) +
  labs(y=expression(GtCO[2]~e/yr))

plot(p)

area_C_stock_density <- base::readRDS("H://fabio_ghg/inst/ghg_fabio_input/area_C_stock_density.rds")
var_luh2 <- base::unique(area_C_stock_density$LUH2)

# Data is aggregated (c3, c4, rangerland, RoW, etc)
C_changes <- area_C_stock_density %>% 
  dplyr::filter(!LUH2 %in% grep("harv", var_luh2, value = TRUE),   # Wood harvest is excluded
                !LUH2 %in% grep("bioh", var_luh2, value = TRUE)
  ) %>% 
  dplyr::select(-`C density Mg ha`) %>% 
  dplyr::group_by(
    ISO3,
    Category,
    Year
  ) %>% 
  dplyr::summarise_if(is.numeric, sum) %>% 
  dplyr::ungroup() %>% 
  dplyr::rename(ISO = ISO3) %>% # harmonize
  aggregate_to_RoW(., Category, Year) %>% 
  dplyr::mutate(`Harvest TgC` = ifelse(Category == "Abandoned",
                                       -`Harvest TgC`,
                                       `Harvest TgC`)) %>% 
  left_join(iso_regions %>% select(-country)) %>% 
  mutate(region = ifelse(ISO == "ANT", "LatAm", region),
         region = ifelse(ISO == "KIR", "East Asia & Pacific", region),
         region = ifelse(ISO == "SYR", "MENA", region),
         region = ifelse(ISO == "TWN", "South Asia", region),
         region = ifelse(ISO %in% c("ANT", "NCL", "PYF"), "LatAm", region),
         region= ifelse(region == "LatAm", "Latin America & C.", region),
         region= ifelse(region == "Africa", "Sub-Saharan Africa", region),
         region= ifelse(region == "MENA", "Middle East & N.Afr.", region)) %>% 
  filter(Category != "Abandoned",
         !is.na(region)) %>% 
  group_by(Year, region) %>% 
  summarise(`Area Mha` = sum(`Harvest TgC`)) %>% 
  mutate(region = factor(region, levels = c("Europe",
                                            "South Asia", 
                                            "Latin America & C.", 
                                            "Sub-Saharan Africa",
                                            "East Asia & Pacific",
                                            "North America",
                                            "Central Asia", 
                                            "Middle East & N.Afr.")))

p2 <- ggplot(data = C_changes, aes(x = Year, y = `Area Mha`)) +
  geom_bar(aes(fill=region), stat='identity') +
  scale_x_discrete(breaks = c(1850, 1875, 1900, 1925, 1950, 1975, 2000, 2025)) +
  #scale_y_continuous( breaks=c(0.80, 1.00, 1.2,1.4, 1.6, 1.80), limits = c(0.8, 1.80), labels = scales::label_percent()) +
  scale_fill_manual(values = c(palette_OkabeIto[1], palette_OkabeIto[4], palette_OkabeIto[3], palette_OkabeIto[2],
                    palette_OkabeIto[5], palette_OkabeIto[8], palette_OkabeIto[7], palette_OkabeIto[6])) +
  common.plot + theme(strip.text = element_text(size = 12),
                      axis.title.y = element_text(size = 10),
                      legend.title = element_blank())+
  guides(fill = guide_legend(nrow = 4, title = "")) +
  #scale_y_continuous(limits = c(0, 2.1), labels = scales::number_format(accuracy = 0.1))+ 
  #scale_y_continuous(limits = c(0, 6), labels = scales::number_format(big.mark = " "))+
  # scale_linetype_manual(labels=c("Consumption-based", "Production-based"),
  #                       values = c("dashed", "solid")) +
  labs(y=expression(`Harvest TgC/yr`))

plot(p2)

FigAmort <- cowplot::plot_grid(p, p2, ncol = 2, align = 'h',  labels = c("a", "b"))

plot(FigAmort)

ggsave( file = "FigAmort.png", path="results/",
        plot = FigAmort,
        dpi = 96,
        width = 25,
        height = 12,
        units = "cm" )


# Figure SI Net flows
population_sel_si <- population %>% 
  filter(Year == 2013, Pop > 2000000, ISO %in% countries$ISO)

population_europe <- population_sel_si %>% 
  filter(ISO %in% (iso_regions %>% filter(region == "Europe") %>% 
                      pull(ISO))) %>% pull(ISO)

population_am <- population_sel_si %>% 
  filter(ISO %in% (iso_regions %>% filter(region %in% c("North America", "LatAm")) %>% 
                     pull(ISO))) %>% pull(ISO)

population_sa <- population_sel_si %>% 
  filter(ISO %in% (iso_regions %>% filter(region %in% c("South Asia")) %>% 
                     pull(ISO))) %>% pull(ISO)

population_af <- population_sel_si %>% 
  filter(ISO %in% (iso_regions %>% filter(region %in% c("Africa")) %>% 
                     pull(ISO))) %>% pull(ISO)

population_ca <- population_sel_si %>% 
  filter(ISO %in% (iso_regions %>% filter(region %in% c("Central Asia")) %>% 
                     pull(ISO))) %>% pull(ISO)

population_mena <- population_sel_si %>% 
  filter(ISO %in% (iso_regions %>% filter(region %in% c("MENA")) %>% 
                     pull(ISO))) %>% pull(ISO)

population_eap <- population_sel_si %>% 
  filter(ISO %in% (iso_regions %>% filter(region %in% c("East Asia & Pacific")) %>% 
                     pull(ISO))) %>% pull(ISO)

labels_countries <- c("DEU" = "Germany",
                             "FRA" = "France",
                             "GBR" = "United Kingdom",
                             "ESP" = "Spain",
                             "ITA" = "Italy",
                             "UKR" = "Ukraine",
                             "TUR" = "Turkey",
                             "NLD" = "The Netherlands",
                             "POL" = "Poland",
                             "GRC" = "Greece",
                             "ROU" = "Romania",
                             "PRT" = "Portugal",
                             "IRL" = "Ireland",
                             "BEL" = "Belgium",
                             "BLR" = "Belarus",
                             "DNK" = "Denmark",
                             "AUT" = "Austria",
                             "SWE" = "Sweden",
                             "HUN" = "Hungary",
                             "BGR" = "Bulgaria", 
                             "CZE" = "Czech Republic",
                             'CHE' = "Switzerland",
                             "NOR" = "Norway", 
                             "FIN" = "Finland",
                             "SRB" = "Serbia",
                             "BIH" = "Bosnia and Herzegovina",
                             "HRV" = "Croatia", 
                             "SVK" = "Slovakia",
                             "ALB" = "Albania",
                             "LTU" = "Lithuania",
                             "LVA" = "Latvia",
                             "SVN" = "Slovenia",
                             "MDA" = "Moldova",
                             "MKD" = "North Macedonia",
                             "BRA" = "Brazil",
                             "USA" = "United States",
                             "CAN" = "Canada",
                             "ARG" = "Argentina",
                             "BOL" = "Bolivia",
                             "CHL" = "Chile",
                             "COL" = "Colombia",
                             "CRI" = "Costa Rica",
                             "CUB" = "Cuba",
                             "DOM" = "Dominican Republic",
                             "ECU" = "Ecuador",
                             "SLV" = "El Salvador",
                             "HTI" = "Haiti",
                             "HND" = "Honduras",
                             "JAM" = "Jamaica",
                             "MEX" = "Mexico",
                             "NIC" = "Nicaragua",
                             "PAN" = "Panama",
                             "PRY" = "Paraguay",
                             "PER" = "Peru",
                             "PRI" = "Puerto Rico",
                             "URY" = "Uruguay",
                             "VEN" = "Venezuela",
                             "GTM" = "Guatemala",
                             "AFG" = "Afganistan",
                             "BGD" = "Bangladesh",
                             "IND" = "India",
                            "KGZ"= "Kyrgyzstan",
                            "LAO" = "Laos",
                            "NPL" = "Nepal", 
                            "PAK"  = "Pakistan",
                            "LKA" = "Sri Lanka",
                      "VNM" = "Vietnam",
                      "AGO" = "Angola",
                      "BEN" =  "Benin",
                      "BWA" = "Botswana",
                      "BFA" =  "Burkina Faso",
                      "BDI" = "Burundi",
                      "CMR" = "Cameroon", 
                      "CAF" = "Central African Republic",
                      "TCD" = "Chad", 
                      "COD" = "Dem. Rep. of Congo",
                      "COG" = "Republic of the Congo",
                      "CIV" = "Cote d'Ivoire",
                      "ETH" = "Ethiopia",
                      "GHA" = "Ghana",
                      "GIN" = "Guinea",
                      "KEN" = "Kenya",
                      "LSO" = "Lesotho",
                      "LBR" = "Liberia",
                      "MDG" = "Madagascar",
                      "MWI" = "Malawi",
                      "MLI" = "Mali",
                      "MRT" = "Mauritania",
                      "MOZ" = "Mozambique",
                      "NAM" = "Namibia",
                      "NER" = "Niger",
                      "NGA" = "Nigeria",
                      "RWA" = "Rwanda",
                      "SEN" = "Senegal",
                      "SLE" = "Sierra Leone",
                      "SOM" = "Somalia",
                      "ZAF" = "South Africa",
                      "SSD" = "South Sudan",
                      "SDN" = "Sudan",
                      "TZA" = "Tanzania",
                      "TGO" = "Togo",
                      "UGA" = "Uganda",
                      "ZMB" = "Zambia",
                      "ZWE"= "Zimbawe" ,
                      "ARM" = "Armenia",
                      "AZE" = "Azerbaijan",
                      "GEO" = "Georgia",
                      "KAZ"= "Kazakhstan",
                      "RUS" =  "Russian Federation",
                      "TJK" = "Tajikistan",
                        "TKM" = "Turkmenistan",
                        "UZB" = "Uzbekistan",
                      "DZA" = "Algeria",
                      "EGY" = "Egypt",
                      "IRN" = "Iran",
                      "IRQ" ="Iraq",
                      "ISR" = "Israel",
                      "JOR" = "Jordan",
                      "KWT" = "Kuwait",
                      "LBN" = "Lebanon",
                      "MAR" = "Morocco",
                      "OMN" = "Oman",
                      "QAT" = "Qatar",
                      "SAU" = "Saudi Arabia",
                      "TUN" = "Tunisia",
                      "ARE"= "United Arab Emirates",
                      "YEM" = "Yemen",
                      "AUS" = "Australia",
                      "KHM"="Cambodia",
                      "CHN"= "China",
                      "HKG"= "Hong Kong",
                      "IDN" = "Indonesia",
                      "JPN" = "Japan",
                      "PRK"="North Korea",
                      "KOR" = "Republic of Korea",
                      "MYS" = "Malaysia",
                      "MNG" = 'Mongolia',
                      "MMR" = "Myanmar",
                      "NZL" = "New Zealand",
                      "PNG" = "Papua New Guinea",
                      "PHL" = "Philippines",
                      "THA" = "Thailand") 

plotRegion <- function(data_pop, nrows, labels_region){ 
  
  data_region <- lapply(data_pop, obtainFig1) %>% 
    rbindlist() %>% mutate_if(is.numeric,function(x){x*1000}) %>% 
    mutate(Year = as.integer(Year/1000))
  
  data_region <- data_region %>% 
    mutate(Max = ifelse(FT > TE, FT, TE),
           Min = ifelse(FT < TE, FT, TE)) %>% 
    reshape2::melt(-c(2:4)) %>% 
    filter(!(ISO == "BLX" & Year %in% c(2000:2013)),
           !(ISO == "BEL" & Year %in% c(1986:1999))) %>% 
    mutate(ISO = ifelse(ISO == "BLX", "BEL", ISO)) %>% 
    mutate(Max = ifelse(ISO == "UKR" & Year %in% c(2005, 2008), NA, Max),
           Min = ifelse(ISO == "UKR" & Year %in% c(2005, 2008), NA, Min))
  
  order_region <- data_region %>% arrange(-value) %>% 
    select(ISO) %>% distinct()
  
  data_ordered <- data_region %>% 
    mutate(ISO = factor(ISO, levels = order_region$ISO)) %>% 
    mutate(X = 0, Y = 250) %>% 
    mutate(Y = ifelse(ISO %in% c("CHN"),
                      2250, Y),
          Y = ifelse(ISO %in% c("BRA", "USA", "IND", "IDN"),
                      1500, Y),
           Y = ifelse(ISO %in% c("RUS", "AUS"),
                      600, Y),
          Y = ifelse(ISO %in% c("COL", "MEX", "ARG"),
                     400, Y),
           Y = ifelse(ISO %in% c("DEU", "FRA", "ESP", "ITA", "GBR", 
                                 "JPN", "MMR","PHL", "THA",
                                 "NGA", "MDG","ETH","ANG", "GIN", "AGO" ),
                      300, Y),
          Y = ifelse(ISO %in% c("IRN", "EGY",
                                "SAU", "DZA", "MAR",
                                "KOR", "NZL", "MYS"),
                     150, Y),
           Y = ifelse(ISO %in% c("UKR", "TUR", "POL", "NLD", "ROU", "BOL", "PER", "ECU", "HND", "GTM",
                                 "BWA", "KEN",  "MOZ","CIV","MLI",
                                 "KHM","MNG"),
                      125, Y),
           Y = ifelse(ISO %in% c("KAZ"),
                      90, Y),
           Y = ifelse(ISO %in% c("GUI",  "CAF", "UGA" ),
                      75, Y),
          Y = ifelse(ISO %in% c( 
                                "CHL", "CUB", "CRI", "DOM", "PAN",
                                "SLE","NER","CAF", "UGA","TCD"),
            60, Y),
           Y = ifelse(ISO %in% c( "GRC", "PRT","BEL","AUT", "SWE",
                                  "NPL","AFG", "LKA", "KGZ",
                                   "COG","ZWE", "BFA", "GHA", "LBR",
                                   "UZB"),
                      50, Y),
           Y = ifelse(ISO %in% c( "IRL", "BLR","HUN","CZE", "CHE",
                                 "SEN",  "NAM", "MWI", "BEN", "MRT",
                                 "IRQ", "YEM", "ARE","ISR", "TUN"),
                      40, Y),
           Y = ifelse(ISO %in% c("DNK",  "BGR","NOR", "FIN", "SRB",
                                 "HKG", "PRK"),
                      30, Y),
           Y = ifelse(ISO %in% c( "JAM", "HTI", "SLV","HRV",
                                  "SOM", "TGO", "COD", "LSO", "RWA", "BDI",
                                 "AZE", "TKM",
                                  "JOR", "LBN", "OMN", "KWT"),
                      20, Y),
           Y = ifelse(ISO %in% c("BIH",  "SVK", "ALB", "LTU", "LVA", "SVN", "MDA", "MKD",
                                 "ARM", "GEO", "TJK"  ),
                      12, Y)) %>% 
    mutate(value =  ifelse(ISO %in% c(ex_USSR, ex_Yug, "BIH") & Year %in% 1986:1991, NA, value),
           Max =  ifelse(ISO %in% c(ex_USSR, ex_Yug, "BIH") & Year %in% 1986:1991, NA, Max),
           Min =  ifelse(ISO %in% c(ex_USSR, ex_Yug, "BIH") & Year %in% 1986:1991, NA, Min)) %>% 
    mutate(value =  ifelse(ISO %in% c("CZE", "SVK") & Year %in% 1986:1993, NA, value),
           Max =  ifelse(ISO %in%  c("CZE", "SVK") & Year %in% 1986:1993, NA, Max),
           Min =  ifelse(ISO %in% c("CZE", "SVK") & Year %in% 1986:1993, NA, Min)) %>% 
    mutate(value =  ifelse(ISO %in% c("SRB") & Year %in% 1986:2005, NA, value),
           Max =  ifelse(ISO %in%  c("SRB") & Year %in% 1986:2005, NA, Max),
           Min =  ifelse(ISO %in% c("SRB") & Year %in% 1986:2005, NA, Min)) %>% 
    filter(!ISO %in% c("PRI",  "SSD", "BDI", "COD", "SOM", "LBY", "QAT", "SGP", "PNG"))
  
  
  p_region <- ggplot(data = data_ordered %>% filter(variable == "FT")) +
  geom_line(aes(x = Year, y = value, linetype = variable), colour = "black") +
    geom_line(data = (data_ordered %>% dplyr::filter(variable == "TE")),
              aes(x = Year, y = value, linetype = variable), colour = "black")+
    geom_ribbon(data = (data_ordered %>%dplyr::filter(variable == "Net balance") %>% select(-variable,-value)),
                aes(x = Year, ymax = Max, ymin = Min, fill = Flow), alpha = .6) +
    common.plot + theme(strip.text = element_text(size = 12),
                        axis.title.y = element_text(size = 10))+
    facet_wrap(~ ISO, nrow = nrows, ncol = 5,
               labeller = labeller(ISO = labels_region), 
               scales="free_y"  ) +
    geom_blank(aes(y = X)) +
    geom_blank(aes(y = Y)) +
    guides(fill = guide_legend(nrow = 1, title = ""), linetype = guide_legend(nrow = 1, title = "")) +
    scale_fill_manual(values = palette_OkabeIto[c(3,6)]) +
    scale_y_continuous(labels = scales::number_format(accuracy = 1))+ 
    #scale_y_continuous(limits = c(0, 2100), labels = scales::number_format(big.mark = " "))+
    scale_linetype_manual(labels=c("consumption-based", "production-based"),
                          values = c("dashed", "solid")) +
    scale_x_continuous(breaks=c(1990, 2000, 2010))+
    labs(y=expression(MtCO[2]~e/yr))
  
  return(p_region)
  
  
  
  }

p_europe <- plotRegion(c(population_europe, "BLX"), 7, labels_countries)
plot(p_europe)

ggsave( file = "FigEurope.png", path="results/",
        plot = p_europe,
        dpi = 96,
        width = 28,
        height = 35,
        units = "cm" )

p_am <- plotRegion(population_am, 5, labels_countries)
plot(p_am)
ggsave( file = "FigAm.png", path="results/",
        plot = p_am,
        dpi = 96,
        width = 28,
        height = 25,
        units = "cm" )

p_sa <- plotRegion(population_sa, 2, labels_countries)
plot(p_sa)
ggsave( file = "FigSA.png", path="results/",
        plot = p_sa,
        dpi = 96,
        width = 28,
        height = 12,
        units = "cm" )

p_af <- plotRegion(population_af, 8, labels_countries)
plot(p_af)

ggsave( file = "FigAf.png", path="results/",
        plot = p_af,
        dpi = 96,
        width = 28,
        height = 35,
        units = "cm" )

p_ca <- plotRegion(population_ca, 2, labels_countries)
plot(p_ca)

ggsave( file = "FigCA.png", path="results/",
        plot = p_ca,
        dpi = 96,
        width = 28,
        height = 12,
        units = "cm" )


p_mena <- plotRegion(population_mena, 3, labels_countries)
plot(p_mena)

ggsave( file = "FigMENA.png", path="results/",
        plot = p_mena,
        dpi = 96,
        width = 28,
        height = 18,
        units = "cm" )

p_eap <- plotRegion(population_eap, 3, labels_countries)
plot(p_eap)

ggsave( file = "FigEAP.png", path="results/",
        plot = p_eap,
        dpi = 96,
        width = 28,
        height = 18,
        units = "cm" )
# Figure bars and shares ---------
extract.trade <- function(ISOs, data.trade, flow){
  
  if("EU28" %in% ISOs){country.sel <- eu_28} else{country.sel <- ISOs}
  
  res <- data.trade[["1986"]] %>% mutate(Year = 1986) %>% 
    bind_rows(data.trade[["2013"]] %>%  mutate(Year = 2013))
  
  if(flow == "Imports"){
    res <- res %>% select(-ISO) %>% 
      rename(ISO = `FD ISO`)
    if("EU28" %in% ISOs){country.sel <- "EU 28"}
    }  
    
  res <- res %>% filter(ISO %in% country.sel) %>% 
    group_by(ISO,  Year) %>% 
    summarise(value = sum(Value)/1000) %>% 
    mutate(variable = flow) %>% 
    ungroup()
  
  
  if("EU28" %in% ISOs){
    
    res <- res %>% mutate(ISO = "EU28") %>% 
      group_by(ISO, Year, variable) %>% 
      summarise(value = sum(value)) %>% 
      ungroup()
    
  }
  
  return(res)
}

data.fig2 <- data.fig1 %>% filter(Year %in% c(1986, 2013),
                                  variable %in% c("Consumer", "Producer", "Net")) %>% 
  mutate(value = ifelse(variable == "Net" & Flow == "Net exports", value*-1, value)) %>% 
  select(-Max, -Min, -Flow) %>% 
  bind_rows(extract.trade(c("IND", "BRA", "CHN", "USA"), exports.results, "Exports"),
            extract.trade("EU28", exports.results.EU, "Exports"),
            extract.trade(c("IND", "BRA", "CHN", "USA"), imports.results, "Imports"),
            extract.trade("EU28", imports.results.EU, "Imports")) %>% 
  mutate(variable = factor(variable, levels = c("Producer", "Exports", "Imports",
                                                "Net", "Consumer"))) %>% 
  mutate(value = abs(value))

plot.bars <- function(ISO_label){
  
 p <- ggplot(data= (data.fig2 %>% filter(ISO == ISO_label))) +
    geom_bar(data = (data.fig2 %>% filter(ISO == ISO_label)),
             aes(x = variable, y = value),
             stat='identity') +
    theme(plot.title = element_text(size=18, face="bold",  hjust = 0.5 ),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size = 12),
          strip.text = element_text(size = 16),
          panel.spacing = unit(2, "lines"),
          panel.grid = element_blank())+
   geom_hline(yintercept = 1200, linetype = "dashed", color = "#20ff1e", size= 1.1) +
    facet_wrap(~ Year) +
    ggtitle(labels_countries[ISO_label]) +
    scale_x_discrete(labels = c("DP", "EX", "IM", "NB", "CF")) +
   scale_y_continuous(limits = function(value) {
     if(max(value) > 1200){c(0, 2000)} else{
       c(0,1200)}
   }, labels = scales::number_format(big.mark = " "))

 if(ISO_label == "BRA"){
   
   p <-  p + #theme(axis.text.y = element_text(size = 12))+
     labs(y = "TCO2e/yr")
 }else{p <-  p + theme(#axis.text.y = element_blank(),
                       axis.title.y = element_blank()) }
 
  return(p)
}

obtainFig2 <- function(year){
  
  agg.results <- function(data.fabio, perspective, emission, year){
    
    agg_results <- data.fabio[[as.character(year)]] %>% 
      filter(ISO %in% c(eu_28, "IND", "BRA", "CHN", "USA", "EU 28")) %>% 
      mutate(ISO = ifelse(ISO %in% c("IND",  "CHN", "USA", "BRA"), ISO, "EU28")) %>% 
      group_by(ISO) %>% 
      summarise(Value = sum(Value)/1000) %>% ungroup() %>% 
      mutate(Year = year,
             Emissions = emission,
             Perspective = perspective)
    
    return(agg_results)
    
  } 
  
  results <- agg.results(footprint.results.agri, "Consumer", "Agricultural", year) %>% 
    bind_rows(agg.results(footprint.results.luc, "Consumer", "LUC", year),
              agg.results(territorial.results.agri, "Producer", "Agricultural", year),
              agg.results(territorial.results.luc, "Producer", "LUC", year)) %>% 
    spread(Perspective, Value) %>% 
    mutate(Net = Consumer-Producer,
           #`Net balance` = Domestic+abs(Net),
           Flow = ifelse(Net >0 , "Net imports", "Net exports"),
           Net = abs(Net))
  
  return(results)
  
}

data.fig2 <- lapply(years[c(1,28)], obtainFig2) %>% 
  data.table::rbindlist() %>% 
  select(-Flow) %>% 
  reshape2::melt(-c(4:6)) %>%
  bind_rows(extract.trade(c("IND", "BRA", "CHN", "USA"), exports.results.agri, "Exports") %>% 
              mutate(Emissions = "Agricultural"),
            extract.trade("EU28", exports.results.EU.agri, "Exports") %>% 
              mutate(Emissions = "Agricultural"),
            extract.trade(c("IND", "BRA", "CHN", "USA"), imports.results.agri, "Imports") %>% 
              mutate(Emissions = "Agricultural"),
            extract.trade("EU28", imports.results.EU.agri, "Imports") %>% 
              mutate(Emissions = "Agricultural"),
            extract.trade(c("IND", "BRA", "CHN", "USA"), exports.results.luc, "Exports") %>% 
              mutate(Emissions = "LUC"),
            extract.trade("EU28", exports.results.EU.luc, "Exports") %>% 
              mutate(Emissions = "LUC"),
            extract.trade(c("IND", "BRA", "CHN", "USA"), imports.results.luc, "Imports") %>% 
              mutate(Emissions = "LUC"),
            extract.trade("EU28", imports.results.EU.luc, "Imports") %>% 
              mutate(Emissions = "LUC")) %>% 
  mutate(variable = factor(variable, levels = c("Producer", "Exports", "Imports",
                                                "Net", "Consumer"))) %>% 
  group_by(ISO, variable, Year) %>% 
  dplyr::mutate(perc = value/sum(value)) %>% 
  ungroup() %>% 
  dplyr::arrange(Emissions) %>% 
  dplyr::mutate(Emissions = factor(Emissions, levels = rev(c("Agricultural",
                                                       "LUC"))))
colors.fig2 <- c("#FFF05A","#DE541E")

plot.shares <- function(ISO_label){
  
  p <- ggplot(data = (data.fig2 %>% filter(ISO == ISO_label)),
                  aes(x = variable, 
                      y = perc,
                      fill = Emissions)) +
    geom_bar(stat='identity', colour="black") +
    theme(
      #anel.border=element_rect(colour="black", fill = NA),
      axis.title.x = element_blank(),
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 9),
      strip.text = element_blank(),
      legend.key = element_rect(colour = NA, fill = NA),
      legend.title = element_blank(),
      legend.text = element_text(size = 14),
      panel.spacing = unit(2, "lines")#,
      #panel.grid = element_blank()
    ) +
    scale_y_continuous(
      breaks = c(0, 0.25, 0.5, 0.75, 1), labels = scales::percent(c(0, 0.25, 0.5, 0.75, 1))) +
    scale_fill_manual(values = colors.fig2, guide = guide_legend(reverse = TRUE)) +
    facet_wrap(~ Year) +
    scale_x_discrete(labels = c("DP", "EX", "IM", "NB", "CF"))
  
  if(ISO_label == "BRA"){
    
    p <-  p + theme(axis.text.y = element_text(size = 12),
                    legend.position = "bottom")+
      labs(y = "% of total global GHG emissions")
  }else{p <-  p + theme(axis.text.y = element_blank(),
                        axis.title.y = element_blank()) }
  
  return(p)
  
}

Fig2 <- cowplot::plot_grid(cowplot::plot_grid(cowplot::plot_grid(plotlist =lapply(c("BRA","CHN", "EU28", "IND", "USA"), plot.bars), nrow = 1, 
                           align = 'v',
                           axis = "t"),
                           cowplot::plot_grid(plotlist =lapply(c("BRA","CHN","USA", "EU28", "IND"), function(ISO_label){plot.shares(ISO_label)+
                                theme(legend.position = "none")}), nrow = 1, 
                                              align = 'v',
                                              axis = "t"),
                           ncol = 1, 
                           align = 'v',
                           axis = "t",
                           rel_heights = c(1,1.5)),
                           cowplot::get_legend(
                             plot.shares("BRA")), ncol = 1, rel_heights = c(1, .1))

plot(Fig2)

ggsave( file = "Fig2.png", path="results/",
        plot = Fig2,
        limitsize = FALSE,
        width = 40,
        height = 15,
        units = "cm")

# Figure Map ------------
extractNorm <- function(data.input, perspective, normalizer){
  
  res <- data.input[["1986"]] %>% 
    mutate(Perspective = perspective, Year = 1986) %>% 
    bind_rows(data.input[["2013"]] %>% 
                mutate(Perspective = perspective, Year = 2013)) %>% 
    group_by(ISO, Year) %>% 
    summarise(Value = sum(Value)) %>% 
    left_join(normalizer,
              by = c("ISO", "Year")) %>% 
    mutate(Norm = Value*1000/Den)
  
  res <- res %>% 
    mutate(Norm = ifelse(Year == "1986" & ISO %in% ex_USSR, res %>% filter(ISO == "SUN" & Year == 1986) %>% pull(Norm), Norm),
           Norm = ifelse(Year == "1986" & ISO %in% ex_Yug, res %>% filter(ISO == "YUG" & Year == 1986) %>% pull(Norm), Norm))
    
    return(res)
  
}

cap <- extractNorm(footprint.results, "Consumer", population %>% select(Year, ISO, Pop) %>% 
                     rename(Den = Pop)) %>% 
  group_by(Year) %>% 
  mutate(Q = quantile(Norm, na.rm = T, prob=c(.90)))

prod <- extractNorm(territorial.results, "Producer", readr::read_delim("H:/fabio_ghg/inst/ghg_fabio_input/Area_Prod_all_full_Years.csv", 
                                                                       ",", escape_double = FALSE, trim_ws = TRUE) %>% 
                      filter(Year %in% c(1986, 2013)) %>% 
                      group_by(ISO, Year) %>% 
                      summarise(Den = sum(`Area harvested`))) %>% 
  ungroup() %>% 
  mutate(Q = quantile(Norm, na.rm = T, prob=c(.90)))

estimateRegion <- function(data.input, perspective){
  
  res <- data.input %>% left_join(countries %>% select(ISO, Continent)) %>% 
    mutate(Continent = ifelse(Continent == "EU", "EUR", Continent)) %>% 
    group_by(Continent, Year) %>% summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
    filter(Continent != "ROW") %>% mutate(Label = "Region") %>% 
    bind_rows(data.input %>% group_by(Year) %>% summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
                mutate(Continent = "WRL",
                       Label = "WRL")) %>% 
    mutate(Norm = Value*1000/Den,
           Continent = factor(Continent, levels = c("WRL", "AFR", "ASI", "EUR",
                                                    "LAM", "NAM", "OCE"))) %>%
    select(-Q)
  
  return(res)
  
}

cap_reg <- estimateRegion(cap)
prod_reg <- estimateRegion(prod)

extractNormTrade <- function(year){
  
  res <- exports.results.all[[year]] %>% 
    mutate(Perspective = "Exports", Year = year) %>% 
    bind_rows(imports.results.all[[year]] %>% 
                mutate(Perspective = "Imports", Year = year) %>% 
                select(-ISO) %>% rename(ISO = `FD ISO`)) %>% 
    group_by(ISO, Perspective, Year) %>% 
    summarise(Value = sum(Value)/1000) %>% 
    spread(Perspective, Value) %>% 
    mutate(Net = Exports-Imports,
           Norm = Net/(Exports+Imports),
           Year = as.integer(Year)) %>% 
    ungroup()
  
  res <- res %>% 
    mutate(Norm = ifelse(Year == 1986 & ISO %in% ex_USSR, res %>% filter(ISO == "SUN" & Year == 1986) %>% pull(Norm), Norm),
           Norm = ifelse(Year == 1986 & ISO %in% ex_Yug, res %>% filter(ISO == "YUG" & Year == 1986) %>% pull(Norm), Norm))
  
  return(res)
  
}

trade <- extractNormTrade(as.character(2013)) %>% 
  bind_rows(extractNormTrade(as.character(1986)))

trade_reg <- trade %>% left_join(countries %>% select(ISO, Continent)) %>% 
  mutate(Continent = ifelse(Continent == "EU", "EUR", Continent)) %>% 
  group_by(Continent, Year) %>% summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
  filter(Continent != "ROW") %>% mutate(Label = "Region") %>% 
  bind_rows(trade %>% group_by(Year) %>% summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
              mutate(Continent = "WRL",
                     Label = "WRL")) %>% 
  mutate(Net = Exports-Imports,
         Norm = Net/(Exports+Imports),
         Continent = factor(Continent, levels = c("WRL", "AFR", "ASI", "EUR",
                                                  "LAM", "NAM", "OCE")))

plotBarsReg <- function(data.bars.fig3, year,  limit_min, limit_max, gradient){
  
  y.label <- switch (gradient,
                          "net" = "Net/Total",
                          "territorial" = "kgCO2e/ha",
                          "footprint" = "kgCO2e/cap" )
  
  p <- ggplot(data= (data.bars.fig3 %>% filter(Year == year))) +
    geom_bar(aes(x = Continent, y = Norm, fill = Label),
             stat='identity') +
    theme(plot.title = element_text(size=9, face="bold",  hjust = 0.5 ),
          axis.title = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_text(size = 5, angle = 45),
          strip.text = element_text(size = 16),
          panel.spacing = unit(2, "lines"),
          panel.grid = element_blank(),
          legend.position = "none") +
    scale_y_continuous(limits = c(limit_min, limit_max))+
    scale_fill_manual(values = c("grey20", "grey50")) +
    ggtitle(y.label)
    #labs(y = y.label)
  
    return(p)
  
}


world_map <- ggplot2::map_data("world") %>%
  left_join(maps::iso3166 %>% select(a3, mapname),
            by = c("region" = "mapname"))

plotMap <- function(data.input, year, limit, legend.levels, gradient){
  
  data.map <- left_join(data.input %>% filter(Year == year) %>% bind_rows(
                       data.frame(ISO = "R", Country = "R", Year = year, Norm = legend.levels[2]),
                       data.frame(ISO = "R", Country = "R", Year = year, Norm = legend.levels[1])),
                       world_map, by = c("ISO" = "a3")) %>% 
    mutate(Norm = ifelse(Norm > limit, limit, Norm))
  
  legend.title <- switch (gradient,
    "net" = "Net GHG balance/Total GHG trade",
    "territorial" = "kgCO2e Territorial/ha",
    "footprint" = "kgCO2e Footprint/cap" )
  
  p <- ggplot(data.map, aes(map_id = region, fill = Norm))+
    geom_map(map = data.map,  color = NA)+
    theme(axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title =  element_blank(),
          legend.key.height = unit(0.2, 'cm'),
          legend.title = element_blank())+
    expand_limits(x = data.map$long, y = data.map$lat)
  
  if(gradient == "net"){
    p <- p +
    scale_fill_gradient2(low ="#1e90ff" , mid = "#ffeec8",
                         high='#ff1e20', na.value = "grey50")
  } else if(gradient == "footprint"){
    p <- p + scale_fill_gradient(low = "#ffeec8", high="#1e90ff", na.value = "grey50")
  } else if(gradient == "territorial"){
    p <- p + scale_fill_gradient(low = "#ffeec8" , high='#ff1e20', na.value = "grey50")
    
  }

  #p <- p +guides(fill=guide_legend(title=legend.title))
  
  if(gradient == "territorial"){
    p <- p + ggtitle(year) + theme(plot.title = element_text(hjust = 0.5))
  }
    
    return(p)
}

# Fig3 <- cowplot::plot_grid( cowplot::plot_grid(cowplot::plot_grid(plotMap(prod, 1986, max(prod$Q, na.rm = T), c(0, max(prod$Q, na.rm = T)), "territorial")+theme(legend.position="none"), 
#                                                                   plotMap(prod, 2013,  max(prod$Q, na.rm = T), c(0, max(prod$Q, na.rm = T)), "territorial")+theme(legend.position="none"), nrow = 1, 
#                                                                   align = 'v',
#                                                                   axis = "t"),
#                                                cowplot::get_legend(plotMap(prod, 1986,  max(prod$Q, na.rm = T), max(prod$Q, na.rm = T), "territorial")), ncol = 1, rel_heights = c(1, .1)),
#   cowplot::plot_grid(cowplot::plot_grid(plotMap(cap, 1986,  max(cap$Q, na.rm = T), c(0, max(cap$Q, na.rm = T)), "footprint")+theme(legend.position="none"), 
#                                               plotMap(cap, 2013, max(cap$Q, na.rm = T), c(0, max(cap$Q, na.rm = T)), "footprint")+theme(legend.position="none"), nrow = 1, 
#                                               align = 'v',
#                                               axis = "t"),
#                            cowplot::get_legend(plotMap(cap, 2013, max(cap$Q), c(0, max(cap$Q)), "footprint")), ncol = 1, rel_heights = c(1, .1)),
#   cowplot::plot_grid(cowplot::plot_grid(plotMap(trade, 1986, 1, c(0, 1), "net")+theme(legend.position="none"), 
#                                         plotMap(trade, 2013, 1, c(0, 1), "net")+theme(legend.position="none"), nrow = 1, 
#                                         align = 'v',
#                                         axis = "t"),
#                      cowplot::get_legend(plotMap(trade, 2013, 1, c(0, 1), "net")), ncol = 1, rel_heights = c(1, .1)),
#   nrow= 3)

Fig3 <- cowplot::plot_grid(
  cowplot::plot_grid(
    cowplot::plot_grid(cowplot::plot_grid(plotMap(prod, 1986, max(prod$Q, na.rm = T), c(0, max(prod$Q, na.rm = T)), "territorial")+theme(legend.position="none"),
                                          plotBarsReg(prod_reg, 1986,0, 25, "territorial"), rel_widths = c(3,1)),
                       cowplot::plot_grid(plotMap(prod, 2013, max(prod$Q, na.rm = T), c(0, max(prod$Q, na.rm = T)), "territorial")+theme(legend.position="none"),
                                          plotBarsReg(prod_reg, 2013,0, 25, "territorial"), rel_widths = c(3,1))),
    cowplot::get_legend(plotMap(prod, 1986,  max(prod$Q, na.rm = T), max(prod$Q, na.rm = T), "territorial")), ncol = 1, rel_heights = c(1, .2)),
  cowplot::plot_grid(
    cowplot::plot_grid(cowplot::plot_grid(plotMap(cap, 1986, max(cap$Q, na.rm = T), c(0, max(cap$Q, na.rm = T)), "footprint")+theme(legend.position="none"),
                                          plotBarsReg(cap_reg, 1986, 0, 10, "footprint"), rel_widths = c(3,1)),
                       cowplot::plot_grid(plotMap(cap, 2013, max(cap$Q, na.rm = T), c(0, max(cap$Q, na.rm = T)), "footprint")+theme(legend.position="none"),
                                          plotBarsReg(cap_reg, 2013, 0, 10, "footprint"), rel_widths = c(3,1))),
    cowplot::get_legend(plotMap(cap, 1986,  max(cap$Q, na.rm = T), max(cap$Q, na.rm = T), "footprint")), ncol = 1, rel_heights = c(1, .2)),
  cowplot::plot_grid(
    cowplot::plot_grid(cowplot::plot_grid(plotMap(trade, 1986, 1, c(-1, 1), "net")+theme(legend.position="none"),
                                          plotBarsReg(trade_reg, 1986, -0.6, 0.95, "net"), rel_widths = c(3,1)),
                       cowplot::plot_grid(plotMap(trade, 2013, 1, c(-1, 1), "net")+theme(legend.position="none"),
                                          plotBarsReg(trade_reg, 2013, -0.6, 0.95, "net"), rel_widths = c(3,1))),
    cowplot::get_legend(plotMap(cap, 1986,  1, c(-1, 1), "net")), ncol = 1, rel_heights = c(1, .2)),
  ncol = 1)

# Fig3 <- cowplot::plot_grid(
#   cowplot::plot_grid(
#     cowplot::plot_grid(cowplot::plot_grid(plotMap(prod, 1986, max(prod$Q, na.rm = T), c(0, max(prod$Q, na.rm = T)), "territorial")+theme(legend.position="none"),
#                                           cowplot::plot_grid(plotBarsReg(prod_reg, 1986, 25, "territorial"),
#                                                              cowplot::get_legend(plotMap(prod, 1986,  max(prod$Q, na.rm = T), max(prod$Q, na.rm = T), "territorial")), ncol = 1, rel_heights = c(1, .2)), rel_widths = c(3,1)),
#                        cowplot::plot_grid(plotMap(prod, 2013, max(prod$Q, na.rm = T), c(0, max(prod$Q, na.rm = T)), "territorial")+theme(legend.position="none"),
#                                           plotBarsReg(prod_reg, 2013, 25, "territorial"), rel_widths = c(3,1)))),
#   cowplot::plot_grid(
#     cowplot::plot_grid(cowplot::plot_grid(plotMap(cap, 1986, max(cap$Q, na.rm = T), c(0, max(cap$Q, na.rm = T)), "footprint")+theme(legend.position="none"),
#                                           plotBarsReg(cap_reg, 1986, 10, "footprint"), rel_widths = c(3,1)),
#                        cowplot::plot_grid(plotMap(cap, 2013, max(cap$Q, na.rm = T), c(0, max(cap$Q, na.rm = T)), "footprint")+theme(legend.position="none"),
#                                           plotBarsReg(cap_reg, 2013, 10, "footprint"), rel_widths = c(3,1))),
#     cowplot::get_legend(plotMap(cap, 1986,  max(cap$Q, na.rm = T), max(cap$Q, na.rm = T), "footprint")), ncol = 1, rel_heights = c(1, .2)),
#   cowplot::plot_grid(
#     cowplot::plot_grid(cowplot::plot_grid(plotMap(trade, 1986, 1, c(0, 1), "net")+theme(legend.position="none"),
#                                           plotBarsReg(cap_reg, 1986, 10, "net"), rel_widths = c(3,1)),
#                        cowplot::plot_grid(plotMap(trade, 2013, 1, c(0, 1), "net")+theme(legend.position="none"),
#                                           plotBarsReg(cap_reg, 2013, 10, "net"), rel_widths = c(3,1))),
#     cowplot::get_legend(plotMap(cap, 1986,  1, c(0, 1), "net")), ncol = 1, rel_heights = c(1, .2)),
#   ncol = 1
# 
# 
# )

plot(Fig3)

ggsave( file = "Fig3.png", path="results/",
        plot = Fig3,
        limitsize = FALSE,
        width = 25,
        height = 17,
        units = "cm")


# Figure Trade balance ---------
obtainTerritorial <- function(year, domestic.data, exports.data){
  
  res <- domestic.data[[year]] %>% bind_rows(exports.data[[year]]) %>% 
    group_by(ISO, Country, Name, Group) %>% 
    summarise(Value = sum(Value))
  
}

extractNetEmi <- function(footprint.input, territorial.input, year, emissions, mode){

  group.fabio <- unique(products$Com.Group)
  
  if(emissions == "All"){
    
    if(mode == "product"){
      
      res <- footprint.input[[year]] 
      
    } else if(mode == "input"){
      
      res <- footprint.input[[year]] %>% select(-ISO) %>% 
        rename(ISO = `FD ISO`)
      
    }
    
     res <- res %>% 
      group_by(ISO, Group) %>% summarise(FT = sum(Value)) %>% 
      left_join(territorial.input[[year]] %>% 
                  group_by(ISO, Group) %>% summarise(DP = sum(Value))) %>% 
      left_join(data.frame(Group = c("Cereals", "Coffee, tea, cocoa", "Oil crops", "Oil cakes", "Live animals", "Fodder crops, grazing",
                                     group.fabio[c(2:4, 7:8, 10:11, 13:14, 16:22)]),
                           Agg = c("Cereals", "Coffee, tea, cocoa", rep("Oil crops and cakes", 2),
                                   rep("Live animals, fodder crops and grazing", 2), rep("Other crops and products", 16)))) %>%
      group_by(ISO, Agg) %>% summarise_if(is.numeric, sum) %>%
      mutate(Net = (DP-FT)/1000,
             Emissions = emissions) %>% 
      ungroup() %>% rename(Group = Agg)
    
  } else {res <- footprint.input[[year]] %>% 
    group_by(ISO) %>% summarise(FT = sum(Value)) %>% 
    left_join(territorial.input[[year]] %>% 
                group_by(ISO) %>% summarise(DP = sum(Value))) %>% 
      mutate(Net = (DP-FT)/1000) %>% 
    mutate(Emissions = emissions)}
  
  return(res)
}

# Countries selected
countries.sel <- extractNetEmi(imports.results.all, exports.results.all, "2013", "All", "input") %>% 
  select(-DP) %>% 
  left_join(countries %>% select(ISO, Country)) %>% 
  filter(Net != 0) %>% 
  group_by(Country) %>% summarise_if(is.numeric, sum) %>% 
  arrange(-Net) %>% filter(abs(Net)> 10) %>% 
  pull(Country)

order_graph <- extractNetEmi(footprint.results.agri, territorial.results.agri, "2013","Agricultural") %>% 
  bind_rows( extractNetEmi(footprint.results.luc, territorial.results.luc, "2013", "LUC")) %>% 
  select(-DP) %>% 
  left_join(countries %>% select(ISO, Country)) %>% 
  filter(Country %in% countries.sel)

plotBalance <- function(year, countries.sel, countries.exc,
                        limit_low, limit_high){
  
  net.agri.luc <- extractNetEmi(footprint.results.agri, territorial.results.agri, year, "Agricultural") %>% 
    bind_rows( extractNetEmi(footprint.results.luc, territorial.results.luc, year, "LUC")) %>% 
    select(-DP) %>% 
    left_join(countries %>% select(ISO, Country)) %>% 
    filter(Net != 0)
  
  data.fig4 <- net.agri.luc %>% 
    mutate(Country = factor(Country, levels = countries.sel)) %>% 
    filter(Country %in% countries.sel) %>% 
    filter(!Country %in% countries.exc) %>% 
    dplyr::mutate(Emissions = factor(Emissions, levels = rev(c("Agricultural",
                                                               "LUC"))))
  
  colors.fig4 <- c("#FFF05A","#DE541E")
  
  p <- ggplot(data = data.fig4, aes(x = Country, y = Net, fill = Emissions))+
    geom_bar(stat='identity', colour = "black") +
    geom_hline(yintercept = -100, linetype = "dashed", color = "#20ff1e", size= 1.1) +
    geom_hline(yintercept = 50, linetype = "dashed", color = "#20ff1e", size= 1.1) +
    theme(
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      #axis.text.y = element_text(size = 12),
      strip.text = element_blank(),
      legend.key = element_rect(colour = NA, fill = NA),
      legend.title = element_blank(),
      legend.text = element_text(size = 8)#,
      #panel.spacing = unit(2, "lines")
    ) +
    coord_flip() +
    scale_fill_manual(values = colors.fig4, guide = guide_legend(reverse = TRUE, nrow = 2)) +
    labs(y = "tCO2e/yr") +
    ylim(limit_low, limit_high)
  
  return(p)
}

colors.fig4b <- c("#9BC1BC", "#ff715b", "#104911", "#bced09", "#785964")

plotBalanceInp <- function(year, imports.data, exports.data, countries.sel, mode,
                           countries.exc,
                           limit_low, limit_high){
  
  net.agri.luc <- extractNetEmi(imports.data, exports.data, year, "All", mode) %>% 
    select(-DP) %>% 
    left_join(countries %>% select(ISO, Country)) %>% 
    filter(Net != 0)
  
  data.fig4 <- net.agri.luc %>% 
    mutate(Country = factor(Country, levels = countries.sel)) %>% 
    filter(Country %in% countries.sel) %>% 
    filter(!Country %in% countries.exc) %>% 
    dplyr::mutate(Group = factor(Group, levels = rev(c("Cereals",
                                                                "Coffee, tea, cocoa", "Oil crops and cakes",
                                                               "Live animals, fodder crops and grazing", "Other crops and products"))))
  
  #colors.fig4 <- c("#FFF05A","#DE541E")
  
  p <- ggplot(data = data.fig4, aes(x = Country, y = Net, fill = Group))+
    geom_bar(stat='identity', colour = "black") +
    geom_hline(yintercept = -100, linetype = "dashed", color = "#20ff1e", size= 1.1) +
    geom_hline(yintercept = 50, linetype = "dashed", color = "#20ff1e", size= 1.1) +
    theme(
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      #axis.text.y = element_text(size = 12),
      strip.text = element_blank(),
      legend.key = element_rect(colour = NA, fill = NA),
      legend.title = element_blank(),
      legend.text = element_text(size = 8)#,
      #panel.spacing = unit(2, "lines")
    ) +
    coord_flip() +
    scale_fill_brewer( palette = 2, guide = guide_legend(reverse = TRUE, nrow = 5)) +
    labs(y = "tCO2e/yr") +
    ylim(limit_low, limit_high)
  
  return(p)
}

agg_key <- data.frame(Final = products$FAO.Name,
                      Group = c(rep("Other foods of plant origin", 60), rep(NA, 2),
                              rep("Other foods of plant origin",6), 
                              rep("Vegetables oils", 22),
                              rep("Other foods of plant origin", 6),
                              rep("Other foods of animal origin", 6),
                              rep("Other foods of animal origin", 11),
                                  "Bovine meat","Other foods of animal origin", "Pigmeat",
                              rep("Other foods of animal origin", 9)))

plotBalanceFinal <- function(year, inpur.data, countries.sel, countries.exc,
                             limit_low, limit_high){
  
  net.final <- inpur.data[[year]] %>% 
    group_by(`FD ISO`, Final) %>% 
      summarise(FT = sum(Value)) %>% 
    left_join(inpur.data[[year]] %>% 
               group_by(`ISO`, Final) %>% 
               summarise(DP = sum(Value)), by = c("Final", "FD ISO" = "ISO")) %>% 
    mutate(Net = (DP-FT)/1000) %>% filter(Net != 0) %>% 
    left_join(agg_key) %>%
    group_by(`FD ISO`, Group) %>% 
    summarise_if(is.numeric, sum) %>% select(-DP, -FT) %>% 
    rename(ISO = `FD ISO`) %>% 
    left_join(countries %>% select(ISO, Country)) %>% 
    filter(Net != 0)
  
  data.fig4 <- net.final %>% 
    mutate(Country = factor(Country, levels = countries.sel)) %>% 
    filter(Country %in% countries.sel) %>% 
    filter(!Country %in% countries.exc) %>% 
    dplyr::mutate(Group = factor(Group, levels = rev(c("Vegetables oils",
                                                       "Other foods of plant origin", "Bovine meat",
                                                       "Pigmeat", "Other foods of animal origin"))))
  
  #colors.fig4 <- c("#FFF05A","#DE541E")
  
  p <- ggplot(data = data.fig4, aes(x = Country, y = Net, fill = Group))+
    geom_bar(stat='identity', colour = "black") +
    geom_hline(yintercept = -100, linetype = "dashed", color = "#20ff1e", size= 1.1) +
    geom_hline(yintercept = 50, linetype = "dashed", color = "#20ff1e", size= 1.1) +
    theme(
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      #axis.text.y = element_text(size = 12),
      strip.text = element_blank(),
      legend.key = element_rect(colour = NA, fill = NA),
      legend.title = element_blank(),
      legend.text = element_text(size = 8)#,
      #panel.spacing = unit(2, "lines")
    ) +
    coord_flip() +
    scale_fill_brewer( palette = 3, guide = guide_legend(reverse = TRUE, nrow =5)) +
    labs(y = "tCO2e/yr") +
    ylim(limit_low, limit_high)
  
  return(p)
}

plotBalanceSumm <- function(year, inpur.data, countries.sel, countries.exc,
                            limit_low, limit_high){
  
  net.summary <- inpur.data[[year]] %>% 
    group_by(`FD ISO`) %>% 
    summarise(FT = sum(Value)) %>% 
    left_join(inpur.data[[year]] %>% 
                group_by(`ISO`) %>% 
                summarise(DP = sum(Value)), by = c("FD ISO" = "ISO")) %>% 
    mutate(Net = (DP-FT)/1000) %>% filter(Net != 0) %>% 
    rename(ISO = `FD ISO`) %>% 
    left_join(countries %>% select(ISO, Country)) %>% 
    filter(Net != 0)
  
  data.fig4 <- net.summary %>% 
    mutate(Country = factor(Country, levels = countries.sel)) %>% 
    filter(Country %in% countries.sel) %>% 
    filter(!Country %in% countries.exc)
  
  #colors.fig4 <- c("#FFF05A","#DE541E")
  
  p <- ggplot(data = data.fig4, aes(x = Country, y = Net))+
    geom_bar(stat='identity', colour = "black") +
    geom_hline(yintercept = -100, linetype = "dashed", color = "#20ff1e", size= 1.1) +
    geom_hline(yintercept = 50, linetype = "dashed", color = "#20ff1e", size= 1.1) +
    theme(
      axis.text.y = element_text(),
      axis.title.y = element_blank(),
      #axis.text.y = element_text(size = 12),
      strip.text = element_blank(),
      legend.key = element_rect(colour = NA, fill = NA),
      legend.title = element_blank(),
      legend.text = element_text(size = 8)#,
      #panel.spacing = unit(2, "lines")
    ) +
    #coord_equal() +
    coord_flip() +
    scale_fill_brewer( palette = 3, guide = guide_legend(reverse = TRUE, nrow = 2)) +
    labs(y = "tCO2e/yr") +
    ylim(limit_low, limit_high)
    #scale_y_continuous(limits =  c(limit_low, limit_high))
  
  return(p)
}


p1 <- cowplot::plot_grid(plotBalanceSumm("2013", footprint.results.final, 
                                         c("China, mainland"), countries.sel[1:50],
                                         -300, 190) + theme(axis.title.x = element_blank()),
                         plotBalanceSumm("2013", footprint.results.final, 
                                         countries.sel, c("Brazil", "China, mainland", "India", "Australia"),
                                         -125, 75) + theme(axis.title.x = element_blank()),
                         plotBalanceSumm("2013", footprint.results.final, 
                                         c("Brazil", "India", "Australia"), countries.sel[4:51],
                                         -1200, 700),
                         align = 'v',
                         #axis = "t",
                         rel_heights = c(0.6,7,1.2),
                         ncol = 1)
plot(p1)

p2 <- cowplot::plot_grid(plotBalance("2013", c("China, mainland"), countries.sel[1:50],
                                        -300, 190) + theme(axis.title.x = element_blank(),
                                                           legend.position = "none"),
                         plotBalance("2013", countries.sel, c("Brazil", "China, mainland", "India", "Australia"),
                                        -125, 75) + theme(axis.title.x = element_blank(),
                                                          legend.position = "none"),
                         plotBalance("2013", c("Brazil", "India", "Australia"), countries.sel[4:51],
                                        -1200, 700)+ theme( legend.position = "none"),
                         align = 'v',
                         #axis = "t",
                         rel_heights = c(0.6,7,1.2),
                         ncol = 1)
plot(p2)

p3 <- cowplot::plot_grid(plotBalanceInp("2013", imports.results.all, exports.results.all,   
                                         c("China, mainland"),"input", countries.sel[1:50],
                                         -300, 190) + theme(axis.title.x = element_blank(),
                                                            legend.position = "none"),
                         plotBalanceInp("2013", imports.results.all, exports.results.all,    
                                         countries.sel, "input", c("Brazil", "China, mainland", "India", "Australia"),
                                         -125, 75) + theme(axis.title.x = element_blank(),
                                                           legend.position = "none"),
                         plotBalanceInp("2013", imports.results.all, exports.results.all,   
                                         c("Brazil", "India", "Australia"), "input", countries.sel[4:51],
                                         -1200, 700) + theme( legend.position = "none"),
                         align = 'v',
                         #axis = "t",
                         rel_heights = c(0.6,7,1.2),
                         ncol = 1)
plot(p3)

p4 <- cowplot::plot_grid(plotBalanceFinal("2013", footprint.results.final,   
                                        c("China, mainland"),countries.sel[1:50],
                                        -300, 190) + theme(axis.title.x = element_blank(),
                                                           legend.position = "none"),
                         plotBalanceFinal("2013", footprint.results.final,    
                                        countries.sel, c("Brazil", "China, mainland", "India", "Australia"),
                                        -125, 75) + theme(axis.title.x = element_blank(),
                                                          legend.position = "none"),
                         plotBalanceFinal("2013", footprint.results.final,   
                                        c("Brazil", "India", "Australia"),  countries.sel[4:51],
                                        -1200, 700) + theme( legend.position = "none"),
                         align = 'v',
                         #axis = "t",
                         rel_heights = c(0.6,7,1.2),
                         ncol = 1)
plot(p4)

Fig4 <- cowplot::plot_grid(
  cowplot::plot_grid(p1, cowplot::plot_grid(p2, p3, p4, nrow = 1), nrow= 1, rel_widths = c(0.55,1)),
  cowplot::plot_grid(cowplot::get_legend(plotBalance("2013", c("Brazil", "India", "Australia"), countries.sel[4:51],
                                                     -1200, 700)),
                    cowplot::get_legend(plotBalanceInp("2013", imports.results.all, exports.results.all,   
                                    c("Brazil", "India", "Australia"), "input", countries.sel[4:51],
                                    -1200, 700)),
                     cowplot::get_legend(plotBalanceFinal("2013", footprint.results.final,   
                                                          c("Brazil", "India", "Australia"),  countries.sel[4:51],
                                                          -1200, 700)), nrow = 1)
  , ncol = 1, rel_heights = c(1, .2))
plot(Fig4)

ggsave( file = "Fig4.png", path="results/",
        plot = Fig4,
        limitsize = FALSE,
        width = 35,
        height = 20,
        units = "cm")

# Circos ----------
dataCircos <- function(data.input,year){
  
  #res <- imports.results.all[[1]] %>% 
  res <- data.input[[year]] %>% 
    left_join(countries %>% select(ISO, Continent)) %>% 
    left_join(countries %>% select(ISO, Continent), by = c("FD ISO" = "ISO")) %>%
    mutate(Continent.x = ifelse(ISO == "BRA", "Brazil", Continent.x),
           Continent.y = ifelse(`FD ISO` == "BRA", "Brazil", Continent.y),
           Continent.x = ifelse(ISO == "CHN", "China", Continent.x),
           Continent.y = ifelse(`FD ISO` == "CHN", "China", Continent.y),
           Continent.x = ifelse(ISO == "IND", "India", Continent.x),
           Continent.y = ifelse(`FD ISO` == "IND", "India", Continent.y),
           Continent.x = ifelse(ISO == "USA", "USA", Continent.x),
           Continent.y = ifelse(`FD ISO` == "USA", "USA", Continent.y)) %>%
    group_by(Continent.x, Continent.y) %>% summarise(Value = sum(Value)/1000) %>% 
    ungroup() %>% rename(ISO = Continent.x, `FD ISO` = Continent.y) %>%
    filter(ISO != "ROW" & `FD ISO` !="ROW") %>% 
    tidyr::spread(ISO, Value)
  
  res.mat <- (as.matrix(res[1:11, 2:12]))
  rownames(res.mat) <- colnames(res.mat)
  
  return(res.mat)
}

plotCircos <- function(data.Circos, year, type){
  
  colors_circos <- rainbow(11)
  
  grid.col = 0
  for (i in 1:11) {   grid.col[i]=colors_circos[i] 
  setNames(grid.col, rownames(data.Circos)[i]) }
  
  col_mat = data.frame(colors_circos)
  for (i in 1:11) {  col_mat = data.frame(col_mat,colors_circos) }
  col_mat = t(as.matrix(col_mat))
  threshold=1
  
  for (i in 1:11) {  col_mat[i,i] = "#00000000" }
  # col_mat[mat<threshold] = "#F1F1F1"
  #col_mat[data.Circos<threshold] = "#E5E5E5"
  
  #for (i in 1:7) {  imp.mat[i,i] = imp.mat[i,i]*0.0} 
  
  #emf(paste0("/results/Figure",year,type,".emf"))
  svg(paste0("results/Fig4",year,type,".svg"))
  #png(paste0("results/Fig4", year, "-", type, ".png")) 
  
  circlize::circos.par(start.degree = 80)
  
  plotFig4 <-circlize::chordDiagram(  data.Circos, 
                                  grid.col = grid.col, 
                                  transparency = 0.10 ,
                                  col=col_mat, 
                                  self.link = 1 ,
                                  link.zindex = rank(imp.mat),
                                  directional = 1#,
                                  #annotationTrack = c("name", "grid"),
                                  #annotationTrackHeight = c(0.01, 0.05,
                                  #                         scale=TRUE)
  )
  #title(year, cex = 0.6)
  circlize::circos.clear()
  
  dev.off()

}

plotCircos(dataCircos(footprint.results.final, "2013"), "2013", "total")
plotCircos(dataCircos(footprint.results.final, "1986"), "1986", "total")

plotCircos(dataCircos(imports.results.all.luc, "2013"), "2013", "LUC")
plotCircos(dataCircos(imports.results.all.luc, "1986"), "1986", "LUC")

plotCircos(dataCircos(imports.results.all.agri, "2013"), "2013", "Agri")
plotCircos(dataCircos(imports.results.all.agri, "1986"), "1986", "Agri")


# Export results for drivers analysis ----------

footprint.iso <- NULL

for(year in 1986:2013){
  
  footprint.iso <- footprint.iso %>% 
    bind_rows(footprint.results[[as.character(year)]] %>% 
                group_by(ISO, Country) %>% 
                summarise(Value = sum(Value)) %>% 
                ungroup() %>% mutate(Indicator = "Consumption footprint",
                                     Year = year))
  
}
  
footprint.iso <- footprint.iso %>% 
  spread(Year, Value)

territorial.iso <- NULL

for(year in 1986:2013){
  
  territorial.iso <- territorial.iso %>% 
    bind_rows(territorial.results[[as.character(year)]] %>% 
                group_by(ISO, Country) %>% 
                summarise(Value = sum(Value)) %>% 
                ungroup() %>% mutate(Indicator = "Territorial",
                                     Year = year))
  
}

territorial.iso <- territorial.iso %>% 
  spread(Year, Value)

imports.iso <- NULL
for(year in 1986:2013){
  
  imports.iso <- imports.iso %>% 
    bind_rows(imports.results.all[[as.character(year)]] %>% 
                group_by(`FD ISO`) %>% 
                summarise(Value = sum(Value)) %>% 
                rename(ISO = `FD ISO`) %>% 
                left_join(countries %>% select(ISO, Country)) %>%  
                ungroup() %>% mutate(Indicator = "Imports",
                                     Year = year)) %>% 
    select(ISO, Country, Value, Indicator, Year)
  
}

territorial.iso <- territorial.iso %>% 
  spread(Year, Value)


imports.iso <- imports.results.all[["2013"]] %>% 
  group_by(`FD ISO`) %>% 
  summarise(Value = sum(Value)) %>% 
  rename(ISO = `FD ISO`) %>% 
  left_join(countries %>% select(ISO, Country)) %>% 
  ungroup() %>% mutate(Indicator = "Imports") %>% 
  select(ISO, Country, Value, Indicator)

exports.iso <- exports.results.all[["2013"]] %>% 
  group_by(ISO, Country) %>% 
  summarise(Value = sum(Value)) %>% 
  ungroup() %>% mutate(Indicator = "Exports")

balance_net <- footprint.iso %>% select(-Indicator) %>% 
  left_join(territorial.iso %>% rename(Ter = Value) %>% 
              select(-Indicator)) %>% 
  mutate(Net = Value-Ter) %>% select(-Value, -Ter)

balance_net_Trade <- imports.iso %>% select(-Indicator) %>% 
  left_join(exports.iso %>% rename(Ter = Value) %>% 
              select(-Indicator)) %>% 
  mutate(Net = Value-Ter) %>% select(-Value, -Ter)

population_2013 <- population %>% filter(Year == 2013) %>% 
  select(-`Series Name`, -`Series Code`) %>% rename(Value = Pop,
                                                    Country = `Country Name`) %>% 
  mutate(Indicator = "Population") %>% 
  select(ISO, Country, Value, Indicator)

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "Footprint")
openxlsx::addWorksheet(wb, "Territorial")

openxlsx::addWorksheet(wb, "Footprint")
openxlsx::addWorksheet(wb, "Territorial")

openxlsx::addWorksheet(wb, "Imports")
openxlsx::addWorksheet(wb, "Exports")
openxlsx::addWorksheet(wb, "Population")

wb <- openxlsx::loadWorkbook("results/Results GHG FABIO.xlsx")

openxlsx::writeData(wb, "Footprint", footprint.iso)
openxlsx::writeData(wb, "Territorial", territorial.iso)
openxlsx::writeData(wb, "Imports", imports.iso)
openxlsx::writeData(wb, "Exports", exports.iso)
openxlsx::writeData(wb, "Population", population_2013)
openxlsx::saveWorkbook(wb,"results/Results GHG FABIO.xlsx", overwrite = T)


# Figure 1 ---------
world_food <- base::readRDS("~/food_footprint/data/world_food.rds")

t1 <- world_food %>% 
  dplyr::filter(Category != "Land Use Change"| Element == "Primary (25 years)") %>% 
  dplyr::mutate(Agg = base::ifelse(base::grepl("CO2", 
                                    Element, 
                                     ignore.case = TRUE), 
                               "CO2", 
                             ifelse(base::grepl("CH4", 
                                          Element, 
                                          ignore.case = TRUE), 
                                    "CH4",
                                    ifelse(base::grepl("N2O", 
                                                 Element, 
                                                 ignore.case = TRUE), 
                                           "N2O",
                                           ifelse(Category == "Land Use Change", 
                                                  "LUC",
                                                  "Other"))))
                ) %>% 
  dplyr::group_by(
    Agg, Year
  ) %>% 
  dplyr::summarise(Gg = sum(Gg)/1000000) %>% 
  dplyr::mutate(Agg = factor(Agg, levels = rev(c("CH4", "CO2", "N2O", "Other", "LUC"))))
  
Fig1a <- ggplot(data = t1,
            aes(x = Year, 
                y = Gg,
                fill = Agg)) +
  geom_area() +
  theme(
    panel.border=element_rect(colour="black", fill = NA),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_blank(),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 18, face = "bold"),
    plot.title = element_text(hjust = 0.5),
    plot.margin = unit(c(1.5, 1, 1, 1), "cm"),
    legend.key = element_rect(colour = NA, fill = NA),
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    legend.position = "bottom"
  ) +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.1,
                                   decimal.mark = '.')) +
  scale_fill_manual(values = rev(c((RColorBrewer::brewer.pal(4,'YlGnBu')), "brown")),
                    label = rev(c(expression("CH"[4]),
                              expression("CO"[2]),
                              expression(paste("N"[2], "O")),
                              "Other",
                              "LUC")),
                    guide = guide_legend(reverse = TRUE)) +
  labs(y = "PgCO2e/yr")
plot(Fig1a)

ve( file = "Fig1a.pdf", path="Graphs/",
        plot = Fig1a,
        width = 17,
        height = 17,
        #height = 40, 
        units = "cm")

# Figure 1-B ----------
world_food <- base::readRDS("~/food_footprint/data/world_food.rds")
world_economy <- base::readRDS("~/food_footprint/data/world_economy.rds")

t2 <- world_food %>% 
  dplyr::filter(Year %in% c(1995:2013)) %>% 
  dplyr::filter(Category != "Land Use Change"| Element == "Primary (25 years)") %>% 
  dplyr::group_by(
    Year
  ) %>% 
  dplyr::summarise(Gg = sum(Gg)/1000000) %>% 
  dplyr::mutate(Variable = "Food consumption") %>% 
  dplyr::bind_rows(
    world_economy %>% 
      dplyr::filter(Product %!in% product_exio$Name[c(1:19, 43:54, 156)]) %>% 
      dplyr::mutate(Variable = ifelse(Product %in% product_exio$Name[20:42], 
                                      "Minning and quarrying",
                                      ifelse(Product %in% product_exio$Name[55:127],
                                             "Manufacturing",
                                             ifelse(Product %in% product_exio$Name[128:149],
                                                    "Energy supply",
                                                    ifelse(Product %in% product_exio$Name[150:151],
                                                           "Construction",
                                                           "Services"))))) %>% 
      dplyr::group_by(
        Year, Variable
      ) %>% 
      dplyr::summarise(Gg = sum(Gg)/1000000)) %>% 
      dplyr::group_by(
        Year
      ) %>% 
      dplyr::mutate(perc = Gg/sum(Gg)) %>% 
      dplyr::ungroup() %>% 
  dplyr::arrange(Variable) %>% 
  dplyr::mutate(Variable = factor(Variable, levels = c("Food consumption",
                                                       "Minning and quarrying",
                                                       "Manufacturing",
                                                       "Energy supply",
                                                       "Construction",
                                                       "Services")))

Fig1b <- ggplot(data = t2,
                aes(x = Year, 
                    y = perc,
                    fill = Variable)) +
  geom_area(stat="identity") +
  theme(
    panel.border=element_rect(colour="black", fill = NA),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_blank(),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 18, face = "bold"),
    plot.title = element_text(hjust = 0.5),
    plot.margin = unit(c(1.5, 1, 1, 1), "cm"),
    legend.key = element_rect(colour = NA, fill = NA),
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    legend.position = "bottom"
  ) +
  scale_y_continuous(
    breaks = c(0, 0.25, 0.5, 0.75, 1), labels = scales::percent(c(0, 0.25, 0.5, 0.75, 1))) +
  scale_fill_manual(values = (RColorBrewer::brewer.pal(6,'Accent'))) +
  labs(y = "% of total global GHG emissions")
plot(Fig1b)

ve( file = "Fig1b.pdf", path="Graphs/",
        plot = Fig1b,
        width = 17,
        height = 17,
        #height = 40, 
        units = "cm")

# Figure 2a ----------
countries_food <- base::readRDS("~/food_footprint/data/countries_food.rds")

population <- readr::read_csv("data/Data_Extract_From_World_Development_Indicators/a999f80e-8914-49fe-a0a1-a6c6a8b5f31c_Data.csv") %>% 
  tidyr::gather(Year, Pop, -c(1:4)) %>%
  dplyr::filter(!is.na(Pop) & Pop != "..") %>% 
  dplyr::mutate(Year = as.integer(substr(Year, 1, 4)),
                Pop = as.numeric(Pop))

t3 <- countries_food %>% 
  dplyr::filter(`ISO FD` %!in% c("ROW", "PYF")) %>% 
  dplyr::group_by(`ISO FD`,
                  Year) %>% 
  dplyr::summarise(Gg = sum(Gg)) %>% 
  dplyr::ungroup() %>% 
  dplyr::left_join(population %>% 
                     dplyr::select(Pop, Year, `Country Code`),
                   by = c("ISO FD" = "Country Code",
                          "Year")) %>% 
  dplyr::left_join(
    countries %>% 
      dplyr::select(ISO, Continent),
    by = c("ISO FD" = "ISO")
  ) %>% 
  dplyr::group_by(Continent, Year) %>% 
  dplyr::summarise(Gg = sum(Gg, na.rm = T),
                   Pop = sum(Pop, na.rm = T),
    Cap = Gg/Pop*1000)

t3b <- t3 %>% 
  dplyr::bind_rows(t3 %>% 
                     dplyr::group_by(Year) %>% 
                     dplyr::summarise_if(is.numeric, sum) %>% 
                     dplyr::mutate(Cap = Gg/Pop*1000,
                                   Continent = "World"))

Fig2a <- ggplot(data = t3b,
                aes(x = Year, 
                    y = Cap, 
                    #fill = `ISO FD`
                    fill = Continent
                    )) +
  geom_line(aes(color = Continent), size=1.5)+
  theme(
    panel.border=element_rect(colour="black", fill = NA),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_blank(),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 18, face = "bold"),
    plot.title = element_text(hjust = 0.5),
    plot.margin = unit(c(1.5, 1, 1, 1), "cm"),
    legend.key = element_rect(colour = NA, fill = NA),
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    legend.position = "bottom"
  ) +
  labs(y = "tCO2e/cap/year") +
  scale_color_manual(values = (RColorBrewer::brewer.pal(8,'Set2')),
                     label = c("Africa",
                               "Asia",
                               "European Union",
                               "Rest of Europe",
                               "South America & Caribbean",
                               "North America",
                               "Oceania",
                               "World"
                               ))
  
plot(Fig2a) 

ve( file = "Fig2a.pdf", path="Graphs/",
        plot = Fig2a,
        width = 17,
        height = 17,
        #height = 40, 
        units = "cm")


# Figure 2b -----------
t4 <- t3 %>% 
  dplyr::group_by(Year) %>% 
  dplyr::mutate(perc = Gg/sum(Gg))
  
Fig2b <- ggplot(data = t4,
                aes(x = Year, 
                    y = perc,
                    fill = Continent)) +
  geom_area(stat="identity") +
  theme(
    panel.border=element_rect(colour="black", fill = NA),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_blank(),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 18, face = "bold"),
    plot.title = element_text(hjust = 0.5),
    plot.margin = unit(c(1.5, 1, 1, 1), "cm"),
    legend.key = element_rect(colour = NA, fill = NA),
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    legend.position = "bottom"
  ) +
  scale_y_continuous(
    breaks = c(0, 0.25, 0.5, 0.75, 1), labels = scales::percent(c(0, 0.25, 0.5, 0.75, 1))) +
  scale_fill_manual(values = (RColorBrewer::brewer.pal(7,'Set2')),
                    label = c("Africa",
                              "Asia",
                              "European Union",
                              "Rest of Europe",
                              "South America & Caribbean",
                              "North America",
                              "Oceania"
                    )) +
  labs(y = "% of total global carbon footprint")
plot(Fig2b)

ggsave( file = "Fig2b.pdf", path="Graphs/",
        plot = Fig2b,
        width = 17,
        height = 17,
        #height = 40, 
        units = "cm")

# Figure 3a ---------
t5 <-countries_food %>% 
  dplyr::filter(`ISO FD` %!in% c("ROW", "PYF")) %>% 
  dplyr::mutate(Agg = ifelse(Name %in% c("Bovine Meat",
                                         "Rice (Milled Equivalent)",
                                         "Pigmeat",
                                         "Fish"
  ),
  Name, Group),
  Agg = ifelse(Agg %!in% c("Bovine Meat",
                           "Rice (Milled Equivalent)",
                           "Pigmeat",
                           "Fish",
                           "Cereals",
                           "Meat",
                           "Milk",
                           "Coffee, tea, cocoa",
                           "Vegetable oils",
                           "Alcohol",
                           "Vegetables, fruit, nuts, pulses, spices"
  ),
  "Other", Agg)) %>% 
  dplyr::group_by(`ISO FD`,
                  Agg,
                  Year) %>% 
  dplyr::summarise(Gg = sum(Gg)) %>% 
  dplyr::ungroup() %>% 
  dplyr::left_join(
    countries %>% 
      dplyr::select(ISO, Continent),
    by = c("ISO FD" = "ISO")
  ) %>% 
  dplyr::group_by(Continent, Agg, Year) %>% 
  dplyr::summarise(Gg = sum(Gg, na.rm = T))

t5b <- countries_food %>% 
  dplyr::filter(`ISO FD` %!in% c("ROW", "PYF")) %>% 
  
  dplyr::mutate(Agg = ifelse(Name %in% c("Bovine Meat",
                                         "Rice (Milled Equivalent)",
                                         "Pigmeat",
                                         "Fish"
                                         ),
                             Name, Group),
                Agg = ifelse(Agg %!in% c("Bovine Meat",
                                         "Rice (Milled Equivalent)",
                                         "Pigmeat",
                                         "Fish",
                                         "Cereals",
                                         "Meat",
                                         "Milk",
                                         "Coffee, tea, cocoa",
                                         "Vegetable oils",
                                         "Alcohol",
                                         "Vegetables, fruit, nuts, pulses, spices"
                ),
                "Other", Agg)) %>% 
  dplyr::group_by(Agg,
                  Year) %>% 
  dplyr::summarise(Gg = sum(Gg)) %>% 
  dplyr::mutate(Continent = "World")

t5c <- t5 %>% 
  dplyr::bind_rows(t5b) %>% 
  dplyr::group_by(Continent, Year) %>% 
  dplyr::mutate(perc = Gg/sum(Gg)) %>% 
  dplyr::arrange(-Gg) %>% 
  dplyr::filter(Gg != 0,
                Gg > 0) %>% 
  dplyr::mutate(Agg = factor(Agg, levels = rev(c("Bovine Meat", 
                                                 "Pigmeat", 
                                                 "Meat", 
                                                 "Milk",
                                                 "Cereals",
                                                 "Rice (Milled Equivalent)",
                                                 "Vegetables, fruit, nuts, pulses, spices",
                                                 "Vegetable oils",
                                                 "Coffee, tea, cocoa",
                                                 "Fish",
                                                 "Alcohol",
                                                 "Other"))))

Fig3a <- ggplot(data = t5c,
                aes(x = Year, 
                    y = perc,
                    fill = Agg)) +
  geom_area() +
  theme(
    panel.border=element_rect(colour="black", fill = NA),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_blank(),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 18, face = "bold"),
    plot.title = element_text(hjust = 0.5),
    plot.margin = unit(c(1.5, 1, 1, 1), "cm"),
    legend.key = element_rect(colour = NA, fill = NA),
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    legend.position = "bottom"
  ) +
  scale_y_continuous(
    breaks = c(0, 0.25, 0.5, 0.75, 1), labels = scales::percent(c(0, 0.25, 0.5, 0.75, 1))) +
  scale_fill_manual(values = rev(c(RColorBrewer::brewer.pal(3,'Reds'), 
                                   RColorBrewer::brewer.pal(3,'Purples'),
                                   RColorBrewer::brewer.pal(3,'Greens'),
                                   RColorBrewer::brewer.pal(3,'Blues'))),
                    guide = guide_legend(reverse = TRUE)) +
  facet_wrap(~Continent, nrow=2,
             labeller = labeller(Continent = c("AFR" = "Africa",
                                               "ASI" = "Asia",
                                               "EU" = "European Union",
                                               "EUR" = "Rest of Europe",
                                              "LAM" = "South America & Caribbean",
                                                "NAM" = "North America",
                                               "OCE" = "Oceania",
                                              "World" = "World"
             ))) +
  labs(y = "% GHG emissions by product")
plot(Fig3a)

ggsave( file = "Fig3a.pdf", path="Graphs/",
        plot = Fig3a,
        width = 37,
        height = 25,
        #height = 40, 
        units = "cm")

# Figure 4 by product ---------
Y_countries <- NULL

for(year in 1986:2013){
  
  Y_fabio <- base::readRDS(paste0("~/FABIO/Y/", year, "_Y.rds"))
  Y_food <- Y_fabio[, grep("Food$", colnames(Y_fabio))]
  colnames(Y_food) <- countries$ISO
  
  Y_countries <- Y_countries %>% 
    dplyr::bind_rows(
      L_labels %>% 
        dplyr::bind_cols(as.data.frame(Y_food)) %>%
        tidyr::gather("ISO FD", "Y", -c(1:4)) %>% 
        dplyr::group_by(`ISO FD`, Name, Group) %>% 
        dplyr::summarise(Y = base::sum(Y)) %>%
        dplyr::mutate(Year = year) %>% 
        dplyr::ungroup()
    )

}

t5 <-countries_food %>% 
  dplyr::filter(`ISO FD` %!in% c("ROW", "PYF")) %>% 
  dplyr::group_by(`ISO FD`,
                  Name,
                  Group,
                  Year) %>% 
  dplyr::summarise(Gg = sum(Gg)) %>% 
  dplyr::ungroup() %>% 
  dplyr::left_join(Y_countries %>% 
                     dplyr::select(`ISO FD`,
                                   Name, 
                                   Year,
                                   Y)) %>% 
  dplyr::mutate(Intensity = Gg/Y) %>% 
  dplyr::filter(is.finite(Intensity) & Intensity != 0)

t6 <- t5 %>% 
  dplyr::filter(Year == 2013, Intensity > 0) %>% 
  dplyr::filter(Group %in% c("Meat",
                             "Vegetables, fruit, nuts, pulses, spices",
                             "Cereals",
                             "Vegetable oils",
                             "Roots and tubers"))
  
plot_group <- function(group_selected){
  p <- ggplot(data = t6 %>% 
                dplyr::filter(Group == group_selected),
         aes(x = Intensity, 
             y = Name)) +
    geom_boxplot() +
    theme(
      panel.border=element_rect(colour="black", fill = NA),
      axis.title.y = element_blank(),
      axis.text = element_text(size = 12),
      strip.text = element_text(size = 14, face = "bold"),
      plot.title = element_text(hjust = 0.5),
      plot.margin = unit(c(1.5, 1, 1, 1), "cm"),
      legend.key = element_rect(colour = NA, fill = NA),
      legend.title = element_blank(),
      legend.text = element_text(size = 14),
      legend.position = "bottom"
    ) +
    geom_vline(xintercept=0.020, linetype="dashed", color = "red") +
    ggtitle(group_selected) +
    labs(x = "tCO2e/kg fresh matter final demand/yr")
} 

graphs <- lapply(unique(t6$Group), plot_group)

Fig4 <- cowplot::plot_grid(plotlist=graphs, ncol = 1, align = 'v',
                           rel_heights =  c(0.1, 0.1, 0.1, 0.1))

ggsave( file = "Fig4b.pdf", path="Graphs/",
        plot = Fig4,
        limitsize = FALSE,
        width = 25,
        height = 37,
        #height = 40, 
        units = "cm")

# Figure 4b ------------
t7 <- t5 %>% 
  dplyr::bind_rows(
    t5 %>% 
      dplyr::group_by(
        Name, Group, Year
      ) %>% 
      dplyr::summarise_if(is.numeric, sum) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(Intensity = (Gg/Y),
                    `ISO FD` = "World")
  ) %>% 
  dplyr::mutate(Intensity = Intensity*1000)

max_product <- t7 %>% 
  dplyr::group_by(Name, Group) %>% 
  dplyr::summarise(Gg = sum(Gg)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(Share = round(Gg/sum(Gg)*100, 1)) %>% 
  dplyr::group_by(Group) %>% 
  dplyr::mutate(Share2 = round(Gg/sum(Gg)*100, 1)) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(Group, -Gg)

max_group <- t7 %>% 
  dplyr::group_by(Group) %>% 
  dplyr::summarise(Gg = sum(Gg)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(Share = round(Gg/sum(Gg)*100, 1)) %>% 
  dplyr::arrange(-Share)

new_classification <- products %>% 
  dplyr::select(FAO.Name, Com.Group) %>% 
  dplyr::filter(Com.Group %in% c("Cereals",
                 "Meat",
                 "Vegetables, fruit, nuts, pulses, spices",
                 "Roots and tubers",
                 "Milk",
                 "Eggs",
                 "Fish",
                 "Animal fats")) %>% 
  dplyr::mutate(Com.Group = ifelse(
    FAO.Name %in% c("Oranges, Mandarines",
                "Bananas",
                "Apples and products",
                "Grapes and products (excl wine)",
                "Pineapples and products",
                "Lemons, Limes and products",
                "Grapefruit and products",
                "Citrus, Other",
                "Plantains",
                "Dates",
                "Fruits, Other"),
    "Fruits",
    Com.Group),
    Com.Group = ifelse(
      FAO.Name %in% c("Tomatoes and products",
                      "Onions",
                      "Pimento",
                      "Beans",
                      "Peas",
                      "Pepper"),
      "Vegetable & Pulses",
      Com.Group),
    Com.Group = ifelse(
      Com.Group %in% c("Milk",
                       "Animal fats",
                       "Fish",
                       "Eggs"),
      "Other animal products",
      Com.Group),
    Com.Group = ifelse(
      Com.Group == "Vegetables, fruit, nuts, pulses, spices",
      "Vegetable & Pulses",
      Com.Group
    )
  )

product_selected <- new_classification %>% 
  dplyr::filter(FAO.Name %!in% c("Oats",
                                 "Cereals, Other",
                                 "Rye and products",
                                 "Barley and products",
                                 "Lemons, Limes and products",
                                 "Grapefruit and products",
                                 "Citrus, Other",
                                 "Plantains",
                                 "Dates",
                                 "Fruits, Other",
                                 "Vegetables, Other",
                                 "Nuts and products",
                                 "Pulses, Other and products",
                                 "Hops",
                                 "Cloves",
                                 "Spices, Other",
                                 "Meat Meal",
                                 "Meat, Other"
                                 ))

shares_product <- countries_food %>% 
  dplyr::group_by(Name, Group, Year) %>% 
  dplyr::summarise(Gg = sum(Gg)) %>% 
  dplyr::left_join(new_classification,
                   by = c("Name" = "FAO.Name")) %>% 
  dplyr::group_by(Com.Group, 
                  Year) %>% 
  dplyr::mutate(perc = Gg/sum(Gg),
                perc = replace(perc,
                               !is.finite(perc),
                               0),
                perc_op = 1-perc) %>% 
  dplyr::filter(!is.na(Com.Group)) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(-perc) %>% 
  dplyr::mutate(Group = Com.Group) %>% 
  dplyr::select(-Com.Group, -Gg) %>% 
  tidyr::gather("Share", "Value", 4:5) %>% 
  dplyr::filter(Name %in% product_selected$FAO.Name) %>% 
  dplyr::mutate(Share = factor(Share, levels = rev(c("perc", "perc_op"))),
                Name = factor(Name, levels = unique(Name)))

t8 <- t7 %>% 
  dplyr::filter(Intensity > 0) %>% 
  dplyr::filter(Name %in% product_selected$FAO.Name) %>% 
  dplyr::left_join(new_classification,
                   by = c("Name" = "FAO.Name")) %>% 
  dplyr::mutate(Group = Com.Group) %>% 
  dplyr::select(-Com.Group) %>% 
dplyr::mutate(Name = factor(Name, levels = unique(shares_product$Name)))

plot_perc_group <- function(group_selected){
 
  ggplot(data = shares_product %>% 
           dplyr::filter(Group == group_selected),
         aes(x = Year, 
             y = Value,
             fill = Share)) +
    geom_area(aes(x = Year, 
                  y = Value,
                  alpha = 0.3)) +
    theme(
      panel.border=element_blank(),
      strip.background = element_rect(fill = 'white'),
      panel.background = element_rect(fill = 'white'),
      axis.ticks.x = element_blank(),
      #axis.line.y = element_line(colour = "black"),
      axis.title.y = element_text(size = 8),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      strip.text = element_blank(),
      plot.title = element_text(hjust = 0.05,
                                vjust = -0.1,
                                face = "bold",
                                size = 14),
      legend.key = element_rect(colour = NA, fill = NA)
    ) +
    theme(legend.position = "none") +
    labs(y = paste0("% of\n", group_selected)) +
    scale_y_continuous(
      breaks = c(0, 1), labels = scales::percent(c(0, 1))) +
    scale_fill_manual(values = c("white", ifelse(group_selected == "Meat",
                                                           "red",
                                                           ifelse(group_selected == "Cereals",
                                                                  "green",
                                                                  ifelse(group_selected == "Fruits",
                                                                         "brown",
                                                                         ifelse(group_selected == "Roots and tubers",
                                                                                "purple",
                                                                                ifelse(group_selected == "Other animal products",
                                                                                       "pink",
                                                                                       "yellow"))))))) +
    facet_grid(~Name) +
    ggtitle(group_selected)

}

plot_ribbon <- function(group_selected){
  
  ggplot(t8 %>% 
           dplyr::filter(`ISO FD` == "World",
                         Group == group_selected), aes(y = Intensity, 
                                                   x = Year)) +
    geom_line( 
      colour = ifelse(group_selected == "Meat",
                      "red",
                      ifelse(group_selected == "Cereals",
                             "green",
                             ifelse(group_selected == "Fruits",
                                    "brown",
                                    ifelse(group_selected == "Roots and tubers",
                                           "purple",
                                           ifelse(group_selected == "Other animal products",
                                                  "pink",
                                                  "yellow")))))) +
    stat_summary(data = t8 %>% 
                   dplyr::filter(`ISO FD` != "World",
                                 Group == group_selected), 
                 geom = "line",
                 fun = median, 
                 alpha = 0.3,
                 colour = "blue") +
    stat_summary(data = t8 %>% 
                   dplyr::filter(`ISO FD` != "World",
                                 Group == group_selected), 
                 geom = "ribbon",
                 fun.min = function(z) {quantile(z,0.1)},
                 fun.max = function(z) {quantile(z,0.9)}, 
               alpha = 0.3,
                 colour = NA) +
    theme(
      strip.background = element_rect(color = "black",
                                      fill = "white",
                                      #size = 1
      ),
      panel.border=element_rect(colour="black", fill = NA),
      axis.title.x = element_blank(),
      axis.text = element_text(size = 12),
      strip.text = element_text(size = 14, face = "bold"),
      plot.title = element_text(hjust = 0.5),
      plot.margin = unit(c(-0.3, 0, 0, 0), "cm"),
    ) +
    labs(y = "kgCo2e/kg fresk matter") +
    theme(legend.position = "none") +
    facet_grid(~Name,
               labeller = labeller(Name = c("Rice (Milled Equivalent)" = "Rice",
                                                 "Wheat and products" = "Wheat",
                                            "Barley and products" = "Barley",
                                            "Maize and products" = "Maize",
                                            "Millet and products" = "Millet",
                                            "Sorghum and products" = "Sorghum",
                                            "Rye and products" = "Rye",
                                            "Bovine Meat" = "Bovine Meat",
                                            "Meat, Other" = "Meat, Other",
                                            "Mutton & Goat Meat" =  "Mutton & Goat",
                                            "Pigmeat" = "Pigmeat",
                                            "Poultry Meat" = "Poultry Meat",
                                           "Offals, Edible" = "Offals",
                                           "Oranges, Mandarines" = "Oranges",
                                           "Bananas" = "Bananas",
                                           "Apples and products" = "Apples",
                                           "Grapes and products (excl wine)" = "Grapes",
                                           "Pineapples and products" = "Pineapples",
                                           "Tomatoes and products" = "Tomatoes",
                                           "Onions" = "Onions",
                                           "Pimento" = "Pimento",
                                           "Pepper" = "Pepper",
                                           "Beans" = "Beans",
                                           "Peas" = "Peas",
                                           "Pet food" = "Pet food",
                                           "Butter, Ghee" = "Butter",
                                           "Eggs" = "Eggs",
                                           "Fats, Animals, Raw" = "Animal fats",
                                           "Fish, Seafood" = "Fish",
                                           "Milk - Excluding Butter" = "Milk",
                                           "Cassava and products" = "Cassava",
                                           "Potatoes and products" = "Potatoes",
                                           "Roots, Other" = "Roots, Other",
                                           "Sweet potatoes" = "Sweet potatoes",
                                           "Yams" = "Yams"
               ))) +
    scale_x_continuous(breaks = c(1980,1990,2000,2010)) +
    labs(y = "kgCO2e/kg/yr")
  
} 

obtain_assembled_graph <- function(group_selected)
  {cowplot::plot_grid(plot_perc_group(group_selected), 
                           plot_ribbon(group_selected), 
                           rel_heights = c(1,2), 
                           ncol = 1, 
                           align = 'v')}

Fig5 <- cowplot::plot_grid(plotlist = lapply(c("Meat",
                                               "Cereals", 
                                               "Fruits",
                                               "Vegetable & Pulses"#,
                                               #"Roots and tubers",
                                               #"Other animal products"
                                               ), 
                                            obtain_assembled_graph), 
                           ncol = 1, 
                           align = 'v')
plot(Fig5)

ggsave( file = "Fig5.pdf", path="Graphs/",
        plot = Fig5,
        limitsize = FALSE,
        width = 25,
        height = 30,
        units = "cm")

Fig5b <- cowplot::plot_grid(plotlist = lapply(c(#"Meat",
                                               #"Cereals", 
                                               #"Fruits",
                                               #"Vegetable & Pulses"#,
                                               "Roots and tubers",
                                               "Other animal products"
), 
obtain_assembled_graph), 
ncol = 1, 
align = 'v')

ggsave( file = "Fig5b.pdf", path="Graphs/",
        plot = Fig5b,
        limitsize = FALSE,
        width = 25,
        height = 15,
        units = "cm")

# Figure 5 Trade ----------
selected_countries <- c("BRA", "AUS", "CAN", "IND", "CHN", "USA",
                        "JPN", "MEX", "ZAF", "IDN", "DEU", "FRA", 
                        "GBR", "ITA", "ESP", "TUR", "NOR", "KOR",
                        "CHE", "RUS")

countries_selected_corr <- dplyr::tibble(ISOcorr = c("CHN", "IND", "IDN", "RUS", "JPN", "DEU", "FRA",
                                                     "GBR", "ITA", "ESP", "TUR", "CHE", "NOR",
                                                     "BRA", "MEX", "USA", "CAN", "AUS", "KOR", "RUS",
                                                     "ZAF", "AFR", "LAM", "ASI"),
                                         Exio = c("CN", "IN","ID", "RU", "JP", "DE",
                                                  "FR", "GB", "IT", "ES","TR", "CH",
                                                  "NO", "BR","MX","US",
                                                  "CA","AU", "KR", "RU", "ZA",
                                                  "WF", "WL", "WA"))

roEU <- country_exio$region[country_exio$region %!in% countries_selected_corr$Exio]
roEU <- roEU[1:23]

countries_selected_corr <- countries_selected_corr %>% 
  dplyr::bind_rows(
    tibble(ISOcorr = "EU",
           Exio = roEU)
  )

selected_regions <- countries %>% 
  dplyr::filter(ISO %!in% c(selected_countries,
                            "ROW")) %>% 
  dplyr::select(Country,
                ISO,
                Continent)

years <- c(1986:2013)
trade_balances <- NULL

for(year in years){
  
  countries_trade <- base::readRDS(base::paste0("~/food_footprint/data/countries_trade_", year, ".rds"))
  
  trade_balances <- trade_balances %>% 
    dplyr::bind_rows(
      countries_trade %>% 
        dplyr::left_join(selected_regions %>% 
                           dplyr::select(ISO, Continent),
                         by = c("Origin" = "ISO")) %>% 
        dplyr::mutate(Origin = ifelse(is.na(Continent),
                                      Origin,
                                      Continent)) %>% 
        dplyr::group_by(Origin, ISO, Year, Flow) %>% 
        dplyr::summarise(Gg = sum(Gg)) %>% 
        dplyr::ungroup() %>% 
        dplyr::left_join(countries_selected_corr,
                         by = c("Origin" = "Exio")) %>% 
        dplyr::mutate(Origin = ifelse(is.na(ISOcorr),
                                      Origin,
                                      ISOcorr)) %>% 
        dplyr::filter((ISO != Origin & Flow == "Imports")|
                        ISO == Origin & Flow == "Exports") %>% 
        dplyr::group_by(ISO, Year, Flow) %>%
        dplyr::summarise(Gg = sum(Gg)) %>% 
        dplyr::ungroup() %>% 
        dplyr::bind_rows(
          countries_trade %>%
            dplyr::left_join(selected_regions %>% 
                               dplyr::select(ISO, Continent),
                             by = c("Origin" = "ISO")) %>% 
            dplyr::mutate(Origin = ifelse(is.na(Continent),
                                          Origin,
                                          Continent)) %>% 
            dplyr::group_by(Origin, ISO, Year, Flow) %>% 
            dplyr::summarise(Gg = sum(Gg)) %>% 
            dplyr::ungroup() %>% 
            dplyr::left_join(countries_selected_corr,
                             by = c("Origin" = "Exio")) %>% 
            dplyr::mutate(Origin = ifelse(is.na(ISOcorr),
                                          Origin,
                                          ISOcorr)) %>% 
            dplyr::filter(ISO == Origin & Flow == "Imports") %>% 
            dplyr::group_by(ISO, Year, Flow) %>%
            dplyr::mutate(Flow = "Domestic") %>% 
            dplyr::summarise(Gg = sum(Gg)) %>% 
            dplyr::ungroup()
        )
  )
  
}

countries_feeding <- c("BRA", "LAM", "AUS", "CAN", "RUS", "IND")
countries_balanced <- c("CHN", "IDN", "TUR", "ASI", "ZAF", "AFR")
countries_asi_na <- c("USA", "MEX", "JPN", "KOR", "CHE", "NOR")
countries_eu <- c("DEU", "ITA", "FRA", "ESP", "EU", "GBR")

pop_regions <- base::lapply(c("AFR",
                        "EU",
                        "LAM",
                        "ASI"), 
                      function(region){
                        selected_regions %>% 
                          dplyr::filter(Continent == region) %>% 
                          dplyr::left_join(population %>% 
                                             dplyr::select(
                                               `Country Code`,
                                               Year,
                                               Pop
                                             ), 
                                           by = c("ISO" = "Country Code")) %>% 
                          dplyr::group_by(Continent, Year) %>% 
                          dplyr::summarise(Pop = sum(Pop))
                        
                      }) %>% 
  data.table::rbindlist() %>% 
  dplyr::filter(Year %in% years) %>% 
  dplyr::rename(`Country Code` = Continent)

data_plot <- trade_balances %>% 
  dplyr::filter(ISO %in% c(countries_feeding,
                           countries_balanced,
                           countries_asi_na,
                           countries_eu)) %>% 
  tidyr::spread(Flow, Gg) %>% 
  dplyr::mutate(Balance = (Exports - Imports)/1000,
                Imports = -Imports/1000,
                Exports = Exports/1000,
                Domestic = Domestic/1000,
                Ribbon = ifelse(Balance < 0, 
                                "Net imports",
                                "Net exports")) %>% 
  tidyr::gather(Flow, Pg, -c(1:2, 7)) %>% 
    dplyr::ungroup() %>% 
  dplyr::left_join(population %>% 
                     dplyr::select(
                       `Country Code`,
                       Year,
                       Pop
                     ) %>% 
                     dplyr::bind_rows(
                       pop_regions
                     ), 
                   by = c("ISO" = "Country Code",
                          "Year")) %>% 
  dplyr::mutate(Capita = Pg*1000000/Pop,
                Capita = ifelse(Capita <0 ,
                                Capita*-1,
                                Capita),
                  ISO = factor(ISO, 
                             levels = c(countries_feeding,
                                        countries_balanced,
                                        countries_asi_na,
                                        countries_eu)),
                Flow = factor(Flow, levels = rev(c("Domestic",
                                               "Imports",
                                               "Exports",
                                               "Balance")))) %>% 
  dplyr::mutate(Pg = ifelse(ISO == "RUS" & Year %in% c(1986:1991),
                            NA, Pg),
                Capita = ifelse(ISO == "RUS" & Year %in% c(1986:1991),
                            NA, Capita))

labels_countries <- c("BRA" = "Brazil",
                      "AUS" = "Australia",
                      "CAN" = "Canada",
                      "IND" = "India",
                      "USA" = "USA",
                      "CHN" = "China",
                      "JPN" = "Japan",
                      "MEX" = "Mexico",
                      "DEU" = "Germany",
                      "GBR" = "UK",
                      "ESP" = "Spain",
                      "ITA" = "Italy",
                      "TUR" = "Turkey",
                      "ZAF" = "South Africa",
                      "IDN" = "Indonesia",
                      "NOR" = "Norway",
                      "FRA" = "France",
                      "CHE" = "Switzerland",
                      "LAM" = "Rest of America",
                      "AFR" = "Rest of Africa",
                      "RUS" = "Russia",
                      "KOR" = "Korea",
                      "EU" = "Rest of EU",
                      "ASI" = "Rest of Asia")

plot_trend_balance <- function(data_balance,
                               countries_selected){
  ggplot(data = data_plot %>% 
           dplyr::filter(Flow %!in% c("Balance"),
                         ISO %in% countries_selected)) +
    geom_col(aes(x = Year, 
                  y = Pg,
                 fill = Flow),
             size = 0.25
             ) +
    geom_hline(yintercept = c(100, -100), 
               color = "blue", 
               linetype = "dashed") +
    theme(
      panel.border=element_rect(colour="black", fill = NA),
      axis.title.x = element_blank(),
      axis.text.y = element_text(size = 12),
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      plot.title = element_text(hjust = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      legend.background = element_blank(),
      legend.position = "right"
    ) +
    guides(fill = guide_legend(nrow = 1,
                               title = "Total GHG flows")) +
  facet_wrap(~ ISO,
             nrow = 1,
             scales = "free_y",
             labeller = labeller(ISO = labels_countries)
  ) +
    scale_x_continuous(breaks = c(1990,  2010)) +
    scale_fill_manual(breaks = c("Domestic",
                                 "Imports",
                                 "Exports",
                                 "Balance"),
      values = c(#"#CCEEFF", "#FFDDDD", 
                 "Domestic" = "#074F57",
                 "Imports" = "#8FB339",
                "Exports" = "#E28413")) +
    labs(y = "GgCO2e/kg/yr")
}

plot_trend_capita <- function(data_balance,
                               countries_selected){
  ggplot(data = (data_plot %>% 
           dplyr::filter(Flow %in% c("Balance",
                                     "Domestic"),
                         ISO %in% countries_selected) %>% 
                           dplyr::group_by(ISO, Year) %>% 
                           dplyr::summarise(Capita = sum(Capita)) %>% 
             dplyr::mutate(Flow = "Domestic + Net balance"))) +
    geom_line(aes(x = Year, 
                  y = Capita,
                  linetype = Flow),
              colour = "black") +
    geom_line(data = (data_plot %>% 
                        dplyr::filter(Flow == "Domestic",
                                      ISO %in% countries_selected)),
                      aes(x = Year, 
                  y = Capita,
                  linetype = Flow),
              colour = "black")+
    
    geom_ribbon(data = (data_plot %>% 
                          dplyr::select(-Pop, -Pg) %>% 
                          dplyr::filter(Flow %in% c("Balance",
                                                    "Domestic"),
                                        ISO %in% countries_selected) %>% 
                          tidyr::spread(Flow, Capita)),
                aes(x = Year, 
                    ymax = Domestic+Balance,
                    ymin = Domestic,
                    fill = Ribbon) ,
                alpha = .3) +
    theme(
      panel.border=element_rect(colour="black", fill = NA),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 12),
      strip.background = element_rect(color = "black",
                                      fill = "white",
                                      #size = 1
                                      ),
      strip.text = element_text(size = 11, face = "bold"),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      legend.spacing.y = unit(-0.1, "cm"),
      legend.position = "left",
      legend.background = element_blank(),
      legend.key = element_rect(fill = "white")#, 
                                #colour = "black")
    ) +
    facet_wrap(~ ISO,
               nrow = 1,
               labeller = labeller(ISO = labels_countries),
               scales = "free"
    ) +
    guides(fill = guide_legend(nrow = 1,
                               title = ""),
           linetype = guide_legend(nrow = 1,
                                   title = "Carbon footprint per capita\n")) +
    #scale_fill_manual(breaks = c("Net exports", "Net imports"),
    #  values = c("Net exports" = "#CCEEFF", "Net imports" =  "#FFDDDD"
    #               )) +
    scale_y_continuous(breaks = function(Capita) {
      if(max(Capita) > 10){c(0, 7.5, 15)} else{
        if(max(Capita) < 10 & max(Capita) > 2){c(0, 2, 4)} else{
          c(0, 1, 2)
        }}
      },
      limits = function(Capita) {
        if(max(Capita) > 10){c(0, 15)} else{
          if(max(Capita) < 10 & max(Capita) > 2){c(0, 4)} else{
            c(0, 2)
          }}
      }) + 
    labs(y = "tCO2e/cap/yr")
}

obtain_figure_trade <- function(group_selected)
{cowplot::plot_grid(plot_trend_capita(data_plot, group_selected) +
                      theme(legend.position="none"), 
                    plot_trend_balance(data_plot, group_selected) +
                      theme(legend.position="none",
                            axis.ticks.x = element_blank(),
                            plot.margin = unit(c(0, 0, 0.25, 0), "cm"))+ # separation between graphs
                      if(group_selected == countries_eu){
                        theme(axis.text.x =  element_text(size = 12))
                      }else{theme(axis.text.x =  element_blank())}, 
                    rel_heights = c(1,1.25), 
                    ncol = 1, 
                    align = 'v')}

Fig6 <- cowplot::plot_grid(
  cowplot::plot_grid(plotlist = base::lapply(list(countries_feeding,
                                                  countries_balanced,
                                                  countries_asi_na,
                                                  countries_eu), 
                                             obtain_figure_trade), 
                     ncol = 1, 
                     align = 'v',
                     axis = "t") +
    theme(plot.margin = unit(c(0, 0, 0.75, 0), "cm")),    # For legend below
  cowplot::plot_grid(
    cowplot::get_legend(
      plot_trend_capita(data_plot, countries_feeding)
    ),
    cowplot::get_legend(
      plot_trend_balance(data_plot, countries_feeding)
    ),
    nrow = 1 
  ),
  ncol = 1, 
  align = 'v',
  rel_heights = c(2, 0.15)
)
  
plot(Fig6)

ggsave( file = "Fig6.pdf", path="Graphs/",
        plot = Fig6,
        limitsize = FALSE,
        width = 25,
        height = 30,
        units = "cm")


# Figure Sankey ----------
base::load("~/food_footprint/data/world_links_2013.RData")

t1 <- m_ghg_prod %>% 
  dplyr::left_join(products %>% 
                     dplyr::select(FAO.Name,
                                   Com.Group),
                   by = c("DP" = 
                            "FAO.Name")) %>% 
  dplyr::left_join(products %>% 
                     dplyr::select(FAO.Name,
                                   Com.Group),
                   by = c("CF" = 
                            "FAO.Name")) %>% 
  dplyr::group_by(Com.Group.x, Com.Group.y) %>% 
  dplyr::summarise(Gg = sum(Gg)) %>% 
  dplyr::rename(`Domestic production` = Com.Group.x ,
                `Consumption footprint` = Com.Group.y) %>% 
  dplyr::filter(Gg > 0) %>% 
  dplyr::ungroup()

t1$`Consumption footprint` <- paste(t1$`Consumption footprint`, " ", sep="")

Nodes <- t1 %>% 
  dplyr::select(`Domestic production`) %>% 
  dplyr::distinct() %>% 
  dplyr::rename(Name =`Domestic production`) %>% 
  dplyr::bind_rows(
    t1 %>% 
      dplyr::select(`Consumption footprint`) %>% 
      dplyr::distinct() %>% 
      dplyr::rename(Name = `Consumption footprint`)
  ) 

t1$IDsource=match(t1$`Domestic production`, Nodes$Name)-1 
t1$IDtarget=match(t1$`Consumption footprint`, Nodes$Name)-1

library(networkD3)

sankeyNetwork(Links = t1, Nodes = Nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "Gg", NodeID = "Name",
              sinksRight=FALSE#,
              #colourScale=ColourScal
              #nodeWidth=40, fontSize=13, nodePadding=20
              )

a1 <- m_ghg_prod %>% 
  dplyr::arrange(-Gg)


base::load("~/food_footprint/data/world_links_2013.RData")

t1b <- m_ghg_prod %>% 
  dplyr::rename(`Domestic production` = DP ,
                `Consumption footprint` = CF) %>% 
  dplyr::filter(Gg > 0) %>% 
  dplyr::ungroup()

t1b$`Consumption footprint` <- paste(t1b$`Consumption footprint`, " ", sep="")

Nodes_t1b <- t1b %>% 
  dplyr::select(`Domestic production`) %>% 
  dplyr::distinct() %>% 
  dplyr::rename(Name =`Domestic production`) %>% 
  dplyr::bind_rows(
    t1b %>% 
      dplyr::select(`Consumption footprint`) %>% 
      dplyr::distinct() %>% 
      dplyr::rename(Name = `Consumption footprint`)
  ) 

t1b$IDsource=match(t1b$`Domestic production`, Nodes_t1b$Name)-1 
t1b$IDtarget=match(t1b$`Consumption footprint`, Nodes_t1b$Name)-1

library(networkD3)

sankeyNetwork(Links = t1b, Nodes = Nodes_t1b,
              Source = "IDsource", Target = "IDtarget",
              Value = "Gg", NodeID = "Name",
              sinksRight=FALSE#,
              #colourScale=ColourScal
              #nodeWidth=40, fontSize=13, nodePadding=20
)


t2 <- m_ghg_iso %>% 
  dplyr::left_join(countries %>% 
                     dplyr::select(ISO,
                                   Continent),
                   by = c("DP" = "ISO")) %>% 
  dplyr::left_join(countries %>% 
                     dplyr::select(ISO,
                                   Continent),
                   by = c("CF" = "ISO")) %>% 
  dplyr::group_by(Continent.x, Continent.y) %>% 
  dplyr::summarise(Gg = sum(Gg)) %>% 
  dplyr::rename(`Domestic production` = Continent.x ,
                `Consumption footprint` = Continent.y) %>% 
  dplyr::filter(Gg > 0) %>% 
  dplyr::ungroup()

t2$`Consumption footprint` <- paste(t2$`Consumption footprint`, " ", sep="")

Nodes_t2 <- t2 %>% 
  dplyr::select(`Domestic production`) %>% 
  dplyr::distinct() %>% 
  dplyr::rename(Name =`Domestic production`) %>% 
  dplyr::bind_rows(
    t2 %>% 
      dplyr::select(`Consumption footprint`) %>% 
      dplyr::distinct() %>% 
      dplyr::rename(Name = `Consumption footprint`)
  ) 

t2$IDsource=match(t2$`Domestic production`, Nodes_t2$Name)-1 
t2$IDtarget=match(t2$`Consumption footprint`, Nodes_t2$Name)-1

library(networkD3)

sankeyNetwork(Links = t2, Nodes = Nodes_t2,
              Source = "IDsource", Target = "IDtarget",
              Value = "Gg", NodeID = "Name",
              sinksRight=FALSE#,
              #colourScale=ColourScal
              #nodeWidth=40, fontSize=13, nodePadding=20
)

t3 <- m_ghg_iso %>% 
  dplyr::rename(`Domestic production` = DP ,
                `Consumption footprint` = CF) %>% 
  dplyr::filter(Gg > 0) %>% 
  dplyr::ungroup()

t3$`Consumption footprint` <- paste(t3$`Consumption footprint`, " ", sep="")

Nodes <- t3 %>% 
  dplyr::select(`Domestic production`) %>% 
  dplyr::distinct() %>% 
  dplyr::rename(Name =`Domestic production`) %>% 
  dplyr::bind_rows(
    t3 %>% 
      dplyr::select(`Consumption footprint`) %>% 
      dplyr::distinct() %>% 
      dplyr::rename(Name = `Consumption footprint`)
  ) 

t3$IDsource=match(t3$`Domestic production`, Nodes$Name)-1 
t3$IDtarget=match(t3$`Consumption footprint`, Nodes$Name)-1

library(networkD3)

sankeyNetwork(Links = t3, Nodes = Nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "Gg", NodeID = "Name",
              sinksRight=FALSE#,
              #colourScale=ColourScal
              #nodeWidth=40, fontSize=13, nodePadding=20
)


t1 <- world_perspective %>% 
  dplyr::left_join(products %>% 
                     dplyr::select(FAO.Name,
                                   Com.Group),
                   by = c("Name" = 
                            "FAO.Name")) %>% 
  dplyr::group_by(Responsibility, Com.Group, Year) %>% 
  dplyr::summarise(Gg = sum(Gg)) %>% 
  dplyr::filter(Year == 2013)



# Fig comparasion ---------
world_perspective <- base::readRDS("~/food_footprint/data/world_perspective.rds")

t1 <- world_perspective %>% 
  dplyr::filter(Year == 2013) %>% 
  dplyr::group_by(
    Name,
    Responsibility
  ) %>% 
  dplyr::summarise(Gg = sum(Gg)/1000) %>% 
  dplyr::arrange(-Gg) %>% 
  dplyr::mutate(Label = ifelse(Gg >700,
                               "Very high",
                               ifelse(Gg < 700 & Gg > 350,
                                      "High",
                                      ifelse(Gg < 350 & Gg > 100,
                                             "Medium", 
                                             ifelse(Gg < 100 & Gg > 10,
                                                    "Low",
                                                    "Negligible"))))) %>% 
  dplyr::filter(Name %in% products$FAO.Name,
                Gg != 0) %>% 
  dplyr::select(-Gg) %>% 
  tidyr::spread(Responsibility, Label) %>% 
  dplyr::mutate_at(2:3, replace_na, replace = "Negligible") %>% 
  dplyr::filter((`Consumption footprint` == "Negligible" & `Domestic production` != "Negligible")|
                  (`Consumption footprint` != "Negligible" & `Domestic production` == "Negligible")|
                  (`Consumption footprint` != "Negligible" & `Domestic production` != "Negligible")) %>% 
  tidyr::gather("Responsibility", "Level", 2:3) %>% 
  dplyr::arrange(match(Name, products$FAO.Name)) %>% 
  dplyr::mutate(Name = factor(Name, levels = rev(products$FAO.Name)),
                Level = factor(Level, levels = c("Very high", "High", "Medium", "Low", "Negligible")),
                Responsibility = factor(Responsibility, levels = c("Domestic production",
                                                                   "Consumption footprint"))) 

Fig7 <- ggplot(t1, aes(Responsibility,
               Name, fill= Level,
               width = 0.2,
               height = 0.8)) + 
  geom_tile() +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_text(size = 12),
    legend.spacing.y = unit(-0.1, "cm"),
    legend.position = "right",
    legend.title = element_blank(),
    legend.background = element_blank() ) +
  scale_fill_manual(values = c("#ff0000", "#ff4d4d", "#ffa64d", "#ffe59a", "grey"))

ggsave( file = "Fig7.pdf", path="Graphs/",
        plot = Fig7,
        limitsize = FALSE,
        width = 15,
        height = 30,
        units = "cm")

# World map -----------
countries_balance_2013 <- base::readRDS("~/food_footprint/data/countries_balance_2013.rds")

t1 <- countries_balance_2013 %>% 
      dplyr::filter(Origin != ISO & Flow == "Imports" &
                      Variable =="") %>% 
      dplyr::group_by(ISO, Year, Flow) %>%
      dplyr::summarise(Gg = sum(Gg)) %>% 
      dplyr::ungroup()

t2 <- countries_balance_2013 %>% 
  dplyr::filter(ISO == Origin, Flow == "Exports") %>% 
  dplyr::group_by(Origin, Year, Flow) %>%
  dplyr::summarise(Gg = sum(Gg)) %>% 
  dplyr::ungroup()

a1 <- filter(countries_balance_2013, ISO == "ESP",
             ISO != Origin) %>% 
  dplyr::group_by(Origin, ISO, Year, Flow, Variable) %>%
  dplyr::summarise(Gg = sum(Gg)) %>% 
  dplyr::ungroup()

a1 <- filter(countries_balance_2013, Origin == "ESP" & ISO != "ESP")
a2 <- filter(countries_balance_2013, Origin != "ESP" & ISO == "ESP")
sum(a1$Gg)
sum(a2$Gg)

a3 <- filter(t1, ISO == "ESP")

# Relation CF territotial ---------
countries_territorial <- readRDS("~/food_footprint/data/countries_territorial.rds")
countries_food <- readRDS("~/food_footprint/data/countries_food.rds")

population <- readr::read_csv("data/Data_Extract_From_World_Development_Indicators/a999f80e-8914-49fe-a0a1-a6c6a8b5f31c_Data.csv") %>% 
  tidyr::gather(Year, Pop, -c(1:4)) %>%
  dplyr::filter(!is.na(Pop) & Pop != "..") %>% 
  dplyr::mutate(Year = as.integer(substr(Year, 1, 4)),
                Pop = as.numeric(Pop))

t1 <- countries_food %>% 
  dplyr::group_by(`ISO FD`) %>% 
  dplyr::summarise(Gg = sum(Gg)) %>% 
  dplyr::arrange(-Gg)

t2 <- countries_food %>% 
  dplyr::group_by(`ISO FD`,
                  Year) %>% 
  dplyr::summarise(Gg = sum(Gg)) %>% 
  dplyr::rename(ISO = `ISO FD`) %>% 
  dplyr::mutate(Perspective = "Footprint") %>% 
  dplyr::bind_rows(
    countries_territorial %>% 
      dplyr::group_by(
        ISO, Year
      ) %>% 
      dplyr::summarise(Gg = sum(Gg)) %>% 
      dplyr::mutate(Perspective = "Territorial")
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::left_join(
    population %>% 
      dplyr::select(`Country Code`,
                    Year, Pop),
    by = c("ISO" = "Country Code",
           "Year")
  ) %>% 
  dplyr::left_join(countries %>% 
                     dplyr::select(ISO,
                                   Continent)) %>%
  dplyr::mutate(Cap = Gg*1000/Pop) %>% 
  dplyr::select(-Pop, -Gg) %>% 
  tidyr::spread(Perspective, Cap) %>% 
  dplyr::mutate(Relation = Footprint/Territorial) %>% 
  dplyr::filter(!is.na(Relation),
                Relation < 10,
                ISO %in% t1$`ISO FD`[1:30])

ggplot(t2, aes(Year,
               Relation,
               group = ISO)) + 
  geom_line(aes(color = ISO)) +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_text(size = 12)#,
    #legend.spacing.y = unit(-0.1, "cm"),
    #legend.position = "bottom",
    #legend.title = element_blank(),
    #legend.background = element_blank()
    ) +
  facet_wrap(~Continent) #+
   # theme(legend.position = "none")
  #scale_fill_manual(values = c("#ff0000", "#ff4d4d", "#ffa64d", "#ffe59a", "grey"))

# To excel ----------
table1 <- world_food %>% 
  tidyr::spread(Year, Gg)

table2 <- world_economy %>% 
  tidyr::spread(Year, Gg)

table3 <- countries_food %>% 
  dplyr::left_join(countries %>% 
                     dplyr::select(ISO, Country),
                   by = c("ISO FD" = "ISO")) %>% 
  tidyr::spread(Year, Gg)

final_demand <- Y_countries %>% 
  dplyr::left_join(countries %>% 
                     dplyr::select(ISO, Country),
                   by = c("ISO FD" = "ISO")) %>% 
  tidyr::spread(Year, Y)

openxlsx::write.xlsx(table1, file="/home/vant/food_footprint/Results.xlsx", 
                     sheetName="Data Figure 1 (a)",
                     row.names=FALSE)

wb <- openxlsx::loadWorkbook("/home/vant/food_footprint/Results.xlsx")
openxlsx::writeData(wb, sheet = "Data Figure 1 (b)", table2, colNames = F)
openxlsx::writeData(wb, sheet = "Other Figures", table3, colNames = F)
openxlsx::writeData(wb, sheet = "Final demand", final_demand, colNames = F)
openxlsx::saveWorkbook(wb,"/home/vant/food_footprint/Results.xlsx",overwrite = T)


p1 <- ggpubr::ggarrange(for(group_selected in unique(t5$Group)){
  plot_group(group_selected)}, 
                  #labels = c("A", "B"),
                  ncol = 2)
plot(p1)
  

  scale_y_continuous(
    breaks = c(0, 0.25, 0.5, 0.75, 1), labels = scales::percent(c(0, 0.25, 0.5, 0.75, 1))) +
  scale_fill_manual(values = rev(c(RColorBrewer::brewer.pal(3,'Reds'), 
                                   RColorBrewer::brewer.pal(3,'Purples'),
                                   RColorBrewer::brewer.pal(3,'Greens'),
                                   RColorBrewer::brewer.pal(3,'Blues'))),
                    guide = guide_legend(reverse = TRUE)) +
  f
  #facet_grid(. ~ Continent) +
 
plot(Fig4)

# regions ---------
country_exio <- read_delim("~/EXIOBASE/unit.txt", 
                           "\t", escape_double = FALSE, trim_ws = TRUE) %>% 
  dplyr::select(region) %>% 
  dplyr::distinct()

exio_eu <- country_exio$region[1:28]
exio_nam <- country_exio$region[c(29, 32)]
exio_asi <- country_exio$region[c(30, 31, 33, 35, 37, 41, 43, 45, 49)]
exio_lam <- country_exio$region[c(34, 36, 46)]
exio_oce <- country_exio$region[38]
exio_eur <- country_exio$region[c(39, 40, 42, 47)]
exio_afr <- country_exio$region[c(44, 48)]

trade_regions <- results_fabio_trade %>% 
  dplyr::left_join(countries %>% 
                     dplyr::select(ISO, Continent),
                   by = c("Origin" = "ISO")) %>% 
  dplyr::mutate(Continent = replace(Continent,
                                    Origin %in% exio_eu,
                                    "EU"),
                Continent = replace(Continent,
                                    Origin %in% exio_nam,
                                    "NAM"),
                Continent = replace(Continent,
                                    Origin %in% exio_asi,
                                    "ASI"),
                Continent = replace(Continent,
                                    Origin %in% exio_lam,
                                    "LAM"),
                Continent = replace(Continent,
                                    Origin %in% exio_oce,
                                    "OCE"),
                Continent = replace(Continent,
                                    Origin %in% exio_eur,
                                    "EUR"),
                Continent = replace(Continent,
                                    Origin %in% exio_afr,
                                    "AFR")) %>% 
  dplyr::group_by(Category, ISO, Continent, Year) %>% 
  dplyr::summarise(Gg = sum (Gg)) %>% 
  dplyr::filter(ISO %!in% c("ESP",
                            "ROW"))
  
trade_regions_flow <- NULL

for(region in unique(trade_regions$ISO)){
  
  trade_regions_flow <- trade_regions_flow %>% 
    dplyr::bind_rows(
      trade_regions %>% 
        dplyr::filter(
          ISO == region | Continent == region
        ) %>% 
        dplyr::mutate(Flow = ifelse(ISO == Continent,
                                                         "Domestic production",
                                    ifelse(ISO == region,
                                   "Imports",
                                   ifelse(ISO != region,
                                          "Exports"))))
    )  
  
}

trade_regions_graph <- trade_regions_flow %>% 
  dplyr::filter(Flow != "Exports") %>% 
  dplyr::mutate(Approach = "Consumption footprint") %>% 
  dplyr::bind_rows(
    trade_regions_flow %>% 
      dplyr::filter(Flow != "Imports") %>% 
      dplyr::mutate(Approach = "Territorial")
  ) %>% 
  dplyr::group_by(Category,
                  ISO,
                  Approach,
                  Year) %>% 
  dplyr::summarise(Gg = sum(Gg)) %>% 
  dplyr::ungroup()

population <- read_csv("data/Data_Extract_From_World_Development_Indicators/a999f80e-8914-49fe-a0a1-a6c6a8b5f31c_Data.csv") %>% 
  dplyr::select(-`Series Name`,
                -`Series Code`,
                -`Country Name`)

colnames(population) <- c("ISO", c(1986:2015))

population_agg <- population %>% 
  tidyr::gather(Year, Pop, -ISO) %>% 
  dplyr::left_join(countries %>% 
                     dplyr::select(ISO,
                                   Continent)) %>% 
  dplyr::filter(!is.na(Pop),
                !is.na(Continent),
                Pop != "..") %>% 
  dplyr::group_by(Continent,
                  Year) %>% 
  dplyr::mutate(Pop = as.numeric(Pop)) %>% 
  dplyr::summarise(Pop = sum(Pop)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(Year = as.integer(Year)) %>% 
  dplyr::bind_rows(
    population %>% 
      tidyr::gather(Year, Pop, -ISO
  ) %>% 
    dplyr::filter(
      ISO == "ESP"
    ) %>% 
    dplyr::mutate(Year = as.integer(Year),
                  Pop = as.numeric(Pop)) %>% 
    dplyr::rename(Continent = ISO)
  )

trade_data <- trade_regions_graph %>% 
  dplyr::bind_rows(
    trade_spain_graph
  ) %>% 
  dplyr::mutate(Agg = ifelse(Category != "Land Use Change",
                             "Agricultural emissions",
                             Category)) %>% 
  dplyr::group_by(Agg,
                  ISO,
                  Approach,
                  Year) %>% 
  dplyr::summarise(Gg = sum(Gg)) %>% 
  dplyr::left_join(
    population_agg,
    by = c("ISO" = "Continent",
           "Year")
  ) %>% 
  dplyr::mutate(t_cap = (Gg*1000)/Pop,
                Year = as.numeric(Year)) %>% 
  dplyr::select(-Pop, -Gg) %>% 
dplyr::ungroup()

trade_data_ag <- trade_data %>% 
  dplyr::bind_rows(
    trade_data %>% 
      dplyr::mutate(Agg = "Total") %>% 
    dplyr::group_by(Agg,
                    ISO,
                    Approach,
                    Year) %>% 
      dplyr::summarise(t_cap = sum(t_cap))
  )

t1 <- trade_data %>% 
  filter(Agg == "Agricultural emissions",
         ISO == "ESP") %>% 
  tidyr::spread(Approach, t_cap)

t2 <- trade_data %>% 
  filter(Agg == "Land Use Change",
         ISO == "ESP") %>% 
  tidyr::spread(Approach, t_cap)

t3 <- trade_data %>% 
  filter(Agg %in% c("Agricultural emissions",
                    "Land Use Change"),
         ISO == "ESP") %>% 
  dplyr::mutate(Agg = "Total") %>% 
  dplyr::group_by(Agg, ISO, Year, Approach) %>% 
  dplyr::summarise(t_cap = sum(t_cap)) %>% 
  tidyr::spread(Approach, t_cap) %>% 
  dplyr::ungroup()

p <- ggplot(data = trade_data_ag %>% 
              filter(ISO == "ESP")) +
  geom_line(data = trade_data_ag %>% 
              filter(ISO == "ESP"),
            aes(x = Year, y = t_cap, linetype = Approach),
            size=2) +
  geom_ribbon(data = t1 , 
              aes(x = Year, ymin = Territorial,
                  ymax =`Consumption footprint`, fill ="Net Imports"), alpha = .5, colour = NA) +
  geom_ribbon(data = t2, 
              aes(x = Year, ymin = Territorial,
                  ymax =`Consumption footprint`, fill = "Net Imports"), alpha = .5, colour = NA) +
  geom_ribbon(data = t3 , 
              aes(x = Year, ymin = Territorial,
                  ymax =`Consumption footprint`, fill = "Net Imports"), alpha = .5, colour = NA) +
  theme(
    #panel.background = element_rect(fill = NA),
    #panel.grid.minor = element_blank(),
    #panel.grid.major = element_blank(),
    #panel.background = element_blank(),
    #axis.line = element_line(),
    panel.border=element_rect(colour="black", fill = NA),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_blank(),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 18, face = "bold"),
    #strip.background = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.margin = unit(c(1.5, 1, 1, 1), "cm"),
    legend.key = element_rect(colour = NA, fill = NA),
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    legend.position = "bottom",
    #legend.position = "bottom"
  ) +
  scale_fill_manual(values = (RColorBrewer::brewer.pal(3,'Accent'))) +
  facet_wrap(~Agg, #ncol = 10, 
             #scales = "free_y"
  ) +
  labs(y = "tonnes CO2e./cap/yr")
  
plot(p)

ggsave( file = "cf vs t.png", path="~/food_footprint/Graphs/",
        plot = p,
        width = 34,
        height = 17,
        #height = 40, 
        units = "cm")

sp_imports_region <- results_fabio_trade %>% 
  dplyr::filter(
    ISO == "ESP" & Origin %!in% c("ESP",
                                  "ES")
  ) %>% 
  dplyr::left_join(countries %>% 
                     dplyr::select(ISO, Continent),
                   by = c("Origin" = "ISO")) %>% 
  dplyr::mutate(Continent = replace(Continent,
                                    Origin %in% exio_eu,
                                    "EU"),
                Continent = replace(Continent,
                                    Origin %in% exio_nam,
                                    "NAM"),
                Continent = replace(Continent,
                                    Origin %in% exio_asi,
                                    "ASI"),
                Continent = replace(Continent,
                                    Origin %in% exio_lam,
                                    "LAM"),
                Continent = replace(Continent,
                                    Origin %in% exio_oce,
                                    "OCE"),
                Continent = replace(Continent,
                                    Origin %in% exio_eur,
                                    "EUR"),
                Continent = replace(Continent,
                                    Origin %in% exio_afr,
                                    "AFR"),
                Continent = ifelse(Continent %in% c("NAM",
                                                    "OCE"),
                                   "NAM & OCE",
                                   Continent)) %>% 
  dplyr::group_by(
    Category,
    Year,
    Continent
  ) %>% 
  dplyr::summarise(Gg = sum(Gg)) %>% 
  dplyr::mutate(Flow = "Imports")
  
sp_exports_region <- results_fabio_trade %>% 
  dplyr::filter(
    ISO != "ESP" & Origin %in% c("ESP",
                                  "ES")
  ) %>%  
  dplyr::mutate(ISO = ifelse(ISO %in% c("NAM",
                                        "OCE"),
                                   "NAM & OCE",
                                   ISO)) %>% 
  dplyr::group_by(
    Category,
    Year,
    ISO
  ) %>% 
  dplyr::summarise(Gg = sum(Gg)) %>% 
  dplyr::mutate(Flow = "Exports") %>% 
  dplyr::left_join(
    dom_prod_sp %>%
      dplyr::ungroup() %>% 
      dplyr::select(
        Category,
        Year,
        Gg
      ) %>% 
      dplyr::mutate(ISO = "EU"),
    by = c("Category", "Year", "ISO")
  ) %>% 
  dplyr::mutate(Gg.y = ifelse(is.na(Gg.y),
                              0,
                              Gg.y),
                Gg = Gg.x-Gg.y) %>% 
  dplyr::select(-Gg.x, -Gg.y)
  
sp_trade_region <- sp_imports_region %>% 
  dplyr::rename(ISO = Continent) %>% 
  dplyr::bind_rows(
    sp_exports_region)  %>% 
  dplyr::filter(ISO != "ROW") %>% 
  tidyr::spread(Flow, Gg) %>% 
  dplyr::mutate(Balance = Exports-Imports) %>% 
  tidyr::gather(Flow, Gg, 4:6) %>% 
  dplyr::mutate(Agg = ifelse(Category != "Land Use Change",
                             "Agricultural emissions",
                             Category)) %>% 
  dplyr::group_by(Agg,
                  ISO,
                  Flow,
                  Year) %>% 
  dplyr::summarise(Gg = sum(Gg)) %>% 
  dplyr::mutate(Year = as.character(Year),
                Gg = ifelse(Flow == "Imports",
                            Gg*-1,
                            Gg))

labels <- c(
  "AFR" = "Africa",
  "ASI" = "Asia",
  "EU" = "European Union",
  "EUR" = "Rest of Europe",
  "LAM" = "Latin America",
  "NAM & OCE" = "North America &\nOceania"
)

p <- ggplot(data = sp_trade_region,
            aes(x = Year, y = Gg/1000)) +
  geom_bar(data = sp_trade_region %>% 
             dplyr::filter(
               Flow %in% c("Imports", 
                               "Exports")
             ),
           stat = "identity",
           aes(fill = Flow)#,
           #colour = "black"
  ) +
  geom_line(data =sp_trade_region %>% 
              dplyr::filter(
                Flow == "Balance"
              ),
            aes(group = Flow,
                colour = Flow
                #fill = Variable
            )) +
  theme(
    #panel.background = element_rect(fill = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    #axis.line = element_line(),
    panel.border=element_rect(colour="black", fill = NA),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_blank(),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14, face = "bold"),
    strip.background = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.margin = unit(c(1.5, 1, 1, 1), "cm"),
    legend.key = element_rect(colour = NA, fill = NA),
    legend.title = element_blank(),
    legend.text = element_text(size = 14)
    #legend.position = "bottom"
  ) +
  scale_colour_manual(values = c("black")) +
  scale_fill_manual(values = (RColorBrewer::brewer.pal(6,'Accent'))) +
  facet_grid (Agg~ISO, #ncol = 2#,
              labeller = labeller(ISO = labels)
  ) +
  scale_x_discrete(breaks = c(1998, 2003, 2008, 2013)) +
  #scale_y_continuous(labels = "comma") +
  labs(y = "Mt CO2e/yr") +
  guides(col = guide_legend(ncol = 1)) 

plot(p)  

ggsave( file = "balance region.png", path="~/food_footprint/Graphs/",
        plot = p,
        width = 34,
        height = 17,
        #height = 40, 
        units = "cm")

# animal products
results_fabio_product <- NULL

for(year in c(2013:2000)){
  
  print(year)
  
  t1 <- readRDS(paste0("~/food_footprint/data/", year,"_results_fabio_product.rds")) %>% 
    dplyr::group_by(
      Category, Product, Flow, Year
    ) %>% 
    dplyr::summarise(Gg = sum(Gg))
  
  results_fabio_product <- results_fabio_product %>% 
    dplyr::bind_rows(t1)
}

base::saveRDS(results_fabio_product, "~/food_footprint/data/results_fabio_product.rds")

meat_int_final <- results_fabio_product %>% 
  dplyr::filter(Category != "Non-agricultural emissions") %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(
    Product %in% c("Bovine Meat",
                   "Pigmeat",
                   "Poultry Meat",
                   "Mutton & Goat Meat",
                   "Meat, Other",
                   "Milk - Excluding Butter",
                   "Butter, Ghee",
                   "Eggs"), Gg != 0) %>% 
  tidyr::spread(Flow, 
                Gg) %>% 
  dplyr::mutate(Balance = Exports-Imports,
                Imports = Imports*-1,
                Year = as.character(Year)
  ) %>% 
  dplyr::arrange(-Balance) %>% 
  tidyr::gather(Flow, Gg, 4:6) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(Product = as.character(Product),
              Product = replace(Product,
                                  Product %in% c(
                                    "Mutton & Goat Meat",
                                    "Meat, Other"
                                  ),
                                  "Meat, Other"),
                Product = replace(Product,
                                 Product %in% c("Milk - Excluding Butter",
                                                "Butter, Ghee"),
                                 "Milk")) %>% 
  dplyr::group_by(Category,
                  Product,
                  Year,
                  Flow) %>% 
  dplyr::summarise(Gg = sum(Gg)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(Product = factor(Product, levels = c("Bovine Meat", 
                                                     "Pigmeat", 
                                                     "Poultry Meat", 
                                                     "Mutton & Goat Meat",
                                                     "Meat, Other",
                                                     "Milk",
                                                     "Eggs")))

p <- ggplot(data = meat_int_final,
            aes(x = Year, y = Gg/1000)) +
  geom_bar(data = meat_int_final %>% 
             dplyr::filter(
               Flow %in% c("Imports", 
                           "Exports")),
           stat = "identity",
           position="stack",
           aes(fill = Flow
           )#,
           #colour = "black"
  ) +
  geom_line(data = meat_int_final %>% 
              dplyr::filter(
                Flow == "Balance"
              ),
            aes(group = Product,
                colour = Flow#,
                #fill = Flow
            )) +
  theme(
    #panel.background = element_rect(fill = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    #axis.line = element_line(),
    panel.border=element_rect(colour="black", fill = NA),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_blank(),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14, face = "bold"),
    strip.background = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.margin = unit(c(1.5, 1, 1, 1), "cm"),
    legend.key = element_rect(colour = NA, fill = NA),
    legend.title = element_blank(),
    legend.text = element_text(size = 14)
    #legend.position = "bottom"
  ) +
  #geom_segment(aes(x = 0, y = 0, xend = as.character(2013.25), yend = 0)) +
  scale_colour_manual(values = c("black")) +
  scale_fill_manual(values = (RColorBrewer::brewer.pal(6,'Accent'))) +
  facet_grid(Category~Product, #ncol = 10, 
             #scales = "free_y"
  ) +
  scale_x_discrete(breaks = c(2001,2006, 2011)) +
  #scale_y_continuous(labels = "comma") +
  labs(y = "Mt CO2e/yr") #+
#guides(col = guide_legend(ncol = 1)) 

plot(p)  

ggsave( file = "balance animal.png", path="~/food_footprint/Graphs/",
        plot = p,
        width = 34,
        height = 17,
        #height = 40, 
        units = "cm")

othr_final <- results_fabio_product %>% 
  dplyr::filter(Category != "Non-agricultural emissions") %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(
    Product %in% c("Olive Oil",
                   #"Alcohol, Non-Food",
                   "Rice (Milled Equivalent)",
                   "Wheat and products",
                   "Maize and products",
                   "Wine") | (Product %in% (products %>% 
                                select(FAO.Name,
                                       Com.Group) %>% 
                                filter((Com.Group %in% c("Vegetables, fruit, nuts, pulses, spices"))) %>% 
                                  pull(FAO.Name))) &&(Product != "Pineapples and products")) %>% 
  tidyr::spread(Flow, 
                Gg) %>% 
  dplyr::mutate(Balance = Exports-Imports,
                Imports = Imports*-1,
                Year = as.character(Year)
  ) %>% 
  dplyr::arrange(-Balance) %>% 
  tidyr::gather(Flow, Gg, 4:6) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(Product = as.character(Product),
                Product = replace(Product,
                                  Product %in% (products %>% 
                                                  select(FAO.Name,
                                                         Com.Group) %>% 
                                                  filter(Com.Group %in% c("Vegetables, fruit, nuts, pulses, spices")) %>% 
                                                  pull(FAO.Name)),
                                  "Vegetables\n& Fruits"),
                Product = replace(Product,
                                  Product %in% c("Rice (Milled Equivalent)"),
                                  "Rice"),
                Product = replace(Product,
                                  Product %in% c("Maize and products"),
                                  "Maize and\nproducts"),
                Product = replace(Product,
                                  Product %in% c("Wheat and products"),
                                  "Wheat and\nproducts")) %>% 
  dplyr::group_by(Category,
                  Product,
                  Year,
                  Flow) %>% 
  dplyr::summarise(Gg = sum(Gg)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(Product = factor(Product, levels = c(
                                                     "Rice",
                                                     "Wheat and\nproducts",
                                                     "Maize and\nproducts",
                                                     "Vegetables\n& Fruits",
                                                     "Olive Oil",
                                                     #"Alcohol, Non-Food",
                                                     "Wine")))
p <- ggplot(data = othr_final,
            aes(x = Year, y = Gg/1000)) +
  geom_bar(data = othr_final %>% 
             dplyr::filter(
               Flow %in% c("Imports", 
                           "Exports")),
           stat = "identity",
           position="stack",
           aes(fill = Flow
           )
  ) +
  geom_line(data = othr_final%>% 
              dplyr::filter(
                Flow == "Balance"
              ),
            aes(group = Product,
                colour = Flow
            )) +
  theme(
    #panel.background = element_rect(fill = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    #axis.line = element_line(),
    panel.border=element_rect(colour="black", fill = NA),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_blank(),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14, face = "bold"),
    strip.background = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.margin = unit(c(1.5, 1, 1, 1), "cm"),
    legend.key = element_rect(colour = NA, fill = NA),
    legend.title = element_blank(),
    legend.text = element_text(size = 14)
    #legend.position = "bottom"
  ) +
  #geom_segment(aes(x = 0, y = 0, xend = as.character(2013.25), yend = 0)) +
  scale_colour_manual(values = c("black")) +
  scale_fill_manual(values = (RColorBrewer::brewer.pal(6,'Accent'))) +
  facet_grid(Category~Product, #ncol = 10, 
             #scales = "free_y"
  ) +
  scale_x_discrete(breaks = c(2001,2006, 2011)) +
  #scale_y_continuous(labels = "comma") +
  labs(y = "Mt CO2e/yr") +
  ylim(-10, 5)
#guides(col = guide_legend(ncol = 1)) 

plot(p)  

ggsave( file = "balance oth.png", path="~/food_footprint/Graphs/",
        plot = p,
        width = 34,
        height = 17,
        #height = 40, 
        units = "cm")

t1 <- results_fabio_product %>% 
  dplyr::filter(Category != "Non-agricultural emissions") %>% 
  dplyr::ungroup() %>% 
  tidyr::spread(Flow, 
                Gg) %>% 
  dplyr::mutate(Balance = Exports-Imports,
                Imports = Imports*-1,
                Year = as.character(Year)
  ) %>% 
  dplyr::arrange(-Balance)

dplyr::filter(
    ISO == "ESP" & Origin %!in% c("ESP",
                                  "ES")
  ) %>% 
  dplyr::left_join(
    products %>% 
      dplyr::select(FAO.Name,
                    Com.Group),
    by = c("Product" = "FAO.Name")
  ) %>% 
  dplyr::filter(Gg != 0) %>% 
  dplyr::group_by(
    Com.Group,
    Category,
    Year
  ) %>% 
  dplyr::summarise_if(is.numeric,  sum) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(Flow = "Imports")

p <- ggplot(data = meat_int_final,
            aes(x = Year, y = Gg/1000)) +
  geom_bar(data = meat_int_final %>% 
             dplyr::filter(
               Flow %in% c("Imports", 
                           "Exports")),
           stat = "identity",
           position="stack",
           aes(fill = Flow
                 #Product,
           )#,
           #colour = "black"
  ) +

  theme(
    #panel.background = element_rect(fill = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    #axis.line = element_line(),
    panel.border=element_rect(colour="black", fill = NA),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_blank(),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14, face = "bold"),
    strip.background = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.margin = unit(c(1.5, 1, 1, 1), "cm"),
    legend.key = element_rect(colour = NA, fill = NA),
    legend.title = element_blank(),
    legend.text = element_text(size = 14)
    #legend.position = "bottom"
  ) +
  #geom_segment(aes(x = 0, y = 0, xend = as.character(2013.25), yend = 0)) +
  scale_colour_manual(values = c("black")) +
  scale_fill_manual(values = (RColorBrewer::brewer.pal(6,'Accent'))) +
  facet_wrap(~Com.Group, #ncol = 10, 
             #scales = "free_y"
  ) +
  scale_x_discrete(breaks = c(2001,2006, 2011)) +
  #scale_y_continuous(labels = "comma") +
  labs(y = "Mt CO2e./año") #+
#guides(col = guide_legend(ncol = 1)) 

plot(p)  














geom_bar(data = meat_int_final %>% 
             dplyr::filter(
               Flow %in% c("Imports", 
                           "Exports")),
           stat = "identity",
           position="stack",
           aes(fill = #Flow
                 Product,
           )#,
           #colour = "black"
  ) +
  geom_line(data = meat_int_final %>% 
              dplyr::filter(
                Flow == "Balance"
              )%>% 
              dplyr::group_by(
                Flow,
                `Product type`,
                Year) %>% 
              dplyr::summarise(Gg = sum(Gg)),
            aes(group = `Product type`,
                colour = Flow#,
                #fill = Flow
            )) +
  theme(
    #panel.background = element_rect(fill = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    #axis.line = element_line(),
    panel.border=element_rect(colour="black", fill = NA),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_blank(),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14, face = "bold"),
    strip.background = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.margin = unit(c(1.5, 1, 1, 1), "cm"),
    legend.key = element_rect(colour = NA, fill = NA),
    legend.title = element_blank(),
    legend.text = element_text(size = 14)
    #legend.position = "bottom"
  ) +
  geom_segment(aes(x = 0, y = 0, xend = as.character(2013.25), yend = 0)) +
  scale_colour_manual(values = c("black")) +
  scale_fill_manual(values = (RColorBrewer::brewer.pal(6,'Accent'))) +
  facet_wrap(~`Product type`, #ncol = 10, 
             #scales = "free_y"
  ) +
  scale_x_discrete(breaks = c(2001,2006, 2011)) +
  #scale_y_continuous(labels = "comma") +
  labs(y = "Mt CO2e./año") #+
#guides(col = guide_legend(ncol = 1)) 

plot(p)  


dplyr::filter(Origin %in% c("ESP",
                              "ES") & ISO == "ESP") %>% 
  dplyr::mutate(Agg = "Domestic production") %>% 
  dplyr::group_by(Agg, Category, Year) %>% 
  dplyr::summarise(Gg = sum(Gg))

imports_sp <- results_fabio_trade %>% 
  dplyr::filter(ISO == "ESP" & Origin %!in% c("ESP",
                                              "ES")) %>% 
  dplyr::mutate(Agg = "Imports") %>% 
  dplyr::group_by(Agg, Category, Year) %>% 
  dplyr::summarise(Gg = sum(Gg))

exports_sp <- results_fabio_trade %>% 
  dplyr::filter(ISO != "ESP" & Origin %in% c("ESP",
                                             "ES")) %>% 
  dplyr::mutate(Agg = "Exports") %>% 
  dplyr::group_by(Agg, Category, Year) %>% 
  dplyr::summarise(Gg = sum(Gg))

#
t1 <-results_fabio_trade %>% 
  dplyr::filter(Origin %in% c("ESP",
                              "ES") & ISO == "ESP") %>%  
  filter(Year == 2013,
         Category == "Agricultural emissions")

t2 <- results_fabio %>% 
  dplyr::filter(Year == 2013,
                Category == "Agricultural emissions") %>% 
  dplyr::filter(substr(Product, 1, 3) == "ESP")


t3 <- results_fabio_trade %>% 
  dplyr::filter(ISO == "ESP" & Origin %!in% c("ESP",
                                              "ES"),
                Year == 2013,
                Category == "Agricultural emissions")

t4 <- results_fabio %>% 
  dplyr::filter(Year == 2013,
                Category == "Agricultural emissions") %>% 
  dplyr::filter(substr(Product, 1, 3) != "ESP")

t5 <- results_fabio_trade %>% 
  dplyr::filter(ISO == "EU",
    #ISO != "ESP", #& Origin %in% c("ESP",
                             #                 "ES"),
                Year == 2013,
                Category == "Agricultural emissions")

sum(t1$Gg) + sum(t3$Gg)
sum(t2$Gg) + sum(t4$Gg)
sum(t5$Gg)

t1 <- results_fabio_trade %>% 
  dplyr::filter(Origin %in% c("ESP",
                              "ES") | ISO == "ESP") %>% 
  dplyr::mutate(Agg = ifelse(ISO ==  "ESP" & Origin %in% c("ESP",
                                                           "ES"),
                             "Domestic production",
                             ifelse(ISO != "ESP",
                                    "Exports",
                                    "Abroad"))) %>% 
  dplyr::group_by(Agg, Category, Year) %>% 
  dplyr::summarise(Gg = sum(Gg))

t2 <-results_fabio_trade %>% 
  dplyr::filter(Category == "Non-agricultural inputs")

data_plot  <- footprint_product_diss_no_outliers %>% 
  dplyr::filter(Flow %in% c("Imports",
                            "Exports")) %>% 
  tidyr::spread(Flow, 
                Gg) %>% 
  dplyr::mutate(Balance = Imports-Exports,
                Exports = Exports*-1,
                Year = as.character(Year)
                ) %>% 
  dplyr::arrange(-Balance) %>% 
  tidyr::gather(Flow, Gg, 9:11)
  
# PLot intermediate & Final


meat_int_final <- data_plot %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(
    Product %in% c("Bovine Meat",
                   "Pigmeat",
                   "Poultry Meat",
                   "Mutton & Goat Meat",
                   "Meat, Other"), Gg != 0) %>% 
  dplyr::mutate(Product = case_when(Product == "Bovine Meat" ~ "Ternera",
                                Product == "Pigmeat"  ~ "Cerdo",
                                Product == "Poultry Meat" ~ "Pollo",
                                Product == "Mutton & Goat Meat" ~ "Otras carnes",
                                Product == "Meat, Other" ~ "Otras carnes"),
                `Product type` = case_when(`Product type` == "Final" ~ "Comercio de productos\nde consumo final",
                                           `Product type` == "Intermediate" ~ "Comercio de productos\n primarios o intermedios")) %>% 
  dplyr::group_by(
    Product,
    Flow,
    `Product type`,
    Year
  ) %>% 
  dplyr::summarise_if(is.numeric,  sum) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(Product = factor(Product, levels = c("Ternera", "Cerdo", "Pollo", "Otras carnes")))


p <- ggplot(data = meat_int_final,
            aes(x = Year, y = Gg/1000)) +
  geom_bar(data = meat_int_final %>% 
             dplyr::filter(
               Flow %in% c("Imports", 
                           "Exports")),
           stat = "identity",
           position="stack",
           aes(fill = #Flow
                 Product,
           )#,
           #colour = "black"
  ) +
  geom_line(data = meat_int_final %>% 
              dplyr::filter(
                Flow == "Balance"
              )%>% 
              dplyr::group_by(
                Flow,
                `Product type`,
                Year) %>% 
              dplyr::summarise(Gg = sum(Gg)),
            aes(group = `Product type`,
                colour = Flow#,
                #fill = Flow
            )) +
  theme(
    #panel.background = element_rect(fill = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    #axis.line = element_line(),
    panel.border=element_rect(colour="black", fill = NA),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_blank(),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14, face = "bold"),
    strip.background = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.margin = unit(c(1.5, 1, 1, 1), "cm"),
    legend.key = element_rect(colour = NA, fill = NA),
    legend.title = element_blank(),
    legend.text = element_text(size = 14)
    #legend.position = "bottom"
  ) +
  geom_segment(aes(x = 0, y = 0, xend = as.character(2013.25), yend = 0)) +
  scale_colour_manual(values = c("black")) +
  scale_fill_manual(values = (RColorBrewer::brewer.pal(6,'Accent'))) +
  facet_wrap(~`Product type`, #ncol = 10, 
             #scales = "free_y"
  ) +
  scale_x_discrete(breaks = c(2001,2006, 2011)) +
  #scale_y_continuous(labels = "comma") +
  labs(y = "Mt CO2e./año") #+
#guides(col = guide_legend(ncol = 1)) 

plot(p)  

ggsave( file = "FigIntFin.png", path="~/Food/",
        plot = p,
        width = 34,
        height = 17,
        #height = 40, 
        units = "cm")

data_plot_meat <- data_plot %>% 
  data.table::setDF() %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(
    Product %in% c("Bovine Meat",
                   "Pigmeat",
                   "Poultry Meat"),
    Gg != 0
    ) %>% 
  dplyr::left_join(products %>% 
                     dplyr::select(FAO.Name,
                                   Com.Group),
                   by = c("Input" = "FAO.Name")) %>% 
  dplyr::mutate(Key = case_when(Input == "Cattle" ~ "Vacuno",
                                Input == "Pigs"  ~ "Porcino",
                                Input == "Maize and products" ~ "Maíz",
                                Input == "Fodder crops" ~ "Forrajes",
                                Input == "Soyabeans" ~ "Soja")) %>% 
  dplyr::mutate(Key = ifelse(is.na(Key), Com.Group, Key)) %>% 
  dplyr::group_by(
    Product,
    Key,
    Flow,
    Year
  ) %>% 
  dplyr::summarise_if(is.numeric,  sum)
  
  

  dplyr::mutate(
    Key2 = case_when(Key == "Vegetables, fruit, nuts, pulses, spices"  ~ "Frutas y verduras",
                     Key == "Roots and tubers"  ~ "Tubérculos",
                     Key == "Cereals"  ~ "Otros cereales",
                     Key == "Live animals"  ~ "Animales, otros",
                     Key == "Oil crops"  ~ "Cultivos\n oleaginosos",
                     Key == "Sugar crops"  ~ "Cultivos\n azucareros",
                     Key == "Milk"  ~ "Leche",
                     Key == "Eggs"  ~ "Huevos"),
    Key2 = replace(Key2, is.na(Key2), Key)
  ) %>% 
  dplyr::group_by(
    Product,
    Key2,
    Flow,
    #`Product type`,
    Year
  ) %>% 
  dplyr::summarise_if(is.numeric,  sum)

p <- ggplot(data = data_plot_meat,
            aes(x = Year, y = Gg/1000)) +
  geom_bar(data = data_plot_meat %>% 
             dplyr::filter(
               Flow %in% c("Imports", 
                           "Exports")),
           stat = "identity",
           position="stack",
           aes(fill = #Flow
                 Key,
           )#,
           #colour = "black"
  ) +
  geom_line(data = data_plot_meat %>% 
              dplyr::filter(
                Flow == "Balance"
              )%>% 
              dplyr::group_by(
                Flow,
                Product,
                #`Product type`,
                Year) %>% 
              dplyr::summarise(Gg = sum(Gg)),
            aes(group = Product,
                colour = Flow#,
                #fill = Flow
            )) +
  theme(
    #panel.background = element_rect(fill = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    #axis.line = element_line(),
    panel.border=element_rect(colour="black", fill = NA),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_blank(),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14, face = "bold"),
    strip.background = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.margin = unit(c(1.5, 1, 1, 1), "cm"),
    legend.key = element_rect(colour = NA, fill = NA),
    legend.title = element_blank(),
    legend.text = element_text(size = 14)
    #legend.position = "bottom"
  ) +
  geom_segment(aes(x = 0, y = 0, xend = as.character(2013.25), yend = 0)) +
  scale_colour_manual(values = c("black")) +
  #scale_fill_manual(values = (RColorBrewer::brewer.pal(12,'Accent'))) +
  facet_wrap(~Product, #ncol = 10, 
             #scales = "free_y"
  ) +
  scale_x_discrete(breaks = c(2001,2006, 2011)) +
  #scale_y_continuous(labels = "comma") +
  labs(y = "Mt CO2e./año") #+
#guides(col = guide_legend(ncol = 1)) 

plot(p) 

ggsave( file = "Animal.png", path="~/Food/",
        plot = p,
        width = 34,
        height = 17,
        #height = 40, 
        units = "cm")


load(paste0("data/Exports_ESP.RData"))
load(paste0("data/Imports_ESP.RData"))

exports_spain_agg <- exports_footprint_no_outliers %>%
  bind_rows(exports_footprint_LUC_no_outliers) %>% 
  filter(Year %in% c(2001:2013)) %>% 
  dplyr::rename(Continent = ISO) %>% 
  dplyr::group_by(
    Continent,
    Year
  ) %>% 
  dplyr::summarise(Exportaciones = sum(Gg)*-1)

imports_spain_agg <- imports_footprint_no_outliers %>% 
  bind_rows(imports_footprint_LUC_no_outliers) %>% 
  filter(Year %in%c(2001:2013)) %>% 
  dplyr::filter(ISO %!in% c("ESP", "ROW")) %>%
  dplyr::left_join(
    countries %>% 
      dplyr::select(ISO,
                    Continent),
    by = c("ISO")
  ) %>% 
  dplyr::group_by(
    Continent,
    Year
  ) %>% 
  dplyr::summarise(Importaciones = sum(Gg)) %>% 
  dplyr::filter(Continent != "ROW")

balance_trade <- exports_spain_agg %>% 
  dplyr::left_join(
    imports_spain_agg
  ) %>%
  dplyr::mutate(Balance = Importaciones + Exportaciones) %>% 
  tidyr::gather(
    "Variable",
    "Gg",
    3:5
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(Year = as.character(Year))

labels <- c(
  "AFR" = "África",
  "ASI" = "Asia",
  "EU" = "Unión Europea",
  "EUR" = "Resto\n de Europa",
  "LAM" = "Latinoamérica",
  "NAM" = "Norteamérica",
  "OCE" = "Oceanía",
  "ROW" = "Resto del mundo"
)

p <- ggplot(data = balance_trade,
            aes(x = Year, y = Gg/1000)) +
  geom_bar(data = balance_trade %>% 
             dplyr::filter(
               Variable %in% c("Importaciones", 
                               "Exportaciones")
             ),
           stat = "identity",
           aes(fill = Variable)#,
           #colour = "black"
  ) +
  geom_line(data = balance_trade %>% 
              dplyr::filter(
                Variable == "Balance"
              ),
            aes(group = Variable,
                colour = Variable
                #fill = Variable
            )) +
  theme(
    #panel.background = element_rect(fill = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    #axis.line = element_line(),
    panel.border=element_rect(colour="black", fill = NA),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_blank(),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14, face = "bold"),
    strip.background = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.margin = unit(c(1.5, 1, 1, 1), "cm"),
    legend.key = element_rect(colour = NA, fill = NA),
    legend.title = element_blank(),
    legend.text = element_text(size = 14)
    #legend.position = "bottom"
  ) +
  scale_colour_manual(values = c("black")) +
  scale_fill_manual(values = (RColorBrewer::brewer.pal(6,'Accent'))) +
  facet_grid (.~`Continent`, #ncol = 2#,
              labeller = labeller(Continent = labels)
  ) +
  scale_x_discrete(breaks = c(2001,2006, 2011)) +
  #scale_y_continuous(labels = "comma") +
  labs(y = "Mt CO2e./año") +
  guides(col = guide_legend(ncol = 1)) 

plot(p)  

ggsave( file = "FigTrade.pdf", path="~/Food/",
        plot = p,
        width = 34,
        height = 17,
        #height = 40, 
        units = "cm")

trade_spain_product <- imports_footprint_no_outliers %>% 
  bind_rows(imports_footprint_LUC_no_outliers) %>% 
  filter(Year %in%c(2001:2013)) %>% 
  dplyr::filter(ISO %!in%c("ESP", "ROW")) %>%
  dplyr::group_by(
    Name,
    Year
  ) %>% 
  dplyr::summarise(Importaciones = sum(Gg)) %>% 
  dplyr::left_join(
    exports_footprint_no_outliers %>%
      bind_rows(exports_footprint_LUC_no_outliers) %>% 
      filter(Year %in%c(2001:2013)) %>% 
      dplyr::group_by(
        Name,
        Year
      ) %>% 
      dplyr::summarise(Exportaciones = sum(Gg)*-1)
  ) %>% 
  dplyr::mutate(Trade = Importaciones + (Exportaciones*-1),
                Balance = Exportaciones + Importaciones) %>% 
  dplyr::arrange(-Trade) %>%
  dplyr::left_join(products %>% 
                     dplyr::select(FAO.Name,
                                   Com.Group),
                   by = c("Name" = "FAO.Name")) %>% 
  dplyr::mutate(Key = case_when(Name == "Cattle" ~ "Vacuno",
                                Name == "Pigs"  ~ "Porcino",
                                Name == "Sheep" ~ "Ovino",
                                Name == "Barley and products" ~ "Cebada",
                                Name == "Wheat and products" ~ "Trigo",
                                Name == "Rice (Milled Equivalent)" ~ "Arroz",
                                Name == "Olives (including preserved)" ~ "Aceituna",
                                Name == "Maize and products" ~ "Maíz",
                                Name == "Fodder crops" ~ "Forrajes",
                                Name == "Soyabeans" ~ "Soja",
                                Name == "Vegetables, fruit, nuts, pulses, spices"  ~ "Frutas y verduras",
                                Name == "Roots and tubers"  ~ "Tubérculos",
                                Name == "Cereals"  ~ "Otros cereales"),
                Key = replace(Key, is.na(Key), Com.Group)) %>% 
  dplyr::mutate(
    Key2 = case_when(Key == "Vegetables, fruit, nuts, pulses, spices"  ~ "Frutas y verduras",
                     Key == "Roots and tubers"  ~ "Tubérculos",
                     Key == "Cereals"  ~ "Otros cereales",
                     Key == "Live animals"  ~ "Animales, otros",
                     Key == "Oil crops"  ~ "Cultivos\n oleaginosos",
                     Key == "Sugar crops"  ~ "Cultivos\n azucareros",
                     Key == "Milk"  ~ "Leche",
                     Key == "Eggs"  ~ "Huevos"),
    Key2 = replace(Key2, is.na(Key2), Key)
  ) %>% 
  dplyr::group_by(
    Key2,
    Year
  ) %>% 
  dplyr::summarise_if(is.numeric,  sum) %>% 
  dplyr::filter(Trade != 0 & Key2 != "Wood" &Key2 != "Fish" &Key2 != "Fibre crops" & Key2 != "Coffee, tea, cocoa")

balance_trade_product <- trade_spain_product %>% 
  tidyr::gather(
    "Variable",
    "Gg",
    3:6
  ) %>% 
  dplyr::mutate(Year = as.character(Year)#,
                #Variable = factor(Variable)
  ) 

balance_trade_animal <- balance_trade_product %>% 
  dplyr::filter(
    Key2 %in% c("Vacuno",
                "Porcino",
                "Ovino",
                "Animales, otros",
                "Leche",
                "Huevos")
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(Key2 = factor(Key2, levels = c("Vacuno",
                                               "Porcino",
                                               "Ovino",
                                               "Animales, otros",
                                               "Leche",
                                               "Huevos")))

balance_other <- balance_trade_product %>% 
  dplyr::filter(
    Key2 %in% c("Cebada",
                "Trigo",
                "Arroz",
                "Maíz",
                "Otros cereales",
                "Forrajes")
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(Key2 = factor(Key2, levels = c("Cebada",
                                               "Trigo",
                                               "Arroz",
                                               "Maíz",
                                               "Otros cereales",
                                               "Forrajes")))

balance_other2 <- balance_trade_product %>% 
  dplyr::filter(
    Key2 %in% c("Frutas y verduras",
                "Aceituna",
                "Soja",
                "Cultivos\n oleaginosos",
                "Tubérculos",
                "Cultivos\n azucareros")
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(Key2 = factor(Key2, levels = c("Frutas y verduras",
                                               "Aceituna",
                                               "Soja",
                                               "Cultivos\n oleaginosos",
                                               "Tubérculos",
                                               "Cultivos\n azucareros")))



plot_product <- function(data_input){
  ggplot(data = data_input,
         aes(x = Year, y = Gg/1000)) +
    geom_bar(data = data_input %>% 
               dplyr::filter(
                 Variable %in% c("Importaciones", 
                                 "Exportaciones")
               ),
             stat = "identity",
             aes(fill = Variable)
    ) +
    geom_line(data = data_input %>% 
                dplyr::filter(
                  Variable == "Balance"
                ),
              aes(group = Variable,
                  colour = Variable
                  #fill = Variable
              )) +
    theme(
      #panel.background = element_rect(fill = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.background = element_blank(),
      #axis.line = element_line(),
      panel.border=element_rect(colour="black", fill = NA),
      axis.title.y = element_text(size = 16),
      axis.title.x = element_blank(),
      axis.text = element_text(size = 12),
      strip.text = element_text(size = 14, face = "bold"),
      strip.background = element_blank(),
      plot.title = element_text(hjust = 0.5),
      plot.margin = unit(c(1.5, 1, 1, 1), "cm"),
      legend.key = element_rect(colour = NA, fill = NA),
      legend.title = element_blank(),
      legend.text = element_text(size = 14)
      #legend.position = "bottom"
    ) +
    scale_colour_manual(values = c("black")) +
    #scale_y_continuous(limits = c(-5.5,
    #                            1.25))+
    scale_fill_manual(values = rev(RColorBrewer::brewer.pal(4,
      #6,
      'Accent'))) +
    facet_grid (.~`Key2`) +
    scale_x_discrete(breaks = c(2001, 2006, 2011)) +
    labs(y = "Mt CO2eq./año") +
    guides(col = guide_legend(ncol = 1)) 
}

p <- plot_product(balance_trade_animal) 

p1 <- plot_product(balance_other) 

p2 <- plot_product(balance_other2) 

plot(p)

library(ggpubr)
figure <- ggarrange(p1, p2,
                    ncol= 1, nrow = 3,
                    legend = "right",
                    #labels = c("A)", "B)"),
                    #font.label = list(size = 18),
                    common.legend = T)

ggsave( file = "FigTradeAnimal.pdf", path="~/Food/",
        plot = p,
        width = 34,
        height = 17,
        #height = 40, 
        units = "cm")

ggsave( file = "FigTradeProduct.pdf", path="~/Food/",
        plot = figure,
        width = 34,
        height = 34,
        #height = 40, 
        units = "cm")


table_cattle <- imports_footprint_no_outliers %>% 
  filter(ISO %in% (countries %>% 
           filter(Continent == "EU") %>% 
           pull(ISO))) %>% 
  spread(Year, Gg) %>% 
  filter(Name == "Cattle")

table_cattle_LUC <- imports_footprint_LUC_no_outliers %>% 
  spread(Year, Gg) %>% 
  filter(Name == "Cattle")

table_cattle_X <- exports_footprint_no_outliers %>% 
  spread(Year, Gg) %>% 
  filter(Name == "Cattle")

m <- footprint %>% 
  filter(Name == "Cattle")
  spread(Year, Gg) 


library(readODS)

write_ods(d6,
          "/home/vant/SCP-HAT project/Other/To_Excel.ods"
)




# Population data
population <- read_csv("data/Data_Extract_From_World_Development_Indicators/a999f80e-8914-49fe-a0a1-a6c6a8b5f31c_Data.csv") %>% 
  dplyr::select(-`Series Name`,
                -`Series Code`,
                -`Country Name`)

colnames(population) <- c("ISO", c(1986:2015))

# Preparing Excel with Results
load(paste0("data/Results_No_Outliers.RData"))
load(paste0("data/Results_Territorial.RData"))

population_sp <- population %>%
  dplyr::filter(ISO  == "ESP") %>% 
  tidyr::gather("Year", "Population", 2:31) %>% 
  dplyr::mutate(Year = as.integer(Year), 
                Population = as.numeric(Population))

emission_other_categories_agg <- other_footprint_no_outliers %>% 
  dplyr::bind_rows(stocks_footprint_no_outliers,
                   balancing_footprint_no_outliers) %>% 
  dplyr::group_by(Responsibility,
                  ISO,
                  Variable,
                  Year) %>% 
  dplyr::summarise(Gg = sum(Gg)) %>% 
  dplyr::left_join(
    population_sp
  ) %>% 
  dplyr::mutate(Capita = Gg*1000000/Population) %>% 
  dplyr::select(-Gg, -Population) %>% 
  tidyr::spread(Year, Capita)

emission_products <- food_footprint_no_outliers %>%
  dplyr::ungroup() %>% 
  dplyr::bind_rows(territorial %>% 
                     dplyr::filter(ISO == "ESP")) %>% 
  dplyr::mutate(Name = substr(Product, 5, length(Product) )) %>% 
  dplyr::group_by(Responsibility,
                  ISO,
                  Name,
                  Year) %>% 
  dplyr::summarise(Gg = sum(Gg)) %>% 
  dplyr::left_join(
    population_sp
  ) %>% 
  dplyr::mutate(Capita = Gg*1000000/Population) %>% 
  dplyr::select(-Gg, -Population) %>% 
  tidyr::spread(Year, Capita) %>% 
  dplyr::arrange(
    Responsibility,
    match(Name, products$FAO.Name))

emission_group <- food_footprint_no_outliers %>%
  dplyr::ungroup() %>% 
  dplyr::bind_rows(territorial %>% 
                     dplyr::filter(ISO == "ESP")) %>% 
  dplyr::mutate(Name = substr(Product, 5, length(Product) )) %>% 
  dplyr::left_join(products %>% 
                     dplyr::select(FAO.Name,
                                   Com.Group),
                   by = c("Name" = "FAO.Name")) %>% 
  dplyr::group_by(Responsibility,
                  ISO,
                  Com.Group,
                  Year) %>% 
  dplyr::summarise(Gg = sum(Gg)) %>% 
  dplyr::left_join(
    population_sp
  ) %>% 
  dplyr::mutate(Capita = Gg*1000000/Population) %>% 
  dplyr::select(-Gg, -Population) %>% 
  tidyr::spread(Year, Capita) %>% 
  dplyr::arrange(
    Responsibility,
    match(Com.Group, products$Com.Group))

emission_group_luc <- food_footprint_no_outliers_luc %>%
  dplyr::bind_rows(territorial_luc %>% 
                     dplyr::filter(ISO == "ESP")) %>% 
  dplyr::mutate(Name = substr(Product, 5, length(Product) )) %>% 
  dplyr::left_join(products %>% 
                     dplyr::select(FAO.Name,
                                   Com.Group),
                   by = c("Name" = "FAO.Name")) %>% 
  dplyr::group_by(Responsibility,
                  ISO,
                  Com.Group,
                  Year) %>% 
  dplyr::summarise(Gg = sum(Gg)) %>% 
  dplyr::left_join(
    population_sp
  ) %>% 
  dplyr::mutate(Capita = Gg*1000000/Population) %>% 
  dplyr::select(-Gg, -Population) %>% 
  tidyr::spread(Year, Capita) %>% 
  dplyr::arrange(
    Responsibility,
    match(Com.Group, products$Com.Group))

emission_element <- food_footprint_no_outliers %>%
  dplyr::bind_rows(territorial %>% 
                     dplyr::filter(ISO == "ESP")) %>% 
  dplyr::group_by(Responsibility,
                  ISO,
                  Element,
                  Year) %>% 
  dplyr::summarise(Gg = sum(Gg)) %>% 
  dplyr::left_join(
    population_sp
  ) %>% 
  dplyr::mutate(Capita = Gg*1000000/Population) %>% 
  dplyr::select(-Gg, -Population) %>% 
  tidyr::spread(Year, Capita)

emission_element_luc <- food_footprint_no_outliers_luc %>%
  dplyr::bind_rows(territorial_luc %>% 
                     dplyr::filter(ISO == "ESP")) %>% 
  dplyr::group_by(Responsibility,
                  ISO,
                  Element,
                  Year) %>% 
  dplyr::summarise(Gg = sum(Gg)) %>% 
  dplyr::left_join(
    population_sp
  ) %>% 
  dplyr::mutate(Capita = Gg*1000000/Population) %>% 
  dplyr::select(-Gg, -Population) %>% 
  tidyr::spread(Year, Capita)

diss_table <- dissagregated_footprint_LUC %>% 
  tidyr::spread("Input", "Gg")

library(readODS)

write_ods(diss_table,
          "/home/vant/SCP-HAT project/Other/To_Excel.ods"
)

# Graph footprint EU countries
footprint_world_2013 <- food_footprint_eu %>% 
  dplyr::bind_rows(food_footprint_noneu,
                   food_footprint_luc_eu,
                   food_footprint_luc_noneu) %>% 
  dplyr::group_by(Responsibility,
                  Year) %>% 
  dplyr::summarise(Gg = sum(Gg)) %>% 
  dplyr::mutate(Population = sum(as.numeric(population$`2013`),
                                 na.rm = T),
                ISO = "World",
                Capita = (Gg*1000)/Population) %>% 
  dplyr::select(-Gg, -Population)
  
eu_15_names <- c(
  "AUT",
  "BEL",
  "DNK",
  "FIN",
  "FRA",
  "DEU",
  "GRC",
  "IRL",
  "ITA",
  "LUX",
  "NLD",
  "PRT",
  "ESP",
  "SWE",
  "GBR"
)

footprint_eu_15_2013 <- food_footprint_eu %>%
  dplyr::bind_rows(food_footprint_luc_eu) %>% 
  dplyr::filter(ISO %in% eu_15_names) %>% 
  dplyr::mutate(Year = as.integer(Year)) %>% 
  dplyr::group_by(Responsibility,
                  ISO,
                  Year) %>% 
  dplyr::summarise(Gg = sum(Gg)) %>% 
  dplyr::left_join(
    population %>% 
      tidyr::gather("Year", "Population", 2:31) %>% 
      dplyr::mutate(Year = as.integer(Year))
  ) %>% 
  dplyr::mutate(Population = replace(Population, is.na(Population), 0),
                Population = as.numeric(Population)) %>% 
  dplyr::group_by(Responsibility,
                  Year) %>% 
  dplyr::summarise(Gg = sum(Gg),
                   Population = sum(Population)) %>% 
  dplyr::mutate(Capita = Gg*1000/Population,
                ISO = "EU-15") %>% 
  dplyr::select(-Gg, -Population)

footprint_eu_rest_2013 <- food_footprint_eu %>%
  dplyr::bind_rows(food_footprint_luc_eu) %>% 
  dplyr::filter(ISO %!in% eu_15_names) %>% 
  dplyr::mutate(Year = as.integer(Year)) %>% 
  dplyr::group_by(Responsibility,
                  ISO,
                  Year) %>% 
  dplyr::summarise(Gg = sum(Gg)) %>% 
  dplyr::left_join(
    population %>% 
      tidyr::gather("Year", "Population", 2:31) %>% 
      dplyr::mutate(Year = as.integer(Year))
  ) %>% 
  dplyr::mutate(Population = replace(Population, is.na(Population), 0),
                Population = as.numeric(Population)) %>% 
  dplyr::group_by(Responsibility,
                  Year) %>% 
  dplyr::summarise(Gg = sum(Gg),
                   Population = sum(Population)) %>% 
  dplyr::mutate(Capita = Gg*1000/Population,
                ISO = "Rest of EU") %>% 
  dplyr::select(-Gg, -Population)

footprint_eu_2013 <- food_footprint_eu %>%
  dplyr::bind_rows(food_footprint_luc_eu) %>% 
  #dplyr::bind_rows(territorial) %>% 
  #dplyr::filter(Year == 2013) %>% 
  dplyr::filter(ISO %in% (countries %>% 
                            dplyr::filter(Continent == "EU") %>% 
                            dplyr::pull(ISO))) %>% 
  dplyr::mutate(Year = as.integer(Year)) %>% 
  dplyr::group_by(Responsibility,
                  ISO,
                  Year) %>% 
  dplyr::summarise(Gg = sum(Gg)) %>% 
  dplyr::left_join(
    population %>% 
      tidyr::gather("Year", "Population", 2:31) %>% 
      dplyr::mutate(Year = as.integer(Year))
  ) %>% 
  dplyr::mutate(Population = replace(Population, is.na(Population), 0),
                Population = as.numeric(Population),
                Capita = Gg*1000/Population) %>% 
  dplyr::select(-Gg, -Population) %>% 
  dplyr::bind_rows(
    footprint_world_2013,
    footprint_eu_15_2013,
    footprint_eu_rest_2013
  ) %>% 
  dplyr::filter(!is.nan(Capita)) %>% 
  dplyr::mutate(Colour = case_when(ISO == "ESP" ~ "Spain",
                                   ISO == "EU-15"  ~ "EU-15",
                                   ISO == "Rest of EU"  ~ "Rest of EU",
                                   ISO == "World"  ~ "World"
                                   ),
                Colour = replace(Colour, is.na(Colour), "EU country")) %>% 
  dplyr::arrange(Capita) %>% 
  dplyr::left_join(countries %>% 
                     dplyr::select(ISO,
                                   Country)) %>% 
  dplyr::mutate(
    Country = replace(Country, is.na(Country), ISO)
  ) 

colors <- c("Spain" = "#993955",
            "World" = "#f4c95d",
            "Rest of EU" = "#dd7230",
            "EU-15" = "#504746",
            "EU country" = "#909cc2") 

new_theme <- theme_minimal() %+replace% 
  theme(legend.position = "bottom")
theme_set(new_theme)

p <- ggplot(data = footprint_eu_2013 %>% 
              dplyr::mutate(Country =
                              factor(Country, levels = unique(footprint_eu_2013$Country))),
              aes(x = Country, y = Capita,  group = Colour, fill = Colour)) +
  geom_bar(stat = "identity",
           colour="black") +
  theme(axis.title.y = element_blank(),
        legend.title = element_blank(),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.line = element_line(),
        legend.text = element_text(
          margin = margin(r = 15, unit = "pt")),
        legend.position = c(0.9, 0.2)) +
  labs(y = "Tonnes CO2 eq. per capita") +
  scale_fill_manual(values = colors) +
  coord_flip()

plot(p)

ggsave("Graphs/Figure1.pdf", 
       plot = p,
       width = 15,
       height = 36, 
       units = "cm")

# Graph 


animal_report <- exports_footprint_no_outliers %>%
  dplyr::bind_rows(exports_footprint_LUC_no_outliers) %>% 
  dplyr::rename(Continent = ISO) %>% 
  dplyr::group_by(
    Continent,
    Name,
    Year
  ) %>% 
  dplyr::summarise(Exportaciones = sum(Gg)) %>% 
  dplyr::group_by(
    Name,
    Year
  ) %>% 
  dplyr::mutate(Perc = Exportaciones/sum(Exportaciones)*100)


t1 <- animal_report %>% 
  filter(Name == "Cattle")
t2 <- animal_report %>% 
  filter(Name == "Pigs")
t3 <- animal_report %>% 
  filter(Name == "Sheep")

remedia1 <- imports_footprint_no_outliers %>% 
  bind_rows(imports_footprint_no_outliers_luc) %>% 
  filter(Year %in%c(2001:2013)) %>% 
  dplyr::filter(ISO %in% c("ESP")) %>% 
  group_by(Year) %>% 
  summarise(Gg = sum(Gg)) %>% 
  mutate(Variable = "Domestic") %>% 
  left_join(
    imports_footprint_no_outliers %>% 
      bind_rows(imports_footprint_no_outliers_luc) %>% 
      filter(Year %in%c(2001:2013)) %>% 
      dplyr::filter(ISO %!in% c("ESP","ROW")) %>% 
      group_by(Year) %>% 
      summarise(Gg = sum(Gg)) %>% 
      mutate(Variable = "Imports"),
    by = c("Year")
  )

t_2013 <- imports_footprint_no_outliers %>% 
  bind_rows(imports_footprint_no_outliers_luc) %>% 
  filter(Year == 2013) %>% 
  dplyr::filter(ISO %!in% c("ESP","ROW")) %>% 
  group_by(Name) %>% 
  summarise(Gg = sum(Gg)) %>% 
  mutate(Perc= (Gg/sum(Gg))*100) %>% 
  filter(!is.nan(Perc)) %>% 
  arrange(-Perc)

remedia3 <- exports_footprint_no_outliers %>% 
  bind_rows(exports_footprint_no_outliers_luc) %>% 
  filter(Year %in%c(2001:2013)) %>% 
  group_by(Name, Year) %>% 
  summarise(Exportaciones = sum(Gg)) %>% 
  mutate(Variable = "Domestic") %>% 
  group_by(Year) %>% 
  mutate(Perc= (Exportaciones/sum(Exportaciones))*100) %>% 
  filter(!is.nan(Perc),
         Year == 2013) %>% 
  arrange(-Perc)


vegs_report <- imports_footprint_no_outliers %>% 
  bind_rows(imports_footprint_no_outliers_luc) %>% 
  filter(Year %in%c(2001:2013)) %>% 
  dplyr::filter(ISO %!in% c("ESP","ROW")) %>% 
  dplyr::group_by(
   Name,
    Year
  ) %>% 
  dplyr::summarise(Importaciones = sum(Gg)) %>% 
  filter(Name == "Barley and products")

rememdio2 <- imports_footprint_no_outliers %>% 
  bind_rows(imports_footprint_no_outliers_luc) %>% 
  filter(Year %in%c(2001:2013)) %>% 
  dplyr::filter(ISO %!in% c("ESP","ROW")) %>% 
  dplyr::group_by(
    Name,
    Year
  ) %>%
  dplyr::summarise(Importaciones = sum(Gg)) %>% 
  group_by(Year) %>% 
  mutate(Perc= (Importaciones/sum(Importaciones))*100) %>% 
  filter(!is.nan(Perc),
         Year == 2013) %>% 
  arrange(-Perc)

exports_excel <- exports_footprint_no_outliers %>% 
  dplyr::group_by(
    Name,
    Year
  ) %>% 
  dplyr::summarise(Exportaciones = sum(Gg)) %>% 
  tidyr::spread(Year, Exportaciones) %>% 
  dplyr::arrange(match(Name, products$FAO.Name))

imports_excel <- imports_footprint_no_outliers %>% 
  dplyr::filter(ISO %!in% c("ESP", "ROW")) %>%
  dplyr::group_by(
    Name,
    Year
  ) %>% 
  dplyr::summarise(Importaciones = sum(Gg)) %>% 
  tidyr::spread(Year, Importaciones) %>% 
  dplyr::arrange(match(Name, products$FAO.Name))

domestic_excel <- imports_footprint_no_outliers %>% 
  dplyr::filter(ISO == "ESP") %>%
  dplyr::group_by(
    Name,
    Year
  ) %>% 
  dplyr::summarise(Importaciones = sum(Gg)) %>% 
  tidyr::spread(Year, Importaciones) %>% 
  dplyr::arrange(match(Name, products$FAO.Name))



table_report <- imports_spain_agg %>% 
  left_join(exports_spain_agg) %>% 
  group_by(Year) %>% 
  summarise_if(is.numeric, sum) %>% 
  mutate(Balance = Exportaciones+Importaciones)

eu_data <- imports_spain_agg %>% 
  left_join(exports_spain_agg) %>% 
  filter(Continent == "EU") %>% 
  mutate(Balance = Exportaciones+Importaciones) %>% 
  arrange(Balance)

lam_data <- imports_spain_agg %>% 
  left_join(exports_spain_agg) %>% 
  filter(Continent == "LAM") %>% 
  mutate(Balance = Exportaciones+Importaciones) %>% 
  arrange(Balance)

asia_data <- imports_spain_agg %>% 
  left_join(exports_spain_agg) %>% 
  filter(Continent == "ASI") %>% 
  mutate(Balance = Exportaciones+Importaciones) %>% 
  arrange(Balance)

afr_data <- imports_spain_agg %>% 
  left_join(exports_spain_agg) %>% 
  filter(Continent == "AFR") %>% 
  mutate(Balance = Exportaciones+Importaciones) %>% 
  arrange(Balance)

cattle_imports <- imports_footprint_no_outliers %>% 
  dplyr::filter(ISO %!in%c("ESP", "ROW")) %>%
  dplyr::group_by(
    ISO,
    Name,
    Year
  ) %>% 
  dplyr::summarise(Importaciones = sum(Gg) *-1) %>% 
  filter(Name == "Cattle") %>% 
  left_join(countries %>% 
              select(ISO, Continent)) %>% 
  group_by(Continent, Year, Name) %>% 
  summarise(Importaciones = sum(Importaciones)) %>% 
  group_by(Year) %>% 
  mutate(Perc = Importaciones/sum(Importaciones) *100)


oil_corps_imports <- imports_footprint_no_outliers %>% 
  bind_rows(imports_footprint_no_outliers_luc) %>% 
  filter(Year %in%c(2001:2013)) %>% 
  dplyr::filter(ISO %!in%c("ESP", "ROW")) %>%
  dplyr::group_by(
    Name,
    Year
  ) %>% 
  dplyr::summarise(Importaciones = sum(Gg) *-1) %>% 
  filter(Name %in% (products %>% 
           filter(Com.Group == "Oil crops") %>% 
           pull(FAO.Name))&(Name%!in% c("Olives (including preserved)",
                                        "Soyabeans"))) %>% 
  group_by(Year) %>% 
  mutate(Perc = Importaciones/sum(Importaciones) *100) %>% 
  filter(Year == 2013)


exports_text_veg <- trade_spain_product %>% 
  filter(Key2 %in%c("Cebada",
                    "Trigo",
                    "Arroz",
                    "Maíz",
                    "Otros cereales",
                    "Forrajes",
                    "Frutas y verduras",
                    "Aceituna",
                    "Soja",
                    "Cultivos\n oleaginosos",
                    "Tubérculos",
                    "Cultivos\n azucareros"))%>% 
  filter(Year == 2013) %>% 
  ungroup() %>% 
    mutate(Perc = Exportaciones/sum(Exportaciones)*100)




##
load(paste0("data/Results_Spain.RData"))

test1 <- food_footprint %>% 
  dplyr::mutate(Name = substr(Product, 5, length(Product)),
                Origin = substr(Product, 1, 3)) %>% 
  dplyr::group_by(Origin, Name) %>% 
  dplyr::mutate(z_score_group = (Gg - mean(Gg)) / sd(Gg)) %>%
  dplyr::filter(z_score_group > 3 | z_score_group < -3)

nrow(test1)/nrow(food_footprint)


emission_eu <- food_footprint_eu %>%
  dplyr::bind_rows(territorial) %>% 
  dplyr::filter(Year == 2013) %>% 
  dplyr::filter(ISO %in% (countries %>% 
                  dplyr::filter(Continent == "EU") %>% 
                  dplyr::pull(ISO))) %>% 
  dplyr::mutate(Year = as.integer(Year)) %>% 
  dplyr::group_by(Responsibility,
                  ISO,
                  Year) %>% 
  dplyr::summarise(Gg = sum(Gg)) %>% 
  dplyr::left_join(
    population %>% 
      tidyr::gather("Year", "Population", 2:31) %>% 
      dplyr::mutate(Year = as.integer(Year))
  ) %>% 
  dplyr::mutate(Population = replace(Population, is.na(Population), 0),
                Population = as.numeric(Population),
    Capita = Gg*1000/Population) %>% 
  dplyr::select(-Gg, -Population) %>% 
  tidyr::spread(ISO,
                Capita)