# Agro-food greenhouse gas emissions are increasingly driven by foreign demand

base::rm(list = ls())   # cleaning workspace

# Packages for data processing
lapply(c("tidyverse", "data.table", "openxlsx", "showtext",
         "tidyverse", "ggthemes", "interactions", "stargazer",
         "olsrr", "ggpmisc", "cowplot", "extrafont", "ggrepel",
         "plm", "scales", "gghighlight", "betareg", "ggridges", 
         "ggstance", "modelsummary", "fixest"), library, character.only = TRUE)

font_add("lato", "Lato-Regular.ttf")
palette_OkabeIto <- c("#E69F00", "#56B4E9", 
                      "#009E73", "#F0E442", 
                      "#0072B2", "#D55E00", 
                      "#CC79A7", "#999999")

# Figure 1 -------------
fig.1a.data <- openxlsx::read.xlsx("SI.xlsx",
                                   sheet = "Figure 1a") 

obtain.Change <- function(fig.data, year.origin){
  
  fig.data %>% group_by(category, system) %>% 
    mutate(change = (value/value[1])-1)
  
}

common.plot <- list(cowplot::theme_half_open(font_family = "lato") + cowplot::background_grid() +
                      theme(axis.text = element_text(size = 10),
                            axis.title.x = element_blank(),
                            legend.spacing.y = unit(-0.1, "cm"),
                            legend.position =  "bottom"))

fig1.a <- ggplot(obtain.Change(fig.1a.data), aes(x=year, y=change, group=category)) +
  geom_point(size=1.5, aes(colour=category)) +
  geom_line(size=0.5, aes(colour=category)) +
  scale_x_continuous(breaks = c(1985, 1990, 1995, 2000, 2005, 2010), limits = c(1985, 2013)) +
  scale_y_continuous( breaks=c(-0.2, 0, 0.2,0.4, 0.6, 0.80), limits = c(-0.3, 0.80), labels = scales::label_percent()) +
  scale_color_manual(values = palette_OkabeIto[c(2,4,1)]) +
  common.plot + theme(strip.text = element_text(size = 12),
                      legend.title = element_blank(),
                      axis.title.y = element_text(size = 10, margin = margin(t = 0, r = 10, b = 0, l = 0)))+
  cowplot::background_grid() +
  theme(axis.title.x=element_blank(), panel.spacing.x = unit(2, "lines"))+
  labs(y="% change in GHG emissions") +
  facet_wrap(~ system, nrow = 1, scales="free") +
  guides(fill = guide_legend(nrow = 1, title = ""))

fig.1b.Abs.data <- openxlsx::read.xlsx("SI.xlsx",
                                       sheet = "Figure 1b-Abs") %>% 
  mutate(max = ifelse(`consumption-based`> `production-based`, `consumption-based`, `production-based`),
         min = ifelse(`consumption-based` < `production-based`, `consumption-based`, `production-based`)) %>% 
  reshape2::melt(-c(4:6)) %>% 
  mutate(flow = ifelse(ISO == "IND" & year %in% c(1998, 1999), "net exports", flow))

fig1.b.abs <- ggplot(data = fig.1b.Abs.data %>% filter(variable == "consumption-based")) +
  geom_line(aes(x = year, y = value, linetype = variable), colour = "black", size = 1) +
  geom_line(data = (fig.1b.Abs.data %>% dplyr::filter(variable == "production-based")),
            aes(x = year, y = value, linetype = variable), colour = "black", size = 1)+
  geom_ribbon(data = (fig.1b.Abs.data %>% dplyr::filter(variable == "net.balance") %>% select(-variable,-value)),
              aes(x = year, ymax = max, ymin = min, fill = flow), alpha = .6) +
  common.plot + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), strip.text = element_text(size = 12),
                      axis.title.y = element_text(size = 10),
                      legend.key.width= unit(1, 'cm'))+
  facet_wrap(~ ISO, nrow = 1, labeller = labeller(ISO = labels_countries), scales="free") +
  guides(fill = guide_legend(nrow = 1, title = ""), linetype = guide_legend(nrow = 1, title = "")) +
  scale_fill_manual(values = palette_OkabeIto[c(3,6)]) +
  scale_y_continuous(limits = c(0, 2), labels = scales::number_format(accuracy = 0.1))+ 
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_x_continuous(breaks=c(1990, 2000, 2010))+
  labs(y=expression(GtCO[2]~e/yr))

fig.1b.Cap.data <- openxlsx::read.xlsx("SI.xlsx",
                                       sheet = "Figure 1b-Cap") %>% 
  mutate(max = ifelse(`consumption-based` > `production-based`, `consumption-based`, `production-based`),
         min = ifelse(`consumption-based` < `production-based`, `consumption-based`, `production-based`),
         flow = ifelse((`consumption-based`- `production-based`) > 0, "net imports", "net exports"),
         net = abs(`consumption-based`- `production-based`)) %>% 
  reshape2::melt(-c(4:5, 9))

fig1.b.cap <- ggplot(data = fig.1b.Cap.data %>% dplyr::filter(variable == "consumption-based")) +
  geom_line(aes(x = year, y = value, linetype = variable), colour = "black", size = 1) +
  geom_line(data = (fig.1b.Cap.data %>%  dplyr::filter(variable == "production-based")),
            aes(x = year,  y = value, linetype = variable),  colour = "black", size = 1)+
  geom_ribbon(data = (fig.1b.Cap.data %>%   dplyr::filter(variable == "net") %>% select(-variable,-value)),
              aes(x = year, ymax = max,  ymin = min,  fill = flow),  alpha = .6) +
  geom_hline(yintercept = 5, linetype = "dashed", color = "red",  size= 1) +
  common.plot +  theme(strip.text = element_blank(), axis.title.y = element_text(size = 10))+
  facet_wrap(~ ISO, nrow = 1, labeller = labeller(ISO = labels_countries),scales="free") +
  guides(linetype = guide_legend(nrow = 1),fill = guide_legend(nrow = 1, title = "")) +
  scale_fill_manual(values = palette_OkabeIto[c(3,6)]) +
  scale_y_continuous(limits = function(Capita) {if(max(Capita) > 10){c(0, 11)} else{ c(0,5)}}, labels = scales::number_format(accuracy = 0.1)) + 
  scale_x_continuous(breaks=c(1990, 2000, 2010))+
  scale_linetype_manual(values = c("dashed", "solid")) +
  labs(y=expression(tCO[2]~e/yr/cap))

Fig1 <- cowplot::plot_grid(fig1.a, cowplot::plot_grid(cowplot::plot_grid(fig1.b.abs+theme(legend.position="none"), 
                                                                         fig1.b.cap+theme(legend.position="none"), ncol = 1, 
                                                                         align = 'v', axis = "t"), cowplot::get_legend(fig1.b.abs), 
                                                      ncol = 1, rel_heights = c(1, .1), align = 'h', axis = "none"), 
                           ncol = 1, align = 'v', rel_heights = c(0.6, 1), labels = c("a", "b"))

ggsave(file = "Fig1_final.png", 
       path="results/",
       plot = Fig1,
       dpi = 96,
       width = 25,
       height = 18,
       units = "cm" )

# Figure 2 -----------

fig.2.data <- openxlsx::read.xlsx("SI.xlsx",
                                  sheet = "Figure 2") 

colours <- c("Europe" = "#E69F00", "South Asia" = "#F0E442",
             "Latin America & C." = "#009E73", "Sub-Saharan Africa" = "#56B4E9", "East Asia & Pacific" = "#0072B2",
             "North America" = "#999999", "Central Asia" = "#CC79A7", "Middle East & N.Afr." = "#D55E00")

ggplot(subset(fig.2.data, pop > 2000), aes(x=exports/territorial, y=imports/footprint)) + 
  geom_abline(intercept=0, slope=1, linetype="dashed", alpha=0.6) +
  geom_point(pch = 21, color = "white",
             aes(fill=factor(region), size=footprint, alpha=present)) + 
  facet_wrap(~ region, scales="free", ncol=4) +
  scale_alpha_discrete(range=c(0.1, 1)) +
  theme_cowplot(font_family = "Lato") +
  scale_y_continuous(labels=scales::number_format(accuracy = 0.1), breaks=c(0.1, 0.5, 0.9)) +
  scale_x_continuous(labels=scales::number_format(accuracy = 0.1), breaks=c(0.1, 0.5, 0.9)) +
  scale_fill_manual(values = colours) +
  geom_text_repel(data = subset(fig.2.data, year == 2013) %>% 
                    mutate(label = ifelse(ISO %in% c("CIV", "BWA", "NAM",
                                                     "IND", "IRL", "BLR",
                                                     "CAN", "USA", "RUS", "SAU", "MAR",
                                                     "PRY", "ARG", "URY", "BRA", "NIC", "VEN", "CHL",
                                                     "IDN", "JPN", "NZL", "AUS"),
                                          paste(ISO), "")),
                  aes(label = label),
                  segment.alpha = 0.5, point.padding = 1,
                  box.padding = 1,
                  force = 1,
                  family = "Lato",
                  max.overlaps = 1000,
                  min.segment.length = 0.1,
                  size = 8/.pt,
                  show.legend = FALSE) +
  coord_cartesian(xlim=c(0.01,1), ylim=c(0.01,1)) +
  scale_radius(
    name = "food consumption GHG footprint (tCO2/yr)",
    range = c(2, 10),
    limits = c(46.9, 1692596.9),
    breaks = c(5000, 20000, 60000, 200000),
    labels = c("5,000   ", "20,000   ", "50,000   ", "100,000"),
    guide = guide_legend(ncol=4,
                         direction = "horizontal",
                         title.position = "left",
                         title.hjust = 0.5,
                         label.position = "right",
                         override.aes = list(fill = "black")
    )
  ) +
  labs(y="share of foreign emissions in national footprint", x="share of territorial emissions embodied in exports") +
  guides(fill="none", alpha="none") +
  theme(legend.position = "bottom", 
        legend.justification = "center")

ggsave(filename = "Fig.2.png", dpi = 800, 
       type = "cairo",
       width = 10, height = 5, units = "in")

# Figure 3a -----------

origin_colors = c("animal-based"="#D55E00", "plant-based"="#009E73")
year_colors = c("1986"="#56B4E9", "2013"="#E69F00")

fig.3ab.data <- openxlsx::read.xlsx("SI.xlsx",
                                    sheet = "Figure 3ab") 

fig.3ab.data$product <- reorder(fig.3ab.data$product, 
                                fig.3ab.data$trade)

fig.3ab.data <- fig.3ab.data %>%
  mutate(product = fct_relevel(product, levels = "Cassava and products", "Beans",
                               "Fruits, Other", "Butter, Ghee", "Maize and products",
                               "Fats, Animals, Raw", "Vegetables, Other",
                               "Offals, Edible", "Meat, Other", "Mutton & Goat Meat",
                               "Sugar, Refined Equiv", "Eggs", "Soyabean Oil",
                               "Fish, Seafood", "Coffee and products", "Wheat and products",
                               "Rice (Milled Equivalent)", "Poultry Meat",
                               "Milk - Excluding Butter", "Pigmeat", "Bovine Meat"))

ggplot() +
  theme_fivethirtyeight(base_family = "Lato") +
  theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white')) +
  geom_point(data=fig.3ab.data, 
             aes(y=product, x=trade, fill=origin, alpha=year),
             size=4.8, shape=21) +
  scale_alpha(range = c(0.5, 0.9)) +
  guides(alpha="none") +
  geom_segment(data=fig.3ab.data,
               aes(y=product, yend=product, x=trade_em_1986, xend=trade_em_2013),
               size=1.44, color="grey30",
               lineend="butt", linejoin="mitre",
               arrow=arrow(length = unit(0.02, "npc"), type="closed")) +
  # labs(y=NULL,
  #     title="Export carbon footprint by final product (GgCOe): 1986 (light) vs 2013 (dark)",
  #    subtitle="Categories contributing >1% of global food trade GHG emissions are shown [LUC 50 years]") +
  scale_x_continuous(labels = unit_format(unit = "K", scale = 1e-3, accuracy=1),
                     breaks = c(0, 200000, 400000, 600000)) +
  theme(plot.title.position = "plot") +
  coord_cartesian(xlim=c(0, 670000)) +
  theme(legend.title = element_blank()) +
  theme(legend.justification = "center") +
  scale_fill_manual(values=origin_colors) +
  theme(#legend.position = "bottom",
    legend.position = "none",
    legend.justification = c(0.5, 0),
    legend.title = element_blank(),
    legend.margin = margin(c(-2, 5, -1, 0)),
    legend.text = element_text(margin = margin(r = 20, unit = "pt")))

ggsave(filename = "Fig.3a.png", dpi = 800, 
       type = "cairo",
       width = 8, height = 5, units = "in")

# Figure 3b -----------

ggplot() +
  theme_fivethirtyeight(base_family = "Lato") +
  theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white')) +
  geom_point(data=fig.3ab.data, 
             aes(y=product, x=foreign_share, fill=origin, 
                 alpha=year),
             size=4.8, shape=21) +
  scale_alpha(range = c(0.5, 0.9)) +
  guides(alpha="none") +
  geom_segment(data=fig.3ab.data,
               aes(y=product, yend=product, x=fgn_share_1986, xend=fgn_share_2013),
               size=1.44, color="grey30",
               lineend="butt", linejoin="mitre",
               arrow=arrow(length = unit(0.02, "npc"), type="closed")) +
  #  labs(y=NULL,
  #      title="Exports as % of emissions by final product: 1986(light) vs 2013(dark)",
  #     subtitle="Products listed by contribution to global food GHG; inputs contributing >1% are shown [LUC 50 yrs]") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = c(0, .2, .4, .6, .8)) +
  coord_cartesian(xlim=c(0, .85)) +
  theme(plot.title.position = "plot") +
  theme(legend.title = element_blank()) +
  theme(legend.justification = "center") +
  scale_fill_manual(values=origin_colors) +
  #scale_colour_manual(values=year_colors) +
  theme(#legend.position = "bottom",
    legend.position = "none",
    legend.justification = c(0.5, 0),
    legend.title = element_blank(),
    legend.margin = margin(c(-2, 5, -1, 0)),
    legend.text = element_text(margin = margin(r = 20, unit = "pt")))

ggsave(filename = "Fig.3b.png", dpi = 800, 
       type = "cairo",
       width = 8, height = 5, units = "in")

# Figure 3c -----------

fig.3c.data <- openxlsx::read.xlsx("SI.xlsx",
                                   sheet = "Figure 3c") 

fig.3c.data$product <- reorder(fig.3c.data$product, 
                               fig.3c.data$share_order)

ggplot() +
  theme_fivethirtyeight(base_family = "Lato") +
  theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white')) +
  geom_point(data=fig.3c.data, 
             aes(y=product, x=exports, fill=origin, alpha=year),
             size=4.8, shape=21) +
  scale_alpha(range = c(0.5, 0.9)) +
  guides(alpha="none") +
  geom_segment(data=fig.3c.data,
               aes(y=product, yend=product, x=exports_1986, xend=exports_2013),
               size=1.44, color="grey30",
               lineend="butt", linejoin="mitre",
               arrow=arrow(length = unit(0.02, "npc"), type="closed")) +
  #  labs(y=NULL,
  #     title="Export carbon footprint by input (GgCOe): 1986 (light) vs 2013 (dark)",
  #     subtitle="Categories contributing >1% of global food trade GHG emissions are shown [LUC 50 years]") +
  scale_x_continuous(labels = unit_format(unit = "K", scale = 1e-3, accuracy=1),
                     breaks = c(0, 200000, 400000, 600000)) +
  coord_cartesian(xlim=c(0,670000)) +
  theme(plot.title.position = "plot") +
  theme(legend.title = element_blank()) +
  theme(legend.justification = "center") +
  scale_fill_manual(values=origin_colors) +
  theme(legend.position = "bottom",
        #  legend.position = "none",
        legend.justification = c(0.5, 0),
        legend.title = element_blank(),
        legend.background = element_rect(fill = 'white', colour = 'white'),
        legend.key = element_blank(),
        legend.margin = margin(c(-2, 5, -1, 0)),
        legend.text = element_text(margin = margin(r = 20, unit = "pt")))

ggsave(filename = "Fig.3c.png", dpi = 800, 
       type = "cairo",
       width = 8, height = 5, units = "in")

# Figure 3d -----------

fig.3d.data <- openxlsx::read.xlsx("SI.xlsx",
                                   sheet = "Figure 3d") 

fig.3d.data <- fig.3d.data %>%
  mutate(product = fct_relevel(product, levels = "Coconuts - Incl Copra", 
                               "Nuts and products", "Palmkernel Oil",
                               "Pigs", "Sugar, Refined Equiv",
                               "Barley and products",
                               "Sheep", "Cocoa Beans and products",
                               "Sugar cane", "Palmkernel Cake", "Milk - Excluding Butter", 
                               "Buffaloes", "Fodder crops",
                               "Rice (Milled Equivalent)", "Coffee and products", 
                               "Wheat and products", "Maize and products", "Cattle",
                               "Soyabeans"))

ggplot() +
  theme_fivethirtyeight(base_family = "Lato") +
  theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white')) +
  geom_point(data=fig.3d.data, 
             aes(y=product, x=fgn_share, fill=origin, 
                 alpha=year),
             size=4.8, shape=21) +
  scale_alpha(range = c(0.5, 0.9)) +
  guides(alpha="none") +
  geom_segment(data=fig.3d.data,
               aes(y=product, yend=product, x=share_1986, xend=share_2013),
               size=1.44, color="grey30",
               lineend="butt", linejoin="mitre",
               arrow=arrow(length = unit(0.02, "npc"), type="closed")) +
  # labs(y=NULL,
  #     title="Export carbon footprint by input (GgCOe): 1986 (light) vs 2013 (dark)",
  #   subtitle="Categories contributing >1% of global food trade GHG emissions are shown [LUC 50 years]") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(plot.title.position = "plot") +
  theme(legend.title = element_blank()) +
  theme(legend.justification = "center") +
  coord_cartesian(xlim=c(0,.85)) +
  scale_fill_manual(values=origin_colors) +
  theme(#legend.position = "bottom",
    legend.position = "none",
    legend.justification = c(0.5, 0),
    legend.title = element_blank(),
    legend.margin = margin(c(-2, 5, -1, 0)),
    legend.text = element_text(margin = margin(r = 20, unit = "pt")))

ggsave(filename = "Fig.3d.png", dpi = 800, 
       type = "cairo",
       width = 8, height = 5, units = "in")

# Figure 4 -----------

fig.4.data <- openxlsx::read.xlsx("SI.xlsx",
                                  sheet = "Figure 4") 

labeled_countries <- c(
  "ETH", "BRA", "USA", "KWT", 
  "CHN", "IDN", "PAK", "ESP", "GHA"
)

df_repel <- select(df, x = gdppc, 
                   y = import_share, ISO) %>%
  mutate(label = ifelse(ISO %in% labeled_countries, 
                        as.character(ISO), ""))

colours <- c("Europe" = "#E69F00", "South Asia" = "#F0E442",
             "Latin America & C." = "#009E73", "Sub-Saharan Africa" = "#56B4E9", "East Asia & Pacific" = "#0072B2",
             "North America" = "#999999", "Central Asia" = "#CC79A7", "Middle East & N.Afr." = "#D55E00")

ggplot(fig.4.data[which(fig.4.data$pop>2000),],
       aes(y=import_pc, x=gdppc)) +
  geom_smooth(data=subset(fig.4.data, present=="present" &
                            pop > 2000),
              method="loess", 
              colour="black", se=TRUE) +
  #            aes(group=ISO, colour=region),
  #           se = FALSE,
  #          method = "lm",
  #         linetype=1) +
  geom_point(pch = 21, color = "white",
             aes(fill=factor(region), 
                 group=factor(ISO),
                 size=footprint, alpha=present)) +
  scale_fill_manual(values = colours) +
  #  scale_colour_manual(values = colours) +
  scale_alpha_discrete(range=c(0.1, 1)) +
  guides(alpha="none") +
  scale_y_continuous(trans = "log", 
                     # labels = scales::number_format(accuracy = 1),
                     breaks = c(0.02, 0.1, 0.5, 2)
  ) +
  scale_x_continuous(trans = "log",
                     # breaks = c(0.05, 0.1, 0.5),
                     breaks = c(500, 2000, 8000, 40000),
                     labels = scales::
                       number_format(accuracy = 1, big.mark=",")
  ) +
  theme_cowplot(font_family = "Lato") +
  theme(#legend.position = c(0.825, 0.1),
    legend.position = "bottom", 
    legend.title = element_blank(),
    legend.margin = margin(0, 0, 0, 0),
    legend.spacing.x = unit(6, "pt"),
    legend.spacing.y = unit(0, "pt"),
    legend.justification = c(0.8,0),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key.width = unit(0, "pt"),
    strip.text = element_text(size = 14, margin = margin(0, 0, 0, 0)),
    strip.background  = element_rect(
      fill = "grey85", colour = "grey85",
      linetype = 1, size = 0.25),
    legend.text = element_text(margin = margin(r = 0.5, unit = 'cm'),
                               size = 14)) +
  scale_radius(
    name = (bquote(total~footprint~(GgCO[2]*e/yr))),
    range = c(2, 10),
    limits = c(46.9, 1710855),
    breaks = c(5000, 20000, 60000, 200000),
    labels = c("5,000   ", "20,000   ", "50,000   ", "100,000"),
    guide = guide_legend(ncol=2,
                         direction = "horizontal",
                         title.position = "top",
                         title.hjust = 0.5,
                         label.position = "right",
                         override.aes = list(fill = "gray40")
    )
  ) +
  stat_poly_eq(data=subset(fig.4.data, present=="present" &
                             pop > 2000),
               formula = y ~ x,
               parse = TRUE,
               small.r = TRUE,
               small.p = TRUE,
               (aes(label = paste("atop(", 
                                  after_stat(rr.label), ",", 
                                  after_stat(p.value.label), ")", sep = "")))
  ) +
  labs(x="real GDP per capita (2011 dollars)", y="displaced emissions\n (tCO2e/yr/cap)") +
  # ylab(bquote(tCO[2]*e/yr/cap)) +
  guides(size="none") +
  geom_text_repel(data = subset(fig.4.data, year == 2013) %>% 
                    mutate(label = ifelse(ISO %in% c("CHN", "BRA", "USA", "DEU", "JPN",
                                                     "KWT", "SEN", "CUB", "HKG", "IND", "MWI"),
                                          paste(ISO), "")),
                  aes(label = label),
                  segment.alpha = 0.5, point.padding = 1,
                  box.padding = 1,
                  force = 1,
                  family = "Lato",
                  max.overlaps = 1000,
                  min.segment.length = 0,
                  size = 8/.pt,
                  show.legend = FALSE) +
  guides(fill = guide_legend(override.aes = list(size=5)))

ggsave(filename = "Fig.4.png", dpi = 800, 
       type = "cairo",
       width = 8, height = 5, units = "in")

# Figure SI-11 -----------

ggplot(fig.4.data[which(fig.4.data$pop>2000),],
       aes(y=import_share, x=gdppc)) +
  geom_smooth(data=subset(fig.4.data, present=="present" &
                            pop > 2000),
              method="loess", 
              colour="black", se=TRUE) +
  #            aes(group=ISO, colour=region),
  #           se = FALSE,
  #          method = "lm",
  #         linetype=1) +
  geom_point(pch = 21, color = "white",
             aes(fill=factor(region), 
                 group=factor(ISO),
                 size=footprint, alpha=present)) +
  scale_fill_manual(values = colours) +
  #  scale_colour_manual(values = colours) +
  scale_alpha_discrete(range=c(0.1, 1)) +
  guides(alpha="none") +
  coord_cartesian(ylim=c(0,100))+
  #  scale_y_continuous(trans = "log", 
  # labels = scales::number_format(accuracy = 1),
  #                breaks = c(0.02, 0.1, 0.5, 2)
  # ) +
  scale_x_continuous(trans = "log",
                     # breaks = c(0.05, 0.1, 0.5),
                     breaks = c(500, 2000, 8000, 40000),
                     labels = scales::
                       number_format(accuracy = 1, big.mark=",")
  ) +
  theme_cowplot(font_family = "Lato") +
  theme(#legend.position = c(0.825, 0.1),
    legend.position = "bottom", 
    legend.title = element_blank(),
    legend.margin = margin(0, 0, 0, 0),
    legend.spacing.x = unit(6, "pt"),
    legend.spacing.y = unit(0, "pt"),
    legend.justification = c(0.8,0),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key.width = unit(0, "pt"),
    strip.text = element_text(size = 14, margin = margin(0, 0, 0, 0)),
    strip.background  = element_rect(
      fill = "grey85", colour = "grey85",
      linetype = 1, size = 0.25),
    legend.text = element_text(margin = margin(r = 0.5, unit = 'cm'),
                               size = 14)) +
  scale_radius(
    name = (bquote(total~footprint~(GgCO[2]*e/yr))),
    range = c(2, 10),
    limits = c(46.9, 1710855),
    breaks = c(5000, 20000, 60000, 200000),
    labels = c("5,000   ", "20,000   ", "50,000   ", "100,000"),
    guide = guide_legend(ncol=2,
                         direction = "horizontal",
                         title.position = "top",
                         title.hjust = 0.5,
                         label.position = "right",
                         override.aes = list(fill = "gray40")
    )
  ) +
  stat_poly_eq(data=subset(fig.4.data, present=="present" &
                             pop > 2000),
               formula = y ~ x,
               parse = TRUE,
               small.r = TRUE,
               small.p = TRUE,
               (aes(label = paste("atop(", 
                                  after_stat(rr.label), ",", 
                                  after_stat(p.value.label), ")", sep = "")))
  ) +
  labs(x="real GDP per capita (2011 dollars)", y="share of food consumption \n emissions displaced") +
  # ylab(bquote(tCO[2]*e/yr/cap)) +
  guides(size="none") +
  geom_text_repel(data = subset(fig.4.data, year == 2013) %>% 
                    mutate(label = ifelse(ISO %in% c("CHN", "BRA", "USA", "DEU",
                                                     "KWT", "SEN", "CUB", "JPN", "IND", "MWI"),
                                          paste(ISO), "")),
                  aes(label = label),
                  segment.alpha = 0.5, point.padding = 1,
                  box.padding = 1,
                  force = 1,
                  family = "Lato",
                  max.overlaps = 1000,
                  min.segment.length = 0,
                  size = 8/.pt,
                  show.legend = FALSE) +
  guides(fill = guide_legend(override.aes = list(size=5)))

ggsave(filename = "figure_fraction_emissions.png", dpi = 800, 
       type = "cairo",
       width = 8, height = 5, units = "in")

# Table SI-3 -------------

panel <- openxlsx::read.xlsx("SI.xlsx",
                             sheet = "Figure 4")

panel.complete <- subset(panel, gdppc > 0 & footprint > 0 & pop > 2000)

panel.complete <-
  mutate(panel.complete,
         footprint_pc =  
           footprint / pop)

models <- list(
  "country.fe"= feols(log(import_pc) ~ log(gdppc) | ISO,
                      data = panel.complete, panel.id = ~ISO+year),
  'country.year.fe' = feols(log(import_pc) ~ log(gdppc) | ISO + year,
                            data = panel.complete, panel.id = ~ISO+year),
  'control.pop' = feols(log(import_pc) ~ log(gdppc) + log(pop) | ISO + year,
                        data = panel.complete, panel.id = ~ISO+year),
  'interaction' = feols(log(import_pc) ~ log(gdppc) * log(pop) | ISO + year,
                        data = panel.complete, panel.id = ~ISO+year)
)

msummary(models,
         stars = c('*' = .1, '**' = .05, '***' = .01))

# Table SI-4 -------------

panel <- openxlsx::read.xlsx("SI.xlsx",
                             sheet = "Figure 4")

panel.complete <- subset(panel, gdppc > 0 & footprint > 0 & pop > 2000)

panel.complete <-
  mutate(panel.complete,
         footprint_pc =  
           footprint / pop)

models.leakage <- list(
  "country.fe"= feols(log(footprint_pc)~log(import_share) | ISO,
                      data = panel.complete, panel.id = ~ISO+year),
  'country.year.fe' = feols(log(footprint_pc)~log(import_share) | ISO + year,
                            data = panel.complete, panel.id = ~ISO+year),
  'control.income' = feols(log(footprint_pc)~log(import_share) + log(gdppc) | ISO + year,
                           data = panel.complete, panel.id = ~ISO+year)
)

msummary(models.leakage,
         stars = c('*' = .1, '**' = .05, '***' = .01))
