# Agro-food greenhouse gas emissions are increasingly driven by foreign demand

base::rm(list = ls())   # cleaning workspace

# Packages for data processing
lapply(c("tidyverse", "data.table", "openxlsx", "showtext",
         "tidyverse", "ggthemes", "interactions", "stargazer",
         "olsrr", "ggpmisc", "cowplot", "extrafont", "ggrepel",
         "plm", "scales", "gghighlight", "betareg", "ggridges", 
         "ggstance"), library, character.only = TRUE)

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

