library(tidyverse)
#tidyverse contains the following packages:
#readr
#dplyr
#ggplot2
library(plotly)

iris <- iris

#histogram
ggplot(data = iris, aes(x = Sepal.Length)) +
  geom_histogram(binwidth = 0.1) +
  labs(x = "Sepal Length", y = "Nombre", title = "Sepal") +
  theme_bw() +
  coord_flip() 

#Line plot
# creation d'un objet plot1 ggplot
plot1 <- ggplot(data = iris, aes(x = Petal.Width, y = Sepal.Width, 
                        colour = Species,
                        shape = Species)) +
  geom_line() +
  labs(x = "Petal Width", y = "Sepal width", 
       colour = "Espece",
       shape = "Espece") +
  theme_classic()

plot1

# enregisrement dans le repertoire output sous le nom flowers.pdf
ggsave(filename = "outputs/flowers.pdf", plot = plot1, width = 5, height = 1)
ggsave(filename = "outputs/flowers2.pdf", plot = plot1, width = 5, height = 4)

plot1 +
  theme_bw()
## a noter: il accepte la commande le changement de thème:
ggsave(filename = "outputs/flowers3.pdf", plot = plot1+theme_bw(), width = 5, height = 4)

#Line plot
emissions <- readRDS(file = "inputs/emissions.rds")
emissions %>% 
  filter(geo == "EU28") %>% 
  ggplot(aes(x = year, y = value, colour = projection)) + 
  geom_line(aes(linetype = projection)) +
  theme_bw() +
  labs(x = "", colour = "") +
  scale_x_continuous(limits = c(2020, 2060), breaks = c(2020, 2025, 2050))

plot_static <- emissions %>% 
  filter(geo == "EU28") %>% 
  ggplot(aes(x = year, y = value, colour = projection)) + 
  geom_line() +
  theme_bw() +
  labs(x = "", colour = "")

ggplotly(plot_static)

# Emissions
emissions2 <- 
  read_delim("inputs/emissions/env_ac_ainah_r2_1_Data.csv", delim = ",",
             comment = "#") %>% 
  group_by(TIME, GEO) %>% 
  mutate(Value = str_replace_all(Value, "[^0-9.]", ""),
         Value = as.numeric(Value)) %>% 
  summarise(value = sum(Value, na.rm = TRUE)) %>% 
  mutate(value = value / 10^3) %>% 
  filter(TIME %in% 2007:2015) %>% 
  filter(!(GEO %in% c("European Union (current composition)", "Norway", "Turkey", "Serbia")))

emissions2 %>% 
  filter(TIME == 2014) %>% 
  mutate(GEO = str_replace_all(GEO, "\\(until 1990 former territory of the FRG\\)", "")) %>% 
  ggplot(aes(x = reorder(GEO, value), y = value)) +
  geom_point() +
  theme_bw() +
  coord_flip() +
  labs(x = "", y = "Million tonnes of CO2e", title = "GHG emissions in 2014") + 
  theme(axis.text.x = element_text(angle = 45, hjust=1))

ggsave(filename = "outputs/GHG_emissions_by_country.pdf", w = 5, h = 5)
         



