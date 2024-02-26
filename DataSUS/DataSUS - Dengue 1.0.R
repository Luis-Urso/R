##
## Data Collection Basics
## by Luis Urso
##

##
## Import Libraries / Setup Libraries
##

install.packages("devtools")
remotes::install_github("rfsaldanha/microdatasus")

library(microdatasus)
library(tidyverse)


##
## DATA LOADING 
##


# Load Death Data (SINAN-DENGUE


data_dengue <- fetch_datasus(year_start = 2023,
                             year_end = 2024,
                             month_start = 01,
                             month_end = 02,
                             information_system = "SINAN-DENGUE")


data_dengue %>% count(ID_AGRAVO)

## Plot the SIA 

data_dengue %>%
  count(DT_NOTIFIC) %>%
  ggplot(aes(x = DT_NOTIFIC, y = n,
             fill = DT_NOTIFIC,
             color = DT_NOTIFIC,
             label = n))+
  geom_bar(stat = "identity")+
  geom_label(color = "black")+
  labs(title = "Evolucao de Casos de Dengue em 2023 e 2024",
       subtitle = "Fonte: DATASUS - SINAN-DENGUE",
       x = "",
       y = "",
       caption = "by Luis A. Urso")+
  theme_minimal()+
  theme(legend.position = "none")



# Load National Est. Base (CNES)

data_CNES_ST <- fetch_datasus(year_start = 2018,
                              year_end = 2018,
                              month_start = 1,
                              month_end = 12,
                              uf = "MG",
                              information_system = "CNES-ST")


cnes_geral <- microdatasus::cadger

cnes_geral_filter <- filter(cnes_geral, CNES=="2218852")


###
### DATA TREATMENT/ TRANSFORMATION 
###


# Data Treatment - SIM

sim_mg <- process_sim(data_sim_mg)

# Data Treatment - SIH

sih_mg <- process_sih(data_sih_mg)

# Data Treatment - SINASC

sinasc_mg <- process_sinasc(data_sinasc_mg)

# Data Treatment - CNES

cnes_st_mg <- process_cnes(data_CNES_ST,information_system = "CNES-ST")

# Filter Data in scope for the Graphics 

sinasc <- sinasc_mg %>%
  select(ESTCIVMAE, SEXO) %>%
  na.omit()


# Visualize the Graphics 

sinasc %>%
  count(ESTCIVMAE) %>%
  ggplot(aes(x = ESTCIVMAE, y = n,
             fill = ESTCIVMAE,
             color = ESTCIVMAE,
             label = n))+
  geom_bar(stat = "identity")+
  geom_label(color = "black")+
  labs(title = "Estado Civil das Mães de Nascidos em Minas Gerais",
       subtitle = "ano de 2019",
       x = "",
       y = "",
       caption = "Elaborado por analisemacro.com.br com dados do DATASUS")+
  theme_minimal()+
  theme(legend.position = "none")


