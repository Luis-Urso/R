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


# Load Death Data (SIM)

data_sim_mg <- fetch_datasus(year_start = 2019,
                             year_end = 2019,
                             uf = "MG",
                             information_system = "SIM-DO")

# Load Hospital Centralized Data (SIH)

data_sih_mg <- fetch_datasus(year_start = 2019,
                             year_end = 2019,
                             month_start = 1,
                             month_end = 12,
                             uf = "MG",
                             information_system = "SIH-RD")

# Load Birth Data (SINASC)

data_sinasc_mg <- fetch_datasus(year_start = 2019,
                                year_end = 2019,
                                uf = "MG",
                                information_system = "SINASC")


# Load Sistema de Informações Ambulatoriais - APAC Medicamentos
# Outras APACs:
#  'AB': ('APAC de Cirurgia Bariátrica', 1, 2008),
#  'ACF': ('APAC de Confecção de Fístula', 1, 2008),
#  'AD': ('APAC de Laudos Diversos', 1, 2008),
#  'AM': ('APAC de Medicamentos', 1, 2008),
#  'AMP': ('APAC de Acompanhamento Multiprofissional', 1, 2008),
#  'AN': ('APAC de Nefrologia', 1, 2008),
#  'AQ': ('APAC de Quimioterapia', 1, 2008),
#  'AR': ('APAC de Radioterapia', 1, 2008),
#  'ATD': ('APAC de Tratamento Dialítico', 1, 2008),
#  'BI': ('Boletim de Produção Ambulatorial individualizado', 1, 2008),
#  'PA': ('Produção Ambulatorial', 7, 1994),
#  'PS': ('RAAS Psicossocial', 1, 2008),
#  'SAD': ('RAAS de Atenção Domiciliar', 1, 2008)


data_sia_mg <- fetch_datasus(year_start = 2022,
                             year_end = 2022,
                             month_start = 01,
                             month_end = 12,
                             uf = "SP",
                             information_system = "SIA-AM")


data_sia_mg_filtered <- filter(data_sia_mg, 
                                 AP_CIDPRI=="M050" | 
                                 AP_CIDPRI=="M051" |
                                 AP_CIDPRI=="M052" |
                                 AP_CIDPRI=="M053" |
                                 AP_CIDPRI=="M058" |
                                 AP_CIDPRI=="M059" |
                                 AP_CIDPRI=="M060" |
                                 AP_CIDPRI=="M061" |
                                 AP_CIDPRI=="M062" |
                                 AP_CIDPRI=="M063" |
                                 AP_CIDPRI=="M064" |
                                 AP_CIDPRI=="M068" | 
                                 AP_CIDPRI=="M069")

## Plot the SIA 

data_sia_mg_filtered %>%
  count(AP_NUIDADE) %>%
  ggplot(aes(x = AP_NUIDADE, y = n,
             fill = AP_NUIDADE,
             color = AP_NUIDADE,
             label = n))+
  geom_bar(stat = "identity")+
  geom_label(color = "black")+
  labs(title = "Casos Artrite Reumatoide em SP (Soropositiva e Soronegativa)",
       subtitle = "Ano de 2022",
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


