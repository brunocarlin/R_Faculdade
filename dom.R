# PNAD 2015 domicílios

rm(list = ls())

library(tidyverse)

# cuidado com o tamanho do terceiro campo
col_sizes <- c(4, 2, 6, 3, 2, 2, 2, 1, 1, 1, 1, 2, 2, 1, 12, 12, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 4, 2, 12, 3, 12, 6, 9, 3, 5, 12, 4, 7, 7, 2, 12, 2, 1, 8)

col_names <- c("V0101", "UF", "V0102", "V0103", "V0104", "V0105", "V0106", "V0201", "V0202", "V0203", "V0204", "V0205", "V0206", "V0207", "V0208", "V0209", "V0210", "V0211", "V0212", "V0213", "V0214", "V0215", "V0216", "V2016", "V0217", "V0218", "V0219", "V0220", "V2020", "V0221", "V0222", "V0223", "V0224", "V0225", "V0226", "V0227", "V02270", "V02271", "V02272", "V02273", "V02274", "V2027", "V0228", "V0229", "V0230", "V0231", "V0232", "V02321", "V02322", "V02323", "V02324", "V02325", "V02326", "V02327", "V02424", "V02425", "V02426", "V2032", "V4105", "V4107", "V4600", "V4601", "V4602", "V4604", "V4605", "V4606", "V4607", "V4608", "V4609", "V4610", "V4611", "V4614", "UPA", "V4617", "V4618", "V4620", "V4621", "V4622", "V4624", "V9992")

dom <- read_fwf("DOM2015.txt", fwf_widths(col_sizes, col_names))

unidades <- tibble(UF = c(11, 12, 13, 14, 15, 16, 17, 21, 22, 23, 24, 25, 26, 27, 28, 29, 31, 32, 33, 35, 41, 42, 43, 50, 51, 52, 53),
                   unidade = c("Rondônia", "Acre", "Amazonas", "Roraima", "Pará", "Amapá", "Tocantins", "Maranhão", "Piauí", "Ceará", "Rio Grande do Norte", "Paraíba", "Pernambuco", "Alagoas", "Sergipe", "Bahia", "Minas Gerais", "Espírito Santo", "Rio de Janeiro", "São Paulo", "Paraná", "Santa Catarina", "Rio Grande do Sul", "Mato Grosso do Sul", "Mato Grosso", "Goiás", "Distrito Federal"))

tipo_domicilios <- tibble(cod = c(2, 4, 6),
                          tipo_dom = c("Casa", "Apartamento", "Cômodo"))

dom %>% 
  #filter(UF %in% c(14, 15)) %>% 
  left_join(unidades,by = "UF") %>% 
  left_join(tipo_domicilios, by = c("V0202" = "cod")) %>%
  select(UF,unidade,tipo_dom) %>% 
  filter(!is.na(tipo_dom)) %>% 
  ggplot() +
  aes(unidade, fill = tipo_dom) +
  geom_bar(position = "fill") +
  coord_flip() +
  theme_bw()



dom %>% 
  #filter(UF %in% c(14, 15)) %>% 
  left_join(unidades,by = "UF") %>% 
  left_join(tipo_domicilios, by = c("V0202" = "cod")) %>%
  select(UF,unidade,tipo_dom) %>% 
  filter(!is.na(tipo_dom)) %>% 
  group_by(UF,tipo_dom) %>% 
  summarise(n = n()) %>% 
  mutate(perc = n/sum(n) * 100) %>% 
  View()
  