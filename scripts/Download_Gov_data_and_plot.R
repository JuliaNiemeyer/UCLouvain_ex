########### Researcher/Data Scientist position – UCLouvain ###################
######## download data from the ‘API de Compras Governamentais’ and count the  ###################
######## number of meatpackers in each state which are suppliers of the government.  ###################

### Exercise by Julia Niemeyer to UCLouvain - 17/09/2024


##########################################################################
## 1. Install and load libraries
# install.packages("tidyverse")
library(tidyverse)
library(ggplot2)

## 2. Load data from API Compras Governamentais
# Frigorificos
frigo <- read_csv("https://compras.dados.gov.br/fornecedores/v1/fornecedores.csv?id_cnae=1011201")
# Matadouto
matad <- read_csv("https://compras.dados.gov.br/fornecedores/v1/fornecedores.csv?id_cnae=1011205")
# Fabricação
fab <- read_csv("https://compras.dados.gov.br/fornecedores/v1/fornecedores.csv?id_cnae=1013901")
# Preparação
prep <- read_csv("https://compras.dados.gov.br/fornecedores/v1/fornecedores.csv?id_cnae=1013902")

## 2. prepare data
# merge tables
merged <- bind_rows(frigo, matad, fab, prep)

## group by UF and count how many suppliers each one has
id_per_UF <- merged %>%
  group_by(UF) %>%
  summarise(unique_ids = n_distinct(Id))

######## 3. Create barchart
# 3.1. create barplot of the total number of suppliers
plot <- id_per_UF %>%
  ggplot(aes(x = UF, y = unique_ids)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  labs(title = "Suppliers per state", x = "States", y = "Number of suppliers") +
  theme_minimal(base_size = 15) +  
  theme(
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),  
    axis.title = element_text(face = "bold"), 
    axis.text = element_text(color = "gray20"),
    #legend.position = element_blank(),
    legend.position = "none"  # Remove the legend
  ) 


## Export barplot as image
ggsave("./results/Suppliers_state.jpeg", plot, width = 8, height = 6, dpi = 400)


###3.2. Create a second option of barplot - the higher the number of supplier, the darker the green
plot2 <- ggplot(id_per_UF, aes(x = UF, y = unique_ids, fill = unique_ids)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen") +
  labs(title = "Suppliers per state", x = "States", y = "Number of suppliers", fill = "Number of suppliers") +
  theme_minimal(base_size = 15) +  
  theme(
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),  
    axis.title = element_text(face = "bold", size = 10), 
    axis.text = element_text(color = "gray20", size = 10),
    legend.title = element_text(size = 12, face = "bold"),  # Customize legend title appearance
    legend.text = element_text(size = 10)  # Customize legend text appearance
  ) 

## Export barplot as image
ggsave("./results/Suppliers_state_2.jpeg", plot2, width = 10, height = 6, dpi = 400)

