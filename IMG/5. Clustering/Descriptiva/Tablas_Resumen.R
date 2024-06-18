# Instalar las librerías necesarias
# install.packages("ggplot2")
# install.packages("reshape2")
# install.packages("ggplotify")

setwd("C:/Users/rsier/Desktop/MASTERS/LaSalle - MUDS/TFM/IMG/5. Clustering/Descriptiva")

# Cargar las librerías
library(ggplot2)
library(reshape2)
library(ggplotify)

# Jugadores

areas_players <- c(0.4807165, 0.9773679,0.6431914,0.7618550, # Cluster 0
                   0.4978966, 0.7023625, 0.4865687, 0.8044425, # Cluster 1
                   0.4221689, 1.5026961, 0.4671690, 0.6203932, # Cluster 2
                   1.672592, 1.347983, 1.187315, 1.356312, # Cluster 3
                   0.5252032, 0.8653762, 0.5856638, 0.6968065, # Cluster 4
                   1.4941275, 2.1381562, 0.7764619, 1.1956160, # Cluster 5
                   1.7177905, 0.9450792, 0.9537424, 1.2808958, # Cluster 6
                   0.7813351, 1.1103094, 0.7647087, 0.8493205, # Cluster 7
                   0.3081227, 1.1853143, 0.4700035, 0.5931079, # Cluster 8
                   1.1505735, 1.7918442, 0.8744959, 1.1061470, # Cluster 9
                   1.806370, 1.850751, 1.092564, 1.460573, # Cluster 10
                   1.766402, 1.212504, 1.138877, 1.215160, # Cluster 11
                   0.2651935, 1.3940233, 0.3280934, 0.3776701, # Cluster 12
                   0.2651935, 1.3940233, 0.3280934, 0.3776701 # Cluster 13
                   )

data <- matrix(areas_players, nrow=14, ncol=4, byrow = T)
colnames(data) <- c("Generales", "Ataque", "Transiciones", "Defensa")
rownames(data) <- paste("Cluster", 0:13, sep="-")

# Convertir a data frame y reestructurar para ggplot
data_melted <- melt(data)

# Crear el gráfico
p <- ggplot(data_melted, aes(x=Var2, y=Var1)) + 
  geom_tile(aes(fill=value), color="white") + 
  geom_text(aes(label=sprintf("%.1f", value)), color="black", size=3) + # Añadir números
  scale_fill_gradient(low="white", high="dodgerblue4") +
  scale_y_discrete(limits = rev(levels(data_melted$Var1))) + # Invertir el eje y
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), # Mantener texto horizontal
        axis.text.y = element_text(angle = 0), # Mantener texto horizontal
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(1, 1, 1, 1), "cm")) + 
  labs(fill = "Value")

# Exportar el gráfico como imagen
ggsave("tabla_resumen_players.png", plot=as.ggplot(p), width=10, height=6)


# Porteros

areas_GK <- c(1.484962, 1.169856, # Cluster 0
              0.06016747, 0.29031127, # Cluster 1
              0.4089426, 0.5364000 # Cluster 2
              )

data <- matrix(areas_GK, nrow=3, ncol=2, byrow = T)
colnames(data) <- c("Generales", "Portería")
rownames(data) <- paste("Cluster", 0:2, sep="-")

# Convertir a data frame y reestructurar para ggplot
data_melted <- melt(data)

# Crear el gráfico
p <- ggplot(data_melted, aes(x=Var2, y=Var1)) + 
  geom_tile(aes(fill=value), color="white") + 
  geom_text(aes(label=sprintf("%.1f", value)), color="black", size=3) + # Añadir números
  scale_fill_gradient(low="white", high="firebrick3") +
  scale_y_discrete(limits = rev(levels(data_melted$Var1))) + # Invertir el eje y
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), # Mantener texto horizontal
        axis.text.y = element_text(angle = 0), # Mantener texto horizontal
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(1, 1, 1, 1), "cm")) + 
  labs(fill = "Value")

# Exportar el gráfico como imagen
ggsave("tabla_resumen_gk.png", plot=as.ggplot(p), width=10, height=6)



# Equipos

areas_team <- c(0.6941906, 1.3375418, 0.6442214, 1.0770643, 0.4373130, # Cluster 0
                 0.6117714, 0.5843592, 0.8758165, 0.7086936, 1.3266034, # Cluster 1
                 0.8692844, 0.5602205, 0.6093398, 0.6786458, 1.0436169, # Cluster 2
                 0.8420970, 1.0608478, 0.5539426, 0.8296179, 0.5855376, # Cluster 3
                 0.3296221, 0.5158200, 0.2422843, 0.4788368, 0.4796137, # Cluster 4
                 1.1337730, 0.5256614, 0.8993048, 0.7424417, 1.2931936, # Cluster 5
                 0.8353533, 0.6972612, 0.9342649, 0.7983814, 0.8273176 # Cluster 6
                 )

data <- matrix(areas_team, nrow=7, ncol=5, byrow = T)
colnames(data) <- c("Generales", "Ataque", "Transiciones", "Defensa", "Portería")
rownames(data) <- paste("Cluster", 0:6, sep="-")

# Convertir a data frame y reestructurar para ggplot
data_melted <- melt(data)

# Crear el gráfico
p <- ggplot(data_melted, aes(x=Var2, y=Var1)) + 
  geom_tile(aes(fill=value), color="white") + 
  geom_text(aes(label=sprintf("%.1f", value)), color="black", size=3) + # Añadir números
  scale_fill_gradient(low="white", high="mediumpurple") +
  scale_y_discrete(limits = rev(levels(data_melted$Var1))) + # Invertir el eje y
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), # Mantener texto horizontal
        axis.text.y = element_text(angle = 0), # Mantener texto horizontal
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(1, 1, 1, 1), "cm")) + 
  labs(fill = "Value")

# Exportar el gráfico como imagen
ggsave("tabla_resumen_teams.png", plot=as.ggplot(p), width=10, height=6)

