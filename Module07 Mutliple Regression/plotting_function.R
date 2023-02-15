# plotting function for Australia

# first install packages
#install.packages("ggplot2")
#install.packages("tidyverse")
#install.packages("viridis")
#install.packages("egg")

# load packages
library("ggplot2")
library("viridis")

# Start function
plotting_function <- function(InputData, Predictions){
  Aus_complete <- cbind(InputData, round(Predictions,2))
  colnames(Aus_complete) <- c(colnames(InputData), "Biomass")
  
  breaks <- round(c(min(Predictions)+1, mean(Predictions), 
                    max(Predictions)-1))
  
  Prediction_map <- ggplot(Aus_complete)+
    geom_raster(aes(x=Longitude, y=Latitude, fill=Biomass))+
    theme_void()+
    scale_fill_viridis_c(begin=1, end=0, name ="Relative biomass", option="E",
                         breaks=breaks, 
                         labels=c("L", "M", "H"))+
    theme(legend.position = "top")
  
  return(Prediction_map)
}


