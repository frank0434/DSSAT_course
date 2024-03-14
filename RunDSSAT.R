#### Setup ####

#run DSSAT simulation via batch_file and create output graph

#Credit for DSSAT R Package

#https://cran.r-project.org/web/packages/DSSAT/index.html

#https://cran.r-project.org/web/packages/DSSAT/citation.html

library(DSSAT)
options(DSSAT.CSM="C:/DSSAT48/DSCSM048.exe")
library(dplyr)
library(ggpubr)
library(ggplot2)

#ceres rice example

#set working directory and run simulation

# setwd("C:/DSSAT48/Rice")

filename <- "PULU0001"

batch_file_path <- paste0(getwd(),'/DSSBatch.V48')
write_dssbatch(x=paste0(filename,'.RIX'), trtno=1, file_name = batch_file_path)

run_dssat()
?write_dssbatch


#read in simulation output from PlantGro file

#(contains everything related to plant growth)

#other outputs for phenology etc might be in other files

plantgro <- read_output('PlantGro.OUT')

#create some exemplary plots

#Tops Weight

ggplot(data=plantgro,aes(x=DAP,y=CWAD)) +
  
  geom_line() +
  
  ylab(expression(Tops~Weight~"("*kg[dm]~ha^{-2}*")")) +
  
  xlab("Days After Planting") +
  
  theme_bw() +
  
  theme(legend.position=c(0.15,0.8))

#Grain Weight

ggplot(data=plantgro,aes(x=DAP,y=GWAD)) +
  
  geom_line() +
  
  ylab(expression(Grain~Weight~"("*kg[dm]~ha^{-2}*")")) +
  
  xlab("Days After Planting") +
  
  theme_bw() +
  
  theme(legend.position=c(0.15,0.8))

#Leaf Area Index

ggplot(data=plantgro,aes(x=DAP,y=LAID)) +
  
  geom_line() +
  
  ylab(expression(Leaf~Area~Index~"("*m^2~m^{-2}*")")) +
  
  xlab("Days After Planting") +
  
  theme_bw() +
  
  theme(legend.position=c(0.15,0.8))



