
#Load libraries
library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(tidyr)
library(dplyr)

#Load Datasets
imdb = read.csv(file = "./imdb.csv")
genre = read.csv(file = "./genre.csv")
country = read.csv(file = "./country.csv")
actors = read.csv(file = "./actors.csv")
director = read.csv(file = "./director.csv")
