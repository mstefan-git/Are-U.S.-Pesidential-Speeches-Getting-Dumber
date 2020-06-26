# Replication file for: "Are U.S. Pesidential Speeches Getting Dumber?"
# RPubs-link: https://rpubs.com/mstefan-rpubs/speeches
# (c) Martin Stefan, June 2020

rm(list = ls())

library(tidyverse)
library(ggplot2)

# load data
speeches <- readRDS("speeches.RDS")
df <- readRDS("scores.RDS")

# drop speeches without date
df <- df[complete.cases(df),]

# remove outliers
df <- df %>% filter(date != "1795-07-10")

# flesch reading ease formula (0 to 100)
ggplot(df, aes(date, fres)) + 
  geom_point() + 
  geom_smooth() + 
  labs(title = "Flesh reading ease", 
       y="", x="")

# flesch-kincaid formula (grade level)
ggplot(df, aes(date, fkgl)) + 
  geom_point() + 
  geom_smooth() + 
  labs(title = "Flesh-Kincaid grade level",
       y="", x="")

# automated readability index (grade level)
ggplot(df, aes(date, ari)) + 
  geom_point() + 
  geom_smooth() + 
  labs(title = "Automated readability index", 
       y="", x="")

# coleman-liau index (grade level)
ggplot(df, aes(date, cli)) + 
  geom_point() + 
  geom_smooth()+ 
  labs(title = "Coleman-Liau index", 
       y="", x="")

# linsear write metric (grade level)
ggplot(df, aes(date, lwm)) + 
  geom_point() + 
  geom_smooth() + 
  labs(title = "Linsear Write metric", 
       y="", x="")
