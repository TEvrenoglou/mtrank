library(tidyverse)
library(netmeta)
library(metafor)
library(meta)
library(PlackettLuce)


setwd("./mtrank")

source("./tcc.R")

source("./mtrank.fit.R")

source("./mtrank.internal.R")

source("./paired.pref.R")

source("./viz.abilities.R")

source("./viz.roe.R")

source("./viz.R")

source("./meta_netmeta-internal.R")

data <- read.csv("./Examples/Data/data_Stowe2010.csv")


#### prepare apply the TCC and transform the long format data into treatment preferences 

ranks <- tcc(data = data,
             studlab = study,
             mean = y,
             n = n,
             sd = sd,
             treat = t,
             mcid = 0.5,
             #l.roe = -0.5,
             #u.roe = 0.5,
             sm = "MD",
             small.values = "desirable"
)


## vizualize the TCC for all study-specific comparisons related to treatment escitalopram

viz(ranks,treat = "COMTI")

## vizualize the TCC for all study-specific comparisons in the network (this potentially produces several graphs)

viz(ranks)


## fit the model
model <- mtrank(ranks)

## get model summary

model$estimates

## get probabilities that each treatment has the higher ability

model$probabilities

## get estimate for the nuisance parameter v (not interpretable value)

model$v

## calculate the probability that bupropion is better than escitalopram

paired_pref(x=model,treat1 = "MAOBI",treat2 = "Dopamine Agonist",prob_type = "better")

## calculate the probability that bupropion is tied with escitalopram

paired_pref(x=model,treat1 = "MAOBI",treat2 = "Dopamine Agonist",prob_type = "tie")

## calculate the probability that bupropion is worse than escitalopram

paired_pref(x=model,treat1 = "MAOBI",treat2 = "Dopamine Agonist",prob_type = "worse")

## calculate all probabilities together

paired_pref(x=model,treat1 = "MAOBI",treat2 = "Dopamine Agonist",prob_type = "all")

## vizualize the results

viz(model)


