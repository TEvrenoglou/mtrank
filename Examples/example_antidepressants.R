library(tidyverse)
library(netmeta)
library(metafor)
library(meta)
library(PlackettLuce)


setwd("C:\\Users\\Theodoros Evrenoglou\\Desktop\\Ranking\\mtrank")

source("./TCC.R")

source("./mtrank.fit.R")

source("./mtrank.internal.R")

source("./paired.pref.R")

source("./viz.abilities.R")

source("./viz.roe.R")

source("./viz.R")

source("./meta_netmeta-internal.R")

data <- read.csv("./Examples/Data/data_antidepressants.csv")


#### prepare apply the TCC and transform the long format data into treatment preferences 

ranks <- TCC(data = data,
             studlab = studyID,
             event = Responders,
             n = Ntotal,
             treat = drug_name,
             l.roe = 0.80,
             u.roe = 1.25,
             sm = "OR",
             small.values = "undesirable"
             )

## vizualize the TCC for all study-specific comparisons related to treatment escitalopram

viz(ranks,treat = "escitalopram")

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

paired_pref(x=model,treat1 = "bupropion",treat2 = "escitalopram",prob_type = "better")

## calculate the probability that bupropion is tied with escitalopram

paired_pref(x=model,treat1 = "bupropion",treat2 = "escitalopram",prob_type = "tie")

## calculate the probability that bupropion is worse than escitalopram

paired_pref(x=model,treat1 = "bupropion",treat2 = "escitalopram",prob_type = "worse")

## calculate all probabilities together

paired_pref(x=model,treat1 = "bupropion",treat2 = "escitalopram",prob_type = "all")

## vizualize the results

viz(model)

