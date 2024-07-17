library("mtrank")

data <- read.csv("./Data/data_Stowe2010.csv")


# prepare apply the TCC and transform the long format data into treatment preferences 

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
             small.values = "desirable")

# vizualize the TCC for all study-specific comparisons related to treatment escitalopram

forest(ranks, treat = "COMTI")

# vizualize the TCC for all study-specific comparisons in the network (this potentially produces several graphs)

forest(ranks)


# fit the model
model <- mtrank(ranks)

# get model summary

model$estimates

## get probabilities that each treatment has the higher ability

model$probabilities

## get estimate for the nuisance parameter v (not interpretable value)

model$v

# calculate the probability that bupropion is better than escitalopram

paired_pref(model, treat1 = "MAOBI", treat2 = "Dopamine Agonist",prob_type = "better")

# calculate the probability that bupropion is tied with escitalopram

paired_pref(model, treat1 = "MAOBI", treat2 = "Dopamine Agonist",prob_type = "tie")

# calculate the probability that bupropion is worse than escitalopram

paired_pref(model, treat1 = "MAOBI", treat2 = "Dopamine Agonist",prob_type = "worse")

# calculate all probabilities together

paired_pref(model, treat1 = "MAOBI", treat2 = "Dopamine Agonist",prob_type = "all")

# vizualize the results

forest(model)
