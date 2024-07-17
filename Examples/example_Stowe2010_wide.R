library("mtrank")

data("Stowe2010", package = "netmeta")

data <- Stowe2010

ranks <- tcc(data = data,
             studlab = study,
             mean = list(y1,y2,y3),
             n = list(n1,n2,n3),
             sd = list(sd1,sd2,sd3),
             treat = list(t1, t2, t3),
             mcid=0.5,
             #l.roe = -0.5, # random value
             #u.roe = 0.5, # random value
             sm = "MD",
             small.values = "desirable"
)

# vizualize the TCC for all study-specific comparisons related to treatment escitalopram

forest(ranks, treat = "COMTI")

# vizualize the TCC for all study-specific comparisons in the network (this potentially produces several graphs)

#forest(ranks)

# fit the model
model <- mtrank(ranks) # same results with the "example_Stowe2010_long.R"

# get model summary

model$estimates

# get probabilities that each treatment has the higher ability

model$probabilities

# get estimate for the nuisance parameter v (not interpretable value)

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
