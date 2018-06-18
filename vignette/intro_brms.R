
# Load libraries
library(metafor)
library(lme4)
library(brms)
library(brmstools)
library(tidyverse)
library(ggplot2)
theme_set(theme_bw())
devtools::document()
#####################
# Load data
data("dat.test.molloy2014")
dat <- dat.test.molloy2014

ggplot(dat, aes(x=yi, y=study)) +
  geom_segment(aes(x = yi-sei*2, xend = yi+sei*2, y=study, yend=study)) +
  geom_point()

# Fixed effect Model

res.fe <- rma(yi, vi, data=as.data.frame(dat), method="FE")
summary(res.fe)
funnel(res.fe, xlab = "Log odds ratio")
radial(res.fe)
metafor::forest(res.fe)
# Random-Effects Model

res.re <- rma(data = dat, yi = yi, vi = vi, slab = dat$study)
summary(res.re )
metafor::forest(res.re)

# Bayesian (very weak informative prior, ~ noninformative prior)
class(dat)

#akin to fixed effect
bm.1 <-ic_meta(dat, hetero_var = FALSE, iter = 5000, warmup = 2000, cores = 4)

#akin to random effect
bm.2 <-ic_meta(dat, hetero_var = TRUE, iter = 5000, warmup = 2000, cores = 4)

(prior <- get_prior(yi | se(sei) ~ 1 + (1|study),
                    data = dat))
summary(bm.out)
bm.out2 <- extract.model(bm.2)



cat( (sum( avg_es(bm.out) > 0.2) / length( avg_es(bm.out) ))*100, "%")

bm.out <- extract.model(bm.out)
brmstools::forest(bm.out2,
       show_data = TRUE,
       av_name = "Effect size")
