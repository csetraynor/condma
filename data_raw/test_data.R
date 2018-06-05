library(tidyverse)
dat <- escalc(measure="ZCOR", ri=ri, ni=ni, data=dat.molloy2014)

dat.test.molloy2014 <- as.data.frame(dat[,-c(5:10)]) %>%
  rename(study = authors) %>%
  mutate(sei = sqrt(vi))
devtools::use_data(dat.test.molloy2014, overwrite = TRUE)



### http://www.metafor-project.org/doku.php/tips:rma_vs_lm_lme_lmer?s%5B%5D=lme4

# rma(yi, vi*summary(res.lm)$sigma^2, data=dat, method="FE")
#
# coef(summary(res.fe))
#
# coef(summary(res.lm))[1,2] / summary(res.lm)$sigma
#
# res.re <- rma(yi, vi, data=dat)
# res.re
#
# library(lme4)
# res.lmer <- lmer(yi ~ 1 + (1 | study), weights = 1/vi, data=dat,
#                  control=lmerControl(check.nobs.vs.nlev="ignore", check.nobs.vs.nRE="ignore"))
# summary(res.lmer)
#
# rma(yi, vi*res.lme$sigma^2, data=dat)
#
#
#
# library(ggplot2)
# ggplot(dat, aes(x=yi, y=study)) +
#   geom_segment(aes(x = yi-sei*2, xend = yi+sei*2, y=study, yend=study)) +
#   geom_point()
#
#
# library(metafor)
# ma_out <- rma(data = dat, yi = yi, sei = sei, slab = dat$study)
# summary(ma_out)



lung <- read_csv("data_raw/TKI_summary.csv")

extract_hr(lung)
