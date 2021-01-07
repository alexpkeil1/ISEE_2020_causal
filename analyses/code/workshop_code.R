######################################################################################################################
# Author: Alex Keil
# Program: workshop_code.R
# Date: 2021-01-07
# Project: ISEE 2020 workshop: Causal inference for environmental mixtures
# Tasks:
# Data in: coalplant
# Description: 
# Released under the GNU General Public License: http://www.gnu.org/copyleft/gpl.html
######################################################################################################################

#(CHANGE THESE PATHNAMES)
outpath = "~/repo/2020_ISEE_causal/analyses/output/"
datapath = "~/repo/2020_ISEE_causal/analyses/data/"
# important packages
# install with: 
# install.packages(c('ggplot2', 'readr', 'dplyr', 'survival', 'tibble', 'boot'), repos = 'https://cran.mtu.edu')



# everything else should work ok without modification, provided that you have installed the packages
library(ggplot2)
library(readr)
library(dplyr)
library(survival)
library(tibble)
library(boot)
library(qgcomp)
# reading in data 
coalplant = read_csv(path.expand(paste0(datapath, "/coalplant2020.csv")))
setwd(path.expand(outpath))
################################################################################
#  Coal plant example - data exploration
################################################################################
head(coalplant,n = 5)
# note: no missing data

# pairwise correlations
round(cor(coalplant[,c("as", "be", "cd")], method="spearman"), 2)

# exposures by race
pbase <- ggplot(data=coalplant) + theme_classic()
prace <- pbase + 
  scale_fill_discrete(name="Black race\nself-report")
p1 <- prace + geom_density(aes(x=as,fill=factor(black), group=factor(black)), alpha=0.4)
p2 <- prace + geom_density(aes(x=be,fill=factor(black), group=factor(black)), alpha=0.4)
p3 <- prace + geom_density(aes(x=cd,fill=factor(black), group=factor(black)), alpha=0.4)
ggsave("as_dens.png", p1, width = 3.5, height=2)
ggsave("be_dens.png", p2, width = 3.5, height=2)
ggsave("cd_dens.png", p3, width = 3.5, height=2)

# univariate outcome
pmdi <- pbase + geom_histogram(aes(x=mdi), bins=30)
ggsave("mdi_dens.png", pmdi, width = 3.5, height=2)


# bivariate scatter plots
pscatter <- pbase + geom_point(aes(x=as,y=be), alpha=0.4)
ggsave("as_be_scatter.png", pscatter, width = 3, height=3)

xr = c(0.5,1.5)
yr = c(1,1)
pscatter_b1 <- pscatter + annotate("segment", x=xr[1], xend=xr[2], y=yr[1], yend=yr[2], color="red", size=2)
xr = c(0.5,1.5)
yr = c(3,3)
pscatter_b2 <- pscatter + annotate("segment", x=xr[1], xend=xr[2], y=yr[1], yend=yr[2], color="red", size=2)
pscatter_b4 <- pscatter_b1 + annotate("segment", x=xr[1], xend=xr[2], y=yr[1], yend=yr[2], color="red", size=2)
xr = c(1.5,2.5)
yr = c(3,3)
pscatter_b3 <- pscatter + annotate("segment", x=xr[1], xend=xr[2], y=yr[1], yend=yr[2], color="red", size=2)
ggsave("as_be_scatter_b1.png", pscatter_b1, width = 3, height=3)
ggsave("as_be_scatter_b2.png", pscatter_b2, width = 3, height=3)
ggsave("as_be_scatter_b3.png", pscatter_b3, width = 3, height=3)
ggsave("as_be_scatter_b4.png", pscatter_b4, width = 3, height=3)

# joint effects
xr = quantile(coalplant$as, probs=c(0.0, 0.25))
yr = quantile(coalplant$be, probs=c(0.0, 0.25))
pscatter_j1 <- pscatter + annotate("segment", x=xr[1], xend=xr[2], y=yr[1], yend=yr[2], color="red", size=2)
pscatter_j5 <- pscatter_j1
xr = quantile(coalplant$as, probs=.25+c(0.0, 0.25))
yr = quantile(coalplant$be, probs=.25+c(0.0, 0.25))
pscatter_j2 <- pscatter + annotate("segment", x=xr[1], xend=xr[2], y=yr[1], yend=yr[2], color="red", size=2)
pscatter_j5 <- pscatter_j5 + annotate("segment", x=xr[1], xend=xr[2], y=yr[1], yend=yr[2], color="red", size=2) 
xr = quantile(coalplant$as, probs=.5+c(0.0, 0.25))
yr = quantile(coalplant$be, probs=.5+c(0.0, 0.25))
pscatter_j3 <- pscatter + annotate("segment", x=xr[1], xend=xr[2], y=yr[1], yend=yr[2], color="red", size=2)
pscatter_j5 <- pscatter_j5 + annotate("segment", x=xr[1], xend=xr[2], y=yr[1], yend=yr[2], color="red", size=2) 
xr = quantile(coalplant$as, probs=.75+c(0.0, 0.25))
yr = quantile(coalplant$be, probs=.75+c(0.0, 0.25))
pscatter_j4 <- pscatter + annotate("segment", x=xr[1], xend=xr[2], y=yr[1], yend=yr[2], color="red", size=2)
pscatter_j5 <- pscatter_j5 + annotate("segment", x=xr[1], xend=xr[2], y=yr[1], yend=yr[2], color="red", size=2) 
ggsave("as_be_scatter_j1.png", pscatter_j1, width = 3, height=3)
ggsave("as_be_scatter_j2.png", pscatter_j2, width = 3, height=3)
ggsave("as_be_scatter_j3.png", pscatter_j3, width = 3, height=3)
ggsave("as_be_scatter_j4.png", pscatter_j4, width = 3, height=3)
ggsave("as_be_scatter_j5.png", pscatter_j5, width = 3, height=3)


# indicator variable regression
indvardat <- qgcomp::quantize(data=coalplant, expnms = c("as", "be"))$data
indvarmod <- glm(mdi ~ factor(as) + factor(be), data = indvardat)
preds <- predict(indvarmod, newdata = data.frame(as=0:3, be=0:3))
pindvar <- pbase + annotate("step", x=0:3, y=preds) +  annotate("point", x=0:3, y=preds) + 
  scale_y_continuous(name="E(mdi)")+ scale_x_continuous(name="Quartile of as, be")
pindvar_2 <- pbase + annotate("step", x=0:3, y=preds) +  annotate("point", x=0:3, y=preds) + 
  scale_y_continuous(name="E(mdi)")+ scale_x_continuous(name="Quartile of as, be")
ggsave("as_be_regline_1.png", pindvar, width = 3, height=3)
ggsave("as_be_regline_2.png", pindvar_2, width = 3, height=3)

# quantile g-computation
qgmod <- qgcomp.noboot(mdi ~ as + be, expnms=c("as", "be"), data=coalplant)
qgmod2 <- qgcomp.boot(mdi ~ factor(as) + factor(be), degree=2, expnms=c("as", "be"), data=coalplant, B=2)
qgpreds <- qgmod$coef[1] + qgmod$coef[2]*(0:3)
xsmooth = seq(0,2.9,0.01)
qgpreds_nl <- qgmod2$coef[1] + qgmod2$coef[2]*(xsmooth) + qgmod2$coef[3]*(xsmooth^2)
pqgc1 <- pindvar_2 + geom_line(aes(x,y, color="Linear"), data = data.frame(x=0:3, y=qgpreds)) + 
  scale_color_discrete(name="Qgcomp")
pqgc2 <- pqgc1 + geom_line(aes(x,y, color="Quad."), data = data.frame(x=xsmooth, y=qgpreds_nl))
ggsave("as_be_regline_qgc1.png", pqgc1, width = 3, height=3)
ggsave("as_be_regline_qgc2.png", pqgc2, width = 3, height=3)

################################################################################
#  Coal plant example - parametric g-formula with point exposure
################################################################################
# MDI (mental development index) appears to be roughly normally distributed
#  so we will model using a linear model

mdimod <- glm(mdi ~ as*urbanicity + be*urbanicity + cd*urbanicity + as*black + be*black + cd*black + as*be + as*cd + be*cd, data=coalplant)

# quick prediction at the median of exposures
preddata <- coalplant
preddata$as  <- median(coalplant$as)
preddata$be <- median(coalplant$be)
preddata$cd <- median(coalplant$cd)
preddata$pred_med <- predict(mdimod, newdata = preddata)
head(preddata, n=5)
# quick summary
mean(preddata$pred_med)
mean(coalplant$mdi)
mean(filter(preddata, black==1)$pred_med)
mean(filter(preddata, black==1)$mdi)

mean(preddata$pred_med)-mean(coalplant$mdi)

mean(filter(preddata, black==1)$pred_med) - mean(filter(preddata, black==1)$mdi)

# underlying model coefficients
summary(mdimod)

# Say we know based on local emissions data and discussions with atmospheric
#  chemists that 91% of As, 96% of Be, and 45% of Cd ambient levels come
#  from a local coal-fired power plant


gformula_means <- function(ymodel, data, ...){
   # function to calculate mean MDI under a given intervention
   require('dplyr')
   # if only 'ymodel' and 'data' are supplied, then natural course is fit
  postinterventionX <- mutate(data,...)
  mean(predict(ymodel, newdata=postinterventionX))
}

#point estimates for mean MDI under each intervention
nc = gformula_means(mdimod, data=coalplant)
no_coal = gformula_means(mdimod, data=coalplant, 
                         cd=cd*(1-0.5), as=as*(1-0.96), be=be*(1-0.91))
no_cd = gformula_means(mdimod, data=coalplant, cd=0)
no_as = gformula_means(mdimod, data=coalplant, as=0)
no_be = gformula_means(mdimod, data=coalplant, be=0)
# expected population mean MDI
print(c(nc=nc, no_coal=no_coal, no_cd=no_cd, no_as=no_as, no_be=no_be), 4)

# bootstrap to get confidence intervals on effects (boot function also gives point estimates from original data)
gformula_meandiffs <- function(data, index){
  mcsample = data[index,]
  bootmod = glm(mdi ~ as*urbanicity + be*urbanicity + cd*urbanicity + as*black + be*black + cd*black + as*be + as*cd + be*cd, data=mcsample)
  #bootmod = glm(mdi ~ as*urbanicity + be*urbanicity + cd*urbanicity + as*be + as*cd + be*cd, data=mcsample)
  nc = gformula_means(bootmod, data=mcsample)
  no_coal = gformula_means(bootmod, data=mcsample, cd=cd*(1-0.5), as=as*(1-0.96), be=be*(1-0.91))
  no_cd = gformula_means(bootmod, data=mcsample, cd=0)
  no_as = gformula_means(bootmod, data=mcsample, as=0)
  no_be = gformula_means(bootmod, data=mcsample, be=0)
  nc = gformula_means(bootmod, data=mcsample)
  # stratified
  nc_bl = gformula_means(bootmod, data=filter(mcsample, black==1))
  no_coal_bl = gformula_means(bootmod, data=filter(mcsample, black==1), cd=cd*(1-0.5), as=as*(1-0.96), be=be*(1-0.91))
  no_cd_bl = gformula_means(bootmod, data=filter(mcsample, black==1), cd=0)
  no_as_bl = gformula_means(bootmod, data=filter(mcsample, black==1), as=0)
  no_be_bl = gformula_means(bootmod, data=filter(mcsample, black==1), be=0)
  #
  nc_nb = gformula_means(bootmod, data=filter(mcsample, black==0))
  no_coal_nb = gformula_means(bootmod, data=filter(mcsample, black==0), cd=cd*(1-0.5), as=as*(1-0.96), be=be*(1-0.91))
  no_cd_nb = gformula_means(bootmod, data=filter(mcsample, black==0), cd=0)
  no_as_nb = gformula_means(bootmod, data=filter(mcsample, black==0), as=0)
  no_be_nb = gformula_means(bootmod, data=filter(mcsample, black==0), be=0)
  c(
    nc_mdi=nc, shutdown=no_coal-nc, attrmdi_cd=no_cd-nc, attrmdi_as=no_as-nc, attrmdi_be=no_be-nc,
    nc_mdi_bl=nc_bl, shutdown_bl=no_coal_bl-nc_bl, attrmdi_cd_bl=no_cd_bl-nc_bl, attrmdi_as_bl=no_as_bl-nc_bl, attrmdi_be_bl=no_be_bl-nc_bl,
    nc_mdi_nb=nc_nb, shutdown_nb=no_coal_nb-nc_nb, attrmdi_cd_nb=no_cd_nb-nc_nb, attrmdi_as_nb=no_as_nb-nc_nb, attrmdi_be_nb=no_be_nb-nc_nb
  )
}

# use non-parametric bootstrap to get confidence intervals (from "boot" package)
set.seed(12321)
bootsamples <- boot(data=coalplant, statistic=gformula_meandiffs, R=500)

# effect estimates with confidence intervals
se = apply(bootsamples$t, 2, sd)
print(cbind(estimate=bootsamples$t0, lowerCI=bootsamples$t0-1.96*se, upperCI=bootsamples$t0+1.96*se), 3)

qgcomp.noboot(f=mdi ~ cd + as + be + black + urbanicity, expnms = c("as", "cd", "be"), data = coalplant)
qgcomp.noboot(f=mdi ~ cd + as + be + urbanicity, expnms = c("as", "cd", "be"), data = filter(coalplant, black==1))
qgcomp.noboot(f=mdi ~ cd + as + be + urbanicity, expnms = c("as", "cd", "be"), data = filter(coalplant, black==0))
