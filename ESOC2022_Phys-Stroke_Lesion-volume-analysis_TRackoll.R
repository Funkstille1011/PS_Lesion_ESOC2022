####################################################################################################
## European Stroke Organisation Conference 2022
## Poster presentation - Analysis script
## Title: 'BRAIN VOLUMETRIC CHANGES AFTER FOUR WEEK AEROBIC TRAINING IN SUBACUTE STROKE PATIENTS 
##         – SUBGROUP ANALYSES FROM THE PHYS-STROKE TRIAL'
## Author: Torsten Rackoll
## Date final version: 22.03.2022
##
####################################################################################################

## prepare analysis
# load required packages
x <- c("tidyverse", "lme4", "lmerTest", "tableone", "splines") 

lapply(x, require, character.only = T)
rm(x)

# load dataset
load("../Phys-Stroke-Data_ESOC.RData")

## as an example, volumes were adjusted following this formula to acount for individual intracranial volume 
#lesion volume
# regr.les = lm(Bas_Lesion ~ Bas_ICV, data = dat) # regress raw volumes over intracranial volume
# slope.les = summary(regr.les)$coefficients[2,1] # save the slope of the regression
# adjVol <- dat$Bas_Lesion - slope.les * (dat$Bas_ICV - meanICV) # calculate the adjusted volume in mm3
# dat$Bas_Lesion.adj <- adjVol/1000 # implement the volume as ml

##### table 1: baseline characteristics #####
# List numerically coded categorical variables
factorVars <- c("Treatment","Geschlecht","Bas_SA_TOAST_final")
## Create a variable list. 
vars <- c("Bas_Alter", "Geschlecht", "time.stroke.mri", "Bas_NIHSS_su", "Bas_Lesion.adj", 
          "Bas_GM.adj", "Bas_WM.adj", "Bas_10m_GS.adj", "Bas_BI_Gesamt", 
          "Bas_SA_TOAST_final")

tableOne <- CreateTableOne(vars = vars, strata = "Treatment", data = dat2, factorVars = factorVars)
x = print(tableOne, nonnormal = c("Bas_NIHSS_su",  "Bas_10m_GS.adj", "Bas_Lesion.adj", "Bas_BI_Gesamt"))


######### Treatment effect ################################
# change in lesion volume in total group
model1 <- lmer(Lesion.diff ~ mean_bas_lesion_volume + mean_age + mean_bas_fac + Geschlecht + (1|Zentrum), data = dat)
summary(model1,conf.int=TRUE)
confint(model1)

# treatment effect
model2 <- lmer(V1_Lesion.adj ~ Treatment + mean_bas_lesion_volume + mean_age + mean_bas_fac + Geschlecht + (1|Zentrum), data = dat)
summary(model2, conf.int = T)
confint(model2)

# adding time from stroke onset and number of intervention sessions (dichomotized =< 17 vs. >17 sessions)
model2.a <- lmer(V1_Lesion.adj ~ Treatment + mean_bas_lesion_volume + mean_stroke_treatment + int.sessions.dic + mean_age + mean_bas_fac + Geschlecht + (1|Zentrum), data = dat)
summary(model2.a, conf.int = T)
confint(model2.a)


## plot change in lesion volume dependent on time of intervention start from stroke onset per group
a = dat %>% ggplot(aes(x = Stroke.Treatment, y = bas_v2_gs, colour = Treatment)) + geom_point() + geom_smooth(method = "lm") + xlim(0,50)
c = dat %>% ggplot(aes(x = Stroke.Treatment, y = Lesion.diff, colour = Int.sessions)) + geom_point() + ylim(-50, 50) + geom_smooth(method = "lm")

## plot change in lesion volume dependent on number of intervention days per group
b = dat %>% ggplot(aes(x = Int.sessions, y = Lesion.diff, colour = Treatment)) + geom_point() + geom_smooth(method = "lm")


# plot lesion difference
q <- ggplot(dat, aes(Stroke.Treatment, Lesion.diff, colour=Treatment)) +
  geom_point() +
  geom_smooth(size=1.5, linetype="solid", se=TRUE, formula=y ~ ns(x,df = 3), method="lm") +
  theme_bw(base_size=12) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p1 <- q + labs(title = "Difference of lesion volume from pre to post intervention per treatment arm",
         y = "Difference in lesion volume pre/post intervention (ml)",
         x = "Time after stroke onset in days",
         colour = "Treatment arm",
         caption = "Values estimated using linear regression with time after stroke modeled\n
         with natural cubic splines (three degrees of freedom using the r-package splines)") +
  scale_color_manual(values = c( "goldenrod2", "midnightblue")) 


#########################################
## create dataset without outliers
dat.wout.out <- dat[-c(29,54),]

# treatment effect without outliers
model2.b <- lmer(V1_Lesion.adj ~ Treatment + mean_bas_lesion_volume + mean_age + mean_bas_fac + Geschlecht + (1|Zentrum), data = dat.wout.out)
summary(model2.b, conf.int = T)
confint(model2.b)

q <- ggplot(dat.wout.out, aes(Stroke.Treatment, Lesion.diff, colour=Treatment)) +
  # Show raw data
  geom_point() +
  # Compare internal geom_smooth calculations with our models
  geom_smooth(size=1.5, linetype="solid", se=TRUE, formula=y ~ ns(x,df = 3), method="lm") +
  theme_classic()

# Plot model predictions
#  geom_line(data=model.predictions) +


p1 <- q + labs(title = "Difference of lesion volume from pre to post intervention per treatment arm",
               y = "Difference in lesion volume pre/post intervention (ml)",
               x = "Time after stroke onset in days",
               colour = "Treatment arm",
               caption = "Values estimated using linear regression with time after stroke modeled\n
         with natural cubic splines (three degrees of freedom using the r-package splines).\n 
         Two outliers were removed.") +
  scale_color_brewer(palette="Paired") 

ggsave(p1, file="Lesion_volume_per_treatment_wout_outlier.jpg",  width = 30, height = 20, units = "cm", dpi=300)



## association of lesion volume with co-primary endpoints
# gait speed at 3 months post stroke
model3 <- lmer(bas_v2_gs ~ Treatment + mean_bas_gs + mean_bas_lesion_volume + mean_age + mean_bas_fac + Geschlecht + (1|Zentrum), data = dat)
summary(model3, conf.int = T)
cor.test(dat$bas_v2_gs, dat$Bas_Lesion.adj)
cor.test(dat$bas_v2_gs, dat$Lesion.diff)
c = dat %>% ggplot(aes(x = Treatment, y = bas_v2_gs)) + geom_boxplot() 
plot(dat$bas_v2_gs, dat$Lesion.diff)

# and gait speed post intervention 
model4 <- lmer(dat$bas_v1_gs ~ Treatment + dat$mean_bas_gs + mean_bas_lesion_volume + mean_age + mean_bas_fac + Geschlecht + (1|Zentrum), data = dat2)
summary(model4, conf.int = T)