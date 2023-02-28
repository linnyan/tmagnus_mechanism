
#### start----
###packages
#devtools::install_github("tomwenseleers/export")
library(grid)
library(tidyverse)
library(lme4)
library(ggplot2)
library(car)
library(emmeans)
library(lmerTest)
library(MuMIn)
library(arm)
library(devtools)
library(plyr)
#install.packages("AER")
#install.packages("pscl")
library(AER)
library(pscl)
#library(export)
#install.packages("Rtools42")
#colorblind pellete
cbPalette <- c( "#CC79A7","#E69F00", "#009E73","grey")

###data and subsets
remove = read.csv("datasheet.csv")
summary(remove)
remove$species = as.factor(remove$species)
remove$treatment = as.factor(remove$treatment)
#exclude HA since already have PV
remove = subset(remove, species != "HA")
remove_re = subset(remove, treatment == "remove")
remove_TM = subset(remove, species == "TM")
remove_TM_re = subset(remove_TM, treatment == "remove")
remove_MA_re = subset(remove_re, species == "MA")
remove_MP_re = subset(remove_re, species == "MP")
remove_PV_re = subset(remove_re, species == "PV")
remove_re$species = factor(remove_re$species , levels=c("TM","MA","MP","PV"))
remove_re_1 = subset(remove_re,sac_num == 1)

# remove outlier with duration of 39

###test plots
ggplot(data = remove_re, aes(x = species, y = dur,fill = species))+
  geom_violin()+theme_classic()
ggplot(data = remove_re, aes(x = species, y = eggs,fill = species))+
  geom_violin()+theme_classic()
ggplot(data = remove_re, aes(x = as.factor(sac_num), y = dur,fill = species))+
  geom_boxplot()+theme_classic()


remove_con_sacall = subset(remove, treatment == "control")
remove_con_sacall_1 = subset(remove_con_sacall, sac_num == 1)
remove_con_sacall_1 = subset(remove_con_sacall_1, species != "MA")
remove_con_sacall_1 = subset(remove_con_sacall_1, species != "MP")
remove_con_sacall_tm = subset(remove_con_sacall, species == "TM")

ggplot(data = remove_con_sacall_tm, aes(x = as.factor(sac_num), y = dur))+
  geom_boxplot()+theme_classic()
kruskal.test(dur~sac_num, data = remove_con_sacall_tm)#***

ggplot(data = remove_con_sacall_1, aes(x = species, y = sac_total,color = species))+
  geom_boxplot()+theme_classic()

#bar
remove_con_sacall_1$species = factor(remove_con_sacall_1$species , levels=c("TM","PV"))

df <- ddply(remove_con_sacall_1, c("species"), summarize, Mean = mean(sac_total), SD = sd(sac_total))
a = ggplot(df, aes(x = species, y = Mean,fill = species)) +
  geom_bar(stat = "identity",alpha = .5,color = "black") + 
  # add 68% CI errorbar 
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2,color = "grey40")+
  theme_classic()+
  scale_color_manual(values=c("#CC79A7","grey"))+
  scale_fill_manual(values=c("#CC79A7","grey"))+ylim(0,7.5)
#density
b = ggplot(data = remove_con_sacall_1, aes(x = sac_total,fill = species))+
  geom_density(data = remove_con_sacall_1, aes(x = sac_total,fill = species),alpha = .5)+
  theme_classic()+coord_flip()+scale_fill_manual(values=c("#CC79A7","grey"))+
  scale_y_reverse()+xlim(0,7.5)

cowplot::plot_grid(b+theme(legend.position = "none",
                           axis.text.y = element_blank(),
                           axis.line.y = element_blank(),
                           axis.ticks.y = element_blank()
),
a+theme(axis.title.y = element_blank()),
nrow = 1, rel_widths = c(1,3))
kruskal.test(sac_total~species, data = remove_con_sacall_1)#***
hist(remove_con_sacall_1$sac_total)

dtest = glmer(sac_total~species+(1|ID), data = remove_con_sacall_1,family = poisson)
summary(dtest)
qqPlot(resid(dtest))
chisq <- sum(resid(dtest, type='pearson')^2)
chisq/df.residual(dtest)#overdispersion fine
dispersiontest(dtest)
### general plots ----
## total sac----
remove_re$species = factor(remove_re$species , levels=c("TM","MA","MP","PV"))
#box
ggplot(data = remove_re_1, aes(x = species, y = sac_total,color = species, fill = species))+
  geom_boxplot(alpha = 0.5)+theme_classic()+
  scale_color_manual(values=cbPalette)+
  scale_fill_manual(values=cbPalette)

#bar
df <- ddply(remove_re_1, c("species"), summarize, Mean = mean(sac_total), SD = sd(sac_total))
a = ggplot(df, aes(x = species, y = Mean,fill = species)) +
  geom_bar(stat = "identity",alpha = .5,color = "black") + 
  # add 68% CI errorbar 
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2,color = "grey40")+
  theme_classic()+
  scale_color_manual(values=cbPalette)+
  scale_fill_manual(values=cbPalette)+ylim(0,41)
#density
b = ggplot(data = remove_re_1, aes(x = sac_total,fill = species))+
  geom_density(data = remove_re_1, aes(x = sac_total,fill = species),alpha = .3)+
  theme_classic()+coord_flip()+scale_fill_manual(values=cbPalette)+
  scale_y_reverse()+xlim(0,41)

cowplot::plot_grid(b+theme(legend.position = "none",
                           axis.text.y = element_blank(),
                           axis.line.y = element_blank(),
                           axis.ticks.y = element_blank()
),
a+theme(axis.title.y = element_blank()),
nrow = 1, rel_widths = c(1,3))
graph2ppt(file = "species-total sac_bar.pptx",width = 7,height = 5)


remove_TM_re_1 = subset(remove_TM_re,sac_num == 1)
mean(remove_TM_re_1$sac_total)
sd(remove_TM_re_1$sac_total)
remove_MA_re_1 = subset(remove_MA_re,sac_num == 1)
mean(remove_MA_re_1$sac_total)
sd(remove_MA_re_1$sac_total)
remove_MP_re_1 = subset(remove_MP_re,sac_num == 1)
mean(remove_MP_re_1$sac_total)
sd(remove_MP_re_1$sac_total)
mod_sac1 = lm(sac_total~species+exp_dur, data = remove_re_1)
summary(mod_sac1)
Anova(mod_sac1)
qqPlot(resid(mod_sac1))
mod_sac2 = lm(log(sac_total)~species+exp_dur, data = remove_re_1)
summary(mod_sac2)
Anova(mod_sac2)
qqPlot(resid(mod_sac2))
mod_sac3 = lmer(log(sac_total)~species+(1|exp_dur), data = remove_re_1)
summary(mod_sac3)
Anova(mod_sac3)
qqPlot(resid(mod_sac3))
mod_sac4 = lmer(log(sac_total)~species+(1|ID), data = remove_re_1)
summary(mod_sac4)
Anova(mod_sac4)
qqPlot(resid(mod_sac4))
emmeans(mod_sac2,pairwise~species)#all ***
AIC(mod_sac1,mod_sac2,mod_sac3)#3

mod_test = lm(sac_total~exp_dur, data = remove_re)
summary(mod_test)
Anova(mod_test)
qqPlot(resid(mod_test))
kruskal.test(sac_total~species, data = remove_re)#***
wilcox.test(remove_TM_re_1$sac_total,remove_MA_re_1$sac_total,alternative = "two.sided")#***
wilcox.test(remove_MP_re_1$sac_total,remove_MA_re_1$sac_total,alternative = "two.sided")#***
wilcox.test(remove_TM_re_1$sac_total,remove_MP_re_1$sac_total,alternative = "two.sided")#***

##(not useful)repro span----
#box
ggplot(data = remove_re_1, aes(x = species, y = exp_dur,fill = species))+
  geom_boxplot(alpha = 0.5)+theme_classic()+
  scale_color_manual(values=cbPalette)+
  scale_fill_manual(values=cbPalette)
#bar
df <- ddply(remove_re_1, c("species"), summarize, Mean = mean(exp_dur), SD = sd(exp_dur))
a = ggplot(df, aes(x = species, y = Mean,fill = species)) +
  geom_bar(stat = "identity",alpha = .5,color = "black") + 
  # add 68% CI errorbar 
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2,color = "grey40")+
  theme_classic()+
  scale_color_manual(values=cbPalette)+
  scale_fill_manual(values=cbPalette)+ylim(0,136)
#density
b = ggplot(data = remove_re_1, aes(x = exp_dur,fill = species))+
  geom_density(data = remove_re_1, aes(x = exp_dur,fill = species),alpha = .3)+
  theme_classic()+coord_flip()+scale_fill_manual(values=cbPalette)+
  scale_y_reverse()+xlim(0,136)

cowplot::plot_grid(b+theme(legend.position = "none",
                           axis.text.y = element_blank(),
                           axis.line.y = element_blank(),
                           axis.ticks.y = element_blank()
),
a+theme(axis.title.y = element_blank()),
nrow = 1, rel_widths = c(1,3))
graph2ppt(file = "species-experiment duration_bar.pptx",width = 7,height = 5)

mod_exp1 = lm(log(exp_dur)~species, data = remove_re_1)
summary(mod_exp1)
Anova(mod_exp1)
qqPlot(resid(mod_exp1))
mod_exp2 = lm(log(exp_dur)~species*sac_num, data = remove_re_1)
summary(mod_exp2)
Anova(mod_exp2)
qqPlot(resid(mod_exp2))
mod_exp3 = lmer(log(exp_dur)~species*sac_num+(1|ID), data = remove_re_1)
summary(mod_exp3)
Anova(mod_exp3)
qqPlot(resid(mod_exp3))

mod_exp4 = lmer(log(exp_dur)~species+(1|ID), data = remove_re_1)
summary(mod_exp4)
Anova(mod_exp4)
qqPlot(resid(mod_exp4))
AIC(mod_exp1,mod_exp2)#4
anova(mod_exp2,mod_exp1)
emmeans(mod_exp1,pairwise~species)#all NS

kruskal.test(exp_dur~species, data = remove_re)#***
wilcox.test(remove_TM_re_1$exp_dur,remove_MA_re_1$exp_dur,alternative = "two.sided")#**
wilcox.test(remove_MP_re_1$exp_dur,remove_MA_re_1$exp_dur,alternative = "two.sided")#***
wilcox.test(remove_TM_re_1$exp_dur,remove_MP_re_1$exp_dur,alternative = "two.sided")#*

length(remove_TM_re_1$exp_dur)
mean(remove_TM_re_1$exp_dur)
sd(remove_TM_re_1$exp_dur)

length(remove_MA_re_1$exp_dur)
mean(remove_MA_re_1$exp_dur)
sd(remove_MA_re_1$exp_dur)

length(remove_MP_re_1$exp_dur)
mean(remove_MP_re_1$exp_dur)
sd(remove_MP_re_1$exp_dur)


#(not useful)fecundity egg total----
#bar
df <- ddply(remove_re_1, c("species"), summarize, Mean = mean(egg_total), SD = sd(egg_total))
a = ggplot(df, aes(x = species, y = Mean,fill = species)) +
  geom_bar(stat = "identity",alpha = .5,color = "black") + 
  # add 68% CI errorbar 
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2,color = "grey40")+
  theme_classic()+
  scale_color_manual(values=cbPalette)+
  scale_fill_manual(values=cbPalette)+ylim(0,788)
#density
b = ggplot(data = remove_re_1, aes(x = egg_total,fill = species))+
  geom_density(data = remove_re_1, aes(x = egg_total,fill = species),alpha = .3)+
  theme_classic()+coord_flip()+scale_fill_manual(values=cbPalette)+
  scale_y_reverse()+xlim(0,788)

cowplot::plot_grid(b+theme(legend.position = "none",
                           axis.text.y = element_blank(),
                           axis.line.y = element_blank(),
                           axis.ticks.y = element_blank()
                           ),
                   a+theme(axis.title.y = element_blank()),
                   nrow = 1, rel_widths = c(1,3))

graph2ppt(file = "species-total egg_bar.pptx",width = 7,height = 5)

#models
mod_fec1 = glmer(egg_total~species+exp_dur+(1|ID), data = remove_re,family = poisson)
summary(mod_fec1)
Anova(mod_fec1)
qqPlot(resid(mod_fec1))

mod_fec2 = lm(log(egg_total)~species, data = remove_re_1)
summary(mod_fec2)
Anova(mod_fec2)
qqPlot(resid(mod_fec2))
shapiro.test(resid(mod_fec1))
mod_fec3 = lm(egg_total~species, data = remove_re_1)
summary(mod_fec3)
Anova(mod_fec3)
qqPlot(resid(mod_fec3))
emmeans(mod_fec2,pairwise~species,adjust = "tukey")#ns
AIC(mod_fec3, mod_fec2)#3

kruskal.test(egg_total~species, data = remove_re_1)#***
wilcox.test(remove_TM_re_1$egg_total,remove_MA_re_1$egg_total,alternative = "two.sided")#ns
wilcox.test(remove_MP_re_1$egg_total,remove_MA_re_1$egg_total,alternative = "two.sided")#***
wilcox.test(remove_TM_re_1$egg_total,remove_MP_re_1$egg_total,alternative = "two.sided")#***

length(remove_TM_re_1$egg_total)
mean(remove_TM_re_1$egg_total)
sd(remove_TM_re_1$egg_total)

length(remove_MA_re_1$egg_total)
mean(remove_MA_re_1$egg_total)
sd(remove_MA_re_1$egg_total)

length(remove_MP_re_1$egg_total)
mean(remove_MP_re_1$egg_total)
sd(remove_MP_re_1$egg_total)



##(TM)15-35-55days----
#sac
remove_TM_re_1 = subset(remove_TM_re,sac_num == 1)
sac = ggplot(data=remove_TM_re_1, aes(x=ID, y=sac_55)) +
  geom_bar(fill = "light blue",stat="identity")+
  geom_bar(aes(y = sac_35),alpha = .5,stat="identity",fill = "yellow")+
  geom_bar(aes(y = sac_15),alpha = .8,stat="identity",fill = "orange")+
  theme_classic()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())
#control
remove_TM_con = subset(remove_TM, treatment == "control")
remove_TM_con_1 = subset(remove_TM_con, sac_num == 1)
remove_TM_1 = subset(remove_TM, sac_num == 1)
ggplot(data=remove_TM_1, aes(x=treatment, y=sac_35,fill = treatment)) +
  geom_boxplot()+
  theme_classic()
#egg
egg = ggplot(data=remove_TM_re_1, aes(x=ID, y=egg_55)) +
  geom_bar(fill = "light blue",stat="identity")+
  geom_bar(aes(y = egg_35),alpha = .5,stat="identity",fill = "yellow")+
  geom_bar(aes(y = egg_15),alpha = .8,stat="identity",fill = "orange")+
  theme_classic()

grid.newpage()
grid.draw(rbind(ggplotGrob(sac),ggplotGrob(egg)))
graph2ppt(file = "sac-egg-15,35,55.pptx",width = 7,height = 5)

#in percentage
remove_TM_re_1$sac_15_per = remove_TM_re_1$sac_15/remove_TM_re_1$sac_55
remove_TM_re_1$sac_35_per = remove_TM_re_1$sac_35/remove_TM_re_1$sac_55
remove_TM_re_1$sac_55_per = 1
sac_p = ggplot(data=remove_TM_re_1, aes(x=ID, y=sac_55_per)) +
  geom_bar(fill = "light blue",stat="identity")+
  geom_bar(aes(y = sac_35_per),alpha = .5,stat="identity",fill = "yellow")+
  geom_bar(aes(y = sac_15_per),alpha = .8,stat="identity",fill = "orange")+
  theme_classic()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())
remove_TM_re_1$egg_15_per = remove_TM_re_1$egg_15/remove_TM_re_1$egg_55
remove_TM_re_1$egg_35_per = remove_TM_re_1$egg_35/remove_TM_re_1$egg_55
remove_TM_re_1$egg_55_per = 1
egg_p = ggplot(data=remove_TM_re_1, aes(x=ID, y=egg_55_per)) +
  geom_bar(fill = "light blue",stat="identity")+
  geom_bar(aes(y = egg_35_per),alpha = .5,stat="identity",fill = "yellow")+
  geom_bar(aes(y = egg_15_per),alpha = .8,stat="identity",fill = "orange")+
  theme_classic()

grid.newpage()
grid.draw(rbind(ggplotGrob(sac_p),ggplotGrob(egg_p)))
### duration ----
##remove----
## TM
remove_TM_re_durNA = subset(remove_TM_re, is.na(dur)==FALSE)
mod_TM_dur1 = glmer(dur~ sac_num+(1|ID),
                     data = remove_TM_re_durNA, family = poisson)
summary(mod_TM_dur1)#***
Anova(mod_TM_dur1)
chisq <- sum(resid(mod_TM_dur1, type='pearson')^2)
chisq/df.residual(mod_TM_dur1)#overdispersion a little bit
qqPlot(resid(mod_TM_dur1))#not fine
shapiro.test(resid(mod_TM_dur1))#not normal
remove_TM_re_durNA$fit_dur = predict(mod_TM_dur1,remove_TM_re_durNA,type = "response")
ggplot(data = remove_TM_re_durNA, aes(x = sac_num, y = fit_dur))+
  theme_classic()+geom_count(alpha = .3,aes(y = dur))+
  geom_smooth(method = "glm",method.args = list(family = 'poisson'),
              color = "orange",fill = "orange")


##MA
remove_MA_re = subset(remove_MA_re, dur<29)#remove the outlier duration 29
mod_MA_dur5 = glmer(dur~sac_num+(1|ID), data = remove_MA_re,family = poisson)
summary(mod_MA_dur5)#<0
Anova(mod_MA_dur5)#***
qqPlot(resid(mod_MA_dur5))#very bad
chisq <- sum(resid(mod_MA_dur5, type='pearson')^2)
chisq/df.residual(mod_MA_dur5)#overdispersion fine
hist(remove_MA_re$dur)
remove_MA_re$dur_fit = predict(mod_MA_dur5,remove_MA_re,type = "response")
ggplot(data = remove_MA_re, aes(x = sac_num, y = dur_fit))+
  geom_smooth(method = "glm",method.args = list(family = 'poisson'),
              color = "#E69F00", fill = "#E69F00") + 
  theme_classic()+geom_count(aes(y = dur),alpha = .3)+ylim(0,12)
#graph2ppt(file = "MA_re_dur.pptx",width = 7,height = 5)

## MP

remove_MP_re_durNA = subset(remove_MP_re, is.na(dur)==FALSE)
mod_MP_dur5 = glmer(dur~sac_num+(1|ID), data = remove_MP_re_durNA,
                    family = poisson)
summary(mod_MP_dur5)#ns
Anova(mod_MP_dur5)#ns
qqPlot(resid(mod_MP_dur5))
chisq <- sum(resid(mod_MP_dur5, type='pearson')^2)
chisq/df.residual(mod_MP_dur5)#overdispersion a little
hist(remove_MP_re$dur)
#AIC(mod_MA_dur1,mod_MA_dur2,mod_MA_dur3,mod_MA_dur4,mod_MA_dur5)#2

remove_MP_re_durNA$dur_fit = predict(mod_MP_dur5,
                                     remove_MP_re_durNA,type = "response")
ggplot(data = remove_MP_re_durNA, aes(x = sac_num, y = dur_fit))+
  geom_smooth(method = "glm",method.args = list(family = 'poisson'),
              color = "#E69F00", fill = "#E69F00") + 
  theme_classic()+geom_count(aes(y = dur),alpha = .3)+ylim(0,12)


#interaction
remove_re_durNA = subset(remove_re, is.na(dur)==FALSE)
#remove_re_durNA$ability = remove_re_durNA$eggs/remove_re_durNA$dur
#ggplot(data = remove_re_durNA, aes(x = sac_num, y = ability,color = species,fill = species))+
 # geom_smooth(method = "lm") + 
  #theme_classic()
#remove PV as they are so different and doesn't have enough sac_num
remove_re_durNA = subset(remove_re_durNA, species != "PV")
mod_all_dur5 = glmer(dur~sac_num+species+(1|ID), data = remove_re_durNA,
                    family = poisson)
summary(mod_all_dur5)#*
Anova(mod_all_dur5)#*
qqPlot(resid(mod_all_dur5))
chisq <- sum(resid(mod_all_dur5, type='pearson')^2)
chisq/df.residual(mod_all_dur5)#overdispersion fine

mod_all_dur6 = glmer(dur~sac_num*species+(1|ID), data = remove_re_durNA,
                     family = poisson)
r.squaredGLMM(mod_all_dur6)
emmeans(mod_all_dur6,pairwise~species,adjust = "tukey",type = "response")
summary(mod_all_dur6)#*
Anova(mod_all_dur6)#*
qqPlot(resid(mod_all_dur6))
chisq <- sum(resid(mod_all_dur6, type='pearson')^2)
chisq/df.residual(mod_all_dur6)#overdispersion fine
AIC(mod_all_dur6,mod_all_dur5)#6
anova(mod_all_dur6,mod_all_dur5)#6
remove_re_durNA$dur_fit = predict(mod_all_dur6,
                                     remove_re_durNA,type = "response")
ggplot(data = remove_re_durNA, aes(x = sac_num, y = dur_fit,color = species,fill = species))+
  geom_smooth(method = "glm",method.args = list(family = 'poisson'),
             ) + 
  theme_classic()+geom_count(aes(y = dur),alpha = .3)+ylim(0,15)

#graph2ppt(file = "all_re_dur.pptx",width = 7,height = 5)

#barplot regardless of sac num
#bar
remove_re_durNA_PV = subset(remove_re, is.na(dur)==FALSE)
df <- ddply(remove_re_durNA_PV, c("species"), summarize, Mean = mean(dur), SD = sd(dur))
a = ggplot(df, aes(x = species, y = Mean,fill = species)) +
  geom_bar(stat = "identity",alpha = .5,color = "black") + 
  # add 68% CI errorbar 
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2,color = "grey40")+
  theme_classic()+
  scale_color_manual(values=cbPalette)+
  scale_fill_manual(values=cbPalette)+ylim(0,32)
#density
b = ggplot(data = remove_re_durNA_PV, aes(x = dur,fill = species))+
  geom_density(data = remove_re_durNA_PV, aes(x = dur,fill = species),alpha = .3)+
  theme_classic()+coord_flip()+scale_fill_manual(values=cbPalette)+
  scale_y_reverse()+xlim(0,32)

cowplot::plot_grid(b+theme(legend.position = "none",
                           axis.text.y = element_blank(),
                           axis.line.y = element_blank(),
                           axis.ticks.y = element_blank()
),
a+theme(axis.title.y = element_blank()),
nrow = 1, rel_widths = c(1,3))

mod_pool_dur1 = glmer(dur~ species+(1|ID),
                       data = remove_re, family = poisson)
summary(mod_pool_dur1)
anova(mod_pool_dur1)
qqPlot(resid(mod_pool_dur1))
shapiro.test(resid(mod_pool_dur1))#***
chisq <- sum(resid(mod_pool_dur1, type='pearson')^2)
chisq/df.residual(mod_pool_dur1)#overdispersion fine
emmeans(mod_pool_dur1,pairwise~species,adjust = "tukey")
##control vs remove----
#remove interval all
ggplot(data = remove_re, aes(x =species, y = dur, fill = species))+
  geom_boxplot(alpha = .5,outlier.shape = NA)+
  theme_classic()
#control interval all
remove_con = subset(remove, treatment == "control")
ggplot(data = remove_con, aes(x =species, y = dur, fill = species))+
  geom_boxplot(alpha = .5,outlier.shape = NA)+
  theme_classic()
#all treatment all sp
ggplot(data = remove, aes(x =species, y = dur, fill = treatment))+
  geom_jitter(aes(color = species),alpha = 0.3)+
  geom_boxplot(alpha = .5)+
  theme_classic()
#TM----
ggplot(data = remove_TM, aes(x = treatment, y = dur, fill = treatment))+
  geom_jitter()+
  geom_boxplot(alpha = .5,fill = c("grey40","#CC79A7"),outlier.shape = NA)+
  theme_classic()+ylim(0,10)
graph2ppt(file = "TM_treatment_dur.pptx",width = 7,height = 5)

#bar
remove_TM_durNA = subset(remove_TM, is.na(dur)==FALSE)
df <- ddply(remove_TM_durNA, c("treatment"), summarize, Mean = mean(dur), SD = sd(dur))
a = ggplot(df, aes(x = treatment, y = Mean,fill = treatment)) +
  geom_bar(stat = "identity",alpha = .5,color = "black") + 
  # add 68% CI errorbar 
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2,color = "grey40")+
  theme_classic()+
  scale_color_manual(values=c("grey","#CC79A7"))+
  scale_fill_manual(values=c("grey","#CC79A7"))+ylim(0,15)
#density
b = ggplot(data = remove_TM_durNA, aes(x = dur,fill = treatment))+
  geom_density(data = remove_TM_durNA, aes(x = dur,fill = treatment),alpha = .3)+
  theme_classic()+coord_flip()+scale_fill_manual(values=c("grey","#CC79A7"))+
  scale_y_reverse()+xlim(0,15)

cowplot::plot_grid(b+theme(legend.position = "none",
                           axis.text.y = element_blank(),
                           axis.line.y = element_blank(),
                           axis.ticks.y = element_blank()
),
a+theme(axis.title.y = element_blank()),
nrow = 1, rel_widths = c(1,3))

test_ctast1 = glmmTMB(dur~treatment + (1|ID),
                     data = remove_TM, family = nbinom1)
summary(test_ctast1)
Anova(test_ctast1)
chisq <- sum(resid(test_ctast1, type='pearson')^2)
chisq/df.residual(test_ctast1)#overdispersion a little bit
r.squaredGLMM(test_ctast1)

test_ctast2 = glmmTMB(dur~treatment*sac_num + (1|ID),
                      data = remove_TM, family = nbinom1)
summary(test_ctast2)
Anova(test_ctast2)
chisq <- sum(resid(test_ctast2, type='pearson')^2)
chisq/df.residual(test_ctast2)#overdispersion a little bit

r.squaredGLMM(test_ctast2)

test_ctast3 = glmer(dur~treatment*sac_num + (1|ID),
                      data = remove_TM, family = poisson)
summary(test_ctast3)
Anova(test_ctast3)
chisq <- sum(resid(test_ctast3, type='pearson')^2)
chisq/df.residual(test_ctast3)#overdispersion a little bit

test_ctast4 = glmer(dur~sac_num+treatment + (1|ID),
                    data = remove_TM, family = poisson)
summary(test_ctast4)
Anova(test_ctast4)
chisq <- sum(resid(test_ctast4, type='pearson')^2)
chisq/df.residual(test_ctast4)#overdispersion a little bit
anova(test_ctast4,test_ctast3)
emmeans(test_ctast4,pairwise~treatment,adjust = "tukey",type = "response")
AIC(test_ctast1,test_ctast2,test_ctast3,test_ctast4)#4



kruskal.test(dur~treatment, data = remove_TM)#***
wilcox.test(dur~treatment, data = remove_TM)#***
remove_TM_con = subset(remove_TM, treatment == "control")
remove_TM_con_NA = subset(remove_TM_con, is.na(dur) == FALSE)
remove_TM_re_NA = subset(remove_TM_re, is.na(dur) == FALSE)

length(remove_TM_re_NA$dur)
mean(remove_TM_re_NA$dur)
sd(remove_TM_re_NA$dur)

length(remove_TM_con_NA$dur)
mean(remove_TM_con_NA$dur)
sd(remove_TM_con_NA$dur)
### eggs----
##remove ----
#barplot regardless of sac num
#bar
remove_re_eggNA = subset(remove_re, is.na(eggs)==FALSE)
df <- ddply(remove_re_eggNA, c("species"), summarize, Mean = mean(eggs), SD = sd(eggs))
a = ggplot(df, aes(x = species, y = Mean,fill = species)) +
  geom_bar(stat = "identity",alpha = .5,color = "black") + 
  # add 68% CI errorbar 
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2,color = "grey40")+
  theme_classic()+
  scale_color_manual(values=cbPalette)+
  scale_fill_manual(values=cbPalette)+ylim(0,32)
#density
b = ggplot(data = remove_re_eggNA, aes(x = eggs,fill = species))+
  geom_density(data = remove_re_eggNA, aes(x = eggs,fill = species),alpha = .3)+
  theme_classic()+coord_flip()+scale_fill_manual(values=cbPalette)+
  scale_y_reverse()+xlim(0,32)

cowplot::plot_grid(b+theme(legend.position = "none",
                           axis.text.y = element_blank(),
                           axis.line.y = element_blank(),
                           axis.ticks.y = element_blank()
),
a+theme(axis.title.y = element_blank()),
nrow = 1, rel_widths = c(1,3))

mod_pool_eggs1 = glmer(eggs~ species+(1|ID),
                      data = remove_re, family = poisson)
summary(mod_pool_eggs1)
Anova(mod_pool_eggs1)
qqPlot(resid(mod_pool_eggs1))
shapiro.test(resid(mod_pool_eggs1))#***
chisq <- sum(resid(mod_pool_eggs1, type='pearson')^2)
chisq/df.residual(mod_pool_eggs1)#overdispersion fine
emmeans(mod_pool_eggs1,pairwise~species,adjust = "tukey")
##all general boxplot
ggplot(data = remove_re, aes(x =species, y = eggs, fill = species))+
  geom_boxplot(alpha = .5)+
  theme_classic()
## TM

mod_TM_eggs1 = glmer(eggs~ sac_num+(1|ID),
                 data = remove_TM_re, family = poisson)
summary(mod_TM_eggs1)#***
Anova(mod_TM_eggs1)
chisq <- sum(resid(mod_TM_eggs1, type='pearson')^2)
chisq/df.residual(mod_TM_eggs1)#overdispersion fine
qqPlot(resid(mod_TM_eggs1))
remove_TM_re$fit_egg = predict(mod_TM_eggs1,remove_TM_re,type = "response")
ggplot(data = remove_TM_re, aes(x = sac_num, y = fit_egg))+
  geom_smooth(color = "orange",fill = "orange") + 
  theme_light()+geom_count(aes(y = eggs))
remove_TM_re$ID_fac = as.factor(remove_TM_re$ID)

ggplot(data = remove_TM_re, aes(x = sac_num, y = fit_egg))+
  theme_classic()+geom_count(alpha = .3,aes(y = eggs))+
  geom_smooth(method = "glm",
              method.args = list(family = 'poisson'),
              color = "#CC79A7",fill = "#CC79A7")
graph2ppt(file = "TM_re_eggs.pptx",width = 7,height = 5)

##MA ns
mod_MA_eggs1 = glmer(eggs~ sac_num+(1|ID),
                     data = remove_MA_re, family = poisson)
summary(mod_MA_eggs1)#0.908
chisq <- sum(resid(mod_MA_eggs1, type='pearson')^2)
chisq/df.residual(mod_MA_eggs1)#overdispersion fine
qqPlot(resid(mod_MA_eggs1))#fine
shapiro.test(resid(mod_MA_eggs1))#nope
remove_MA_re$fit_egg = predict(mod_MA_eggs1,remove_MA_re,type = "response")
ggplot(data = remove_MA_re, aes(x = sac_num, y = fit_egg))+
  theme_light()+geom_count(alpha = .3,aes(y = eggs))+
  geom_smooth(method = "glm",method.args = list(family = 'poisson'),
              color = "#E69F00",fill = "#E69F00")
graph2ppt(file = "MA_re_eggs.pptx",width = 7,height = 5)

## MP ns
remove_MP_re_sub = subset(remove_MP_re, eggs < 30)#remove one outlier
mod_MP_eggs1 = glmer(eggs~ sac_num+(1|ID),
                     data = remove_MP_re_sub, family = poisson)
summary(mod_MP_eggs1)#NS
chisq <- sum(resid(mod_MP_eggs1, type='pearson')^2)
chisq/df.residual(mod_MP_eggs1)#overdispersion fine
qqPlot(resid(mod_MP_eggs1))#fine
shapiro.test(resid(mod_MP_eggs1))#yes
remove_MP_re_sub$fit_egg = predict(mod_MP_eggs1,remove_MP_re_sub,type = "response")
ggplot(data = remove_MP_re_sub, aes(x = sac_num, y = fit_egg))+
  theme_light()+geom_count(alpha = .3,aes(y = eggs))+
  geom_smooth(method = "glm",method.args = list(family = 'poisson'),
              color = "#009E73",fill = "#009E73")
graph2ppt(file = "MP_re_eggs.pptx",width = 7,height = 5)


## PV NS

mod_PV_eggs1 = glmer(eggs~ sac_num+(1|ID),
                     data = remove_PV_re, family = poisson)
summary(mod_PV_eggs1)#NS
Anova(mod_PV_eggs1)#ns
chisq <- sum(resid(mod_PV_eggs1, type='pearson')^2)
chisq/df.residual(mod_PV_eggs1)#overdispersion fine
qqPlot(resid(mod_PV_eggs1))#fine
shapiro.test(resid(mod_PV_eggs1))#yes
remove_PV_re$fit_egg = predict(mod_PV_eggs1,remove_PV_re,type = "response")
ggplot(data = remove_PV_re, aes(x = sac_num, y = fit_egg))+
  theme_light()+geom_count(alpha = .3,aes(y = eggs))+
  geom_smooth(method = "glm",method.args = list(family = 'poisson'),
              color = "#009E73",fill = "#009E73")
graph2ppt(file = "MP_re_eggs.pptx",width = 7,height = 5)

#all interaction
mod_all_eggs1 = glmer(eggs~ sac_num*species+(1|ID),
                     data = remove_re, family = poisson)
summary(mod_all_eggs1)
Anova(mod_all_eggs1)
qqPlot(resid(mod_all_eggs1))
shapiro.test(resid(mod_all_eggs1))#***
chisq <- sum(resid(mod_all_eggs1, type='pearson')^2)
chisq/df.residual(mod_all_eggs1)#overdispersion fine

mod_all_eggs2 = glmer(eggs~ sac_num+species+(1|ID),
                      data = remove_re, family = poisson)
summary(mod_all_eggs2)
Anova(mod_all_eggs2)
qqPlot(resid(mod_all_eggs2))
chisq <- sum(resid(mod_all_eggs2, type='pearson')^2)
chisq/df.residual(mod_all_eggs2)#overdispersion fine


AIC(mod_all_eggs1,mod_all_eggs2)#1
anova(mod_all_eggs1,mod_all_eggs2)

remove_re$fit_egg = predict(mod_all_eggs1,remove_re,type = "response")
ggplot(data = remove_re, aes(x = sac_num, y = fit_egg,color = species))+
  theme_light()+geom_count(alpha = .5,aes(y = eggs))+
  geom_smooth(aes(group = species,color = species),method = "glm",
              method.args = list(family = 'poisson'))
#nonfit
ggplot(data = remove_re, aes(x = sac_num, y = eggs,color = species,fill = species))+
  theme_classic()+geom_count(alpha = .5)+
  geom_smooth(aes(group = species,color = species),method = "glm",
              method.args = list(family = 'poisson'))+
  scale_color_manual(values=cbPalette)+
  scale_fill_manual(values=cbPalette)
graph2ppt(file = "all_interaction_re_eggs.pptx",width = 7,height = 5)

##control----
#TM eggs

mod_TM_sac1 = glmer(sac_total~ treatment+(1|ID),
                     data = remove_TM, family = poisson)
summary(mod_TM_sac1)
chisq <- sum(resid(mod_TM_sac1, type='pearson')^2)
chisq/df.residual(mod_TM_sac1)#overdispersion fine
qqPlot(resid(mod_TM_sac1))
remove_TM_re$fit_egg = predict(mod_TM_eggs1,remove_TM_re,type = "response")
remove_TM_1 = subset(remove_TM, sac_num == 1)
ggplot(data = remove_TM_1, aes(x = treatment, y = sac_total, fill = treatment))+
  geom_boxplot(alpha = .7,fill = c("grey40","#CC79A7"))+#geom_count(alpha = .5)+
  theme_classic()
graph2ppt(file = "TM_treatment_eggs.pptx",width = 7,height = 5)

mod_TM_sac2 = lm(log(sac_total)~treatment, data = remove_TM_1)
summary(mod_TM_sac2)
Anova(mod_TM_sac2)
qqPlot(resid(mod_TM_sac2))

mod_TM_sac3 = lm(sac_total~treatment, data = remove_TM_1)
summary(mod_TM_sac3)
Anova(mod_TM_sac3)
qqPlot(resid(mod_TM_sac3))

AIC(mod_TM_sac2,mod_TM_sac3)
wilcox.test(sac_total~treatment, data = remove_TM_1)#***


length(remove_TM_con_1$sac_total)
mean(remove_TM_con_1$sac_total)
sd(remove_TM_con_1$sac_total)

length(remove_TM_re_1$sac_total)
mean(remove_TM_re_1$sac_total)
sd(remove_TM_re_1$sac_total)

#bar
df <- ddply(remove_TM, c("treatment"), summarize, Mean = mean(sac_total), SD = sd(sac_total))
a = ggplot(df, aes(x = treatment, y = Mean,fill = treatment)) +
  geom_bar(stat = "identity",alpha = .5,color = "black") + 
  # add 68% CI errorbar 
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2,color = "grey40")+
  theme_classic()+
  scale_color_manual(values=c("grey","#CC79A7"))+
  scale_fill_manual(values=c("grey","#CC79A7"))+ylim(0,42)
#density
b = ggplot(data = remove_TM_durNA, aes(x = sac_total,fill = treatment))+
  geom_density(data = remove_TM_durNA, aes(x = sac_total,fill = treatment),alpha = .3)+
  theme_classic()+coord_flip()+scale_fill_manual(values=c("grey","#CC79A7"))+
  scale_y_reverse()+xlim(0,42)

cowplot::plot_grid(b+theme(legend.position = "none",
                           axis.text.y = element_blank(),
                           axis.line.y = element_blank(),
                           axis.ticks.y = element_blank()
),
a+theme(axis.title.y = element_blank()),
nrow = 1, rel_widths = c(1,3))
### test----
remove$species = factor(remove$species , levels=c("TM","MA","MP","HA"))
remove_1 = subset(remove, sac_num == 1)
mod_dur_egg1 = lm(eggs~dur, data = remove_TM_re)
summary(mod_dur_egg1)
Anova(mod_dur_egg1)
qqPlot(resid(mod_dur_egg1))
ggplot(data = remove_1, aes(x = treatment, y = sac_total, color = species))+
  geom_boxplot(aes(fill = species),alpha = .5)+
  theme_classic()+
  scale_fill_manual(values=cbPalette)+
  scale_color_manual(values=cbPalette)
graph2ppt(file = "all_treatment_totalsac.pptx",width = 7,height = 5)

#+HA
remove_con = subset(remove, treatment == "control")
remove_con_1 = subset(remove_con, sac_num == 1)
ggplot(data = remove_con_1, aes(x = species, y = sac_total))+
  geom_boxplot(aes(fill = species),alpha = .5)+
  theme_classic()+
  scale_fill_manual(values=cbPalette)+
  scale_color_manual(values=cbPalette)
ggplot(data = remove_con, aes(x = species, y = dur))+
  geom_boxplot(aes(fill = species),alpha = .5)+
  theme_classic()+
  scale_fill_manual(values=cbPalette)+
  scale_color_manual(values=cbPalette)
graph2ppt(file = "all_treatment_totalsac.pptx",width = 7,height = 5)

remove_TM_con_1 = subset(remove_TM, sac_num == 1)
remove_TM_con_1 = subset(remove_TM_con_1, treatment == "control")

remove_MA_con_1 = subset(remove_MA, sac_num == 1)
remove_MA_con_1 = subset(remove_MA_con_1, treatment == "control")

remove_MP_con_1 = subset(remove_MP, sac_num == 1)
remove_MP_con_1 = subset(remove_MP_con_1, treatment == "control")

summary(remove_MP_con_1)

length(remove_TM_con_1$sac_total)
mean(remove_TM_con_1$sac_total)
sd(remove_TM_con_1$sac_total)

length(remove_MA_con_1$sac_total)
mean(remove_MA_con_1$sac_total)
sd(remove_MA_con_1$sac_total)

length(remove_MP_con_1$sac_total)
mean(remove_MP_con_1$sac_total)
sd(remove_MP_con_1$sac_total)

#species and interval
remove$species = factor(remove$species, levels = c("TM", "MA", "MP"))
ggplot(data = remove, aes(x = species, y = dur, fill = species))+
  geom_boxplot(alpha = .5,outlier.shape = NA)+
  theme_classic()+
  scale_fill_manual(values=cbPalette)+
  scale_color_manual(values=cbPalette)+ylim(0,12)
graph2ppt(file = "all_species_dur.pptx",width = 7,height = 5)
remove_MP =  subset(remove, species == "MP")
remove_MA =  subset(remove, species == "MA")
kruskal.test(dur~species, data = remove)#***
wilcox.test(remove_TM$dur,remove_MA$dur,alternative = "two.sided")#ns
wilcox.test(remove_MP$dur,remove_MA$dur,alternative = "two.sided")#***
wilcox.test(remove_MP$dur,remove_TM$dur,alternative = "two.sided")#***

remove_TM_NA = subset(remove_TM, is.na(dur) == FALSE)
remove_MA_NA = subset(remove_MA, is.na(dur) == FALSE)
remove_MP_NA = subset(remove_MP, is.na(dur) == FALSE)
length(remove_TM_NA$dur)
mean(remove_TM_NA$dur)
sd(remove_TM_NA$dur)

length(remove_MA_NA$dur)
mean(remove_MA_NA$dur)
sd(remove_MA_NA$dur)

length(remove_MP_NA$dur)
mean(remove_MP_NA$dur)
sd(remove_MP_NA$dur)

#interval treatment
ggplot(data = remove, aes(x = species, y = dur, color = treatment, alpha = species))+
  geom_boxplot(aes(fill = treatment),alpha = .5)+
  theme_classic()+
  scale_fill_manual(values=cbPalette)+
  scale_color_manual(values=cbPalette)+ylim(0,13)
graph2ppt(file = "all_treatment_totalsac.pptx",width = 7,height = 5)


