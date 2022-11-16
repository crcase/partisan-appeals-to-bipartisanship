###### Setup ######  
rm(list = ls(all.names = TRUE))
setwd("C:/Users/Colin/Dropbox/5. Research Projects/bipartisanship")
set.seed(1996)
######  Load Packages ###### 
library(ggplot2)
library(tidyr)
library(dplyr)
library(ggpubr)

###### Load Survey Data Set ######
df <- readRDS('bipartisan_data.rds')

#######  Figure 2: Support for Legislation ###### 
# Remove NAs for response
figure2 <- subset(df, !is.na(support.leg))

figure2.all <- figure2 %>%
  group_by(counter_message) %>%
  summarise(mean_support = mean(support.leg), sd_support = sd(support.leg),
            n = sum(response))
figure2.all$majority_party_respon <- 'All Respondents'

figure2.party <- figure2 %>%
  filter(majority_party_respon == 'Majority Party'|majority_party_respon == 'Minority Party') %>% 
  group_by(counter_message, majority_party_respon) %>%
  summarise(mean_support = mean(support.leg), sd_support = sd(support.leg),
            n = sum(response))

figure2.plotdata <- rbind(figure2.all,figure2.party)
figure2.plotdata$lb <- figure2.plotdata$mean_support +
  ((qnorm(0.025)*figure2.plotdata$sd_support)/sqrt(figure2.plotdata$n))
figure2.plotdata$ub <- figure2.plotdata$mean_support +
  ((qnorm(0.975)*figure2.plotdata$sd_support)/sqrt(figure2.plotdata$n))



figure2.final <- ggplot(figure2.plotdata, aes(x = as.factor(majority_party_respon), y = mean_support, 
                             group = counter_message, color = counter_message)) +
  geom_pointrange(aes(ymin = lb,
                      ymax = ub),
                  size = 1.5, 
                  position = position_dodge(width = .2))+
  ylim(3.5,6)+
  theme_bw()+
  xlab('\nRespondent Party')+
  ylab('Mean Support for Legislation\n') +
  labs(color = 'Counter Message')+
  theme(axis.text.x =element_text(size  = 17),
        axis.text.y = element_text(size = 17), 
        axis.title = element_text(size=20),
        legend.text=element_text(size=17),
        legend.title = element_text(size = 17))+
  guides(fill = guide_legend(keywidth = 4, keyheight = 1),
         linetype=guide_legend(keywidth = 4, keyheight = 1))+
  scale_color_manual(values=c('grey1','grey45', 'grey80'))

figure2.final

ggsave('plots_wave7/all_support.png', width = 10, height = 8)

## t-test (Partisan vs None)
# All Respondents
t.test(figure2$support.leg[figure2$counter_message == 'Partisan'],
       figure2$support.leg[figure2$counter_message == 'None'], 
       alternative = 'less')
# Majority Party Respondents
t.test(figure2$support.leg[figure2$counter_message == 'Partisan' & figure2$majority_party_respon == 'Majority Party'],
       figure2$support.leg[figure2$counter_message == 'None' & figure2$majority_party_respon == 'Majority Party'], 
       alternative = 'less')
# Minority Party Respondents
t.test(figure2$support.leg[figure2$counter_message == 'Partisan' & figure2$majority_party_respon == 'Minority Party'],
       figure2$support.leg[figure2$counter_message == 'None' & figure2$majority_party_respon == 'Minority Party'], 
       alternative = 'less')

## t-test (Non-partisan vs None)
# All Respondents
t.test(figure2$support.leg[figure2$counter_message == 'Non-partisan'],
       figure2$support.leg[figure2$counter_message == 'None'], 
       alternative = 'less')
# Majority Party Respondents
t.test(figure2$support.leg[figure2$counter_message == 'Non-partisan' & figure2$majority_party_respon == 'Majority Party'],
       figure2$support.leg[figure2$counter_message == 'None' & figure2$majority_party_respon == 'Majority Party'], 
       alternative = 'less')
# Minority Party Respondents
t.test(figure2$support.leg[figure2$counter_message == 'Non-partisan' & figure2$majority_party_respon == 'Minority Party'],
       figure2$support.leg[figure2$counter_message == 'None' & figure2$majority_party_respon == 'Minority Party'], 
       alternative = 'less')

rm(list=setdiff(ls(), "df"))

####### Figure 3: Approval of Legislature #######  
# Remove NAs for response
figure3 <- subset(df, !is.na(leg.approval))

figure3.all <- figure3 %>%
  group_by(counter_message) %>%
  summarise(mean_approval = mean(leg.approval), sd_approval = sd(leg.approval),
            n = sum(response))
figure3.all$majority_party_respon <- 'All Respondents'

figure3.party <- figure3 %>%
  filter(majority_party_respon == 'Majority Party'|majority_party_respon == 'Minority Party') %>% 
  group_by(counter_message, majority_party_respon) %>%
  summarise(mean_approval = mean(leg.approval), sd_approval = sd(leg.approval),
            n = sum(response))

figure3.plotdata <- rbind(figure3.all,figure3.party)
figure3.plotdata$lb <- figure3.plotdata$mean_approval +
  ((qnorm(0.025)*figure3.plotdata$sd_approval)/sqrt(figure3.plotdata$n))
figure3.plotdata$ub <- figure3.plotdata$mean_approval +
  ((qnorm(0.975)*figure3.plotdata$sd_approval)/sqrt(figure3.plotdata$n))



figure3.final <- ggplot(figure3.plotdata, aes(x = as.factor(majority_party_respon), y = mean_approval, 
                                              group = counter_message, color = counter_message)) +
  geom_pointrange(aes(ymin = lb,
                      ymax = ub),
                  size = 1.5, 
                  position = position_dodge(width = .2))+
  ylim(3.5,6)+
  theme_bw()+
  xlab('\nRespondent Party')+
  ylab('Mean Approval of Legislature\n') +
  labs(color = 'Counter Message')+
  theme(axis.text.x =element_text(size  = 17),
        axis.text.y = element_text(size = 17), 
        axis.title = element_text(size=20),
        legend.text=element_text(size=17),
        legend.title = element_text(size = 17))+
  guides(fill = guide_legend(keywidth = 4, keyheight = 1),
         linetype=guide_legend(keywidth = 4, keyheight = 1))+
  scale_color_manual(values=c('grey1','grey45', 'grey80'))

figure3.final

ggsave('plots_wave7/all_approval.png', width = 10, height = 8)


## t-test (Partisan vs None)
# All Respondents
t.test(figure3$leg.approval[figure3$counter_message == 'Partisan'],
       figure3$leg.approval[figure3$counter_message == 'None'], 
       alternative = 'less')
# Majority Party Respondents
t.test(figure3$leg.approval[figure3$counter_message == 'Partisan' & figure3$majority_party_respon == 'Majority Party'],
       figure3$leg.approval[figure3$counter_message == 'None' & figure3$majority_party_respon == 'Majority Party'], 
       alternative = 'less')
# Minority Party Respondents
t.test(figure3$leg.approval[figure3$counter_message == 'Partisan' & figure3$majority_party_respon == 'Minority Party'],
       figure3$leg.approval[figure3$counter_message == 'None' & figure3$majority_party_respon == 'Minority Party'], 
       alternative = 'less')

## t-test (Non-partisan vs None)
# All Respondents
t.test(figure3$leg.approval[figure3$counter_message == 'Non-partisan'],
       figure3$leg.approval[figure3$counter_message == 'None'], 
       alternative = 'less')
# Majority Party Respondents
t.test(figure3$leg.approval[figure3$counter_message == 'Non-partisan' & figure3$majority_party_respon == 'Majority Party'],
       figure3$leg.approval[figure3$counter_message == 'None' & figure3$majority_party_respon == 'Majority Party'], 
       alternative = 'less')
# Minority Party Respondents
t.test(figure3$leg.approval[figure3$counter_message == 'Non-partisan' & figure3$majority_party_respon == 'Minority Party'],
       figure3$leg.approval[figure3$counter_message == 'None' & figure3$majority_party_respon == 'Minority Party'], 
       alternative = 'less')

rm(list=setdiff(ls(), "df"))

#######  Figure 4: Support and Approval, Exclude None ####### 
# Remove NAs for response
figure4.support <- subset(df, !is.na(support.leg))

figure4.support.all <- figure4.support %>%
  group_by(counter_message) %>%
  summarise(mean_support = mean(support.leg), sd_support = sd(support.leg),
            n = sum(response))
figure4.support.all$majority_party_respon <- 'All Respondents'

figure4.support.party <- figure4.support %>%
  filter(majority_party_respon == 'Majority Party'|majority_party_respon == 'Minority Party') %>% 
  group_by(counter_message, majority_party_respon) %>%
  summarise(mean_support = mean(support.leg), sd_support = sd(support.leg),
            n = sum(response))

figure4.support.plotdata <- rbind(figure4.support.all,figure4.support.party)
figure4.support.plotdata$lb <- figure4.support.plotdata$mean_support +
  ((qnorm(0.025)*figure4.support.plotdata$sd_support)/sqrt(figure4.support.plotdata$n))
figure4.support.plotdata$ub <- figure4.support.plotdata$mean_support +
  ((qnorm(0.975)*figure4.support.plotdata$sd_support)/sqrt(figure4.support.plotdata$n))

figure4.support.plotdata <- subset(figure4.support.plotdata, counter_message != 'None')


figure4.support.final <- ggplot(figure4.support.plotdata, aes(x = as.factor(majority_party_respon), y = mean_support, 
                                              group = counter_message, color = counter_message)) +
  geom_pointrange(aes(ymin = lb,
                      ymax = ub),
                  size = 1.5, 
                  position = position_dodge(width = .2))+
  ylim(3.5,6)+
  theme_bw()+
  xlab('\nRespondent Party')+
  ylab('Mean Support for Legislation\n') +
  labs(color = 'Counter Message')+
  theme(axis.text.x =element_text(size  = 17),
        axis.text.y = element_text(size = 17), 
        axis.title = element_text(size=20),
        legend.text=element_text(size=17),
        legend.title = element_text(size = 17))+
  guides(fill = guide_legend(keywidth = 4, keyheight = 1),
         linetype=guide_legend(keywidth = 4, keyheight = 1))+
  scale_color_manual(values=c('grey45', 'grey80'))

figure4.support.final


# Remove NAs for response
figure4.approval <- subset(df, !is.na(leg.approval))

figure4.approval.all <- figure4.approval %>%
  group_by(counter_message) %>%
  summarise(mean_approval = mean(leg.approval), sd_approval = sd(leg.approval),
            n = sum(response))
figure4.approval.all$majority_party_respon <- 'All Respondents'

figure4.approval.party <- figure4.approval %>%
  filter(majority_party_respon == 'Majority Party'|majority_party_respon == 'Minority Party') %>% 
  group_by(counter_message, majority_party_respon) %>%
  summarise(mean_approval = mean(leg.approval), sd_approval = sd(leg.approval),
            n = sum(response))

figure4.approval.plotdata <- rbind(figure4.approval.all,figure4.approval.party)
figure4.approval.plotdata$lb <- figure4.approval.plotdata$mean_approval +
  ((qnorm(0.025)*figure4.approval.plotdata$sd_approval)/sqrt(figure4.approval.plotdata$n))
figure4.approval.plotdata$ub <- figure4.approval.plotdata$mean_approval +
  ((qnorm(0.975)*figure4.approval.plotdata$sd_approval)/sqrt(figure4.approval.plotdata$n))

figure4.approval.plotdata <- subset(figure4.approval.plotdata, counter_message != 'None')

figure4.approval.final <- ggplot(figure4.approval.plotdata, aes(x = as.factor(majority_party_respon), y = mean_approval, 
                                              group = counter_message, color = counter_message)) +
  geom_pointrange(aes(ymin = lb,
                      ymax = ub),
                  size = 1.5, 
                  position = position_dodge(width = .2))+
  ylim(3.5,6)+
  theme_bw()+
  xlab('\nRespondent Party')+
  ylab('Mean Approval of Legislature\n') +
  labs(color = 'Counter Message')+
  theme(axis.text.x =element_text(size  = 17),
        axis.text.y = element_text(size = 17), 
        axis.title = element_text(size=20),
        legend.text=element_text(size=17),
        legend.title = element_text(size = 17))+
  guides(fill = guide_legend(keywidth = 4, keyheight = 1),
         linetype=guide_legend(keywidth = 4, keyheight = 1))+
  scale_color_manual(values=c('grey45', 'grey80'))

figure4.approval.final

joint <- ggarrange(figure4.support.final, figure4.approval.final, ncol=2, nrow=1, common.legend = TRUE, legend="right")

joint

ggsave('plots_wave7/partneut_joint.png', width = 16, height = 8)




## t-test (Partisan vs Non-partisan: Support)
# All Respondents
t.test(figure4.support$support.leg[figure4.support$counter_message == 'Partisan'],
       figure4.support$support.leg[figure4.support$counter_message == 'Non-partisan'], 
       alternative = 'less')
# Majority Party Respondents
t.test(figure4.support$support.leg[figure4.support$counter_message == 'Partisan' & figure4.support$majority_party_respon == 'Majority Party'],
       figure4.support$support.leg[figure4.support$counter_message == 'Non-partisan' & figure4.support$majority_party_respon == 'Majority Party'], 
       alternative = 'greater')
# Minority Party Respondents
t.test(figure4.support$support.leg[figure4.support$counter_message == 'Partisan' & figure4.support$majority_party_respon == 'Minority Party'],
       figure4.support$support.leg[figure4.support$counter_message == 'Non-partisan' & figure4.support$majority_party_respon == 'Minority Party'], 
       alternative = 'less')

## t-test (Partisan vs Non-partisan: Approval)
# All Respondents
t.test(figure4.approval$leg.approval[figure4.approval$counter_message == 'Partisan'],
       figure4.approval$leg.approval[figure4.approval$counter_message == 'Non-partisan'], 
       alternative = 'less')
# Majority Party Respondents
t.test(figure4.approval$leg.approval[figure4.approval$counter_message == 'Partisan' & figure4.approval$majority_party_respon == 'Majority Party'],
       figure4.approval$leg.approval[figure4.approval$counter_message == 'Non-partisan' & figure4.approval$majority_party_respon == 'Majority Party'], 
       alternative = 'greater')
# Minority Party Respondents
t.test(figure4.approval$leg.approval[figure4.approval$counter_message == 'Partisan' & figure4.approval$majority_party_respon == 'Minority Party'],
       figure4.approval$leg.approval[figure4.approval$counter_message == 'Non-partisan' & figure4.approval$majority_party_respon == 'Minority Party'], 
       alternative = 'less')

rm(list=setdiff(ls(), "df"))


####### Figure 5: Majority FT ####### 

# Remove NAs for response
figure5 <- subset(df, !is.na(majorityft))

figure5.all <- figure5 %>%
  group_by(counter_message) %>%
  summarise(mean_majorityft = mean(majorityft), sd_majorityft = sd(majorityft),
            n = sum(response))
figure5.all$majority_party_respon <- 'All Respondents'

figure5.party <- figure5 %>%
  filter(majority_party_respon == 'Majority Party'|majority_party_respon == 'Minority Party') %>% 
  group_by(counter_message, majority_party_respon) %>%
  summarise(mean_majorityft = mean(majorityft), sd_majorityft = sd(majorityft),
            n = sum(response))

figure5.plotdata <- rbind(figure5.all,figure5.party)
figure5.plotdata$lb <- figure5.plotdata$mean_majorityft +
  ((qnorm(0.025)*figure5.plotdata$sd_majorityft)/sqrt(figure5.plotdata$n))
figure5.plotdata$ub <- figure5.plotdata$mean_majorityft +
  ((qnorm(0.975)*figure5.plotdata$sd_majorityft)/sqrt(figure5.plotdata$n))



figure5.final <- ggplot(figure5.plotdata, aes(x = as.factor(majority_party_respon), y = mean_majorityft, 
                                              group = counter_message, color = counter_message)) +
  geom_pointrange(aes(ymin = lb,
                      ymax = ub),
                  size = 1.5, 
                  position = position_dodge(width = .2))+
  ylim(30,80)+
  theme_bw()+
  xlab('\nRespondent Party')+
  ylab('Mean Feeling Thermometer\n for Majority Party\n') +
  labs(color = 'Counter Message')+
  theme(axis.text.x =element_text(size  = 17),
        axis.text.y = element_text(size = 17), 
        axis.title = element_text(size=20),
        legend.text=element_text(size=17),
        legend.title = element_text(size = 17))+
  guides(fill = guide_legend(keywidth = 4, keyheight = 1),
         linetype=guide_legend(keywidth = 4, keyheight = 1))+
  scale_color_manual(values=c('grey1','grey45', 'grey80'))

figure5.final

ggsave('plots_wave7/all_majorityft.png', width = 10, height = 8)


## t-test (Partisan vs None)
# All Respondents
t.test(figure5$majorityft[figure5$counter_message == 'Partisan'],
       figure5$majorityft[figure5$counter_message == 'None'], 
       alternative = 'less')
# Majority Party Respondents
t.test(figure5$majorityft[figure5$counter_message == 'Partisan' & figure5$majority_party_respon == 'Majority Party'],
       figure5$majorityft[figure5$counter_message == 'None' & figure5$majority_party_respon == 'Majority Party'], 
       alternative = 'less')
# Minority Party Respondents
t.test(figure5$majorityft[figure5$counter_message == 'Partisan' & figure5$majority_party_respon == 'Minority Party'],
       figure5$majorityft[figure5$counter_message == 'None' & figure5$majority_party_respon == 'Minority Party'], 
       alternative = 'less')

## t-test (Non-partisan vs None)
# All Respondents
t.test(figure5$majorityft[figure5$counter_message == 'Non-partisan'],
       figure5$majorityft[figure5$counter_message == 'None'], 
       alternative = 'less')
# Majority Party Respondents
t.test(figure5$majorityft[figure5$counter_message == 'Non-partisan' & figure5$majority_party_respon == 'Majority Party'],
       figure5$majorityft[figure5$counter_message == 'None' & figure5$majority_party_respon == 'Majority Party'], 
       alternative = 'less')
# Minority Party Respondents
t.test(figure5$majorityft[figure5$counter_message == 'Non-partisan' & figure5$majority_party_respon == 'Minority Party'],
       figure5$majorityft[figure5$counter_message == 'None' & figure5$majority_party_respon == 'Minority Party'], 
       alternative = 'less')

## t-test (Non-partisan vs Partisan)
# All Respondents
t.test(figure5$majorityft[figure5$counter_message == 'Non-partisan'],
       figure5$majorityft[figure5$counter_message == 'Partisan'], 
       alternative = 'less')
# Majority Party Respondents
t.test(figure5$majorityft[figure5$counter_message == 'Non-partisan' & figure5$majority_party_respon == 'Majority Party'],
       figure5$majorityft[figure5$counter_message == 'Partisan' & figure5$majority_party_respon == 'Majority Party'], 
       alternative = 'less')
# Minority Party Respondents
t.test(figure5$majorityft[figure5$counter_message == 'Non-partisan' & figure5$majority_party_respon == 'Minority Party'],
       figure5$majorityft[figure5$counter_message == 'Partisan' & figure5$majority_party_respon == 'Minority Party'], 
       alternative = 'less')


rm(list=setdiff(ls(), "df"))


####### Figure 6: Minority FT #######

# Remove NAs for response
figure6 <- subset(df, !is.na(minorityft))

figure6.all <- figure6 %>%
  group_by(counter_message) %>%
  summarise(mean_minorityft = mean(minorityft), sd_minorityft = sd(minorityft),
            n = sum(response))
figure6.all$majority_party_respon <- 'All Respondents'

figure6.party <- figure6 %>%
  filter(majority_party_respon == 'Majority Party'|majority_party_respon == 'Minority Party') %>% 
  group_by(counter_message, majority_party_respon) %>%
  summarise(mean_minorityft = mean(minorityft), sd_minorityft = sd(minorityft),
            n = sum(response))

figure6.plotdata <- rbind(figure6.all,figure6.party)
figure6.plotdata$lb <- figure6.plotdata$mean_minorityft +
  ((qnorm(0.025)*figure6.plotdata$sd_minorityft)/sqrt(figure6.plotdata$n))
figure6.plotdata$ub <- figure6.plotdata$mean_minorityft +
  ((qnorm(0.975)*figure6.plotdata$sd_minorityft)/sqrt(figure6.plotdata$n))



figure6.final <- ggplot(figure6.plotdata, aes(x = as.factor(majority_party_respon), y = mean_minorityft, 
                                              group = counter_message, color = counter_message)) +
  geom_pointrange(aes(ymin = lb,
                      ymax = ub),
                  size = 1.5, 
                  position = position_dodge(width = .2))+
  ylim(30,80)+
  theme_bw()+
  xlab('\nRespondent Party')+
  ylab('Mean Feeling Thermometer\n for Minority Party\n') +
  labs(color = 'Counter Message')+
  theme(axis.text.x =element_text(size  = 17),
        axis.text.y = element_text(size = 17), 
        axis.title = element_text(size=20),
        legend.text=element_text(size=17),
        legend.title = element_text(size = 17))+
  guides(fill = guide_legend(keywidth = 4, keyheight = 1),
         linetype=guide_legend(keywidth = 4, keyheight = 1))+
  scale_color_manual(values=c('grey1','grey45', 'grey80'))

figure6.final

ggsave('plots_wave7/all_minorityft.png', width = 10, height = 8)


## t-test (Partisan vs None)
# All Respondents
t.test(figure6$minorityft[figure6$counter_message == 'Partisan'],
       figure6$minorityft[figure6$counter_message == 'None'], 
       alternative = 'less')
# Majority Party Respondents
t.test(figure6$minorityft[figure6$counter_message == 'Partisan' & figure6$majority_party_respon == 'Majority Party'],
       figure6$minorityft[figure6$counter_message == 'None' & figure6$majority_party_respon == 'Majority Party'], 
       alternative = 'less')
# Minority Party Respondents
t.test(figure6$minorityft[figure6$counter_message == 'Partisan' & figure6$majority_party_respon == 'Minority Party'],
       figure6$minorityft[figure6$counter_message == 'None' & figure6$majority_party_respon == 'Minority Party'], 
       alternative = 'less')

## t-test (Non-partisan vs None)
# All Respondents
t.test(figure6$minorityft[figure6$counter_message == 'Non-partisan'],
       figure6$minorityft[figure6$counter_message == 'None'], 
       alternative = 'less')
# Majority Party Respondents
t.test(figure6$minorityft[figure6$counter_message == 'Non-partisan' & figure6$majority_party_respon == 'Majority Party'],
       figure6$minorityft[figure6$counter_message == 'None' & figure6$majority_party_respon == 'Majority Party'], 
       alternative = 'less')
# Minority Party Respondents
t.test(figure6$minorityft[figure6$counter_message == 'Non-partisan' & figure6$majority_party_respon == 'Minority Party'],
       figure6$minorityft[figure6$counter_message == 'None' & figure6$majority_party_respon == 'Minority Party'], 
       alternative = 'less')

## t-test (Non-partisan vs Partisan)
# All Respondents
t.test(figure6$minorityft[figure6$counter_message == 'Non-partisan'],
       figure6$minorityft[figure6$counter_message == 'Partisan'], 
       alternative = 'greater')
# Majority Party Respondents
t.test(figure6$minorityft[figure6$counter_message == 'Non-partisan' & figure6$majority_party_respon == 'Majority Party'],
       figure6$minorityft[figure6$counter_message == 'Partisan' & figure6$majority_party_respon == 'Majority Party'], 
       alternative = 'greater')
# Minority Party Respondents
t.test(figure6$minorityft[figure6$counter_message == 'Non-partisan' & figure6$majority_party_respon == 'Minority Party'],
       figure6$minorityft[figure6$counter_message == 'Partisan' & figure6$majority_party_respon == 'Minority Party'], 
       alternative = 'greater')


rm(list=setdiff(ls(), "df"))




####### Appendix D/Figure 1: Support by Majority Party #######
# Remove NAs for response
figure3 <- subset(df, !is.na(support.leg))

figure3.all <- figure3 %>%
  group_by(counter_message, majority_party_leg) %>%
  summarise(mean_support = mean(support.leg), sd_support = sd(support.leg),
            n = sum(response))
figure3.all$majority_party_respon <- 'All Respondents'

figure3.party <- figure3 %>%
  filter(majority_party_respon == 'Majority Party'|majority_party_respon == 'Minority Party') %>% 
  group_by(counter_message, majority_party_respon, majority_party_leg) %>%
  summarise(mean_support = mean(support.leg), sd_support = sd(support.leg),
            n = sum(response))

figure3.plotdata <- rbind(figure3.all,figure3.party)
figure3.plotdata$lb <- figure3.plotdata$mean_support +
  ((qnorm(0.025)*figure3.plotdata$sd_support)/sqrt(figure3.plotdata$n))
figure3.plotdata$ub <- figure3.plotdata$mean_support +
  ((qnorm(0.975)*figure3.plotdata$sd_support)/sqrt(figure3.plotdata$n))

figure3.plotdata.dem <- subset(figure3.plotdata, majority_party_leg == 'Democrat')
figure3.plotdata.rep <- subset(figure3.plotdata, majority_party_leg == 'Republican')



figure3.final.dem <- ggplot(figure3.plotdata.dem, aes(x = as.factor(majority_party_respon), y = mean_support, 
                                              group = counter_message, color = counter_message)) +
  geom_pointrange(aes(ymin = lb,
                      ymax = ub),
                  size = 1.5, 
                  position = position_dodge(width = .2))+
  ylim(3.5,6)+
  theme_bw()+
  xlab('\nRespondent Party\n (Democratic Legislature)')+
  ylab('Mean Support for Legislation\n') +
  labs(color = 'Counter Message')+
  theme(axis.text.x =element_text(size  = 17),
        axis.text.y = element_text(size = 17), 
        axis.title = element_text(size=20),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 15))+
  guides(fill = guide_legend(keywidth = 4, keyheight = 1),
         linetype=guide_legend(keywidth = 4, keyheight = 1))+
  scale_color_manual(values=c('Grey1','Grey46', 'grey80'))

figure3.final.dem


figure3.final.rep <- ggplot(figure3.plotdata.rep, aes(x = as.factor(majority_party_respon), y = mean_support, 
                                                      group = counter_message, color = counter_message)) +
  geom_pointrange(aes(ymin = lb,
                      ymax = ub),
                  size = 1.5, 
                  position = position_dodge(width = .2))+
  ylim(3.5,6)+
  theme_bw()+
  xlab('\nRespondent Party\n (Republican Legislature)')+
  ylab('Mean Support for Legislation\n') +
  labs(color = 'Counter Message')+
  theme(axis.text.x =element_text(size  = 17),
        axis.text.y = element_text(size = 17), 
        axis.title = element_text(size=20),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 15))+
  guides(fill = guide_legend(keywidth = 4, keyheight = 1),
         linetype=guide_legend(keywidth = 4, keyheight = 1))+
  scale_color_manual(values=c('Grey1','Grey46', 'grey80'))

figure3.final.rep


joint <- ggarrange(figure3.final.dem, figure3.final.rep, ncol=2, nrow=1, common.legend = TRUE, legend="right")

joint

ggsave('plots_wave7/figure3_appendix.png', width = 16, height = 8)

rm(list=setdiff(ls(), "df"))


####### Appendix D/Figure 2: Approval by Majority Party ####### 
# Remove NAs for response
figure4 <- subset(df, !is.na(leg.approval))

figure4.all <- figure4 %>%
  group_by(counter_message, majority_party_leg) %>%
  summarise(mean_approval = mean(leg.approval), sd_approval = sd(leg.approval),
            n = sum(response))
figure4.all$majority_party_respon <- 'All Respondents'

figure4.party <- figure4 %>%
  filter(majority_party_respon == 'Majority Party'|majority_party_respon == 'Minority Party') %>% 
  group_by(counter_message, majority_party_respon, majority_party_leg) %>%
  summarise(mean_approval = mean(leg.approval), sd_approval = sd(leg.approval),
            n = sum(response))

figure4.plotdata <- rbind(figure4.all,figure4.party)
figure4.plotdata$lb <- figure4.plotdata$mean_approval +
  ((qnorm(0.025)*figure4.plotdata$sd_approval)/sqrt(figure4.plotdata$n))
figure4.plotdata$ub <- figure4.plotdata$mean_approval +
  ((qnorm(0.975)*figure4.plotdata$sd_approval)/sqrt(figure4.plotdata$n))


figure4.plotdata.dem <- subset(figure4.plotdata, majority_party_leg == 'Democrat')
figure4.plotdata.rep <- subset(figure4.plotdata, majority_party_leg == 'Republican')

figure4.final.dem <- ggplot(figure4.plotdata.dem, aes(x = as.factor(majority_party_respon), y = mean_approval, 
                                              group = counter_message, color = counter_message)) +
  geom_pointrange(aes(ymin = lb,
                      ymax = ub),
                  size = 1.5, 
                  position = position_dodge(width = .2))+
  ylim(3.5,6)+
  theme_bw()+
  xlab('\nRespondent Party \n (Democratic Legislature)')+
  ylab('Mean Approval of Legislature\n') +
  labs(color = 'Counter Message')+
  theme(axis.text.x =element_text(size  = 17),
        axis.text.y = element_text(size = 17), 
        axis.title = element_text(size=20),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 15))+
  guides(fill = guide_legend(keywidth = 4, keyheight = 1),
         linetype=guide_legend(keywidth = 4, keyheight = 1))+
  scale_color_manual(values=c('Grey1','Grey46', 'grey80'))

figure4.final.dem

figure4.final.rep <- ggplot(figure4.plotdata.rep, aes(x = as.factor(majority_party_respon), y = mean_approval, 
                                                      group = counter_message, color = counter_message)) +
  geom_pointrange(aes(ymin = lb,
                      ymax = ub),
                  size = 1.5, 
                  position = position_dodge(width = .2))+
  ylim(3.5,6)+
  theme_bw()+
  xlab('\nRespondent Party\n (Republican Legislature)')+
  ylab('Mean Approval of Legislature\n') +
  labs(color = 'Counter Message')+
  theme(axis.text.x =element_text(size  = 17),
        axis.text.y = element_text(size = 17), 
        axis.title = element_text(size=20),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 15))+
  guides(fill = guide_legend(keywidth = 4, keyheight = 1),
         linetype=guide_legend(keywidth = 4, keyheight = 1))+
  scale_color_manual(values=c('Grey1','Grey46', 'grey80'))

figure4.final.rep

joint <- ggarrange(figure4.final.dem, figure4.final.rep, ncol=2, nrow=1, common.legend = TRUE, legend="right")

joint

ggsave('plots_wave7/figure4_appendix.png', width = 16, height = 8)


rm(list=setdiff(ls(), "df"))

####### Appendix D/Figure 3: Majority Ft by Majority Party #######
# Remove NAs for response
figure6 <- subset(df, !is.na(majorityft))

figure6.all <- figure6 %>%
  group_by(counter_message, majority_party_leg) %>%
  summarise(mean_majorityft = mean(majorityft), sd_majorityft = sd(majorityft),
            n = sum(response))
figure6.all$majority_party_respon <- 'All Respondents'

figure6.party <- figure6 %>%
  filter(majority_party_respon == 'Majority Party'|majority_party_respon == 'Minority Party') %>% 
  group_by(counter_message, majority_party_respon, majority_party_leg) %>%
  summarise(mean_majorityft = mean(majorityft), sd_majorityft = sd(majorityft),
            n = sum(response))

figure6.plotdata <- rbind(figure6.all,figure6.party)
figure6.plotdata$lb <- figure6.plotdata$mean_majorityft +
  ((qnorm(0.025)*figure6.plotdata$sd_majorityft)/sqrt(figure6.plotdata$n))
figure6.plotdata$ub <- figure6.plotdata$mean_majorityft +
  ((qnorm(0.975)*figure6.plotdata$sd_majorityft)/sqrt(figure6.plotdata$n))


figure6.plotdata.dem <- subset(figure6.plotdata, majority_party_leg == 'Democrat')
figure6.plotdata.rep <- subset(figure6.plotdata, majority_party_leg == 'Republican')


figure6.final.dem <- ggplot(figure6.plotdata.dem, aes(x = as.factor(majority_party_respon), y = mean_majorityft, 
                                              group = counter_message, color = counter_message)) +
  geom_pointrange(aes(ymin = lb,
                      ymax = ub),
                  size = 1.5, 
                  position = position_dodge(width = .2))+
  ylim(30,80)+
  theme_bw()+
  xlab('\nRespondent Party\n (Democratic Legislature)')+
  ylab('Mean Feeling Thermometer\n for Majority Party\n') +
  labs(color = 'Counter Message')+
  theme(axis.text.x =element_text(size  = 17),
        axis.text.y = element_text(size = 17), 
        axis.title = element_text(size=20),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 15))+
  guides(fill = guide_legend(keywidth = 4, keyheight = 1),
         linetype=guide_legend(keywidth = 4, keyheight = 1))+
  scale_color_manual(values=c('Grey1','Grey46', 'grey80'))

figure6.final.dem

figure6.final.rep <- ggplot(figure6.plotdata.rep, aes(x = as.factor(majority_party_respon), y = mean_majorityft, 
                                              group = counter_message, color = counter_message)) +
  geom_pointrange(aes(ymin = lb,
                      ymax = ub),
                  size = 1.5, 
                  position = position_dodge(width = .2))+
  ylim(30,80)+
  theme_bw()+
  xlab('\nRespondent Party \n(Republican Legislature)')+
  ylab('Mean Feeling Thermometer\n for Majority Party\n') +
  labs(color = 'Counter Message')+
  theme(axis.text.x =element_text(size  = 17),
        axis.text.y = element_text(size = 17), 
        axis.title = element_text(size=20),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 15))+
  guides(fill = guide_legend(keywidth = 4, keyheight = 1),
         linetype=guide_legend(keywidth = 4, keyheight = 1))+
  scale_color_manual(values=c('Grey1','Grey46', 'grey80'))

figure6.final.rep

joint <- ggarrange(figure6.final.dem, figure6.final.rep, ncol=2, nrow=1, common.legend = TRUE, legend="right")

joint

ggsave('plots_wave7/figure6_appendix.png', width = 16, height = 8)



rm(list=setdiff(ls(), "df"))



####### Appendix D/Figure 4: Minority Ft by Majority Party ####### 
# Remove NAs for response
figure7 <- subset(df, !is.na(minorityft))

figure7.all <- figure7 %>%
  group_by(counter_message, majority_party_leg) %>%
  summarise(mean_minorityft = mean(minorityft), sd_minorityft = sd(minorityft),
            n = sum(response))
figure7.all$majority_party_respon <- 'All Respondents'

figure7.party <- figure7 %>%
  filter(majority_party_respon == 'Majority Party'|majority_party_respon == 'Minority Party') %>% 
  group_by(counter_message, majority_party_respon, majority_party_leg) %>%
  summarise(mean_minorityft = mean(minorityft), sd_minorityft = sd(minorityft),
            n = sum(response))

figure7.plotdata <- rbind(figure7.all,figure7.party)
figure7.plotdata$lb <- figure7.plotdata$mean_minorityft +
  ((qnorm(0.025)*figure7.plotdata$sd_minorityft)/sqrt(figure7.plotdata$n))
figure7.plotdata$ub <- figure7.plotdata$mean_minorityft +
  ((qnorm(0.975)*figure7.plotdata$sd_minorityft)/sqrt(figure7.plotdata$n))

figure7.plotdata.dem <- subset(figure7.plotdata, majority_party_leg == 'Democrat')
figure7.plotdata.rep <- subset(figure7.plotdata, majority_party_leg == 'Republican')


figure7.final.dem <- ggplot(figure7.plotdata.dem, aes(x = as.factor(majority_party_respon), y = mean_minorityft, 
                                              group = counter_message, color = counter_message)) +
  geom_pointrange(aes(ymin = lb,
                      ymax = ub),
                  size = 1.5, 
                  position = position_dodge(width = .2))+
  ylim(30,80)+
  theme_bw()+
  xlab('\nRespondent Party\n (Democratic Legislature)')+
  ylab('Mean Feeling Thermometer\n for Minority Party\n') +
  labs(color = 'Counter Message')+
  theme(axis.text.x =element_text(size  = 17),
        axis.text.y = element_text(size = 17), 
        axis.title = element_text(size=20),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 15))+
  guides(fill = guide_legend(keywidth = 4, keyheight = 1),
         linetype=guide_legend(keywidth = 4, keyheight = 1))+
  scale_color_manual(values=c('Grey1','Grey46', 'grey80'))

figure7.final.dem

figure7.final.rep <- ggplot(figure7.plotdata.rep, aes(x = as.factor(majority_party_respon), y = mean_minorityft, 
                                                      group = counter_message, color = counter_message)) +
  geom_pointrange(aes(ymin = lb,
                      ymax = ub),
                  size = 1.5, 
                  position = position_dodge(width = .2))+
  ylim(30,80)+
  theme_bw()+
  xlab('\nRespondent Party\n (Republican Legislature)')+
  ylab('Mean Feeling Thermometer\n for Minority Party\n') +
  labs(color = 'Counter Message')+
  theme(axis.text.x =element_text(size  = 17),
        axis.text.y = element_text(size = 17), 
        axis.title = element_text(size=20),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 15))+
  guides(fill = guide_legend(keywidth = 4, keyheight = 1),
         linetype=guide_legend(keywidth = 4, keyheight = 1))+
  scale_color_manual(values=c('Grey1','Grey46', 'grey80'))

figure7.final.rep

joint <- ggarrange(figure7.final.dem, figure7.final.rep, ncol=2, nrow=1, common.legend = TRUE, legend="right")

joint

ggsave('plots_wave7/figure7_appendix.png', width = 16, height = 8)



####### Appendix E/Figure 1: Support by Partisan Strength and Party (Majority/Minority) #######
# Remove NAs for response
figure3 <- subset(df, !is.na(support.leg))

figure3.party <- figure3 %>%
  filter(majority_party_respon == 'Majority Party'|majority_party_respon == 'Minority Party') %>% 
  group_by(counter_message, majority_party_respon, part.strength) %>%
  summarise(mean_support = mean(support.leg), sd_support = sd(support.leg),
            n = sum(response))

figure3.plotdata <- figure3.party
figure3.plotdata$lb <- figure3.plotdata$mean_support +
  ((qnorm(0.025)*figure3.plotdata$sd_support)/sqrt(figure3.plotdata$n))
figure3.plotdata$ub <- figure3.plotdata$mean_support +
  ((qnorm(0.975)*figure3.plotdata$sd_support)/sqrt(figure3.plotdata$n))

figure3.plotdata.maj <- subset(figure3.plotdata, majority_party_respon == 'Majority Party')
figure3.plotdata.min <- subset(figure3.plotdata, majority_party_respon == 'Minority Party')


figure3.final.maj <- ggplot(figure3.plotdata.maj, aes(x = as.factor(part.strength), y = mean_support, 
                                              group = counter_message, color = counter_message)) +
  geom_pointrange(aes(ymin = lb,
                      ymax = ub),
                  size = 1.5, 
                  position = position_dodge(width = .2))+
  ylim(3.5,6)+
  theme_bw()+
  xlab("\nRespondent Strength of \nPartisanship (Majority Party)")+
  ylab('Mean Support for Legislation\n') +
  labs(color = 'Counter Message')+
  theme(axis.text.x =element_text(size  = 17),
        axis.text.y = element_text(size = 17), 
        axis.title = element_text(size=20),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 15))+
  guides(fill = guide_legend(keywidth = 4, keyheight = 1),
         linetype=guide_legend(keywidth = 4, keyheight = 1))+
  scale_color_manual(values=c('Grey1','Grey46', 'grey80'))

figure3.final.maj

figure3.final.min <- ggplot(figure3.plotdata.min, aes(x = as.factor(part.strength), y = mean_support, 
                                                      group = counter_message, color = counter_message)) +
  geom_pointrange(aes(ymin = lb,
                      ymax = ub),
                  size = 1.5, 
                  position = position_dodge(width = .2))+
  ylim(3.5,6)+
  theme_bw()+
  xlab("\nRespondent Strength of \nPartisanship (Minority Party)")+
  ylab('Mean Support for Legislation\n') +
  labs(color = 'Counter Message')+
  theme(axis.text.x =element_text(size  = 17),
        axis.text.y = element_text(size = 17), 
        axis.title = element_text(size=20),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 15))+
  guides(fill = guide_legend(keywidth = 4, keyheight = 1),
         linetype=guide_legend(keywidth = 4, keyheight = 1))+
  scale_color_manual(values=c('Grey1','Grey46', 'grey80'))

figure3.final.min

joint <- ggarrange(figure3.final.maj, figure3.final.min, ncol=2, nrow=1, common.legend = TRUE, legend="right")

joint

ggsave('plots_wave7/partstrength_support.png', width = 16, height = 8)




# T Test Code for Checking

### None versus Partisan Counter
t.test(figure3$support.leg[figure3$counter_message == 'None' & figure3$majority_party_respon == 'Majority Party' &
                             figure3$part.strength == 'Strong Partisans'],
       figure3$support.leg[figure3$counter_message == 'Partisan' & figure3$majority_party_respon == 'Majority Party'&
                             figure3$part.strength == 'Strong Partisans'])

t.test(figure3$support.leg[figure3$counter_message == 'None' & figure3$majority_party_respon == 'Majority Party' &
                             figure3$part.strength == 'Weak Partisans'],
       figure3$support.leg[figure3$counter_message == 'Partisan' & figure3$majority_party_respon == 'Majority Party'&
                             figure3$part.strength == 'Weak Partisans'])


t.test(figure3$support.leg[figure3$counter_message == 'None' & figure3$majority_party_respon == 'Majority Party' &
                             figure3$part.strength == 'Leaners'],
       figure3$support.leg[figure3$counter_message == 'Partisan' & figure3$majority_party_respon == 'Majority Party'&
                             figure3$part.strength == 'Leaners'])
#
t.test(figure3$support.leg[figure3$counter_message == 'None' & figure3$majority_party_respon == 'Majority Party' &
                             figure3$part.strength == 'Strong Partisans'],
       figure3$support.leg[figure3$counter_message == 'Non-partisan' & figure3$majority_party_respon == 'Majority Party'&
                             figure3$part.strength == 'Strong Partisans'])


t.test(figure3$support.leg[figure3$counter_message == 'None' & figure3$majority_party_respon == 'Majority Party' &
                             figure3$part.strength == 'Weak Partisans'],
       figure3$support.leg[figure3$counter_message == 'Non-partisan' & figure3$majority_party_respon == 'Majority Party'&
                             figure3$part.strength == 'Weak Partisans'])

t.test(figure3$support.leg[figure3$counter_message == 'None' & figure3$majority_party_respon == 'Majority Party' &
                             figure3$part.strength == 'Leaners'],
       figure3$support.leg[figure3$counter_message == 'Non-partisan' & figure3$majority_party_respon == 'Majority Party'&
                             figure3$part.strength == 'Leaners'])

#
t.test(figure3$support.leg[figure3$counter_message == 'Non-partisan' & figure3$majority_party_respon == 'Majority Party' &
                             figure3$part.strength == 'Strong Partisans'],
       figure3$support.leg[figure3$counter_message == 'Partisan' & figure3$majority_party_respon == 'Majority Party'&
                             figure3$part.strength == 'Strong Partisans'])

t.test(figure3$support.leg[figure3$counter_message == 'Non-partisan' & figure3$majority_party_respon == 'Majority Party' &
                             figure3$part.strength == 'Weak Partisans'],
       figure3$support.leg[figure3$counter_message == 'Partisan' & figure3$majority_party_respon == 'Majority Party'&
                             figure3$part.strength == 'Weak Partisans'])

t.test(figure3$support.leg[figure3$counter_message == 'Non-partisan' & figure3$majority_party_respon == 'Majority Party' &
                             figure3$part.strength == 'Leaners'],
       figure3$support.leg[figure3$counter_message == 'Partisan' & figure3$majority_party_respon == 'Majority Party'&
                             figure3$part.strength == 'Leaners'])

# Minority Party
t.test(figure3$support.leg[figure3$counter_message == 'None' & figure3$majority_party_respon == 'Minority Party' &
                             figure3$part.strength == 'Strong Partisans'],
       figure3$support.leg[figure3$counter_message == 'Partisan' & figure3$majority_party_respon == 'Minority Party'&
                             figure3$part.strength == 'Strong Partisans'])

t.test(figure3$support.leg[figure3$counter_message == 'None' & figure3$majority_party_respon == 'Minority Party' &
                             figure3$part.strength == 'Weak Partisans'],
       figure3$support.leg[figure3$counter_message == 'Partisan' & figure3$majority_party_respon == 'Minority Party'&
                             figure3$part.strength == 'Weak Partisans'])


t.test(figure3$support.leg[figure3$counter_message == 'None' & figure3$majority_party_respon == 'Minority Party' &
                             figure3$part.strength == 'Leaners'],
       figure3$support.leg[figure3$counter_message == 'Partisan' & figure3$majority_party_respon == 'Minority Party'&
                             figure3$part.strength == 'Leaners'])
#
t.test(figure3$support.leg[figure3$counter_message == 'None' & figure3$majority_party_respon == 'Minority Party' &
                             figure3$part.strength == 'Strong Partisans'],
       figure3$support.leg[figure3$counter_message == 'Non-partisan' & figure3$majority_party_respon == 'Minority Party'&
                             figure3$part.strength == 'Strong Partisans'])


t.test(figure3$support.leg[figure3$counter_message == 'None' & figure3$majority_party_respon == 'Minority Party' &
                             figure3$part.strength == 'Weak Partisans'],
       figure3$support.leg[figure3$counter_message == 'Non-partisan' & figure3$majority_party_respon == 'Minority Party'&
                             figure3$part.strength == 'Weak Partisans'])

t.test(figure3$support.leg[figure3$counter_message == 'None' & figure3$majority_party_respon == 'Minority Party' &
                             figure3$part.strength == 'Leaners'],
       figure3$support.leg[figure3$counter_message == 'Non-partisan' & figure3$majority_party_respon == 'Minority Party'&
                             figure3$part.strength == 'Leaners'])

#
t.test(figure3$support.leg[figure3$counter_message == 'Non-partisan' & figure3$majority_party_respon == 'Minority Party' &
                             figure3$part.strength == 'Strong Partisans'],
       figure3$support.leg[figure3$counter_message == 'Partisan' & figure3$majority_party_respon == 'Minority Party'&
                             figure3$part.strength == 'Strong Partisans'])

t.test(figure3$support.leg[figure3$counter_message == 'Non-partisan' & figure3$majority_party_respon == 'Minority Party' &
                             figure3$part.strength == 'Weak Partisans'],
       figure3$support.leg[figure3$counter_message == 'Partisan' & figure3$majority_party_respon == 'Minority Party'&
                             figure3$part.strength == 'Weak Partisans'])

t.test(figure3$support.leg[figure3$counter_message == 'Non-partisan' & figure3$majority_party_respon == 'Minority Party' &
                             figure3$part.strength == 'Leaners'],
       figure3$support.leg[figure3$counter_message == 'Partisan' & figure3$majority_party_respon == 'Minority Party'&
                             figure3$part.strength == 'Leaners'])




####### Appendix E/Figure 2: Approval by Strength and Party (Majority/Minority) ####### 
# Remove NAs for response
figure4 <- subset(df, !is.na(leg.approval))

figure4.party <- figure4 %>%
  filter(majority_party_respon == 'Majority Party'|majority_party_respon == 'Minority Party') %>% 
  group_by(counter_message, majority_party_respon, part.strength) %>%
  summarise(mean_approval = mean(leg.approval), sd_approval = sd(leg.approval),
            n = sum(response))

figure4.plotdata <- figure4.party
figure4.plotdata$lb <- figure4.plotdata$mean_approval +
  ((qnorm(0.025)*figure4.plotdata$sd_approval)/sqrt(figure4.plotdata$n))
figure4.plotdata$ub <- figure4.plotdata$mean_approval +
  ((qnorm(0.975)*figure4.plotdata$sd_approval)/sqrt(figure4.plotdata$n))

figure4.plotdata.maj <- subset(figure4.plotdata, majority_party_respon == 'Majority Party')
figure4.plotdata.min <- subset(figure4.plotdata, majority_party_respon == 'Minority Party')



figure4.final.maj <- ggplot(figure4.plotdata.maj, aes(x = as.factor(part.strength), y = mean_approval, 
                                              group = counter_message, color = counter_message)) +
  geom_pointrange(aes(ymin = lb,
                      ymax = ub),
                  size = 1.5, 
                  position = position_dodge(width = .2))+
  ylim(3.5,6)+
  theme_bw()+
  xlab('\nRespondent Strength of \nPartisanship (Majority Party)')+
  ylab('Mean Approval of Legislature\n') +
  labs(color = 'Counter Message')+
  theme(axis.text.x =element_text(size  = 17),
        axis.text.y = element_text(size = 17), 
        axis.title = element_text(size=20),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 15))+
  guides(fill = guide_legend(keywidth = 4, keyheight = 1),
         linetype=guide_legend(keywidth = 4, keyheight = 1))+
  scale_color_manual(values=c('Grey1','Grey46', 'grey80'))

figure4.final.maj

figure4.final.min <- ggplot(figure4.plotdata.min, aes(x = as.factor(part.strength), y = mean_approval, 
                                                      group = counter_message, color = counter_message)) +
  geom_pointrange(aes(ymin = lb,
                      ymax = ub),
                  size = 1.5, 
                  position = position_dodge(width = .2))+
  ylim(3.5,6)+
  theme_bw()+
  xlab('\nRespondent Strength of \nPartisanship (Minority Party)')+
  ylab('Mean Approval of Legislature\n') +
  labs(color = 'Counter Message')+
  theme(axis.text.x =element_text(size  = 17),
        axis.text.y = element_text(size = 17), 
        axis.title = element_text(size=20),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 15))+
  guides(fill = guide_legend(keywidth = 4, keyheight = 1),
         linetype=guide_legend(keywidth = 4, keyheight = 1))+
  scale_color_manual(values=c('Grey1','Grey46', 'grey80'))

figure4.final.min


joint <- ggarrange(figure4.final.maj, figure4.final.min, ncol=2, nrow=1, common.legend = TRUE, legend="right")

joint

ggsave('plots_wave7/partstrength_approval.png', width = 16, height = 8)

# T Test Code for Checking

### None versus Partisan Counter
t.test(figure4$leg.approval[figure4$counter_message == 'None' & figure4$majority_party_respon == 'Majority Party' &
                             figure4$part.strength == 'Strong Partisans'],
       figure4$leg.approval[figure4$counter_message == 'Partisan' & figure4$majority_party_respon == 'Majority Party'&
                             figure4$part.strength == 'Strong Partisans'])

t.test(figure4$leg.approval[figure4$counter_message == 'None' & figure4$majority_party_respon == 'Majority Party' &
                             figure4$part.strength == 'Weak Partisans'],
       figure4$leg.approval[figure4$counter_message == 'Partisan' & figure4$majority_party_respon == 'Majority Party'&
                             figure4$part.strength == 'Weak Partisans'])


t.test(figure4$leg.approval[figure4$counter_message == 'None' & figure4$majority_party_respon == 'Majority Party' &
                             figure4$part.strength == 'Leaners'],
       figure4$leg.approval[figure4$counter_message == 'Partisan' & figure4$majority_party_respon == 'Majority Party'&
                             figure4$part.strength == 'Leaners'])
#
t.test(figure4$leg.approval[figure4$counter_message == 'None' & figure4$majority_party_respon == 'Majority Party' &
                             figure4$part.strength == 'Strong Partisans'],
       figure4$leg.approval[figure4$counter_message == 'Non-partisan' & figure4$majority_party_respon == 'Majority Party'&
                             figure4$part.strength == 'Strong Partisans'])


t.test(figure4$leg.approval[figure4$counter_message == 'None' & figure4$majority_party_respon == 'Majority Party' &
                             figure4$part.strength == 'Weak Partisans'],
       figure4$leg.approval[figure4$counter_message == 'Non-partisan' & figure4$majority_party_respon == 'Majority Party'&
                             figure4$part.strength == 'Weak Partisans'])

t.test(figure4$leg.approval[figure4$counter_message == 'None' & figure4$majority_party_respon == 'Majority Party' &
                             figure4$part.strength == 'Leaners'],
       figure4$leg.approval[figure4$counter_message == 'Non-partisan' & figure4$majority_party_respon == 'Majority Party'&
                             figure4$part.strength == 'Leaners'])

#
t.test(figure4$leg.approval[figure4$counter_message == 'Non-partisan' & figure4$majority_party_respon == 'Majority Party' &
                             figure4$part.strength == 'Strong Partisans'],
       figure4$leg.approval[figure4$counter_message == 'Partisan' & figure4$majority_party_respon == 'Majority Party'&
                             figure4$part.strength == 'Strong Partisans'])

t.test(figure4$leg.approval[figure4$counter_message == 'Non-partisan' & figure4$majority_party_respon == 'Majority Party' &
                             figure4$part.strength == 'Weak Partisans'],
       figure4$leg.approval[figure4$counter_message == 'Partisan' & figure4$majority_party_respon == 'Majority Party'&
                             figure4$part.strength == 'Weak Partisans'])

t.test(figure4$leg.approval[figure4$counter_message == 'Non-partisan' & figure4$majority_party_respon == 'Majority Party' &
                             figure4$part.strength == 'Leaners'],
       figure4$leg.approval[figure4$counter_message == 'Partisan' & figure4$majority_party_respon == 'Majority Party'&
                             figure4$part.strength == 'Leaners'])

# Minority Party
t.test(figure4$leg.approval[figure4$counter_message == 'None' & figure4$majority_party_respon == 'Minority Party' &
                             figure4$part.strength == 'Strong Partisans'],
       figure4$leg.approval[figure4$counter_message == 'Partisan' & figure4$majority_party_respon == 'Minority Party'&
                             figure4$part.strength == 'Strong Partisans'])

t.test(figure4$leg.approval[figure4$counter_message == 'None' & figure4$majority_party_respon == 'Minority Party' &
                             figure4$part.strength == 'Weak Partisans'],
       figure4$leg.approval[figure4$counter_message == 'Partisan' & figure4$majority_party_respon == 'Minority Party'&
                             figure4$part.strength == 'Weak Partisans'])


t.test(figure4$leg.approval[figure4$counter_message == 'None' & figure4$majority_party_respon == 'Minority Party' &
                             figure4$part.strength == 'Leaners'],
       figure4$leg.approval[figure4$counter_message == 'Partisan' & figure4$majority_party_respon == 'Minority Party'&
                             figure4$part.strength == 'Leaners'])
#
t.test(figure4$leg.approval[figure4$counter_message == 'None' & figure4$majority_party_respon == 'Minority Party' &
                             figure4$part.strength == 'Strong Partisans'],
       figure4$leg.approval[figure4$counter_message == 'Non-partisan' & figure4$majority_party_respon == 'Minority Party'&
                             figure4$part.strength == 'Strong Partisans'])


t.test(figure4$leg.approval[figure4$counter_message == 'None' & figure4$majority_party_respon == 'Minority Party' &
                             figure4$part.strength == 'Weak Partisans'],
       figure4$leg.approval[figure4$counter_message == 'Non-partisan' & figure4$majority_party_respon == 'Minority Party'&
                             figure4$part.strength == 'Weak Partisans'])

t.test(figure4$leg.approval[figure4$counter_message == 'None' & figure4$majority_party_respon == 'Minority Party' &
                             figure4$part.strength == 'Leaners'],
       figure4$leg.approval[figure4$counter_message == 'Non-partisan' & figure4$majority_party_respon == 'Minority Party'&
                             figure4$part.strength == 'Leaners'])

#
t.test(figure4$leg.approval[figure4$counter_message == 'Non-partisan' & figure4$majority_party_respon == 'Minority Party' &
                             figure4$part.strength == 'Strong Partisans'],
       figure4$leg.approval[figure4$counter_message == 'Partisan' & figure4$majority_party_respon == 'Minority Party'&
                             figure4$part.strength == 'Strong Partisans'])

t.test(figure4$leg.approval[figure4$counter_message == 'Non-partisan' & figure4$majority_party_respon == 'Minority Party' &
                             figure4$part.strength == 'Weak Partisans'],
       figure4$leg.approval[figure4$counter_message == 'Partisan' & figure4$majority_party_respon == 'Minority Party'&
                             figure4$part.strength == 'Weak Partisans'])

t.test(figure4$leg.approval[figure4$counter_message == 'Non-partisan' & figure4$majority_party_respon == 'Minority Party' &
                             figure4$part.strength == 'Leaners'],
       figure4$leg.approval[figure4$counter_message == 'Partisan' & figure4$majority_party_respon == 'Minority Party'&
                             figure4$part.strength == 'Leaners'])


####### Appendix E/Figure 3: Majority Ft by Strength and Party (Majority/Minority) #######  
# Remove NAs for response
figure6 <- subset(df, !is.na(majorityft))

figure6.party <- figure6 %>%
  filter(majority_party_respon == 'Majority Party'|majority_party_respon == 'Minority Party') %>% 
  group_by(counter_message, majority_party_respon, part.strength) %>%
  summarise(mean_majorityft = mean(majorityft), sd_majorityft = sd(majorityft),
            n = sum(response))

figure6.plotdata <- figure6.party
figure6.plotdata$lb <- figure6.plotdata$mean_majorityft +
  ((qnorm(0.025)*figure6.plotdata$sd_majorityft)/sqrt(figure6.plotdata$n))
figure6.plotdata$ub <- figure6.plotdata$mean_majorityft +
  ((qnorm(0.975)*figure6.plotdata$sd_majorityft)/sqrt(figure6.plotdata$n))

figure6.plotdata.maj <- subset(figure6.plotdata, majority_party_respon == 'Majority Party')
figure6.plotdata.min <- subset(figure6.plotdata, majority_party_respon == 'Minority Party')


figure6.final.maj <- ggplot(figure6.plotdata.maj, aes(x = as.factor(part.strength), y = mean_majorityft, 
                                              group = counter_message, color = counter_message)) +
  geom_pointrange(aes(ymin = lb,
                      ymax = ub),
                  size = 1.5, 
                  position = position_dodge(width = .2))+
  ylim(30,85)+
  theme_bw()+
  xlab('\nRespondent Strength of \nPartisanship (Majority Party)')+
  ylab('Mean Feeling Thermometer\n for Majority Party\n') +
  labs(color = 'Counter Message')+
  theme(axis.text.x =element_text(size  = 17),
        axis.text.y = element_text(size = 17), 
        axis.title = element_text(size=20),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 15))+
  guides(fill = guide_legend(keywidth = 4, keyheight = 1),
         linetype=guide_legend(keywidth = 4, keyheight = 1))+
  scale_color_manual(values=c('Grey1','Grey46', 'grey80'))

figure6.final.maj

figure6.final.min <- ggplot(figure6.plotdata.min, aes(x = as.factor(part.strength), y = mean_majorityft, 
                                                      group = counter_message, color = counter_message)) +
  geom_pointrange(aes(ymin = lb,
                      ymax = ub),
                  size = 1.5, 
                  position = position_dodge(width = .2))+
  ylim(30,85)+
  theme_bw()+
  xlab('\nRespondent Strength of \nPartisanship (Minority Party)')+
  ylab('Mean Feeling Thermometer\n for Majority Party\n') +
  labs(color = 'Counter Message')+
  theme(axis.text.x =element_text(size  = 17),
        axis.text.y = element_text(size = 17), 
        axis.title = element_text(size=20),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 15))+
  guides(fill = guide_legend(keywidth = 4, keyheight = 1),
         linetype=guide_legend(keywidth = 4, keyheight = 1))+
  scale_color_manual(values=c('Grey1','Grey46', 'grey80'))

figure6.final.min

joint <- ggarrange(figure6.final.maj, figure6.final.min, ncol=2, nrow=1, common.legend = TRUE, legend="right")

joint

ggsave('plots_wave7/partstrength_majfeeling.png', width = 16, height = 8)


####### Appendix E/Figure 4: Minority Ft by Strength and Party (Majority/Minority) #######  
# Remove NAs for response
figure7 <- subset(df, !is.na(minorityft))

figure7.party <- figure7 %>%
  filter(majority_party_respon == 'Majority Party'|majority_party_respon == 'Minority Party') %>% 
  group_by(counter_message, majority_party_respon, part.strength) %>%
  summarise(mean_minorityft = mean(minorityft), sd_minorityft = sd(minorityft),
            n = sum(response))

figure7.plotdata <- figure7.party
figure7.plotdata$lb <- figure7.plotdata$mean_minorityft +
  ((qnorm(0.025)*figure7.plotdata$sd_minorityft)/sqrt(figure7.plotdata$n))
figure7.plotdata$ub <- figure7.plotdata$mean_minorityft +
  ((qnorm(0.975)*figure7.plotdata$sd_minorityft)/sqrt(figure7.plotdata$n))

figure7.plotdata.maj <- subset(figure7.plotdata, majority_party_respon == 'Majority Party')
figure7.plotdata.min <- subset(figure7.plotdata, majority_party_respon == 'Minority Party')


figure7.final.maj <- ggplot(figure7.plotdata.maj, aes(x = as.factor(part.strength), y = mean_minorityft, 
                                              group = counter_message, color = counter_message)) +
  geom_pointrange(aes(ymin = lb,
                      ymax = ub),
                  size = 1.5, 
                  position = position_dodge(width = .2))+
  ylim(20,80)+
  theme_bw()+
  xlab('\nRespondent Strength of \nPartisanship (Majority Party)')+
  ylab('Mean Feeling Thermometer\n for Minority Party\n') +
  labs(color = 'Counter Message')+
  theme(axis.text.x =element_text(size  = 17),
        axis.text.y = element_text(size = 17), 
        axis.title = element_text(size=20),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 15))+
  guides(fill = guide_legend(keywidth = 4, keyheight = 1),
         linetype=guide_legend(keywidth = 4, keyheight = 1))+
  scale_color_manual(values=c('Grey1','Grey46', 'grey80'))

figure7.final.maj

figure7.final.min <- ggplot(figure7.plotdata.min, aes(x = as.factor(part.strength), y = mean_minorityft, 
                                                      group = counter_message, color = counter_message)) +
  geom_pointrange(aes(ymin = lb,
                      ymax = ub),
                  size = 1.5, 
                  position = position_dodge(width = .2))+
  ylim(30,80)+
  theme_bw()+
  xlab('\nRespondent Strength of \nPartisanship (Minority Party)')+
  ylab('Mean Feeling Thermometer\n for Minority Party\n') +
  labs(color = 'Counter Message')+
  theme(axis.text.x =element_text(size  = 17),
        axis.text.y = element_text(size = 17), 
        axis.title = element_text(size=20),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 15))+
  guides(fill = guide_legend(keywidth = 4, keyheight = 1),
         linetype=guide_legend(keywidth = 4, keyheight = 1))+
  scale_color_manual(values=c('Grey1','Grey46', 'grey80'))

figure7.final.min


joint <- ggarrange(figure7.final.maj, figure7.final.min, ncol=2, nrow=1, common.legend = TRUE, legend="right")

joint

ggsave('plots_wave7/partstrength_minfeeling.png', width = 16, height = 8)


t.test(figure7$minorityft[figure7$counter_message == 'None' & figure7$majority_party_respon == 'Minority Party' &
                              figure7$part.strength == 'Strong Partisans'],
       figure7$minorityft[figure7$counter_message == 'Partisan' & figure7$majority_party_respon == 'Minority Party'&
                              figure7$part.strength == 'Strong Partisans'])



