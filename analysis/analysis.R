this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

library(plyr)
library(dplyr)
library(reshape)
library(ggplot2)
library(gtable)
library(lme4)
library(tidyverse)
library(simr)
library(lmerTest)
library(brms)
library(bootstrap)
library(ggpubr)
library(broom)    

`%notin%` <- Negate(`%in%`)

#Read data file#
raw_data_path1 <- "../pilot_2a_ling145/pilot_2a_ling145-trials.csv"
data1<-read.csv(raw_data_path1)

raw_data_path2 <- "../pilot_2b_ling145/pilot_2b_ling145-trials.csv"
data2<-read.csv(raw_data_path2)

data_summary1 = data1 %>%
  group_by(condition) %>%
  summarise(mean_confidence = mean(meaning_confidence), mean_acc = mean(rating))
data_summary1

data_summary2 = data2 %>%
  group_by(condition) %>%
  summarise(mean_confidence = mean(meaning_confidence), mean_acc = mean(rating))
data_summary2

#By-condition satiation graph for pilot 2a#
data_no_practice1 = data1 %>% 
  filter(block_number != "practice") %>%
  mutate(block_number = as.numeric(block_number))

trial_means1 = data_no_practice1 %>%
  group_by(block_number,condition) %>%
  summarise(rating = mean(rating))

satiation_plot1 <- ggplot(data_no_practice1, aes(x=block_number, y=rating, color = condition, fill = condition)) + 
  geom_point(data=trial_means1,alpha=.9) + 
 # scale_color_manual(values=cbPalette) +
#  scale_fill_manual(values=cbPalette) +
  xlab("Presentation Order") +
  ylab("Acceptability rating")+
  geom_smooth(method=lm, aes(fill=condition))+theme_bw()
satiation_plot1

satiation_plot_faceted1 <- satiation_plot1 + facet_wrap(~condition, scales = "free")
satiation_plot_faceted1


#By-condition satiation graph for pilot 2b#
data_no_practice2 = data2 %>% 
  filter(block_number != "practice") %>%
  mutate(block_number = as.numeric(block_number))

trial_means2 = data_no_practice2 %>%
  group_by(block_number,condition) %>%
  summarise(rating = mean(rating))

satiation_plot2 <- ggplot(data_no_practice2, aes(x=block_number, y=rating, color = condition, fill = condition)) + 
  geom_point(data=trial_means1,alpha=.9) + 
  # scale_color_manual(values=cbPalette) +
  #  scale_fill_manual(values=cbPalette) +
  xlab("Presentation Order") +
  ylab("Acceptability rating")+
  geom_smooth(method=lm, aes(fill=condition))+theme_bw()
satiation_plot2

satiation_plot_faceted2 <- satiation_plot2 + facet_wrap(~condition, scales = "free")
satiation_plot_faceted2


#get the satiation rate for each participant-condition combo for group a#

slope_data1 = data_no_practice1 %>%
  group_by(workerid, condition)%>%
  do(tidy(lm(rating ~ block_number, data = .))) %>% 
  filter(term == "block_number") %>%    
  select(workerid, condition, estimate) 

merged_data1 <- data_no_practice1 %>%
  left_join(slope_data1, by = c("workerid", "condition"))

merged1_trial_means= merged_data1 %>%
  group_by(workerid, condition)%>%
  summarise(estimate = mean(estimate), meaning_confidence = mean(meaning_confidence))

confidence_slope_plot1 <- ggplot(merged_data1, aes(x=meaning_confidence, y=estimate, color = condition, fill = condition)) + 
#  geom_point(data=merged1_trial_means,alpha=.9) + 
  # scale_color_manual(values=cbPalette) +
  #  scale_fill_manual(values=cbPalette) +
  xlab("Confidence Rating") +
  ylab("Satiation Rate")+
  geom_smooth(method=lm)+theme_bw()
confidence_slope_plot1

confidence_slope_faceted1 <- confidence_slope_plot1 + facet_wrap(~condition, scales = "free")
confidence_slope_faceted1

#confidence_slope plot for group b
slope_data2 = data_no_practice2 %>%
  group_by(workerid, condition)%>%
  do(tidy(lm(rating ~ block_number, data = .))) %>% 
  filter(term == "block_number") %>%    
  select(workerid, condition, estimate) 

merged_data2 <- data_no_practice2 %>%
  left_join(slope_data2, by = c("workerid", "condition"))

merged2_trial_means= merged_data2 %>%
  group_by(workerid, condition)%>%
  summarise(estimate = mean(estimate), meaning_confidence = mean(meaning_confidence))

confidence_slope_plot2 <- ggplot(merged_data2, aes(x=meaning_confidence, y=estimate, color = condition, fill = condition)) + 
  #  geom_point(data=merged1_trial_means,alpha=.9) + 
  # scale_color_manual(values=cbPalette) +
  #  scale_fill_manual(values=cbPalette) +
  xlab("Confidence Rating") +
  ylab("Satiation Rate")+
  geom_smooth(method=lm)+theme_bw()
confidence_slope_plot2

confidence_slope_faceted2 <- confidence_slope_plot2 + facet_wrap(~condition, scales = "free")
confidence_slope_faceted2

