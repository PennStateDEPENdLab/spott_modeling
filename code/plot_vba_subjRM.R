#plot 1 subj
#library(R.matlab)
#mfile <- readMat("/Users/mnh5174/Data_Analysis/spott_modeling/outputs/vba_out/ffx/suuvid_base/fit_102_suuvid_base_multisession0.mat")
library(cowplot)
library(tidyverse)
library(gridExtra)

# RM: what does this do?
# https://www.rdocumentation.org/packages/grDevices/versions/3.6.2/topics/dev
# graphics.off() shuts down all open graphics devices.
graphics.off()
rm(list=ls(all=TRUE))

# #fix curkey bleed
# # s102_raw <- read.csv("/Users/mnh5174/Data_Analysis/spott_modeling/data/vba_input/102_spott_50.csv")
# # table(s102_raw$key)
# # s102_raw  <- s102_raw %>% arrange(instrial, latency_bin) %>% group_by(instrial) %>%
# #   mutate(bin_num=1:n(), first_press=which(key > 0)[1], curkey_mod=if_else(bin_num < first_press, 0L, curkey))
# # 
# # table(s102_raw$curkey_mod)
# 
# # setwd("/Users/zzo1/Dropbox/GitLAb/spott_modeling/spott_modeling/figures/")
# # trial_stats <- read.csv("/Users/zzo1/Dropbox/GitLAb/spott_modeling/spott_modeling/outputs/vba_out/ffx/pandaa_vba_input/time2pl/pandaa_vba_input_time2pl_ffx_trial_outputs.csv")

setwd("~/Documents/Lab_DEPENd/MotivationalVigor_PIT/SPOTT/spott_modeling/outputs/vba_out/ffx/pandaa_vba_input/exp")
trial_stats <- read.csv("pandaa_vba_input_exp_ffx_trial_outputs.csv")
group_stats <- read.csv("pandaa_vba_input_exp_ffx_global_statistics.csv")

# # single subj for 102 with correction on clairvoyance
# # s102 <- read.csv("/Users/mnh5174/Data_Analysis/spott_modeling/outputs/vba_out/ffx/suuvid_base/pandaa_suuvid_base_ffx_prompt_outputs_onesub.csv") %>% slice(1:1534) %>% filter(id==102) %>% mutate(sample=1:n(), time=sample*50/1000) %>%
# #   filter(sample > 100 & sample < 350)

#RM
# bothResults = read.csv("bothResults.csv") C-G are VBA results (analysis done on the transformed scale), rest are from stan
# IDs = bothResults$id
IDs <- group_stats$id
# IDs <- unique(trial_stats$id)
  
howManyP = 15 #65 # RM: the content still got cut in the PDF
adjHeight = 20*(howManyP/5) #height of pdf file, scaling with # of participants
plist <- list()
howLong = 1000
for (ii in 1:howManyP){

# select data from trial_stats that correspond to ID = ii, then take the 101st to 999th trials    
s102 <- trial_stats %>% filter(id==IDs[ii]) %>% mutate(sample=1:n(), time=sample*50/1000) %>%
     filter(sample > 100 & sample < howLong)

# gather(): Writing Q1, Q2 in long form
# s102 has variables Q1 and Q2, with values
# q_df has a variable "Action_Value" to indicate Q1 or Q2; variable Q is the values
q_df <- s102 %>% gather(key="Action_Value", value="Q", Q1, Q2)
# Note: pivor_longer() is an updated approach to gather()
# https://tidyr.tidyverse.org/reference/pivot_longer.html#:~:text=pivot_longer()%20is%20an%20updated,no%20longer%20under%20active%20development.


# Plot Q1 and Q2 values
ggplot(q_df, aes(x=time, y=Q, color=Action_Value)) + geom_line()

# Plot the difference between Q1-Q2
ggplot(s102, aes(x=time, y=Q1-Q2)) + geom_line() + geom_hline(yintercept=0)

# Scaling Q1, Q2, Q1-Q2, so they are plotted at the desired position/coordinates in the final plot
# E.g., Q1 is scaled by 0.3 and then put at y=1.6 (the second blue bar from the bottom)
q_df <- s102 %>% mutate(q1_norm=0.3*Q1/max(Q1) + 1.6, q2_norm=0.3*Q2/max(Q2) + 3.2, qdiff=0.6*((Q1-Q2)/max(Q1-Q2)) + 4.6) %>%
  gather(key="Action_Value", value="Q", q1_norm, q2_norm, qdiff)

#Plot q1_norm and q2_norm
(qplot <- ggplot(q_df, aes(x=time, y=Q, color=Action_Value)) + geom_line())

choice_df <- s102 %>% gather(key="option", value="chosen", y1, y2, none) %>%  # Write y1, y2, none in long form
  gather(key="option_pred", value="chosen_pred", y1_pred, y2_pred, none_pred) %>% # Write, y1_pred, y2_pred, none_pred in long form
  filter(chosen == 1 & sample > 100 & sample < howLong) %>% 
  mutate(option_num=dplyr::recode(option, none=0, y1=1, y2=2.6), time=sample*50/1000, #RM: Why these numbers?
         option_pred_num=dplyr::recode(option_pred, none_pred=0.3, y1_pred=1.3, y2_pred=2.9))
# (cplot <- ggplot(choice_df, aes(x=sample, y=option)) + geom_point(alpha=0.8, size=0.3))

cplot <- ggplot(choice_df, aes(x=time, y=option_num, ymin=option_num-0.1, ymax=option_num+0.1)) + geom_linerange(size=0.5) +
  #geom_tile(aes(x=time, y=option_pred_num, alpha=chosen_pred), color="white", fill="blue", height=0.2) +
  geom_tile(aes(x=time, y=option_pred_num, fill=chosen_pred, color=chosen_pred), height=0.2) +
  geom_line(data=q_df, aes(x=time, y=Q, group=Action_Value, alpha=NULL, ymin=NULL, ymax=NULL), show.legend = FALSE) +
  #scale_alpha_continuous("Model-\npredicted\nprobability", range=c(0.1,.95), breaks=c(0, 0.25, 0.5, 0.75, 1))
  #geom_tile(aes(x=time, y=option_pred_num, color=chosen_pred, fill=chosen_pred), height=0.2, interpolate=TRUE) + 
  #scale_color_viridis_c() + scale_fill_viridis_c()
  scale_fill_distiller("Model-\npredicted\nprobability", palette = "Blues") + scale_color_distiller("Model-\npredicted\nprobability", palette="Blues") +
  geom_hline(yintercept=4.6, linetype="dashed") # 4.6 units above the black bar (y=0)

#plot_grid(qplot, cplot, ncol=1, align="h")

pdf("vba_subj1-15.pdf", width=10, height=adjHeight)
oneP = filter(group_stats, id == IDs[ii])
plist[[ii]] = cplot + xlab("Time (seconds)") + theme(axis.text.y = element_blank(), axis.title.y = element_blank(), 
                                             axis.ticks.y = element_blank(), axis.line.y = element_blank()) +
 ggtitle(sprintf("ID: %d MATLAB VSens:%4.2f;baseV:%4.2f;Learning:%4.2f;Temp:%4.2f;Cost:%4.2f",ii, oneP$gamma_transformed,oneP$nu_transformed,oneP$alpha_transformed,oneP$kappa_transformed,oneP$omega_transformed))
}
do.call(grid.arrange, c(plist, nrow = howManyP))
dev.off()

