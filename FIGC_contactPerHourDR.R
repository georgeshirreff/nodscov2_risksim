
#######################
#### load packages ####
#######################

library(tidyverse)
library(magrittr)
library(ggplot2)
library(lubridate)
library(data.table)
library(gridExtra)
library(GGally)
library(igraph)
library(egg)

###################
#### read data ####
###################

list_ward <- read_csv("input/Private/list_ward_complete.csv")
admission <- read_csv("input/Private/id_function_complete.csv")
timespent_mins <- read_csv(file = "input/Private/met_timespent_mins_complete.csv")
# list_ward <- read_csv("input/list_ward_partial.csv")
# admission <- read_csv("input/id_function_partial.csv")
# timespent_mins <- read_csv(file = "input/met_timespent_mins_partial.csv")

Label_tib <- read_csv("input/newID_order.csv")

#########################
#### prepare objects ####
#########################

timespent <- timespent_mins %>% 
  select(id, time_J1, time_N1, time_J2) %>% 
  pivot_longer(-id, names_prefix = "time_", names_to = "shift", values_to = "total_mins")


cats <- admission$catHosp %>% unique %>% sort
# order them
cats <- c("patient", "visitor", cats[cats != "investigation"], "investigation") %>% unique
stats <- c("PA", "V", "PE")

admission_types = admission %>% 
  transmute(id, status, catHosp = factor(catHosp, cats))

# function determining probability of infection per duration of contact
sig <- function(x, a) (1 - exp(-x* a))/(1 + exp(-x* a))
a = 0.1

################################
#### pivot the contact data ####
################################

list_ward_thisthat_saveid <- list_ward %>% 
  left_join(admission_types %>% rename_all(function(x) paste0(x, "_from")), by = c("from" = "id_from")) %>% 
  left_join(admission_types %>% rename_all(function(x) paste0(x, "_to")), by = c("to" = "id_to")) %>% 
  filter(catHosp_from != "investigation", catHosp_to != "investigation") %>% 
  mutate(across(starts_with("status"), ~factor(.x, levels = stats))) %>% 
  mutate(combcatHosp_from = catHosp_from %>% {case_when(. == "patient" ~ "Patient"
                                                        , . == "visitor" ~ "Visitor"
                                                        , . == "nurse" ~ "Nurse"
                                                        , . == "physician" ~ "Physician"
                                                        , . == "aux nurse" ~ "Nurse"
                                                        , . == "administration" ~ "Other"
                                                        , . == "ext physician" ~ "Physician"
                                                        , . == "logistic" ~ "Other"
                                                        , . == "student nurse" ~ "Nurse"
                                                        , . == "reeducation staff" ~ "Other"
                                                        , . == "other" ~ "Other")}) %>% 
  mutate(combcatHosp_to = catHosp_to %>% {case_when(. == "patient" ~ "Patient"
                                                    , . == "visitor" ~ "Visitor"
                                                    , . == "nurse" ~ "Nurse"
                                                    , . == "physician" ~ "Physician"
                                                    , . == "aux nurse" ~ "Nurse"
                                                    , . == "administration" ~ "Other"
                                                    , . == "ext physician" ~ "Physician"
                                                    , . == "logistic" ~ "Other"
                                                    , . == "student nurse" ~ "Nurse"
                                                    , . == "reeducation staff" ~ "Other"
                                                    , . == "other" ~ "Other")}) %>% 
  group_by(newID, wardType, from, to, status_from, status_to, catHosp_from, catHosp_to, combcatHosp_from, combcatHosp_to) %>% #first, aggregate contacts which occurred between the same people on the same day
  # filter(length > 60) %>% #AJMAL
  summarise(dur_mins = sum(length)/60) %>% 
  mutate(id_from = from, id_to = to) %>% 
  # filter(dur_mins > 1) %>% #AJMAL
  pivot_longer(cols = c("from", "to"), names_to = "direction", values_to = "id") %>% #then lengthen the table so that each contact counts for both parties
  mutate(this_status = case_when(direction == "from" ~ status_from
                                 , direction == "to" ~ status_to)
         , that_status = case_when(direction == "from" ~ status_to
                                   , direction == "to" ~ status_from)
         , this_catHosp = case_when(direction == "from" ~ catHosp_from
                                    , direction == "to" ~ catHosp_to)
         , that_catHosp = case_when(direction == "from" ~ catHosp_to
                                    , direction == "to" ~ catHosp_from)
         , this_combcatHosp = case_when(direction == "from" ~ combcatHosp_from
                                        , direction == "to" ~ combcatHosp_to)
         , that_combcatHosp = case_when(direction == "from" ~ combcatHosp_to
                                        , direction == "to" ~ combcatHosp_from)
         
         , other_id = case_when(direction == "from" ~ id_to
                                , direction == "to" ~ id_from)) %>% 
  select(-id_from, -id_to)


##############################
#### analysis of the data ####
##############################

#### total duration of study in each ward ####
ward_Ttotal <- timespent_mins %>% transmute(id, Ttotal = difftime(N2, J1, units = "mins") %>% as.numeric) %>% 
  left_join(list_ward_thisthat_saveid %>% ungroup %>% select(id, newID)) %>% 
  select(newID, Ttotal) %>% unique

#### Hbar for each ward  ####
# being average time spent with a captor i.e. on the ward
# standardised to 24 hours
ward_hours2 <- timespent_mins %>% 
  transmute(id, time_total) %>% 
  full_join(list_ward_thisthat_saveid %>% ungroup %>% select(id, newID, this_status) %>% unique) %>% 
  group_by(newID) %>% 
  summarise(time_total = mean(time_total)) %>% 
  left_join(ward_Ttotal) %>% 
  mutate(Hbar = 24/Ttotal*time_total)

#### Hbar for each ward and status  ####
# being average time spent with a captor i.e. on the ward, for each status (PA, V, PE)
# standardised to 24 hours
ward_hours2_status <- timespent_mins %>% 
  transmute(id, time_total) %>% 
  full_join(list_ward_thisthat_saveid %>% ungroup %>% select(id, newID, this_status) %>% unique) %>% 
  filter(!is.na(newID)) %>% 
  group_by(newID, this_status) %>% 
  summarise(time_total = mean(time_total)) %>% 
  ungroup %>% 
  complete(newID, this_status, fill = list(time_total = 0)) %>% 
  left_join(ward_Ttotal) %>% 
  mutate(Hbar = 24/Ttotal*time_total)


#### make a single table with zeros ####

# this is the one we want to be our master id-level table for calculating matrices, R0s and even exclusions
matrix_data2 <- list_ward_thisthat_saveid %>% 
  mutate(pInfPerContact = sig(dur_mins, a/60)) %>%
  group_by(newID, this_status, id, that_status) %>% 
  summarise(#n_contacts = n()
            n_contacts = length(unique(other_id))
            , dur_mins = mean(dur_mins)
            , pInfPerContact = mean(pInfPerContact)) %>% 
  ungroup %>% complete(nesting(newID, this_status, id), that_status
                       , fill = list(n_contacts = 0
                                     , contacts_per_hour = 0
                                     , dur_mins = NA
                                     , pInfPerContact = NA)) %>% 
  left_join(timespent %>%
              group_by(id) %>%
              summarise(total_mins = sum(total_mins))) %>%
  mutate(contacts_per_hour = n_contacts/(total_mins/60)) %>% 
  select(newID, this_status, id, total_mins, that_status
         , n_contacts, contacts_per_hour, dur_mins, pInfPerContact)



# from everyone to everyone
complete_DR_gen <- matrix_data2 %>% 
  group_by(newID, id) %>% 
  summarise(dur_mins = sum(dur_mins*n_contacts, na.rm = T)/sum(ifelse(!is.na(dur_mins), n_contacts, 0))
            , pInfPerContact = sum(pInfPerContact*n_contacts, na.rm = T)/sum(ifelse(!is.na(pInfPerContact), n_contacts, 0))
            , contacts_per_hour = sum(contacts_per_hour)
            , n_contacts = sum(n_contacts)) %>% 
  group_by(newID) %>% 
  summarise_at(c("contacts_per_hour", "dur_mins", "pInfPerContact"), mean) %>% 
  ungroup %>%
  left_join(ward_hours2 %>% select(newID, Hbar)) %>%
  mutate(daily_risk = replace_na(pInfPerContact, replace = 0)*contacts_per_hour*Hbar) %>% 
  select(newID, contacts_per_hour, dur_mins, pInfPerContact, Hbar, daily_risk)


# from each this_status to each that_status
# this is almost identical to matrix_mean2 above
complete_DR_status <- matrix_data2 %>% 
  mutate(prod = contacts_per_hour*ifelse(is.na(dur_mins), 0, dur_mins)) %>% 
  group_by(newID, this_status, that_status) %>% 
  # summarise_at(c("n_contacts", "contacts_per_hour", "dur_mins", "pInfPerContact", "prod"), ~mean(.x, na.rm = T)) %>% 
  summarise(total_contacts = sum(n_contacts)
            , across(c("n_contacts", "contacts_per_hour", "dur_mins", "pInfPerContact", "prod"), ~mean(.x, na.rm = T))) %>%
  ungroup %>% complete(newID, this_status, that_status
                       , fill = list(total_contacts = 0)) %>% #adds in entirely missing statuses, and we don't give replacements because it's better if they are NA
  ungroup %>%
  left_join(ward_hours2_status %>% select(newID, this_status, Hbar)) %>%
  mutate(daily_risk = replace_na(pInfPerContact, replace = 0)*contacts_per_hour*Hbar) %>% 
  select(newID, this_status, that_status, contacts_per_hour, dur_mins, pInfPerContact, Hbar, daily_risk)

# from each this_status to all
complete_DR_thisstatus <- complete_DR_status %>% 
  group_by(newID, this_status) %>% 
  summarise(contacts_per_hour = sum(contacts_per_hour)
            , daily_risk = sum(daily_risk)
            , Hbar = Hbar[1]) %>% 
  mutate(pInfPerContact = daily_risk/Hbar/contacts_per_hour
         , dur_mins = NA) %>% 
  select(newID, this_status, contacts_per_hour, dur_mins, pInfPerContact, Hbar, daily_risk)

complete_DR_thatstatus <- complete_DR_status %>% 
  group_by(newID, that_status) %>% 
  summarise(contacts_per_hour = sum(contacts_per_hour, na.rm = T)
            , daily_risk = sum(daily_risk, na.rm = T)
            , Hbar = Hbar[1]) %>% 
  mutate(pInfPerContact = daily_risk/Hbar/contacts_per_hour
         , dur_mins = NA) %>% 
  select(newID, that_status, contacts_per_hour, dur_mins, pInfPerContact, Hbar, daily_risk)

complete_DR_thatstatus %>% 
  filter(that_status == "PE") %>% 
  arrange(daily_risk)

complete_DR_check <- complete_DR_status %>% 
  group_by(newID) %>% 
  summarise(contacts_per_hour = sum(contacts_per_hour, na.rm = T)
            , daily_risk = sum(daily_risk, na.rm = T)
            , Hbar = Hbar[1]) %>% 
  mutate(pInfPerContact = daily_risk/Hbar/contacts_per_hour
         , dur_mins = NA) %>% 
  select(newID, contacts_per_hour, dur_mins, pInfPerContact, Hbar, daily_risk)


#### prepare boxplot accessory ####

whisker_top = function(x) {
  lqr = quantile(x, prob = 0.25);
  uqr = quantile(x, prob = 0.75);
  iqr = uqr - lqr;
  return(max(x[x < uqr + iqr]))}
  
whisker_bottom = function(x) {
  lqr = quantile(x, prob = 0.25);
  uqr = quantile(x, prob = 0.75);
  iqr = uqr - lqr;
  return(min(x[x > lqr - iqr]))}

boxplot_DR_gen_numbers <- complete_DR_gen %>% 
  select(daily_risk) %>% 
  summarise_all(.funs = list(whisker_bottom = whisker_bottom
                             , lqr = function(x) quantile(x, prob = 0.25)
                             , median = median
                             , uqr = function(x) quantile(x, prob = 0.75)
                             , whisker_top = whisker_top
                             , iqr = function(x) {quantile(x, prob = 0.75) - quantile(x, prob = 0.25)}
  ))
             
boxplot_DR_gen <- complete_DR_gen %>% 
  ggplot() + geom_boxplot(aes(y = daily_risk))






#### make main plot ####

# complete_DR_gen$daily_risk %>% range
DR_plot <-
  rbind(complete_DR_gen %>% mutate(this_status = "ALL", that_status = "ALL")
      , complete_DR_thisstatus %>% mutate(that_status = "ALL")
      # , complete_DR_thatstatus %>% mutate(this_status = "ALL")
      , complete_DR_status) %>% 
  mutate_at(c("this_status", "that_status"), function(x) factor(x, levels = c("ALL", "PA", "V", "PE"))) %>% 
  # mutate_at(c("this_status", "that_status"), function(x) factor(x, labels = c("All", "Patients", "Visitors", "HCWs"))) %>% 
  mutate_at(c("that_status"), function(x) factor(x, labels = c("All", "Patients", "Visitors", "HCWs"))) %>% 
  mutate_at(c("this_status"), function(x) factor(x, labels = paste0("Index case = ", c("any", "patient", "visitor", "healthcare worker")))) %>% 
  mutate(newID = factor(newID, levels = Label_tib$newID)) %>% 
  ggplot(aes(x = newID, y = daily_risk, fill = that_status)) + 
  geom_bar(stat = "identity", position = position_dodge(width=0.8)
           , width = 0.8
  ) + 
  facet_grid(this_status~.) +
  scale_fill_manual(values = c(All = "grey70", Patients = "red", Visitors = "black", HCWs = "blue")) + 
  theme_bw() + 
  labs(x = "", y = "Number of secondary infections per day", fill = "") + 
  geom_text(aes(x = 1, y = 0.8, label = this_status), hjust = 0, size = 6) +
  # annotate("text", aes(x= 1, y = 0.8, label = label), data = tibble(label = paste("Index case =", c("any", "patient", "visitor", "healthcare worker")))) 
  theme(panel.grid = element_blank()
        , axis.text.x = element_text(angle = 45, hjust = 1)
        , plot.margin = unit(c(1, 1, 1, 1), "cm")
        , strip.text = element_blank()
        , strip.background.y = element_rect(fill = "white")
        , legend.direction = "horizontal", legend.position = "top"
  ) + 
  coord_cartesian(ylim = c(0, 0.8))+ 
  geom_vline(xintercept = seq(0.5, by = 1, length.out = 15), color="lightgray", size=.5, alpha=.5) # set vertical lines between x groups


DR_boxplot <- rbind(complete_DR_gen %>% mutate(this_status = "ALL", that_status = "ALL")
      , complete_DR_thisstatus %>% mutate(that_status = "ALL")
      , complete_DR_status) %>% 
  mutate_at(c("this_status", "that_status"), function(x) factor(x, levels = c("ALL", "PA", "V", "PE"))) %>% 
  mutate_at(c("this_status", "that_status"), function(x) factor(x, labels = c("All", "Patients", "Visitors", "HCWs"))) %>% 
  # left_join(wardType_combineref) %>% 
  ggplot(aes(y = daily_risk)) + 
  geom_boxplot() +
  # geom_point(aes(x = 0, shape = wardTypeCombine, colour = newID), position = position_jitter(width = 0.3)) +
  facet_grid(this_status~.) + 
  theme_bw() + 
  theme(strip.text = element_blank()
        , axis.title.y = element_blank()
        , axis.text.y = element_blank()
        , axis.text.x = element_blank())
DR_boxplot

# wardType_combineref = list_ward %>% 
#   select(newID, wardType) %>% unique %>% 
#   transmute(newID, wardTypeCombine = case_when(wardType %in%  c("Medical ICU", "Surgical ICU") ~ "Adult ICU"
#                                      , wardType == "Infectious Diseases" ~ "Infectious diseases"
#                                      , T ~ wardType) 
#   )
# 
# wardType_shapes = c(`Neonatal ICU`=15,`General paediatrics`=16,`Paediatric emergency`=17
#                               , `Adult ICU`=0,`Internal medicine`=1,`Adult emergency`=2
#                               , `Infectious diseases`=3,`Geriatry`=4,`Pneumology`=5) 
# 
# 
# DR_jittershape <- 
#   rbind(complete_DR_gen %>% mutate(this_status = "ALL", that_status = "ALL")
#                     , complete_DR_thisstatus %>% mutate(that_status = "ALL")
#                     # , complete_DR_status
#         ) %>%
#   mutate_at(c("this_status"), function(x) factor(x, levels = c("ALL", "PA", "V", "PE"))) %>%
#   mutate_at(c("this_status"), function(x) factor(x, labels = c("All", "Patients", "Visitors", "HCWs"))) %>%
#   left_join(wardType_combineref) %>% 
#   ggplot(aes(x = 0, y = daily_risk, shape = wardTypeCombine)) + 
#   geom_point(position = position_jitter(width = 0.1), colour = "grey50") +
#   coord_cartesian(xlim = c(-1, 1)
#                   , ylim = c(0, 0.8)) + 
#   facet_grid(this_status~.) + 
#   scale_shape_manual(values = wardType_shapes) + 
#   theme_bw() + 
#   guides(colour = F) + 
#   theme(strip.text = element_blank()
#         , axis.title.y = element_blank()
#         , axis.text.y = element_blank()
#         , axis.text.x = element_blank()
#         , axis.title.x = element_blank()
#         , axis.ticks.x = element_blank()
#   ) + 
#   labs(shape = "Ward type")
  

DR_barplot_boxplot <- egg::ggarrange(DR_plot, DR_boxplot, ncol = 2, widths = c(20, 3))

ggsave(DR_barplot_boxplot, filename = "output/Fig_DAR_boxplots.pdf", width = 23, height = 25, units = "cm", device = "pdf")


# DR_barplot_jitter <- egg::ggarrange(DR_plot, DR_jittershape, ncol = 2, widths = c(20, 7))
# 
# ggsave(DR_barplot_jitter, filename = "output/Fig_DAR_jitterplots.pdf", width = 23, height = 30, units = "cm", device = "pdf")

# additional plot showing averagehours spent on ward

Hbar_plot <-
  rbind(complete_DR_gen %>% mutate(this_status = "ALL", that_status = "ALL")
        , complete_DR_thisstatus %>% mutate(that_status = "ALL")
        , complete_DR_status) %>% 
  mutate(newID = factor(newID, levels = Label_tib$newID)) %>% 
  mutate_at(c("this_status", "that_status"), function(x) factor(x, levels = c("ALL", "PA", "V", "PE"))) %>% 
  # mutate_at(c("this_status", "that_status"), function(x) factor(x, labels = c("All", "Patients", "Visitors", "HCWs"))) %>% 
  mutate_at(c("this_status", "that_status"), function(x) factor(x, labels = c("All", "Patients", "Visitors", "HCWs"))) %>% 
  # mutate_at(c("this_status"), function(x) factor(x, labels = paste0("Index case = ", c("any", "patient", "visitor", "healthcare worker")))) %>% 
  ggplot(aes(x = newID, y = Hbar, fill = this_status)) +
  geom_bar(stat = "identity"
           , position = position_dodge(width=0.8)
           , width = 0.8
  ) + 
  scale_fill_manual(values = c(All = "grey70", Patients = "red", Visitors = "black", HCWs = "blue")) + 
  # facet_grid(this_status~.) +
  theme_bw() + 
  labs(x = "", y = "Average hours spent\non ward in 24 hours", fill = "") + 
  # geom_text(aes(x = 1, y = 60, label = this_status), hjust = 0, size = 6) +
  # geom_text(x = 1, y = 0.8, colour = "black", aes(label = fac_label)
  #           , data = tibble(fac_label = paste("Index case =", c("any", "patient", "visitor", "healthcare worker"))
  #                           , this_status = paste("Index case =", c("any", "patient", "visitor", "healthcare worker")))
  #           ) +
  
  # annotate("text", aes(x= 1, y = 0.8, label = label), data = tibble(label = paste("Index case =", c("any", "patient", "visitor", "healthcare worker")))) 
  theme(panel.grid = element_blank()
        , axis.text.x = element_text(angle = 45, hjust = 1)
        , plot.margin = unit(c(1, 1, 1, 1), "cm")
        , strip.text = element_blank()
        , strip.background.y = element_rect(fill = "white")
        , legend.direction = "horizontal", legend.position = "top"
  ) + 
  geom_vline(xintercept = seq(0.5, by = 1, length.out = 15), color="lightgray", size=.5, alpha=.5) # set vertical lines between x groups
Hbar_plot

ggsave(Hbar_plot, filename = "output/Fig_Hbar.pdf", width = 20, height = 10, units = "cm", device = "pdf")
ggsave(Hbar_plot, filename = "output/Fig_Hbar.png", width = 20, height = 10, units = "cm")





#### some numbers for the paper ####

complete_DR_gen %>% 
  pull(daily_risk) %>% range

complete_DR_thisstatus %>% 
  filter(this_status == "PA") %>% 
  pull(daily_risk) %>% range

complete_DR_thisstatus %>% 
  # filter(ward_id != 15) %>%
  filter(this_status == "V") %>% 
  pull(daily_risk) %>% sort

complete_DR_thisstatus %>% 
  mutate(PED = grepl("Ped", newID) | grepl("Neonat", newID)) %>% 
  filter(this_status == "V") %>% 
  group_by(PED) %>% 
  summarise(range(daily_risk, na.rm = T))

complete_DR_thisstatus %>% 
  left_join(Label_tib) %>% 
  mutate(PED = grepl("Ped", newID) | grepl("Neonat", newID)) %>% 
  filter(this_status == "V") %>% 
  arrange(daily_risk) %>% 
  select(daily_risk, PED)

complete_DR_thisstatus %>% 
  filter(this_status == "PE") %>% 
  pull(daily_risk) %>% sort


complete_DR_status %>% 
  filter(this_status == "PA") %>% 
  group_by(newID) %>% 
  arrange(newID, -daily_risk) %>% 
  slice(1) %>% 
  pull(that_status) %>% table

complete_DR_status %>% 
  filter(this_status == "PE") %>% 
  group_by(newID) %>% 
  arrange(newID, -daily_risk) %>% 
  slice(1) %>% 
  pull(that_status) %>% table


rbind(complete_DR_gen %>% mutate(this_status = "ALL", that_status = "ALL")
      , complete_DR_thisstatus %>% mutate(that_status = "ALL")
      , complete_DR_status) %>% 
  group_by(this_status, that_status, emerg = grepl("emergency", newID)) %>% 
  summarise_at(c("pInfPerContact", "contacts_per_hour", "Hbar"), function(x) mean(x, na.rm = T)) %>% 
  print(n = 100)

complete_DR_status %>% 
  filter(this_status == "PA") %>% 
  filter(that_status == "PA")
