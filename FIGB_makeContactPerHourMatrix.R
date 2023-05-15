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
library(lemon)

###################
#### read data ####
###################

# list_ward <- read_csv("input/Private/list_ward_complete.csv")
# admission <- read_csv("input/Private/id_function_complete.csv")
# timespent_mins <- read_csv(file = "input/Private/met_timespent_mins_complete.csv")
list_ward <- read_csv("input/list_ward_partial.csv")
admission <- read_csv("input/id_function_partial.csv")
timespent_mins <- read_csv(file = "input/met_timespent_mins_partial.csv")

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
a = 0.1 #shape parameter


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
 
# this is the one we want to be our master id-level table for calculating matrices, R0s and even exclusions

matrix_data2_allrow <- list_ward_thisthat_saveid %>% 
  mutate(pInfPerContact = sig(dur_mins, a/60)) %>%
  {rbind(., mutate(., that_status = "ALL"))} %>% 
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



# then calculate the means of this to give the to-from status-level matrices
matrix_mean2_allrow <- matrix_data2_allrow %>%
  mutate(prod = contacts_per_hour*ifelse(is.na(dur_mins), 0, dur_mins)) %>% 
  group_by(newID, this_status, that_status) %>% 
  # summarise_at(c("n_contacts", "contacts_per_hour", "dur_mins", "pInfPerContact", "prod"), ~mean(.x, na.rm = T)) %>% 
  summarise(total_contacts = sum(n_contacts)
            , across(c("n_contacts", "contacts_per_hour", "dur_mins", "pInfPerContact", "prod"), ~mean(.x, na.rm = T))) %>%
  ungroup %>% complete(newID, this_status, that_status
                       , fill = list(total_contacts = 0)) %>% #adds in entirely missing statuses, and we don't give replacements because it's better if they are NA
  # left_join(Didier_wardNames %>% 
  #             transmute(ward_id
  #                       , DidierLabel = paste0("#", ward_id, " ", DidierName)
  #                       , DidierLabel = factor(DidierLabel, levels = unique(DidierLabel)))) %>% 
  # filter(!is.na(DidierLabel)) %>% 
  mutate(newID = factor(newID, levels = Label_tib$newID)) %>% 
  filter(!is.na(newID)) %>% 
  ungroup %>% 
  mutate(across(c("this_status", "that_status"), function(x) case_when(x == "PA" ~ "Patient"
                                                                       , x == "V" ~ "Visitor"
                                                                       , x == "PE" ~ "HCW"
                                                                       , x == "ALL" ~ "All"))) %>% 
  mutate(this_status = factor(this_status, levels = c("Patient", "Visitor", "HCW"))) %>% 
  mutate(that_status = factor(that_status, levels = c("All", "Patient", "Visitor", "HCW"))) %>% 
  mutate(that_status = factor(that_status, labels = c("All", "Patients", "Visitors", "HCWs")))


  

# calculate the range of each statistic

matrix_mean2_allrow %>% 
  ungroup %>% 
  summarise_at(c("contacts_per_hour", "dur_mins", "prod"), ~range(.x, na.rm = T))

max_contacts_per_hour = 3
max_dur_mins = 2000
max_prod = 120


###########################
#### make matrix plots ####
###########################

matrixplot_ctcph_allrow <- matrix_mean2_allrow %>%
  # ggplot(aes(y = this_status, x=that_status))+
  ggplot(aes(x = this_status, y=that_status))+
  theme_bw()+
  geom_tile(aes(fill=contacts_per_hour)) + 
  geom_text(aes(label = round(contacts_per_hour, 1))) + 
  scale_fill_gradientn(colours=c("white","red")
                       # ,breaks=c(0,2,4,6,8),limits=c(0,8)
                       , limits = c(0, max_contacts_per_hour)
                       , na.value = "grey50"
  ) + 
  # facet_wrap(.~DidierLabel, nrow = 2) + 
  facet_wrap(.~newID, nrow = 2) + 
  # labs(y = "On average each Y forms...", x = "...new contacts per hour with X", fill = "Contacts\nper hour")
  labs(x = "On average each X forms...", y = "...new contacts per hour with Y", fill = "Contacts\nper hour")

# matrixplot_durmins <- matrix_mean2 %>% 
matrixplot_durmins_allrow <- matrix_mean2_allrow %>% 
  # ggplot(aes(y = this_status, x=that_status))+
  ggplot(aes(x = this_status, y=that_status))+
  theme_bw()+
  geom_tile(aes(fill=dur_mins)) + 
  geom_text(aes(label = round(dur_mins, 0))) +
  scale_fill_gradientn(colours=c("white", "white", "yellow")
                       # ,breaks=c(0,2,4,6,8),limits=c(0,8)
                       , limits = c(0.1, max_dur_mins)
                       , na.value = "grey"
                       , trans = "log10"
  ) + 
  # facet_wrap(.~DidierLabel, nrow = 2) + 
  facet_wrap(.~newID, nrow = 2) + 
  # labs(y = "For each Y the average contact lasts...", x = "...minutes with each X", fill = "Minutes")
  labs(x = "For each X the average contact lasts...", y = "...minutes with Y", fill = "Minutes")

# matrixplot_prod <- matrix_mean2 %>% 
matrixplot_prod_allrow <- matrix_mean2_allrow %>% 
  # ggplot(aes(y = this_status, x=that_status))+
  ggplot(aes(x = this_status, y=that_status))+
  theme_bw()+
  geom_tile(aes(fill=prod)) + 
  geom_text(aes(label = round(prod, 0))) + 
  scale_fill_gradientn(colours=c("white","orange")
                       # ,breaks=c(0,2,4,6,8),limits=c(0,8)
                       , limits = c(0, max_prod)
                       , na.value = "grey50"
  ) + 
  # facet_wrap(.~DidierLabel, nrow = 2) + 
  facet_wrap(.~newID, nrow = 2) + 
  # labs(y = "Each Y forms new contacts each hour\nwhich cumulatively last...", x = "...with all X", fill = "Minutes\nper hour")
  labs(x = "Cumulative contact minutes per hour for each X...", y = "...with all Y", fill = "Minutes\nper hour")



gtable_show_names(matrixplot_ctcph_allrow)
matrixplot_ctcph_allrow_repos <- reposition_legend(matrixplot_ctcph_allrow, panel = "panel-8-2", position = "top")
matrixplot_durmins_allrow_repos <- reposition_legend(matrixplot_durmins_allrow, panel = "panel-8-2", position = "top")
matrixplot_prod_allrow_repos <- reposition_legend(matrixplot_prod_allrow, panel = "panel-8-2", position = "top")


ggsave(matrixplot_prod_allrow_repos
       , filename = paste0("output/Fig_contactmatrix_allrow.pdf")
       , units = "cm", width = 40, height = 15, dpi = 100, device = "pdf")
ggsave(matrixplot_prod_allrow_repos
       , filename = paste0("output/Fig_contactmatrix_allrow.png")
       , units = "cm", width = 40, height = 15, dpi = 300)

ggsave(matrixplot_ctcph_allrow_repos
       , filename = paste0("output/Fig_ctcphmatrix_allrow.pdf")
       , units = "cm", width = 40, height = 15, dpi = 100, device = "pdf")
ggsave(matrixplot_ctcph_allrow_repos
       , filename = paste0("output/Fig_ctcphmatrix_allrow.png")
       , units = "cm", width = 40, height = 15, dpi = 300)


ggsave(matrixplot_durmins_allrow_repos
       , filename = paste0("output/Fig_durminsmatrix_allrow.pdf")
       , units = "cm", width = 40, height = 15, dpi = 100, device = "pdf")
ggsave(matrixplot_durmins_allrow_repos
       , filename = paste0("output/Fig_durminsmatrix_allrow.png")
       , units = "cm", width = 40, height = 15, dpi = 300)

