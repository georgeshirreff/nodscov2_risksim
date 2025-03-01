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
library(ggh4x)

###################
#### read data ####
###################

list_ward <- read_csv("input/Private/list_ward_complete.csv")
admission <- read_csv("input/Private/id_function_complete.csv")
timespent_mins <- read_csv(file = "input/Private/met_timespent_mins_complete.csv")
# list_ward <- read_csv("input/list_ward_partial.csv")
# admission <- read_csv("input/id_function_partial.csv")
# timespent_mins <- read_csv(file = "input/met_timespent_mins_partial.csv")

# ward labels
Label_tib <- read_csv("input/newID_order.csv")

#########################
#### prepare objects ####
#########################

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
                                                        , . == "administration" ~ "Other HCW"
                                                        , . == "ext physician" ~ "Physician"
                                                        , . == "logistic" ~ "Other HCW"
                                                        , . == "student nurse" ~ "Nurse"
                                                        , . == "reeducation staff" ~ "Other HCW"
                                                        , . == "other" ~ "Other HCW")}) %>% 
  mutate(combcatHosp_to = catHosp_to %>% {case_when(. == "patient" ~ "Patient"
                                                        , . == "visitor" ~ "Visitor"
                                                        , . == "nurse" ~ "Nurse"
                                                        , . == "physician" ~ "Physician"
                                                        , . == "aux nurse" ~ "Nurse"
                                                        , . == "administration" ~ "Other HCW"
                                                        , . == "ext physician" ~ "Physician"
                                                        , . == "logistic" ~ "Other HCW"
                                                        , . == "student nurse" ~ "Nurse"
                                                        , . == "reeducation staff" ~ "Other HCW"
                                                        , . == "other" ~ "Other HCW")}) %>% 
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
ward_Ttotal <- timespent_mins %>% transmute(newID, Ttotal = difftime(N2, J1, units = "mins") %>% as.numeric) %>% unique

#### Hbar for each ward  ####
# being average time spent with a captor i.e. on the ward
# standardised to 24 hours
ward_hours2 <- timespent_mins %>% 
  transmute(id, newID, time_total) %>%
  filter(id %in% list_ward_thisthat_saveid$id) %>%
  group_by(newID) %>% 
  summarise(time_total = mean(time_total)) %>% 
  left_join(ward_Ttotal) %>% 
  mutate(Hbar = 24/Ttotal*time_total)

# connectivity measures for each individual

#degree
deg <- list_ward %>% 
  select(from, to) %>% 
  unique %>% 
  graph_from_data_frame(directed = F) %>% 
  degree %>% 
  {tibble(id = names(.), degree = .)}

#betweenness
btw <- 
  list_ward %>%
  select(from, to) %>% 
  unique %>% 
  graph_from_data_frame(directed = F) %>% 
  igraph::betweenness() %>% 
  {tibble(id = names(.), betweenness = .)}


# contact hours
dur <- list_ward %>% 
  group_by(from, to) %>% 
  summarise(dur_mins = sum(length)/60) %>% 
  pivot_longer(cols = c("from", "to"), names_to = "direction", values_to = "id") %>% 
  group_by(id) %>% 
  summarise(hours_in_contact = sum(dur_mins)/60)


# closeness
cls = tibble(id = character(0), closeness = numeric(0))
ward_names <- list_ward$newID %>% unique
for(ward in ward_names){
  
  
  cls_piece <- list_ward %>%
    filter(newID == ward) %>% 
    select(from, to) %>% 
    unique %>% 
    graph_from_data_frame(directed = F) %>% 
    closeness %>% 
    {tibble(id = names(.), closeness = .)}
  
  cls <- rbind(cls, cls_piece)
}

set.seed(10)

# combine measures by id
measures_byid = deg %>% 
  left_join(btw) %>% 
  left_join(dur) %>% 
  left_join(cls) %>% 
  left_join(list_ward_thisthat_saveid %>% 
              ungroup %>% 
              transmute(newID, id, this_status, this_catHosp, this_combcatHosp) %>% 
              unique) %>% 
  filter(this_catHosp != "investigation")

# rank ids by each measure
percrank <- measures_byid %>% 
  group_by(newID) %>% 
  select(-this_status) %>% 
  mutate(across(.cols = c("degree", "betweenness", "closeness", "hours_in_contact")
                # , function(x) rank(x)
                , .fns = list(revrandrank = ~rank(-., ties.method = "random"))
                ))


percrank_status <- measures_byid %>% 
  group_by(newID, this_status) %>% 
  mutate(across(.cols = c("degree", "betweenness", "closeness", "hours_in_contact")
                # , function(x) rank(x)
                , .fns = list(revrandrank = ~rank(-., ties.method = "random"))
  ))


percrank_combcat <- measures_byid %>% 
  group_by(newID, this_combcatHosp) %>% 
  mutate(across(.cols = c("degree", "betweenness", "closeness", "hours_in_contact")
                # , function(x) rank(x)
                , .fns = list(revrandrank = ~rank(-., ties.method = "random"))
  ))




ward_Ttotal <- timespent_mins %>% transmute(id, Ttotal = difftime(N2, J1, units = "mins") %>% as.numeric) %>% 
  inner_join(list_ward_thisthat_saveid %>% ungroup %>% select(id, newID)) %>% 
  select(newID, Ttotal) %>% unique






# run through the same code with different exclusions
exclusion_level = 0.95
w_id_vec = percrank$newID %>% unique
combcats = c("All", "Patient", "Visitor", "HCW", "Nurse", "Physician", "Other HCW")
n_rands = 3

# w_id = w_id_vec[2]
# this_combcat = "Visitor"
# excl = "deg"

if(F){ # this if statement included because the analysis can take a long time

DR_excl <- NULL
for(w_id in w_id_vec){
  print(w_id)
  w_id_numeric = which(w_id_vec == w_id)
  # print(w_id)
  ### total number of IDs in the dataset for this ward
  # w_nIDs <- percrank %>% filter(newID == w_id) %>% pull(id) %>% unique %>% length
  w_nIDs <- list_ward_thisthat_saveid %>% filter(newID == w_id) %>% pull(id) %>% unique %>% length
  
  n_to_exclude = round(w_nIDs * (1 - exclusion_level))
  
  # this_combcat = "HCW"
  for(this_combcat in combcats){
    # print(stat)
    if(this_combcat == "All"){
      percrank_ct = percrank %>% filter(newID == w_id)
    } else if (this_combcat == "HCW") {
      percrank_ct = percrank_status %>% filter(newID == w_id) %>% filter(this_status == "PE")
    } else {
      percrank_ct = percrank_combcat %>% filter(newID == w_id) %>% filter(this_combcatHosp == this_combcat)
    }
    for(excl in c("none"
                  # , "rand1", "rand2", "rand3"
                  , paste0("rand", 1:n_rands)
                  , "deg", "btw", "cls", "dur")){
      # print(excl)
      if(excl == "none"){
        ids_toexclude_ward = percrank_ct %>% 
          mutate(excluded = F) %>% 
          filter(excluded) %>% 
          pull(id)
        
      } else if (substr(excl, start = 1, stop = 3) == "ran") {
        rand_seed_component = as.numeric(gsub("rand", "", excl))
        set.seed(12345 * w_id_numeric * which(combcats == this_combcat)^2 + rand_seed_component*100)
          ids_toexclude_ward <- percrank_ct$id %>% {sample(., size = min(length(.), n_to_exclude))}
      # } else if (excl == "rand1"){
      #   set.seed(12345 * w_id * which(combcats == this_combcat)^2 + 111)
      #   ids_toexclude_ward <- percrank_ct$id %>% {sample(., size = min(length(.), n_to_exclude))}
      # } else if (excl == "rand2"){
      #   set.seed(12345 * w_id * which(combcats == this_combcat)^2 + 222)
      #   ids_toexclude_ward <- percrank_ct$id %>% {sample(., size = min(length(.), n_to_exclude))}
      # } else if (excl == "rand3"){
      #   set.seed(12345 * w_id * which(combcats == this_combcat)^2 + 333)
      #   ids_toexclude_ward <- percrank_ct$id %>% {sample(., size = min(length(.), n_to_exclude))}
      } else if (excl == "deg"){
        ids_toexclude_ward = percrank_ct %>% 
          mutate(excluded = degree_revrandrank <= n_to_exclude) %>% 
          filter(excluded) %>% 
          pull(id)
      } else if (excl == "btw"){
        ids_toexclude_ward = percrank_ct %>% 
          mutate(excluded = betweenness_revrandrank <= n_to_exclude) %>% 
          filter(excluded) %>% 
          pull(id)
      } else if (excl == "cls"){
        ids_toexclude_ward = percrank_ct %>% 
          mutate(excluded = closeness_revrandrank <= n_to_exclude) %>% 
          filter(excluded) %>% 
          pull(id)
      } else if (excl == "dur"){
        ids_toexclude_ward = percrank_ct %>% 
          mutate(excluded = hours_in_contact_revrandrank <= n_to_exclude) %>% 
          filter(excluded) %>% 
          pull(id)
      }
      
      n_excluded = length(ids_toexclude_ward)
      
      # print(c(w_id, stat, excl, w_nIDs, n_to_exclude, n_excluded))
      
      DR_this <- list_ward_thisthat_saveid %>% 
        mutate(pInfPerContact = sig(dur_mins, a/60)) %>%
        filter(newID == w_id) %>% 
        # filter(!(id %in% ids_toexclude_ward) #use this to exclude them altogether
        #        , !(other_id %in% ids_toexclude_ward)
        #        ) %>%
        mutate(pInfPerContact_excl = case_when(id %in% ids_toexclude_ward ~ 0
                                               , other_id %in% ids_toexclude_ward ~ 0
                                               , T ~ pInfPerContact)
               # , dur_mins_excl = case_when(id %in% ids_toexclude_ward ~ 0
               #                           , other_id %in% ids_toexclude_ward ~ 0
               #                           , T ~ dur_mins)
               ) %>% 
        group_by(newID, wardType, id) %>% 
        summarise(#n_contacts = n()
                  n_contacts = length(unique(other_id)) 
                  # , dur_mins = mean(dur_mins)
                  , pInfPerContact = mean(pInfPerContact_excl)
                  , .groups = "keep") %>% 
        left_join(timespent %>%
                      group_by(id) %>%
                      summarise(total_mins = sum(total_mins))
                  , by = "id") %>%
        mutate(contacts_per_hour = n_contacts/(total_mins/60)) %>% 
        # mutate(prod = contacts_per_hour*ifelse(is.na(dur_mins), 0, dur_mins)) %>% 
        group_by(newID, wardType) %>% 
        summarise_at(c("n_contacts", "contacts_per_hour", "pInfPerContact"
                       # , "dur_mins", "prod"
                       ), ~mean(.x, na.rm = T)) %>% 
        ungroup %>%
        left_join(ward_hours2 %>% select(newID, Hbar)
                  , by = "newID") %>%
        mutate(daily_risk = pInfPerContact*contacts_per_hour*Hbar)
      

      
      if(is.null(DR_excl)){
        DR_excl <- DR_this %>% transmute(newID, wardType, excl, combcat = this_combcat
                                            , w_nIDs = w_nIDs
                                            , supposed_to_exclude = ifelse(excl == "none", 0, n_to_exclude)
                                            , n_excluded = n_excluded
                                            , pInfPerContact, contacts_per_hour, Hbar, daily_risk)   
      } else {
        DR_excl <- rbind(DR_excl
                            , DR_this %>% transmute(newID, wardType, excl, combcat = this_combcat
                                                    , w_nIDs = w_nIDs
                                                    , supposed_to_exclude = ifelse(excl == "none", 0, n_to_exclude)
                                                    , n_excluded = n_excluded
                                                    , pInfPerContact, contacts_per_hour, Hbar, daily_risk)      )
      }
      
    }
  }
}

  write_csv(DR_excl, "output/DR_excl.csv")

} else {
  DR_excl <- read_csv("output/DR_excl.csv")  
}



DR_excl <- DR_excl %>% 
  {bind_rows(filter(., substr(excl, start = 1, stop = 3) != "ran")
             , filter(., substr(excl, start = 1, stop = 3) == "ran") %>% 
               group_by(newID, wardType, combcat) %>% 
               summarise_if(is.numeric, mean) %>% 
               mutate(excl = "rand"))} %>% 
  arrange(newID, excl, combcat) %>% 
  mutate(excludeby = case_when(excl == "none" ~ "No exclusion"
                            # , excl == "rand1" ~ "rand1"
                            # , excl == "rand2" ~ "rand2"
                            # , excl == "rand3" ~ "rand3"
                            , excl == "rand" ~ "Random"
                            , excl == "deg" ~ "Degree"
                            , excl == "btw" ~ "Betweenness"
                            , excl == "cls" ~ "Closeness"
                            , excl == "dur" ~ "Contact hours")) %>% 
  mutate(excludeby = factor(excludeby, levels = c("No exclusion"
                                            , "Random"
                                            , "Degree", "Closeness", "Betweenness", "Contact hours"))) %>% 
mutate(wardTypeCombine = case_when(wardType %in%  c("Medical ICU", "Surgical ICU") ~ "Adult ICU"
                                   , wardType == "Infectious Diseases" ~ "Infectious diseases"
                                     , T ~ wardType) 
  ) %>% 
  mutate(wardTypeCombine = wardTypeCombine %>% factor %>% fct_infreq)


# proportional change for each
DR_pctChange <- DR_excl %>% 
  select(newID, wardTypeCombine, excludeby, combcat, daily_risk, supposed_to_exclude, n_excluded) %>% 
  {left_join(filter(., excludeby == "No exclusion") %>% rename(DR_baseline = daily_risk) %>% select(-excludeby, -n_excluded, -supposed_to_exclude)
             , filter(., excludeby != "No exclusion"))} %>% 
  # filter(supposed_to_exclude == n_excluded) %>% #excludes any rows where there were not enough individuals to exclude
  mutate(DR_ratio = daily_risk/DR_baseline
         , DR_reduction = (DR_baseline - daily_risk)/DR_baseline
         , reduction_per_person = DR_reduction/n_excluded)



#### average change across all wards 

DR_pctChange_itt_bar = DR_pctChange %>%
  # R0_pctChange_cat_itt_bar = R0_changeVsRandom_cat %>%
  # filter(supposed_to_exclude == n_excluded) %>% 
  group_by(excludeby, combcat) %>% 
  summarise(median_DR_ratio = median(DR_ratio)
            , median_DR_reduction = median(DR_reduction))

# create plot

redDARplot_4status <-
  DR_pctChange %>% left_join(DR_pctChange_itt_bar) %>%
  # filter(excludeby %in% c("Random", "Degree", "Contact hours")) %>% 
  filter(excludeby %in% c("Degree", "Contact hours")) %>% 
  mutate(combcat = factor(combcat, levels = c("Patient", "Visitor", "HCW", "All"))) %>% 
  filter(!is.na(combcat)) %>% 
  mutate(excludeby = fct_recode(excludeby
                                , `Total\ncontact hours` = "Contact hours"
                                , `Total\ndistinct contacts` = "Degree"
  )) %>% 
  ggplot(aes(x = excludeby, colour = newID)) + 
  geom_jitter(aes(y = DR_reduction, shape = wardTypeCombine),  width = 0.2) +
  # geom_jitter(aes(y = DR_reduction, shape = clusterAjmal),  width = 0.2) + 
  geom_crossbar(aes(y = median_DR_reduction
                    , ymin = median_DR_reduction
                    , ymax = median_DR_reduction
  )
  , size=0.5,col="red", width = .5) +
  # facet_grid(which_row~factor(combcat, levels = combcats), drop = T) + 
  facet_wrap(.~combcat) + 
  theme_bw() + 
  theme(legend.background = element_rect(linetype = 1, size = 0.5, colour = 1)
        # , axis.title.x = element_text(hjust = 0.1)
        ) + 
  guides(colour = F) +
  theme(strip.background = element_rect(fill = NA)) + 
  scale_y_continuous(labels = function(x) scales::percent(x, accuracy = 1)) + 
  scale_shape_manual(values = c(`Neonatal ICU`=15,`General paediatrics`=16,`Paediatric emergency`=17
                                , `Adult ICU`=0,`Internal medicine`=1,`Adult emergency`=2
                                , `Infectious diseases`=3,`Geriatry`=4,`Pneumology`=5)) + 
  
  # scale_shape_manual(values = c(`Neonatal ICU`=3,`General paediatrics`=5,`Paediatric emergency`=2
  #                               , `Adult ICU`=0,`Internal medicine`=16,`Adult emergency`=4
  #                               , `Infectious diseases`=1,`Geriatry`=15,`Pneumology`=17)) + 
  labs(y = "Reduction in number of secondary infections per day per infected individual", x = "High risk individuals targeted by..."
       # , caption = expression(frac(DAR[baseline]-DAR,DAR[baseline]))
       , shape = "Ward type") #+ 
# coord_cartesian(ylim = c(0, NA))

redDARplot_4status



ggsave(plot = redDARplot_4status, "output/Fig_DARreduction.jpg", device = "jpeg"
       , width = 20, height = 15, units = "cm", dpi = 600)

ggsave(plot = redDARplot_4status, "output/Fig_DARreduction.pdf", device = "pdf"
       , width = 20, height = 15, units = "cm")


DR_pctChange_itt_bar


redDARplot_barReduction_plotdata <-
  DR_pctChange %>% left_join(DR_pctChange_itt_bar) %>%
  # filter(excludeby %in% c("Random", "Degree", "Contact hours")) %>% 
  mutate(newID = factor(newID, levels = Label_tib$newID)) %>% 
  
  filter(excludeby %in% c("Degree", "Contact hours"
                          # , "Random"
                          )) %>% 
  mutate(combcat = factor(combcat, levels = c("All", "HCW", "Patient", "Visitor"))) %>% 
  filter(!is.na(combcat)) %>% 
  mutate(excludeby = fct_recode(excludeby
                                , `Total contact hours` = "Contact hours"
                                , `Total distinct contacts` = "Degree"
                                # , `At random` = "Random"
  ))



redDARplot_barReduction <- redDARplot_barReduction_plotdata %>% 
  ggplot() + 
  geom_bar(aes(x = newID, y = DR_reduction), stat = "identity", position = position_dodge(width=0.8), width = 0.2) +
  # geom_segment(aes(x = newID, xend = newID, y = 0, yend = DR_reduction)) +
  # geom_point(aes(x = newID, y = DR_reduction)) +
  
  geom_crossbar(data = redDARplot_barReduction_plotdata %>% 
                  select(combcat, excludeby, median_DR_reduction) %>% 
                  unique
                  , aes(x = "Geriatry #1"
                    , y = median_DR_reduction
                    , ymin = median_DR_reduction
                    , ymax = median_DR_reduction
  ), size=0.2,col="red", width = 10, alpha = 0.8) +
  geom_text(data = redDARplot_barReduction_plotdata %>% 
              select(combcat, excludeby, median_DR_reduction) %>% 
              unique
            , aes(x = "Geriatry #1"
                  , y = median_DR_reduction
                  , label = paste0(round(median_DR_reduction*100), "%"))
            , nudge_y = 0.05
            , colour = "red"
            , size = 3) + 
  
  # facet_grid(which_row~factor(combcat, levels = combcats), drop = T) + 
  facet_nested_wrap(.~excludeby + combcat, nrow = 2) + 
  theme_bw() + 
  theme(legend.background = element_rect(linetype = 1, size = 0.5, colour = 1)
        , axis.text.x = element_text(angle = 45, hjust = 1)
        # , axis.title.x = element_text(hjust = 0.1)
  ) + 
  guides(colour = F) +
  theme(strip.background = element_rect(fill = NA)
        , axis.text.x = element_text(size = 6)
        , panel.grid.major.x = element_blank()) + 
  scale_y_continuous(labels = function(x) scales::percent(x, accuracy = 1)) + 
  # scale_shape_manual(values = c(`Neonatal ICU`=15,`General paediatrics`=16,`Paediatric emergency`=17
  #                               , `Adult ICU`=0,`Internal medicine`=1,`Adult emergency`=2
  #                               , `Infectious diseases`=3,`Geriatry`=4,`Pneumology`=5)) + 
  
  # scale_shape_manual(values = c(`Neonatal ICU`=3,`General paediatrics`=5,`Paediatric emergency`=2
  #                               , `Adult ICU`=0,`Internal medicine`=16,`Adult emergency`=4
  #                               , `Infectious diseases`=1,`Geriatry`=15,`Pneumology`=17)) + 
  labs(y = "Reduction in number of secondary infections\nper day per infected individual"
       # , x = "High risk individuals targeted by..."
       , x = ""
       # , caption = expression(frac(DAR[baseline]-DAR,DAR[baseline]))
       # , shape = "Ward type"
  ) #+ 

redDARplot_barReduction
# coord_cartesian(ylim = c(0, NA))
ggsave(plot = redDARplot_barReduction, "output/redDARplot_barReduction.pdf", device = "pdf"
       , width = 30, height = 15, units = "cm")

ggsave(plot = redDARplot_barReduction, "output/redDARplot_barReduction.jpg", device = "jpeg"
       , width = 30, height = 15, units = "cm", dpi = 600)



###### some figures for the paper
DR_pctChange_itt_bar$excludeby %>% table
DR_pctChange_itt_bar %>% 
  filter(excludeby == "Contact hours")

DR_pctChange_itt_bar %>% 
  filter(combcat %in% c("All", "HCW", "Patient")) %>% 
  filter(excludeby %in% c("Random", "Degree", "Contact hours"))
  
DR_pctChange_itt_bar %>% 
  filter(combcat %in% c("All")) %>% 
  filter(excludeby %in% c("Random", "Degree", "Contact hours"))

DR_pctChange_itt_bar %>% 
  filter(combcat %in% c("Patient")) %>% 
  filter(excludeby %in% c("Random", "Degree", "Contact hours"))

DR_pctChange_itt_bar %>% 
  filter(combcat %in% c("HCW")) %>% 
  filter(excludeby %in% c("Random", "Degree", "Contact hours"))

DR_pctChange_itt_bar %>% 
  filter(combcat %in% c("All"))

DR_pctChange_itt_bar %>% 
  filter(combcat %in% c("Patient"))
 