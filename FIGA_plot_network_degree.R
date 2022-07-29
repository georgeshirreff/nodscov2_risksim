# load packages

library(igraph)
library(ggraph)
library(ggpubr)
library(tidyverse)
library(magrittr)
library(data.table)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(ggcorrplot)
library(lubridate)
library(gridExtra)
library(gtable)
library(grid)
library(ggplotify)
library(scales)




# read files

# list_ward <- read_csv("input/list_ward_complete.csv")
# admission <- read_csv("input/id_function_complete.csv")
list_ward <- read_csv("input/list_ward_partial.csv")
admission <- read_csv("input/id_function_partial.csv")

pers_symb <- read_csv("input/personnel_symbols.csv")

# prepare objects

cats <- admission$catHosp %>% unique %>% sort
cats <- c("patient", "visitor", cats[cats != "investigation"], "investigation") %>% unique
stats <- c("PA", "V", "PE")

admission_types = admission %>% 
  transmute(id, status, catHosp = factor(catHosp, cats))


# removes the investigator category, from list_ward and from admission
invest = unique(admission$id[which(admission$catHosp=="investigation")])

lines = which(admission$id %in% invest)
if(length(lines)!=0)admission = admission[-lines,]



# pivot contact data 

list_ward_thisthat <- list_ward %>% 
  left_join(admission_types %>% rename_all(function(x) paste0(x, "_from")), by = c("from" = "id_from")) %>% 
  left_join(admission_types %>% rename_all(function(x) paste0(x, "_to")), by = c("to" = "id_to")) %>% 
  filter(catHosp_from != "investigation", catHosp_to != "investigation") %>% 
  mutate(across(starts_with("status"), ~factor(.x, levels = stats))) %>% 
  group_by(newID, wardType, from, to, status_from, status_to, catHosp_from, catHosp_to) %>% #first, aggregate contacts which occurred between the same people on the same day
  summarise(dur_mins = sum(length)/60)%>% 
  pivot_longer(cols = c("from", "to"), names_to = "direction", values_to = "id") %>% #then lengthen the table so that each contact counts for both parties
  mutate(this_status = case_when(direction == "from" ~ status_from
                                 , direction == "to" ~ status_to)
         , that_status = case_when(direction == "from" ~ status_to
                                   , direction == "to" ~ status_from)
         , this_catHosp = case_when(direction == "from" ~ catHosp_from
                                    , direction == "to" ~ catHosp_to)
         , that_catHosp = case_when(direction == "from" ~ catHosp_to
                                    , direction == "to" ~ catHosp_from))

contact_hours = list_ward_thisthat %>% 
  group_by(newID, id) %>% 
  summarise(dur_mins = sum(dur_mins)) %>% 
 mutate(hours_in_contact = dur_mins/60)


# ward labels

Label_tib <- newID_COVIDstat %>% 
  # mutate(newname = paste0("#", newID, ". ", DidierName, "\n", COVIDstat))
  # mutate(newname = paste0(newID, "\n", COVIDstat))
  mutate(newname = newID)

conn_tibble = tibble(newID = character(0), status = character(0), catSymb = character(0)
                   , degree = numeric(0), betweenness = numeric(0), closeness = numeric(0))

# loop through wards

gg_nets = list()
# ward = names(list_ward)[2]
ward_names <- list_ward$newID %>% unique
for(titl in ward_names){
  
  mat = list_ward %>% filter(newID == titl) %>% 
    group_by(from, to) %>% 
    summarise(length = sum(length)) %>% 
    filter(!(from %in% invest), !(to %in% invest))
  
  # set up the graph object
  mygraph <- graph_from_data_frame( as.matrix(mat[,c(1,2)]), directed = TRUE )
  
  vertex_characteristics <- tibble(id = V(mygraph)$name) %>% 
    left_join(admission %>% select(id, status, catHosp))
  V(mygraph)$status = vertex_characteristics$status
  V(mygraph)$catHosp = vertex_characteristics$catHosp
  V(mygraph)$catSymbol <- pers_symb$catSymbol2[match(V(mygraph)$catHosp, pers_symb$catHosp)]
  
  
  # E(mygraph)$shift = mat.shift$shift
  E(mygraph)$length = mat$length
  
  
  color_man_nb = c(PA = "red", V = "black", PE = "blue", ALL = "grey")
  
  status_numbers = V(mygraph)$status %>% 
    factor(levels = c("PA", "V", "PE")) %>% 
    recode_factor(PA = "patients", V = "visitors", PE = "HCWs") %>%
    table %>% 
    {paste(names(.), ., sep = "=")} %>% 
    paste(collapse = ", ")
  
  gg_net <- ggraph(mygraph,  layout = 'kk') + 
    geom_edge_link(alpha = 0.1) +
    # scale_edge_color_manual(name="Shift",values=c(J1 = "lightblue", N1 = "darkblue", J2 = "green")) +
    # facet_grid(.~factor(shift, levels = shift_names))+ggtitle(titl) + 
    # geom_node_point(aes(color = status), size=1.5) + 
    geom_node_text(aes(color = status, label = catSymbol), size=2) + 
    scale_color_manual(name="Status", values=color_man_nb) + 
    theme_bw() + theme(axis.line=element_blank()
                       , axis.text.x=element_blank()
                       , axis.text.y=element_blank()
                       , axis.ticks=element_blank()
                       , axis.title.x=element_blank()
                       , axis.title.y=element_blank()
                       , panel.grid = element_blank()
                       , panel.border = element_blank()
                       , text = element_text(size = 10)
                       , plot.title = element_text(hjust = 0.5)) + 
    labs(title = paste(titl, status_numbers, sep = "\n")) +
    guides(color = F)
  

  gg_nets[[titl]] = gg_net

    names(degree(mygraph))
  conn_tibble <- rbind(conn_tibble
        , tibble(newID = ward
                 , id = V(mygraph)$name
                 , status = V(mygraph)$status
                 , catSymbol = V(mygraph)$catSymbol
                 , degree = degree(mygraph)
                 , betweenness = betweenness(mygraph)
                 , closeness = closeness(mygraph)
         ) %>% left_join(contact_hours %>% select(id, hours_in_contact))
          )
  
  
}



  

# create connectivity table

conn_tibble_all <- conn_tibble %>% 
  {
    rbind(mutate(., status = "ALL")
          , .
          , left_join(., admission_types %>% 
                        transmute(id, combcatHosp = catHosp %>% {case_when(. == "patient" ~ "Patient"
                                                                           , . == "visitor" ~ "Visitor"
                                                                           , . == "nurse" ~ "Nurse"
                                                                           , . == "physician" ~ "Physician"
                                                                           , . == "aux nurse" ~ "Nurse"
                                                                           , . == "administration" ~ "Administration/\nLogistic"
                                                                           , . == "ext physician" ~ "Physician"
                                                                           , . == "logistic" ~ "Administration/\nLogistic"
                                                                           , . == "student nurse" ~ "Nurse"
                                                                           , . == "reeducation staff" ~ "Other HCW"
                                                                           , . == "other" ~ "Other HCW")})) %>% 
            filter(status == "PE") %>% 
            mutate(status = combcatHosp) %>% 
            select(-combcatHosp))
  } %>% 
  mutate(status_label = case_when(status == "ALL" ~ "All"
                                  , status == "PA" ~ "Patient"
                                  , status == "V" ~ "Visitor"
                                  , status == "PE" ~ "HCW"
                                  , T ~ status)) %>% 
  mutate(status_label = factor(status_label, c("All", "Patient", "Visitor", "HCW", "Nurse", "Physician", "Administration/\nLogistic", "Other HCW")))

# create violin plots

deg_violin <- newID_COVIDstat %>% 
  left_join(conn_tibble_all) %>% 
  pivot_longer(cols = c("degree", "closeness", "betweenness", "hours_in_contact")) %>% 
  mutate(name = str_to_title(name) %>% {gsub("_", " ", .)} ) %>% 
  filter(name %in% c("Degree")) %>% 
  
  group_by(status_label) %>% mutate(mean = mean(value)
                                    , median = median(value)) %>% ungroup %>% 
  
  # filter(name %in% c("Degree", "Hours in contact")) %>% 
  ggplot(aes(x = status_label, fill = status_label)) +
  geom_violin(aes(y = value), colour = NA, scale = "count", ) +
  geom_vline(xintercept = 4.5, linetype = "dashed") + 
  geom_crossbar(aes(y = mean, ymin = mean, ymax = mean)
                , size=0.5,col="orange", width = .5) +
  # geom_crossbar(aes(y = median, ymin = median, ymax = median)
  #               , size=0.5,col="purple", width = .5) +
  
  facet_grid(name~., scales = "free_y", switch = "y") + 
  
  # scale_y_continuous(trans = "log10", breaks = waiver()) +
  scale_fill_manual(values = c(All = "grey70"
                               , Patient = "red", Visitor = "black", HCW = "blue"
                               , Nurse = "cornflowerblue", Physician = "cornflowerblue"
                               , `Administration/\nLogistic` = "cornflowerblue"
                               , `Other HCW` = "cornflowerblue")) + 
  theme_bw() + 
  guides(fill = "none") +
  theme(strip.placement = "outside"
        , strip.background = element_blank()
        , axis.text.x = element_blank()
  ) + 
  labs(x = "", y = "", fill = "") 

deg_violin

hrs_violin <- newID_COVIDstat %>% 
  left_join(conn_tibble_all) %>% 
  pivot_longer(cols = c("degree", "closeness", "betweenness", "hours_in_contact")) %>% 
  mutate(name = str_to_title(name) %>% {gsub("_", " ", .)} ) %>% 
  filter(name %in% c("Hours in contact")) %>% 
  
  group_by(status_label) %>% mutate(mean = mean(value)
                                    , median = median(value)) %>% ungroup %>% 
  
  # filter(name %in% c("Degree", "Hours in contact")) %>% 
  ggplot(aes(x = status_label, fill = status_label)) +
  geom_violin(aes(y = value), colour = NA, scale = "count", ) +
  geom_vline(xintercept = 4.5, linetype = "dashed") + 
  geom_crossbar(aes(y = mean, ymin = mean, ymax = mean)
                , size=0.5,col="orange", width = .5) +
  # geom_crossbar(aes(y = median, ymin = median, ymax = median)
  #               , size=0.5,col="purple", width = .5) +
  
  facet_grid(name~., scales = "free_y", switch = "y") + 
  scale_y_continuous(trans = "log10", breaks = waiver()) +
  scale_fill_manual(values = c(All = "grey70"
                               , Patient = "red", Visitor = "black", HCW = "blue"
                               , Nurse = "cornflowerblue", Physician = "cornflowerblue"
                               , `Administration/\nLogistic` = "cornflowerblue"
                               , `Other HCW` = "cornflowerblue")) + 
  theme_bw() + 
  guides(fill = "none") +
  theme(strip.placement = "outside"
        , strip.background = element_blank()
        # , axis.text.x = element_blank()
  ) + 
  labs(x = "", y = "", fill = "") 

deghrs_violin <- arrangeGrob(deg_violin, hrs_violin, nrow = 2)


# combine violin plots

deg_grob <- ggplotGrob(deg_violin)
hrs_grob <- ggplotGrob(hrs_violin)
g <- rbind(deg_grob, hrs_grob, size = "first")
g$widths <- unit.pmax(deg_grob$widths, hrs_grob$widths)
grid.newpage()
grid.draw(g)

ggsave(g, filename = "output/Fig_violin.pdf", width = 20, height = 10, units = "cm", device = "pdf")


# create network figure

gg_nets <- gg_nets[Label_tib$newname]

ggsave(filename = "output/Fig_networks.pdf"
       , arrangeGrob(grobs = gg_nets, ncol = 3)
       , units = "cm", width = 40, height = 60, dpi = 100, device = "pdf")


        






