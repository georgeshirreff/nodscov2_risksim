#######################
#### load packages ####
#######################

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




###################
#### read data ####
###################

# list_ward <- read_csv("input/Private/list_ward_complete.csv")
# admission <- read_csv("input/Private/id_function_complete.csv")
list_ward <- read_csv("input/list_ward_partial.csv")
admission <- read_csv("input/id_function_partial.csv")

pers_symb <- read_csv("input/personnel_symbols.csv")
pers_symb_lab <- read_csv("input/personnel_symbols_label.csv")

# visual categorisation of ward networks
vis <- read_csv("input/visCat.csv")

# ward labels
Label_tib <- read_csv("input/newID_order.csv")

#########################
#### prepare objects ####
#########################

cats <- admission$catHosp %>% unique %>% sort
cats <- c("patient", "visitor", cats[cats != "investigation"], "investigation") %>% unique
stats <- c("PA", "V", "PE")
color_man_nb = c(PA = "red", V = "black", PE = "blue", ALL = "grey")

admission_types = admission %>% 
  transmute(id, status, catHosp = factor(catHosp, cats))


# removes the investigator category, from list_ward and from admission
invest = unique(admission$id[which(admission$catHosp=="investigation")])

lines = which(admission$id %in% invest)
if(length(lines)!=0)admission = admission[-lines,]



################################
#### pivot the contact data ####
################################

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



############################
#### loop through wards ####
############################


conn_tibble = tibble(newID = character(0), status = character(0), catSymb = character(0)
                   , degree = numeric(0), betweenness = numeric(0), closeness = numeric(0))
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
  
  # apply individual characteristics to graph vertices
  vertex_characteristics <- tibble(id = V(mygraph)$name) %>% 
    left_join(admission %>% select(id, status, catHosp))
  V(mygraph)$status = vertex_characteristics$status
  V(mygraph)$catHosp = vertex_characteristics$catHosp
  V(mygraph)$catSymbol <- pers_symb$catSymbol2[match(V(mygraph)$catHosp, pers_symb$catHosp)]
  
  # length of time applied to the edges of the graph
  E(mygraph)$length = mat$length
  
  # create heading for each graph panel
  status_numbers = V(mygraph)$status %>% 
    factor(levels = c("PA", "V", "PE")) %>% 
    recode_factor(PA = "patients", V = "visitors", PE = "HCWs") %>%
    table %>% 
    {paste(names(.), ., sep = "=")} %>% 
    paste(collapse = ", ")
  
  # produce graph plot for this ward
  gg_net <- ggraph(mygraph,  layout = 'kk') + 
    geom_edge_link(alpha = 0.1) +
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
  

  # assign plot to list of plots
  gg_nets[[titl]] = gg_net

  # get connectivity data from graph into table
  conn_tibble <- rbind(conn_tibble
        , tibble(newID = titl
                 , id = V(mygraph)$name
                 , status = V(mygraph)$status
                 , catSymbol = V(mygraph)$catSymbol
                 , degree = degree(mygraph)
                 , betweenness = betweenness(mygraph)
                 , closeness = closeness(mygraph)
         ) %>% left_join(contact_hours %>% select(id, hours_in_contact))
          )
  
  
}


# create network figure

# gg_nets <- gg_nets[Label_tib$newID]
# 
# ggsave(filename = "output/Fig_networks.pdf"
#        , arrangeGrob(grobs = gg_nets, ncol = 3)
#        , units = "cm", width = 40, height = 60, dpi = 100, device = "pdf") 

# create network figure by visual category
gg_nets_split <- gg_nets[vis$newID[vis$visCat == "Split"]]
gg_nets_even <- gg_nets[vis$newID[vis$visCat == "Even"]]
gg_nets_dens <- gg_nets[vis$newID[vis$visCat == "DenseCentre"]]
gg_nets_centr <- gg_nets[vis$newID[vis$visCat == "Centralised"]]

# create legend
pers_symb_lab_mat <- pers_symb_lab %>% as.matrix
colnames(pers_symb_lab_mat) = NULL
pers_legend <- ggtexttable(pers_symb_lab_mat
                           , theme = ttheme("blank"
                                            # , colnames.style = colnames_style(size = 0, fill = "white", color = "white")
                                            # , rownames.style = rownames_style(size = 0)
                                            #                  , tbody.style = tbody_style(color = "blue",
                                            #                                              fill = c("white"), hjust=0, x=0)
                           )
) %>%
  table_cell_font(row = 1, col = 1:2, color = "red") %>%
  table_cell_font(row = 2, col = 1:2, color = "black") %>% 
  table_cell_font(row = 3:7, col = 1:2, color = "blue")

# include legend as one of the plots
gg_nets_centr[["legend"]] <- pers_legend


# output this plot
ggsave(# filename = "output/Didier/BigPaper/Fig_networks_categorised.pdf"
  filename = "output/Fig_networks_categorised.jpg"
  , arrangeGrob(arrangeGrob(grobs = gg_nets_split
                            , left = textGrob("Split", gp = gpar(col = "black", fontsize = 20)), ncol = 4)
                , arrangeGrob(grobs = gg_nets_even
                              , left = textGrob("Even", gp = gpar(col = "black", fontsize = 20)), ncol = 4)
                , arrangeGrob(grobs = gg_nets_dens
                              , left = textGrob("Dense\ncentre", gp = gpar(col = "black", fontsize = 20)), ncol = 4)
                , arrangeGrob(grobs = gg_nets_centr
                              , left = textGrob("Centralised", gp = gpar(col = "black", fontsize = 20)), ncol = 4)
                , ncol = 1)
  
  , units = "cm", width = 50, height = 60, dpi = 600
  , device = "jpeg"
  # , device = "pdf"
)

# create overall connectivity table for violin plots
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



# create plot of contact degree
deg_horiz_violin <- Label_tib %>% 
  left_join(conn_tibble_all) %>% 
  mutate(status_label = fct_rev(status_label)) %>% 
  pivot_longer(cols = c("degree", "closeness", "betweenness", "hours_in_contact")) %>% 
  mutate(name = str_to_title(name) %>% {gsub("_", " ", .)} ) %>% 
  filter(name %in% c("Degree")) %>% 
  
  group_by(status_label) %>% 
  mutate(mean = mean(value)
         , median = median(value)) %>% 
  ungroup %>% 

  ggplot(aes(y = status_label, fill = status_label)) +
  geom_violin(aes(x = value), colour = NA, scale = "area") +
  geom_hline(yintercept = 4.5, linetype = "dashed") + 
  geom_point(aes(x = median, y = status_label), col = 'orange', size = 1.5) +
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
  labs(x = "Number of distinct contacts\nover period of study", y = "", fill = "") 

# create plot of contact duration
hrs_horiz_violin <- Label_tib %>% 
  left_join(conn_tibble_all) %>% 
  mutate(status_label = fct_rev(status_label)) %>% 
  pivot_longer(cols = c("degree", "closeness", "betweenness", "hours_in_contact")) %>% 
  mutate(name = str_to_title(name) %>% {gsub("_", " ", .)} ) %>% 
  filter(name %in% c("Hours in contact")) %>% 
  
  group_by(status_label) %>% mutate(mean = mean(value)
                                    , median = median(value)) %>% ungroup %>% 
  
  ggplot(aes(y = status_label, fill = status_label)) +
  geom_violin(aes(x = value), colour = NA, scale = "area") +
  geom_hline(yintercept = 4.5, linetype = "dashed") + 
  geom_point(aes(x = median, y = status_label), col = 'orange', size = 1.5) +
  scale_x_continuous(trans = "log10", breaks = waiver(), labels = function(x) format(x, scientific = F, trim = T, drop0trailing = T)) +
  scale_fill_manual(values = c(All = "grey70"
                               , Patient = "red", Visitor = "black", HCW = "blue"
                               , Nurse = "cornflowerblue", Physician = "cornflowerblue"
                               , `Administration/\nLogistic` = "cornflowerblue"
                               , `Other HCW` = "cornflowerblue")) + 
  theme_bw() + 
  guides(fill = "none") +
  theme(strip.placement = "outside"
        , strip.background = element_blank()
        , axis.text.y = element_blank()
  ) + 
  labs(x = "Total hours in contact\nover period of study", y = "", fill = "") 
hrs_horiz_violin
deghrs_horiz_violin <- arrangeGrob(deg_horiz_violin, hrs_horiz_violin, ncol = 2)



# combine these plots into a single figure
deg_horiz_grob <- ggplotGrob(deg_horiz_violin)
hrs_horiz_grob <- ggplotGrob(hrs_horiz_violin)
g <- cbind(deg_horiz_grob, hrs_horiz_grob, size = "first")
g$heights <- unit.pmax(deg_horiz_grob$heights, hrs_horiz_grob$heights)
grid.newpage()
grid.draw(g)

ggsave(g, filename = "output/Fig_horiz_violin.pdf", width = 15, height = 10, units = "cm", device = "pdf")
ggsave(g, filename = "output/Fig_horiz_violin.png", width = 15, height = 10, units = "cm")




