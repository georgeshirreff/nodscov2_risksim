ward_hop_id <- read_csv("input/ward_hop_id.csv")
newID_COVIDstat <- read_csv("input/typeID_COVIDstat.csv")


load("input/list_ward.RData", verbose = T)
list_ward %>% 
  rbindlist(idcol = "list_ward_name") %>% 
  left_join(ward_hop_id) %>% 
  left_join(newID_COVIDstat %>% transmute(ward_id, wardType = DidierName, newID)) %>% 
  select(-hosp_ward, -list_ward_name) %>% 
  filter(!is.na(newID)) -> list_ward_complete
list_ward_complete %>% 
  write_csv("input/list_ward_complete.csv")

load("input/admission_ctc_nodscov2.RData", verbose = T)
admission %>% 
  left_join(newID_COVIDstat %>% transmute(ward_id, wardType = DidierName, newID)) %>% 
  select(-hospital, -hosp_ward, -list_ward_name, -ward) -> id_function_complete

id_function_complete %>% 
  write_csv("input/id_function_complete.csv")

timespent_mins <- read_csv(file = "input/met_timespent_mins.csv")
timespent_mins %>% 
  left_join(ward_hop_id) %>% 
  left_join(newID_COVIDstat %>% transmute(ward_id, wardType = DidierName, newID)) %>% 
  filter(!is.na(newID)) %>% 
  select(-hosp_ward, -list_ward_name, -ward_id) -> timespent_mins_complete

timespent_mins_complete %>% 
  write_csv("input/met_timespent_mins_complete.csv")
# timespent_mins <- read_csv("input/met_timespent_mins_complete.csv")


# now make partial version

set.seed(1)
N_froms = 5
sampled_froms = character(0)
invest_ids <- id_function_complete %>% filter(catHosp == "investigation") %>% pull(id)


for(ward in unique(list_ward_complete$newID)){
  sampled_froms = c(sampled_froms, list_ward_complete %>% 
                      filter(!(from %in% invest_ids),!(to %in% invest_ids)) %>% 
    filter(newID == ward) %>% 
    pull(from) %>% 
    sample(size = N_froms))
}

list_ward_partial <- list_ward_complete %>% 
  filter(from %in% sampled_froms)

sampled_ids <- list_ward_partial %>% 
  {c(.$from, .$to)} %>% 
  unique

id_function_partial <- id_function_complete %>% 
  filter(id %in% sampled_ids)


timespent_mins_partial <- timespent_mins_complete %>% 
  filter(id %in% sampled_ids)

list_ward_partial %>% 
  write_csv("input/list_ward_partial.csv")


id_function_partial %>% 
  write_csv("input/id_function_partial.csv")

timespent_mins_partial %>% 
  write_csv("input/met_timespent_mins_partial.csv")
