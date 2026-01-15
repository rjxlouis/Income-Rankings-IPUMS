# ==============================================================================
# ALL HELPER FUNCTIONS
# ==============================================================================
# This file contains all the core functions for data loading, preparation,
# aggregation, and ranking using Renee's Aggregations Code
# 
# And all of sofia's helper functions for replicating and improving IPUMS family
# pointers (EDITED pointers only).
#
# ==============================================================================

library(data.table)
library(ipumsr)
library(progress)
library(future)
library(furrr)
library(tidyverse)
library(foreach)
library(igraph)
library(tidycensus)
library(tictoc)
library(doParallel)

# ==============================================================================
# SOFIA'S HELPER FUNCTIONS
# ==============================================================================
# ==============================================================================
# 0. HARMONIZATION
# ==============================================================================
variables = readxl::read_xlsx('data/manual/relevant_variables.xlsx')
relshipp_harmonize = readxl::read_xlsx('data/manual/relevant_variables.xlsx',
                                       sheet='relshipp_harmonize') %>%
  select(relshipp_old,relshipp_new,year)


harmonize_in_laws = function(df){
  
  df = df %>%
    mutate(hh_with_in_law = ifelse(relshipp == 31,1,0)) %>%
    group_by(serialno) %>%
    mutate(hh_with_in_law = ifelse(sum(hh_with_in_law) > 0,1,0))
  
  hh_with_in_law = df %>%
    filter(hh_with_in_law == 1) %>%
    mutate(in_law_recode = 0,
           head_never_married = ifelse(relshipp == 20 & mar == 5,1,0), 
           head_spouse = ifelse(relshipp %in% c(21,23),1,0),
           married_children = ifelse(relshipp %in% c(25:27,30) & mar == 1,1,0),
           married_sibling = ifelse(relshipp == 28 & mar == 1,1,0)) %>%
    group_by(serialno) %>%
    mutate(head_spouse = ifelse(sum(head_spouse) > 0,1,0),
           married_children = ifelse(sum(married_children) > 0,1,0),
           head_never_married = ifelse(sum(head_never_married) > 0,1,0),
           married_sibling = ifelse(sum(married_sibling) > 0,1,0))
  
  done = df %>% filter(hh_with_in_law == 0)
  
  # must be married
  # if spouse of head not present, 17 years younger than head
  # if spouse of head presetn, 10 years younger than head
  # and 17 years younger than heads spouse
  # if married children or grandchildren, 
  # 6 years younger than head and head spouse
  hh_with_in_law = hh_with_in_law %>%
    arrange(serialno) %>%
    group_by(serialno) %>%
    mutate(head_age = agep[relshipp == 20],
           head_spouse_age = ifelse(head_spouse == 1, agep[relshipp %in% c(21,23)], NA),
           age_diff_to_head = head_age - agep,
           age_diff_head_spouse = ifelse(is.na(head_spouse_age),0,head_spouse_age - agep),
           in_law_recode = case_when(
             relshipp != 31 ~ 0,
             mar == 5 ~ 0,
             age_diff_to_head >= 17 & head_spouse == 0 ~ 32,
             age_diff_to_head >= 9 & age_diff_head_spouse >= 17 ~ 32,
             married_children == 1 & age_diff_to_head >= 5 & age_diff_head_spouse >= 5 ~ 32,
             married_children == 1 & age_diff_to_head >= 5 & head_spouse == 0 ~ 32,
             TRUE ~ 0
           )) %>%
    ungroup()
  
  
  # sibling-in-law 
  # which we will code as other relative (33)
  
  hh_with_in_law = hh_with_in_law %>%
    mutate(in_law_recode = case_when(
      relshipp != 31 | in_law_recode != 0 ~ in_law_recode,
      mar == 5 ~ 33,
      head_never_married == 1 ~ 33,
      age_diff_to_head <= 16 & age_diff_to_head >= -10 ~ 33,
      head_spouse == 1 & age_diff_head_spouse <= 16 & age_diff_head_spouse >= -10 ~ 33,
      head_spouse == 1 & age_diff_head_spouse > 0 & age_diff_to_head < 0 ~ 33,
      married_sibling == 1 & age_diff_to_head <= 20 & age_diff_to_head >= -17 ~ 33,
      married_sibling == 1 & head_spouse == 1 & age_diff_head_spouse <= 20 & age_diff_head_spouse >= -17 ~ 33,
      TRUE ~ 0
    ))
  
  # 
  hh_with_in_law = hh_with_in_law %>%
    mutate(in_law_recode = case_when(
      relshipp != 31 | in_law_recode != 0 ~ in_law_recode,
      head_spouse == 0 & age_diff_to_head <= -11 ~ 31,
      head_spouse == 1 & age_diff_head_spouse <= -11 ~ 31,
      TRUE ~ 0
    ))
  
  # reclassify sibling-in-law as parent-in-law based on age differences
  
  hh_with_in_law = hh_with_in_law %>%
    mutate(in_law_recode = case_when(
      in_law_recode == 33 & head_spouse == 0 & age_diff_to_head <= -18 ~ 31,
      in_law_recode == 33 & head_spouse == 1 & age_diff_head_spouse <= -18 ~ 31,
      TRUE ~ in_law_recode))
  
  # i think we any remaining missing codes sibling-in-law
  hh_with_in_law = hh_with_in_law %>%
    mutate(in_law_recode = ifelse(relshipp == 31 & in_law_recode == 0,33,in_law_recode),
           in_law_recode = ifelse(in_law_recode == 31 & head_never_married == 1, 33, in_law_recode))
  
  hh_with_in_law = hh_with_in_law %>%
    mutate(relshipp = ifelse(relshipp == 31,in_law_recode,relshipp))
  
  df = bind_rows(done,
                 hh_with_in_law %>%
                   select(-in_law_recode,-head_never_married,-head_spouse,
                          -married_children,-married_sibling,-head_age,-head_spouse_age,-age_diff_to_head,
                          -age_diff_head_spouse)) %>%
    select(-hh_with_in_law)
  
  return(df)
  
}

#years = as.character(2017:2023)
#years = as.character(2013:2016)
#years = c('2012')
#years = as.character(2011:2008)

years = c('2000')

for(curr_year in years){
  
  curr_variables = variables %>% filter(year == 'all' | year == curr_year)
  
  if(curr_year == '2000'){
    filename = 'data/raw/decennial_2000.csv'
  }else{
    filename = paste0('data/raw/acs_',curr_year,'.csv')
  }
  
  df = fread(filename)
  
  df = df %>%
    janitor::clean_names() %>%
    select(curr_variables$old_name) %>%
    set_names(curr_variables$new_name)
  
  # for testing
  #set.seed(1)
  #households = df %>% pull(ipums_serial) %>% unique()
  #sample = sample(households,size=length(households)/5,replace=F)
  #df = df %>% filter(ipums_serial %in% sample)
  
  if(curr_year == '2000'){
    df = df %>%
      mutate(statefip = str_pad(as.character(statefip),
                                2,side='left',pad='0')) %>%
      mutate(serialno = paste0(as.character(serialno),
                               as.character(statefip)))
  }
  
  if(as.numeric(curr_year) <= 2018){
    
    # all partners coded as opposite sex
    # due to crosswalk between <=2018 and 2019+ codes
    # use sex to figure out if opposite or same-sex
    # 21 = Opposite-sex husband/wife/spouse
    # 22 = Opposite-sex unmarried partner
    # 23 = Same-sex husband/wife/spouse
    # 24 = Same-sex unmarried partner
    
    var_year = case_when(
      as.numeric(curr_year) >= 2008 ~ 2018,
      as.numeric(curr_year) == 2000 ~ 2000,
      TRUE ~ 2007
    )
    
    curr_relshipp_harmonize = relshipp_harmonize %>%
      filter(year == var_year) %>%
      select(-year)
    
    df = merge(df,curr_relshipp_harmonize,by.x='relshipp',by.y='relshipp_old') %>%
      mutate(relshipp = relshipp_new) %>%
      select(-relshipp_new)
    
    df = df %>%
      group_by(serialno) %>%
      # remember sporder != sploc
      mutate(sex_of_ref = sex[sporder == 1]) %>%
      ungroup() %>%
      mutate(relshipp = case_when(
        # if "opposite-sex spouse" is same sex, recode to 23
        relshipp == 21 & sex == sex_of_ref ~ 23,
        relshipp == 22 & sex == sex_of_ref ~ 24,
        TRUE ~ relshipp
      )) %>%
      select(-sex_of_ref)
    
    if(as.numeric(curr_year) <= 2007){
      
      df = harmonize_in_laws(df)
      
    }
    
    if(curr_year == '2000'){
      df = df %>%
        mutate(housing_unit_type = housing_unit_type + 1)
    }
    
  }
  
  outfile = paste0('data/intermediate/clean_acs/acs_',curr_year,'.csv')
  fwrite(df,outfile)
}

# ==============================================================================
# 1. SPLOC
# ==============================================================================
enumerate_bundles <- function(A_ids, B_ids) {
  nA <- length(A_ids)
  nB <- length(B_ids)
  if (nA > nB) stop("There must be at least as many B_ids as A_ids")
  
  # recursive helper
  dfs <- function(i, used) {
    if (i > nA) return(list(integer(0)))  # one empty solution
    bundles <- list()
    for (b_idx in seq_along(B_ids)) {
      if (!(b_idx %in% used)) {
        subsols <- dfs(i + 1, c(used, b_idx))
        for (s in subsols) {
          bundles[[length(bundles) + 1]] <- c(b_idx, s)
        }
      }
    }
    bundles
  }
  
  raw <- dfs(1, integer(0))
  lapply(raw, function(idx) data.frame(A = A_ids, B = B_ids[idx]))
}


bundles_from_pairs <- function(candidate_bundles, A_col = "A", B_col = "B") {
  A <- as.character(candidate_bundles[[A_col]])
  B <- as.character(candidate_bundles[[B_col]])
  
  # make node names distinct across sides (avoid clashes like "A1" vs "B1")
  A_nodes <- paste0("A:", A)
  B_nodes <- paste0("B:", B)
  
  g <- graph_from_data_frame(
    d = data.frame(from = A_nodes, to = B_nodes, stringsAsFactors = FALSE),
    directed = FALSE
  )
  
  comp <- components(g)
  mem  <- comp$membership
  
  # tidy output: component id per node, and a per-bundle list
  out_nodes <- data.frame(
    node   = names(mem),
    bundle = as.integer(mem),
    id     = sub("^[AB]:", "", names(mem)),
    side   = ifelse(grepl("^A:", names(mem)), "A", "B"),
    stringsAsFactors = FALSE
  )
  
  # split into bundles (each element has the A’s and B’s in that connected set)
  bundles <- split(out_nodes[order(out_nodes$side, out_nodes$id), ], out_nodes$bundle)
  bundles <- as.data.frame(bundles)
  unique_bundles = unique(bundles$X1.bundle)
  
  best_couples = foreach::foreach(b = unique_bundles,.combine='rbind') %do% {
    
    best_couple = bundles %>% filter(X1.bundle == b)
    best_couple = best_couple %>%
      arrange(X1.id) %>%
      group_by(X1.side) %>%
      mutate(rank = row_number()) %>%
      select(X1.id,X1.side,rank) %>%
      ungroup()
    A = best_couple %>%
      filter(X1.side == 'A') %>%
      select(X1.id,rank) %>%
      set_names(c('id_A','rank_A'))
    B = best_couple %>%
      filter(X1.side == 'B') %>%
      select(X1.id,rank) %>%
      set_names(c('id_B','rank_B'))
    best_couple = cross_join(A,B) %>%
      mutate(rank_diff = abs(rank_A - rank_B)) %>%
      slice_min(order_by=rank_diff,n=1) %>%
      select(id_A,id_B) 
    
    best_couple = bind_rows(best_couple %>% set_names(c('sporder','sploc_test')),
                            best_couple %>% select(id_B,id_A) %>% set_names(c('sporder','sploc_test')))
    
    return(best_couple)
    
  }
  
  return(best_couples)
  
}


resolve_age_match = function(top_by_age_diff,bundles){
  
  curr_bundles = bundles %>% filter(bundle_id %in% top_by_age_diff$bundle_id)
  
  curr_bundles = curr_bundles %>%
    mutate(couple = paste0(A,'-',B)) %>%
    group_by(couple) %>%
    mutate(num = n()) 
  
  # couples that appear in all optimal bundles
  final_couples = curr_bundles %>%
    ungroup() %>%
    filter(num == nrow(top_by_age_diff)) 
  
  candidate_bundles = curr_bundles %>%
    filter(!couple %in% final_couples$couple) 
  
  final_couples = final_couples %>%
    select(A,B) %>%
    unique()
  
  final_couples = bind_rows(final_couples %>% set_names(c('sporder','sploc_test')),
                            final_couples %>% select(B,A) %>% set_names(c('sporder','sploc_test')))
  
  
  candidate_bundles = bundles_from_pairs(candidate_bundles)
  
  return(bind_rows(final_couples %>% mutate_all(as.numeric),
                   candidate_bundles %>% mutate_all(as.numeric)))
  
  
  
}

find_optimal_match = function(matches_simple){
  
  root_ids = matches_simple %>% filter(in_root_cat == 1) %>% pull(sporder)
  dest_ids = matches_simple %>% filter(in_dest_cat == 1) %>% pull(sporder)
  # m \leq n
  if(length(root_ids) >= length(dest_ids)){
    A_ids = dest_ids
    B_ids = root_ids
  }else{
    A_ids = root_ids
    B_ids = dest_ids
  }
  
  num_bundles = factorial(length(B_ids)) / factorial(length(B_ids) - length(A_ids))
  
  if(num_bundles < 100){
    
    bundles = enumerate_bundles(A_ids, B_ids)
    bundles = foreach(i = 1:length(bundles),.combine=bind_rows) %do% {
      bundle = bundles[[i]] %>%
        mutate(bundle_id = i)
    }
    
    bundles = bundles %>%
      filter(A != B) %>%
      group_by(bundle_id) %>%
      mutate(n_pairs = n()) %>%
      ungroup() %>%
      mutate(max_pairs = max(n_pairs)) %>%
      filter(n_pairs == max_pairs) %>%
      select(-n_pairs,-max_pairs) %>%
      mutate(couple_desc = paste0(pmin(A,B),pmax(A,B))) %>%
      group_by(bundle_id,couple_desc) %>%
      mutate(repeats = ifelse(n()>1,1,0)) %>%
      group_by(bundle_id) %>%
      mutate(repeats = sum(repeats)) %>%
      filter(repeats == 0) %>%
      select(-couple_desc,-repeats) %>%
      group_by(bundle_id) %>%
      mutate(unique_members = length(unique(c(A,B))),
             all_members = length(c(A,B))) %>%
      filter(unique_members == all_members) %>%
      select(-unique_members,-all_members)
    
    if(nrow(bundles) == 0){
      return(NULL)
    }
    
    bundles = bundles %>%
      merge(.,matches_simple %>% select(sporder,agep,sex),by.x='A',by.y='sporder',all.x=T) %>%
      merge(.,matches_simple %>% select(sporder,agep,sex),by.x='B',by.y='sporder',all.x=T) %>%
      mutate(age_diff = abs(agep.x - agep.y),
             same_sex = ifelse(sex.x == sex.y,1,0)) 
    
    bundle_stats = bundles %>%
      group_by(bundle_id) %>%
      summarise(total_age_diff = sum(age_diff,na.rm=T),
                total_same_sex = sum(same_sex,na.rm=T))
    
    top_by_sex = bundle_stats %>%
      slice_min(n=1,order_by=total_same_sex,with_ties=T)
    
    #return NULL is best bundle contains same sex couple
    #we can modify this later
    
    if(min(top_by_sex$total_same_sex) > 0){
      return(NULL)
    }
    
    top_by_age_diff = top_by_sex %>%
      slice_min(n=1,order_by=total_age_diff,with_ties=T)
    
    if(nrow(top_by_age_diff) > 1){
      
      return(resolve_age_match(top_by_age_diff,bundles))
      
    } else{
      
      best_bundle = bundles %>%
        filter(bundle_id == top_by_age_diff$bundle_id[1]) %>%
        select(A,B)
      best_bundle = bind_rows(best_bundle %>% set_names(c('sporder','sploc_test')),
                              best_bundle %>% select(B,A) %>% set_names(c('sporder','sploc_test'))) %>%
        mutate_all(as.numeric)
      
      return(best_bundle)
      
    }
    
    
  } else{
    
    print(paste0('Too many bundles: ',num_bundles))
    return(NULL)
    
  }
}

# now, only married people can have non-zero sploc
#curr_hh_id = 2022001110661
#all_hh = df_rel
#one_to_many = T
find_best_match_in_hh = function(curr_hh_id,all_hh,one_to_many = F){
  
  #curr_hh_id = households[5]
  
  curr_hh = all_hh %>% filter(serialno == curr_hh_id)
  
  nrow_start = nrow(curr_hh)
  
  if(!one_to_many){
    
    matches = curr_hh %>%
      select(sporder,agep,sex)
    
    max_couples = floor(nrow(matches)/2)
    
    
  } else {
    
    matches = curr_hh %>%
      select(sporder,agep,sex,in_root_cat,in_dest_cat)
    
    max_couples_naive = floor(nrow(matches)/2)
    max_couples = min(sum(matches$in_root_cat == 1,na.rm=T),
                      sum(matches$in_dest_cat == 1,na.rm=T))
    max_couples = min(max_couples_naive,max_couples)
    
  }
  
  matches = matches %>%
    arrange(sporder) %>%
    mutate(rowid = row_number()) 
  
  matches_simple = matches
  
  # dont join w code above
  matches = matches %>%
    inner_join(matches,by = character()) %>%
    filter(rowid.x < rowid.y) 
  
  if(one_to_many){
    matches = matches %>%
      filter((in_root_cat.x == 1 & in_dest_cat.y == 1) |
               (in_root_cat.y == 1 & in_dest_cat.x == 1))
  }
  
  
  matches = matches %>%
    mutate(same_sex = 1-abs(sex.x - sex.y),
           diff_age = abs(agep.x - agep.y),
           diff_order = abs(sporder.x - sporder.y)) %>%
    select(sporder.x,sporder.y,same_sex,diff_age,diff_order,
           sex.x,sex.y,agep.x,agep.y) %>%
    # to do serially
    # arrange(same_sex,sporder.x,diff_age,diff_order) 
    # to prioritize better matches (rather than person order)
    arrange(same_sex,diff_age,diff_order,sporder.x) 
  
  mult_same_sex = matches %>%
    filter(same_sex == 1) %>%
    summarise(max_pairs = min(n_distinct(sporder.x))) %>%
    pull(max_pairs)
  
  #Unlike some other IPUMS data projects, 
  #IPUMS USA does not pair same-sex couples in households that
  #appear to contain multiple same-sex couples
  
  if(mult_same_sex > 1){
    matches = matches %>%
      filter(same_sex == 0)
  }
  
  top_match = NULL
  # have if and return statement here if bundles are okay
  if(max_couples > 1 & one_to_many == T){
    
    # get bundle
    # if too many bundles
    # we move on
    # else, we return
    top_match = find_optimal_match(matches_simple)
    
  }
  
  if(is.null(top_match)){
    
    # add if statement
    # if max one couple, we can just do this
    # but num_possible_matches is not recovering the right number right now
    
    matches = matches %>%
      select(-sex.x,-sex.y,-agep.x,-agep.y)
    
    if(nrow(matches) == 0) return(curr_hh)
    
    top_match = data.frame()
    
    while(nrow(matches) > 0){
      top_match = bind_rows(matches[1,1:2] %>% set_names('sporder','sploc_test'),
                            matches[1,2:1] %>% set_names('sporder','sploc_test'),
                            top_match)
      matches = matches %>%
        filter(!sporder.x %in% top_match$sporder) %>%
        filter(!sporder.y %in% top_match$sporder)
    }
  }
  
  curr_hh = curr_hh %>%
    select(-sploc_test) %>%
    merge(.,top_match,by='sporder',all.x=T) 
  
  if(nrow_start != nrow(curr_hh)){
    print(paste0('Error in matching in household ',curr_hh_id))
  }
  
  return(curr_hh)
}

links_one_to_one = function(curr_r, df){
  
  
  # identify relevant households
  # must have at least two people with relation curr_r who are married
  # and who have sploc NA
  df = df %>%
    mutate(rel = ifelse((relshipp == curr_r & mar == 1 & is.na(sploc_test)), 1, 0)) %>%
    group_by(serialno) %>%
    mutate(num_rel = sum(rel,na.rm=T)) 
  
  # add IDs to track rows
  df = df %>% ungroup() %>% mutate(id = row_number())
  
  # keep only relevant rows in households where there are at least two such people
  df_rel = df %>% filter(num_rel >= 2 & rel == 1) 
  
  df_save = df %>% filter(!id %in% df_rel$id)
  
  # case 1 = easy case: only two people in household with relshipp == curr_r
  # can just map to each other
  
  df_case_1 = df_rel %>%
    filter(num_rel == 2) %>%
    group_by(serialno) %>%
    mutate(order = row_number()) %>%
    mutate(sploc_test = ifelse(order == 1, sporder[order == 2], sporder[order == 1])) %>%
    mutate(sploc_clarity = ifelse(!is.na(sploc_test) & is.na(sploc_clarity),1,sploc_clarity))
  
  df_rel = df_rel %>% filter(!id %in% df_case_1$id)
  if(nrow(df_rel) > 0){
    # case 3 or 4 = find opposite sex with closest age or order
    households = unique(df_rel$serialno)
    df_rel = map_dfr(households,find_best_match_in_hh,df_rel)
    df_rel = df_rel %>%
      mutate(sploc_clarity = ifelse(!is.na(sploc_test) & is.na(sploc_clarity),2,sploc_clarity))
  }
  df = bind_rows(df_save,df_case_1,df_rel) %>%
    select(-rel,-num_rel,-id,-order)
  
  return(df)
  
}


#r_root = c(33)
#r_dest = c(33,30,25,26,27,28)
links_one_to_many = function(r_root, r_dest, df){
  
  df = df %>%
    mutate(in_root_cat = ifelse(relshipp %in% r_root & is.na(sploc_test) & mar == 1,1,0),
           in_dest_cat = ifelse(relshipp %in% r_dest & is.na(sploc_test) & mar == 1,1,0)) %>%
    group_by(serialno) %>%
    mutate(num_in_root = sum(in_root_cat,na.rm=T),
           num_in_dest = sum(in_dest_cat,na.rm=T)) %>%
    ungroup() %>%
    mutate(id = row_number())
  
  # keep only relevant rows in households where there is at leat one person in each category
  df_rel = df %>% 
    filter(num_in_root >= 1 & num_in_dest >= 1 & (in_root_cat == 1 | in_dest_cat == 1)) %>%
    mutate(in_root_or_dest = min(1,in_root_cat + in_dest_cat)) %>%
    group_by(serialno) %>%
    mutate(num_in_root_or_dest = sum(in_root_or_dest,na.rm=T)) %>%
    ungroup()
  
  
  df_save = df %>% filter(!id %in% df_rel$id)
  
  # case 1 = easy case: only one person in root category and one person in dest category
  
  df_case_1 = df_rel %>%
    filter(num_in_root_or_dest == 2) %>%
    filter(num_in_root >= 1 & num_in_dest >= 1) %>%
    group_by(serialno) %>%
    mutate(order = row_number()) %>%
    mutate(sploc_test = ifelse(order == 1, sporder[order == 2], sporder[order == 1])) %>%
    ungroup() %>%
    mutate(sploc_clarity = ifelse(!is.na(sploc_test) & is.na(sploc_clarity),1,sploc_clarity))
  
  df_rel = df_rel %>% filter(!id %in% df_case_1$id)
  
  if(nrow(df_rel) > 0){
    households = unique(df_rel$serialno)
    
    df_rel = map_dfr(households,find_best_match_in_hh,df_rel,one_to_many = T)
    df_rel = df_rel %>% 
      mutate(sploc_clarity = ifelse(!is.na(sploc_test) & is.na(sploc_clarity),2,sploc_clarity))
    
  }
  df = bind_rows(df_save,df_case_1,df_rel) %>%
    select(-in_root_cat,-in_root_or_dest,-num_in_root_or_dest,
           -in_dest_cat,-num_in_root,-num_in_dest,-id,-order)
  
  return(df)
  
}

direct_links = function(df){
  
  df = df %>%
    ungroup() %>%
    mutate(spouse_to_head = ifelse(relshipp %in% c(21,22,23,24),1,0)) %>%
    group_by(serialno) %>%
    mutate(num_spouses_to_head = sum(spouse_to_head,na.rm=T)) %>%
    ungroup() %>%
    # check to see if there are households with multiple spouses to head
    # should hopefully always be 0
    mutate(serialno = as.character(serialno)) %>%
    group_by(serialno) %>%
    mutate(sploc_test = ifelse(spouse_to_head == 1,sporder[relshipp == 20],sploc_test)) %>%
    group_by(serialno) %>%
    mutate(sploc_test = ifelse(relshipp == 20 & num_spouses_to_head == 1,
                               sporder[spouse_to_head == 1],sploc_test)) 
  
  #if(mult_spouses > 0){
  #  print(paste0('Households with multiple spouses to head: ',mult_spouses))
  #}
  
  df = df %>%
    ungroup() %>%
    select(-spouse_to_head,-num_spouses_to_head)
  
  df = df %>%
    mutate(sploc_clarity = ifelse(!is.na(sploc_test),1,sploc_clarity))
  
  return(df)
  
  
}

get_step = function(pre_df,post_df,curr_step){
  
  sploc_before = pre_df %>% 
    filter(!is.na(sploc_test) | sploc_test != 0) %>%
    pull(serialno_sporder)
  
  sploc_after = post_df %>%
    filter(!is.na(sploc_test) | sploc_test != 0) %>%
    pull(serialno_sporder)
  
  matched_in_step = sploc_after[!sploc_after %in% sploc_before]
  
  post_df = post_df %>%
    mutate(sploc_step = ifelse(serialno_sporder %in% matched_in_step,
                               curr_step,sploc_step))
  
  return(post_df)
  
}

get_sploc = function(df){
  
  curr_year = unique(df$year)
  
  df = df %>%
    mutate(sploc_test = NA_real_,
           sploc_step = NA_real_,
           sploc_clarity = NA_real_) %>%
    mutate(serialno_sporder = paste0(serialno,'-',sporder))
  
  # first dealing with cases when the relationship to head is spouse or unmarried partner
  pre = df
  df = direct_links(df)
  df = get_step(pre,df,'step_1')
  
  # 29 = father or mother
  pre = df
  df = links_one_to_one(29,df) 
  df = get_step(pre,df,'step_2')
  
  # 25 = biological son or daughter, 26 = adopted son or daughter, 27 = stepchild, 32 = child-in-law
  pre = df
  df = links_one_to_many(c(25,26,27),c(32),df) 
  df = get_step(pre,df,'step_3')
  
  if(as.numeric(curr_year) == 2000){
    
    # Sibling to Sibling-in-law
    pre = df
    df = links_one_to_many(c(28),c(51),df) 
    df = get_step(pre,df,'step_3a')
    
    
    # Aunt/Uncle to Aunt/Uncle
    pre = df
    df = links_one_to_one(54,df) 
    df = get_step(pre,df,'step_3b')
    
  }
  
  
  # 31 = parent-in-law
  pre = df
  df = links_one_to_one(31,df) 
  df = get_step(pre,df,'step_4')
  
  # 34 = roommate or housemate
  pre = df
  df = links_one_to_one(34,df) 
  df = get_step(pre,df,'step_5')
  
  # 36 = other nonrelative
  pre = df
  df = links_one_to_one(36,df) 
  df = get_step(pre,df,'step_6')
  
  if(as.numeric(curr_year) == 2000){
    # Roomer/boarder to Roomer/boarder
    pre = df
    df = links_one_to_one(56,df) 
    df = get_step(pre,df,'step_6a')
  }
  
  if(as.numeric(curr_year) == 2000){
    # Other relative to Grandchild, Child, Sibling, Cousin, Niece/Nephew, Sibling-in-law
    pre = df
    df = links_one_to_many(c(33),c(33,30,25,26,27,28,55,52,51),df) 
    df = get_step(pre,df,'step_7')
    
    # Nonrelative to Roomer, Housemate, Partner/roommate
    pre = df
    df = links_one_to_many(c(36),c(34,56),df) 
    df = get_step(pre,df,'step_8')
    
  }else{
    # 33 = other relative, 30 = grandchild, 28 = brother or sister
    pre = df
    df = links_one_to_many(c(33),c(33,30,25,26,27,28),df) 
    df = get_step(pre,df,'step_7')
    
    # Nonrelative to Roomer, Housemate, Partner/roommate
    pre = df
    df = links_one_to_many(c(36),c(34),df) 
    df = get_step(pre,df,'step_8')
  }
  
  # 25 = biological child (removed)
  #df = links_one_to_one(25,df)
  
  # 30 = grandchild (removed)
  #df = links_one_to_one(30,df) 
  
  # 28 = brother or sister (removed)
  #df = links_one_to_one(28,df) 
  
  if(as.numeric(curr_year) == 2000){
    # Sibling-in-law to Sibling-in-law
    pre = df
    df = links_one_to_one(51,df) 
    df = get_step(pre,df,'step_11a')
    
    # Cousin to Cousin
    pre = df
    df = links_one_to_one(55,df) 
    df = get_step(pre,df,'step_11b')
    
    # Niece/nephew to Niece/nephew
    pre = df
    df = links_one_to_one(52,df) 
    df = get_step(pre,df,'step_11c')
    
    # Other relative to Grandparent, Aunt/Uncle, Parent, Householder
    pre = df
    df = links_one_to_many(c(33),c(53,54,29,20),df) 
    df = get_step(pre,df,'step_11c')
    
  }else{
    
    pre = df
    df = links_one_to_many(c(33),c(29,20),df) 
    df = get_step(pre,df,'step_12')
    
  }
  
  pre = df
  df = links_one_to_many(c(20),c(33,36),df)
  df = get_step(pre,df,'step_13')
  
  df = df %>%
    mutate(sploc_test = ifelse(is.na(sploc_test),0,sploc_test),
           sploc_step = ifelse(is.na(sploc_step),'no match',sploc_step),
           sploc_clarity = ifelse(is.na(sploc_clarity),'no match',sploc_clarity))
  
  return(df)
  
}

# edits:
# remove links between child-child, grandchild-grandchild, sibling-sibling
# ==============================================================================
# 2. MOMLOC / POPLOC
# ==============================================================================
child_to_parent_one_to_one = function(curr_df,parent_code,child_code,age_cap=F){
  
  age_diff_min_mom = ifelse(age_cap,15,0)
  age_diff_max_mom = ifelse(age_cap,44,200)
  
  age_diff_min_pop = ifelse(age_cap,15,0)
  age_diff_max_pop = ifelse(age_cap,60,200)
  
  curr_df = curr_df %>%
    mutate(is_parent = ifelse(relshipp %in% parent_code,1,0)) %>%
    group_by(serialno) %>%
    mutate(num_parents = sum(is_parent),
           
           first_parent_loc = ifelse(num_parents %in% c(1,2),
                                     min(sporder[relshipp %in% parent_code]),0),
           first_parent_sex = ifelse(first_parent_loc != 0, sex[sporder == first_parent_loc], 0),
           first_parent_age = ifelse(first_parent_loc != 0, agep[sporder == first_parent_loc], 0),
           
           second_parent_loc = ifelse(num_parents == 2,max(sporder[relshipp %in% parent_code]),0),
           second_parent_sex = ifelse(second_parent_loc != 0, sex[sporder == second_parent_loc], 0),
           second_parent_age = ifelse(second_parent_loc != 0, agep[sporder == second_parent_loc], 0),
           
           first_parent_sp_loc = ifelse(first_parent_loc != 0,sploc[sporder == first_parent_loc],0),
           first_parent_sp_sex = ifelse(first_parent_sp_loc != 0, sex[sporder == first_parent_sp_loc],0),
           first_parent_sp_age = ifelse(first_parent_sp_loc != 0, agep[sporder == first_parent_sp_loc],0))
  
  curr_df = curr_df %>% ungroup()
  one_parent = curr_df %>% filter(num_parents == 1)
  two_parents = curr_df %>% filter(num_parents == 2)
  other = curr_df %>% filter(!num_parents %in% c(1,2))
  
  one_parent = one_parent %>%
    mutate(momloc_test = ifelse(poploc_test == 0 & momloc_test == 0 & relshipp %in% child_code & first_parent_sex == 2 &
                                  (first_parent_age - agep >= age_diff_min_mom & first_parent_age - agep <= age_diff_max_mom),
                                first_parent_loc,momloc_test),
           poploc_test = ifelse(poploc_test == 0 & momloc_test != 0 & relshipp %in% child_code & first_parent_sp_sex == 1 &
                                  (first_parent_sp_age - agep >= age_diff_min_pop & first_parent_sp_age - agep <= age_diff_max_pop),
                                first_parent_sp_loc,poploc_test),
           poploc_test = ifelse(poploc_test == 0 & momloc_test == 0 & relshipp %in% child_code & first_parent_sex == 1 &
                                  (first_parent_age - agep >= age_diff_min_pop & first_parent_age - agep <= age_diff_max_pop),
                                first_parent_loc,poploc_test),
           momloc_test = ifelse(poploc_test != 0 & momloc_test == 0 & relshipp %in% child_code & first_parent_sp_sex == 2 &
                                  (first_parent_sp_age - agep >= age_diff_min_mom & first_parent_sp_age - agep <= age_diff_max_mom),
                                first_parent_sp_loc,momloc_test),
           momloc2_test = ifelse(momloc_test != 0 & poploc_test == 0 & momloc2_test == 0 & relshipp %in% child_code & first_parent_sp_sex == 2 &
                                   (first_parent_sp_age - agep >= age_diff_min_mom & first_parent_sp_age - agep <= age_diff_max_mom),
                                 first_parent_sp_loc,momloc2_test),
           poploc2_test = ifelse(momloc_test == 0 & poploc_test != 0 & poploc2_test == 0 & relshipp %in% child_code & first_parent_sp_sex == 1 &
                                   (first_parent_sp_age - agep >= age_diff_min_pop & first_parent_sp_age - agep <= age_diff_max_pop),
                                 first_parent_sp_loc,poploc2_test)) %>%
    
    ungroup()
  
  two_parents = two_parents %>%
    mutate(momloc_test = ifelse(poploc_test == 0 & momloc_test == 0 & relshipp %in% child_code & first_parent_sex == 2 &
                                  (first_parent_age - agep >= age_diff_min_mom & first_parent_age - agep <= age_diff_max_mom),
                                first_parent_loc,momloc_test),
           poploc_test = ifelse(poploc_test == 0 & momloc_test == 0 & relshipp %in% child_code & first_parent_sex == 1 &
                                  (first_parent_age - agep >= age_diff_min_pop & first_parent_age - agep <= age_diff_max_pop),
                                first_parent_loc,poploc_test),
           
           poploc_test = ifelse(poploc_test == 0 & momloc_test != 0 & relshipp %in% child_code & second_parent_sex == 1 &
                                  (second_parent_age - agep >= age_diff_min_pop & second_parent_age - agep <= age_diff_max_pop),
                                second_parent_loc,poploc_test),
           momloc_test = ifelse(poploc_test != 0 & momloc_test == 0 & relshipp %in% child_code & second_parent_sex == 2 &
                                  (second_parent_age - agep >= age_diff_min_mom & second_parent_age - agep <= age_diff_max_mom),
                                second_parent_loc,momloc_test),
           
           momloc2_test = ifelse(momloc_test != 0 & poploc_test == 0 & momloc2_test == 0 & relshipp %in% child_code & second_parent_sex == 2 &
                                   (second_parent_age - agep >= age_diff_min_mom & second_parent_age - agep <= age_diff_max_mom),
                                 second_parent_loc,momloc2_test),
           poploc2_test = ifelse(momloc_test == 0 & poploc_test != 0 & poploc2_test == 0 & relshipp %in% child_code & second_parent_sex == 1 &
                                   (second_parent_age - agep >= age_diff_min_pop & second_parent_age - agep <= age_diff_max_pop),
                                 second_parent_loc,poploc2_test)) %>%
    
    ungroup()
  
  
  curr_df = bind_rows(other,one_parent,two_parents)
  
  curr_df = curr_df %>%
    select(-first_parent_loc,-first_parent_sex,-first_parent_sp_loc,-first_parent_sp_sex)
  
  
  return(curr_df)
  
}


child_to_parent_many = function(curr_df,parent_code,child_code,age_cap=200) {
  
  if(age_cap == 200){
    mar_stats = unique(curr_df$mar)
  }else{
    mar_stats = c(5)
  }
  
  save = curr_df %>%
    ungroup() %>%
    arrange(serialno,sporder) %>%
    mutate(row_id = row_number()) %>%
    mutate(in_couple = ifelse(relshipp %in% parent_code & sploc != 0,1,0),
           rel_child = ifelse(relshipp %in% child_code & momloc_test == 0 & poploc_test == 0 
                              & agep <= age_cap & mar %in% mar_stats,1,0)) %>%
    group_by(serialno) %>%
    mutate(num_couples = sum(in_couple),
           num_children = sum(rel_child)) %>%
    ungroup()
  
  children = save %>%
    filter(rel_child == 1) %>%
    arrange(serialno,desc(agep),desc(sex)) %>%
    group_by(serialno) %>%
    mutate(age_rank = ceiling(row_number() * (num_couples/num_children))) %>%
    ungroup()
  
  adults = save %>%
    filter(relshipp %in% parent_code) %>%
    filter(agep >= 15) %>%
    filter(serialno %in% children$serialno) %>%
    mutate(has_couple = ifelse(sploc != 0,1,0),
           ever_married = ifelse(mar <= 4,1,0)) %>%
    select(serialno,relshipp,sporder,agep,sex,has_couple,ever_married,sploc,sploc_sex,sploc_agep)
  
  couples = adults %>% 
    filter(has_couple == 1) %>%
    arrange(serialno,desc(agep),desc(sex)) %>%
    group_by(serialno) %>%
    mutate(age_rank = row_number())
  
  adults = bind_rows(adults %>% filter(has_couple == 0),
                     couples)
  
  children = merge(children %>% select(row_id,serialno,sporder,agep,age_rank,momloc,poploc,parent_loc_clarity),
                   adults,by='serialno',suffixes=c('_child','_parent')) %>%
    mutate(age_diff = agep_parent - agep_child) %>%
    filter((sex == 2 & age_diff >= 15 & age_diff <= 44) |
             (sex == 1 & age_diff >= 15 & age_diff <= 60)) %>%
    filter(sporder_child != sporder_parent) %>%
    group_by(serialno,sporder_child) %>%
    slice_max(order_by=has_couple,n=1,with_ties=T) %>%
    mutate(count = n()) %>%
    ungroup()
  
  done = children %>%
    filter(count == 1) 
  
  if(nrow(done) > 0){
    done = done %>%
      mutate(parent_loc_clarity = ifelse(parent_loc_clarity == 0,1,parent_loc_clarity))
  }
  
  mult_couples = children %>%
    filter(count > 1 & has_couple == 1)
  
  mult_couples = mult_couples %>%
    mutate(age_rank_diff = abs(age_rank_child - age_rank_parent)) %>%
    arrange(desc(sex),sporder_parent) %>%
    group_by(serialno,sporder_child) %>%
    slice_min(n=1,order_by=age_rank_diff,with_ties=F) 
  
  if(nrow(mult_couples) > 0){
    mult_couples = mult_couples %>%
      mutate(parent_loc_clarity = ifelse(parent_loc_clarity == 0,2,parent_loc_clarity))
  }
  
  mult_single = children %>%
    filter(count > 1 & has_couple == 0) %>%
    mutate(ever_married_f = ifelse(sex == 2 & ever_married == 1,1,0),
           ever_married_f_age = ifelse(ever_married_f == 1,agep_parent,0),
           ever_married_m = ifelse(sex == 1 & ever_married == 1,1,0),
           ever_married_f_age = ifelse(ever_married_m == 1,agep_parent,0)) %>%
    arrange(desc(ever_married_f),desc(ever_married_f_age),desc(ever_married_m),desc(ever_married_f_age),desc(sex),desc(agep_parent)) %>%
    group_by(serialno,sporder_child) %>%
    slice(1)
  
  if(nrow(mult_single) > 0){
    mult_single = mult_single %>%
      mutate(parent_loc_clarity = ifelse(parent_loc_clarity == 0,3,parent_loc_clarity))
  }
  
  final = bind_rows(done,mult_couples,mult_single) %>%
    mutate(momloc_test = ifelse(sex == 2,sporder_parent,0),
           poploc_test = ifelse(sex == 1,sporder_parent,0),
           momloc_test = ifelse(sex == 1 & !is.na(sploc_sex) & sploc_sex == 2,sploc,momloc_test),
           poploc_test = ifelse(sex == 2 & !is.na(sploc_sex) & sploc_sex == 1,sploc,poploc_test),
           momloc2_test = ifelse(sex == 2 & !is.na(sploc_sex) & sploc_sex == 2,sploc,0),
           poploc2_test = ifelse(sex == 1 & !is.na(sploc_sex) & sploc_sex == 1,sploc,0)) 
  
  curr_kids = save %>% filter(row_id %in% final$row_id)
  
  curr_kids = curr_kids %>% 
    select(-momloc_test,-poploc_test,-momloc2_test,-poploc2_test,-parent_loc_clarity) %>%
    merge(.,final %>% select(row_id,momloc_test,poploc_test,momloc2_test,poploc2_test,parent_loc_clarity),
          by='row_id')
  
  save = bind_rows(save %>% filter(!(row_id %in% final$row_id)),
                   curr_kids)
  
  return(save)
  
}

direct_links = function(df){
  
  df = df %>%
    group_by(serialno) %>%
    mutate(refsex = sex[sporder==1],
           refspouse_loc = sploc_test[sporder==1],
           refspouse_sex = ifelse(refspouse_loc != 0,sex[sporder==refspouse_loc],0),
           # setting reference people as parents
           momloc_test = ifelse(relshipp %in% c(25,26,27) & refsex == 2, 1, momloc_test),
           poploc_test = ifelse(relshipp %in% c(25,26,27) & refsex == 1, 1, poploc_test),
           # setting spouse of reference people as parents
           momloc_test = ifelse(relshipp %in% c(25,26,27) & refsex != 2 & refspouse_sex == 2, refspouse_loc, momloc_test),
           poploc_test = ifelse(relshipp %in% c(25,26,27) & refsex != 1 & refspouse_sex == 1, refspouse_loc, poploc_test),
           momloc2_test = ifelse(relshipp %in% c(25,26,27) & refsex == 2 & refspouse_sex == 2, refspouse_loc, momloc2_test),
           poploc2_test = ifelse(relshipp %in% c(25,26,27) & refsex == 1 & refspouse_sex == 1, refspouse_loc, poploc2_test)) %>%
    ungroup() %>%
    select(-refsex,-refspouse_loc,-refspouse_sex)
  
  return(df)
  
}

get_step = function(pre_df,post_df,curr_step){
  
  parent_loc_before = pre_df %>% 
    filter(momloc_test != 0 | poploc_test != 0) %>%
    pull(serialno_sporder)
  
  parent_loc_after = post_df %>%
    filter(momloc_test != 0 | poploc_test != 0) %>%
    pull(serialno_sporder)
  
  matched_in_step = parent_loc_after[!parent_loc_after %in% parent_loc_before]
  
  post_df = post_df %>%
    mutate(parent_loc_step = ifelse(serialno_sporder %in% matched_in_step,
                                    curr_step,parent_loc_step))
  
  return(post_df)
  
}

get_parent_loc = function(df){
  
  curr_year = unique(df$year)
  
  df = df %>% 
    mutate(momloc_test = 0,
           poploc_test = 0,
           momloc2_test = 0,
           poploc2_test = 0,
           parent_loc_step = 0,
           parent_loc_clarity = 0) %>%
    mutate(serialno_sporder = paste0(serialno,'-',sporder))
  
  pre = df
  df = direct_links(df)
  df = get_step(pre,df,'step_1')
  
  # reference person to parent
  pre = df 
  df = child_to_parent_one_to_one(df,c(29),c(20))
  df = get_step(pre,df,'step_2')
  
  # spouse/partner of the reference person (21, 22, 23, 24) and parent in law of the reference person (31)
  # maybe we modify to allow for other codes for spouse/partner of reference person? 
  pre = df
  df = child_to_parent_one_to_one(df,c(31),c(21, 22, 23, 24))
  df = get_step(pre,df,'step_3')
  
  # between sibling (28) and parent of reference person (29)
  pre = df
  df = child_to_parent_one_to_one(df,c(29),c(28))
  df = get_step(pre,df,'step_4')
  
  df = df %>%
    mutate(parent_loc_clarity = ifelse(momloc_test != 0 | poploc_test != 0,1,parent_loc_clarity))
  
  couple_ids = df %>%
    filter(sporder < sploc) %>%
    arrange(sporder) %>%
    group_by(serialno) %>%
    mutate(couple_id = row_number()) %>%
    select(serialno,sporder,sploc,couple_id) %>%
    gather(type,sporder,sporder,sploc) %>%
    select(-type) %>%
    ungroup() %>%
    mutate(couple_id = str_pad(couple_id, width = 3, side = "left", pad = "0"))
  
  df = merge(df,couple_ids,by=c('serialno','sporder'),all.x=T)
  
  sploc_sex_age = df %>%
    select(serialno,sporder,sex,agep) %>%
    rename(sploc = sporder,
           sploc_sex = sex,
           sploc_agep = agep)
  
  
  df = merge(df,sploc_sex_age,by=c('serialno','sploc'),all.x=T)
  
  if(as.numeric(curr_year) == 2000){
    # Cousin to Aunt/Uncle
    pre = df
    df = child_to_parent_many(df,parent_code=c(54),child_code=c(55))
    df = get_step(pre,df,'step_5a')
    
    # Parent to Grandparent
    pre = df
    df = child_to_parent_many(df,parent_code=c(53),child_code=c(29))
    df = get_step(pre,df,'step_5b')
    
    
    # Sibling-in-law to Parent-in-law
    pre = df
    df = child_to_parent_many(df,parent_code=c(31),child_code=c(51))
    df = get_step(pre,df,'step_5c')
    
    # Niece/nephew to sibling or sibling in law
    pre = df
    df = child_to_parent_many(df,parent_code=c(28,51),child_code=c(52))
    df = get_step(pre,df,'step_5c')
    
  }
  
  # 25 = biological son or daughter
  # 26 = adopted son or daughter
  # 27 = stepchild
  # 30 = grandchild
  pre = df
  df = child_to_parent_many(df,parent_code=c(25,26,27),child_code=c(30))
  df = get_step(pre,df,'step_5')
  
  # 32 = son-in-law or daughter-in-law
  # third- through fifth-level links only occur when the "child" half of the link is under 22 and is single.
  pre = df
  df = child_to_parent_many(df,parent_code=c(32),child_code=c(30),age_cap=22)
  df = get_step(pre,df,'step_6')
  
  # 33 = other relative
  # 28 = brother or sister
  # 30 = grand child
  # Other relative to Other Relative, Niece/Nephew, Sibling , Sibling-in-law
  if(as.numeric(curr_year) == 2000){
    pre = df
    df = child_to_parent_many(df,parent_code=c(28,33,51,52),
                              child_code=c(33),age_cap=22)
    df = get_step(pre,df,'step_7')
    
    # Roomer/Boarder to Roomer/Boarder
    pre = df
    df = child_to_parent_many(df,parent_code=c(56),
                              child_code=c(56),age_cap=22)
    df = get_step(pre,df,'step_7a')
    
  }else{
    pre = df
    df = child_to_parent_many(df,parent_code=c(28,33),child_code=c(33),age_cap=22)
    df = get_step(pre,df,'step_7')
  }
  
  # Housemate to Housemate
  pre = df
  df = child_to_parent_many(df,parent_code=c(34),child_code=c(34),age_cap=22)
  df = get_step(pre,df,'step_8')
  
  # Nonrelative to Partner, Nonrelative, Partner/roommate
  pre = df
  df = child_to_parent_many(df,parent_code=c(36,22),child_code=c(36),age_cap=22)
  df = get_step(pre,df,'step_9')
  
  return(df)
  
  
}

# edits
# remove parent in law rule for partner of head of household

# ==============================================================================
# 3. SUBFAMILY IDS
# ==============================================================================
get_famunit = function(df){
  
  df = df %>% ungroup()  
  
  #22 and 24 not actually in famunit unless joined by SPLOC
  # 51, 52, 53, 54, 55 from 2000 decennial in famunit as well
  
  df = df %>%
    mutate(rel_to_head = ifelse((relshipp <= 33 & !relshipp %in% c(22,24)) | 
                                  relshipp %in% 51:55,1,0)) %>%
    mutate(any_rel_to_head = ifelse(rel_to_head == 1 | 
                                      sploc_test == 1 |
                                      momloc_test == 1 |
                                      poploc_test == 1 |
                                      momloc2_test == 1 |
                                      poploc2_test == 1, 1,0)) %>%
    group_by(serialno) %>%
    mutate(num_people = n(),
           num_any_rel_to_head = sum(any_rel_to_head)) %>%
    ungroup()
  
  done = df %>%
    filter((num_people == num_any_rel_to_head) | housing_unit_type %in% c(2,3)) %>%
    mutate(famunit_test = 1)
  
  df = df %>%
    filter(!(serialno %in% done$serialno))
  
  famunits = df %>%
    select(serialno,sporder, rel_to_head,sploc_test, 
           momloc_test, poploc_test,
           momloc2_test, poploc2_test) %>%
    pivot_longer(cols = c(-serialno,-sporder), values_to = "linked_id") %>%
    mutate(linked_id = ifelse(linked_id == 0, sporder, linked_id)) %>%
    distinct(serialno, sporder, linked_id) %>%
    group_by(serialno) %>%
    summarise(family_map = list({
      g <- graph_from_data_frame(select(cur_data(), sporder, linked_id), 
                                 directed = FALSE)
      comps <- components(g)$membership
      tibble(sporder = as.integer(names(comps)),
             famunit_test = comps)
    }), .groups = "drop") %>%
    unnest(family_map)
  
  df = df %>%
    merge(.,famunits %>% select(serialno,sporder,famunit_test), 
          by = c('serialno','sporder'))
  
  df = bind_rows(df,done) %>% ungroup()
  
  return(df)
  
  
}

get_subfam = function(df){
  
  df = df %>%
    group_by(serialno) %>%
    mutate(parent_ids = list(na.omit(unique(c(momloc_test, poploc_test,
                                              momloc2_test, poploc2_test)))),
           is_parent  = as.integer(sporder %in% parent_ids[[1]])) %>%
    ungroup() %>%
    mutate(subfam_test = ifelse((sporder == 1 |
                                   sploc_test == 1 |
                                   (momloc_test == 1 & sploc == 0 & is_parent == 0) |
                                   (poploc_test == 1 & sploc == 0 & is_parent == 0) |
                                   (momloc2_test == 1 & sploc == 0 & is_parent == 0) |
                                   (poploc2_test == 1 & sploc == 0 & is_parent == 0)),0,NA),
           subfam_test = ifelse(housing_unit_type %in% c(2,3),0,subfam_test)) %>%
    group_by(serialno) %>%
    mutate(num_members = n()) %>%
    group_by(serialno, famunit_test) %>%
    mutate(num_in_famunit = n()) %>%
    ungroup() %>%
    mutate(subfam_test = ifelse(num_members == 2,0,subfam_test),
           subfam_test = ifelse(num_in_famunit == 1,0,subfam_test))
  
  # parent of head or spouse of head also gets subfam 0
  df = df %>%
    group_by(serialno) %>%
    mutate(spouse_of_head = sploc[sporder ==1],
           parent_of_head = list(unique(na.omit(c(momloc[sporder ==1],
                                                  poploc[sporder ==1],
                                                  momloc2[sporder ==1],
                                                  poploc2[sporder ==1])))),
           is_parent_of_head = as.integer(sporder %in% parent_of_head[[1]])) %>%
    ungroup() %>%
    mutate(subfam_test = ifelse(is_parent_of_head == 1 & sploc == 0,0,subfam_test)) 
  
  df = df %>%
    mutate(has_subfam = ifelse(!is.na(subfam_test),1,0)) %>%
    group_by(serialno) %>%
    mutate(num_has_subfam = sum(has_subfam)) %>%
    ungroup()
  
  curr = df 
  missing = curr %>% filter(is.na(subfam_test))
  
  subfams = missing %>%
    select(serialno,sporder,agep,is_parent,sploc_test,momloc_test, poploc_test,
           momloc2_test, poploc2_test) %>%
    mutate_at(c('momloc_test', 'poploc_test',
                'momloc2_test', 'poploc2_test'), ~ ifelse(is_parent == 1,0,.)) %>%
    mutate(parent_married = ifelse(momloc_test != 0 & poploc_test != 0,1,0),
           parent_married = ifelse(momloc_test & momloc2_test != 0,1,parent_married),
           parent_married = ifelse(poploc_test & poploc2_test != 0,1,parent_married)) %>%
    mutate_at(c('momloc_test', 'poploc_test',
                'momloc2_test', 'poploc2_test'), ~ ifelse(sploc_test != 0 & parent_married == 1,0,.)) %>%
    mutate_at(c('momloc_test', 'poploc_test',
                'momloc2_test', 'poploc2_test'), ~ ifelse(agep >= 18,0,.)) %>%
    select(-is_parent,-parent_married,-agep) %>%
    pivot_longer(cols = c(-serialno,-sporder), values_to = "linked_id") %>%
    mutate(linked_id = ifelse(linked_id == 0, sporder, linked_id)) %>%
    group_by(serialno) %>%
    mutate(nodes = list(unique(c(sporder)))) %>%
    ungroup() %>%
    mutate(linked_id = ifelse(linked_id %in% nodes[[1]], linked_id, sporder)) %>%
    distinct(serialno, sporder, linked_id) %>%
    group_by(serialno) %>%
    summarise(family_map = list({
      g <- graph_from_data_frame(select(cur_data(), sporder, linked_id), 
                                 directed = FALSE)
      comps <- components(g)$membership
      tibble(sporder = as.integer(names(comps)),
             subfam_test = comps)
    }), .groups = "drop") %>%
    unnest(family_map) %>%
    arrange(sporder) %>%
    group_by(serialno,subfam_test) %>%
    mutate(subfam_test = ifelse(n() == 1,0,subfam_test)) %>%
    ungroup() %>%
    mutate(subfam_test = ifelse(subfam_test == 0,NA,subfam_test)) %>%
    group_by(serialno) %>%
    mutate(subfam_test = frank(subfam_test, ties.method = "dense",na.last='keep'),
           subfam_test = ifelse(is.na(subfam_test),0,subfam_test)) %>%
    ungroup()
  
  missing = merge(missing %>% select(-subfam_test),subfams,by = c('serialno','sporder'))
  
  df = bind_rows(curr %>% filter(!is.na(subfam_test)),
                 missing)
  
  
  return(df)
  
}

get_sftype_sfrelate = function(df){
  
  rel_to_head = df %>%
    filter(subfam_test == 0) %>%
    mutate(sftype_test = 0,
           sfrelate_test = 0)
  
  subfams = df %>% filter(subfam_test != 0)
  
  if(nrow(subfams) > 0){
    subfams = subfams %>%
      group_by(serialno,subfam_test) %>%
      mutate(sporder_in_subfam = list(c(sporder))) %>%
      ungroup() %>%
      mutate(momloc_test_temp = momloc_test,
             poploc_test_temp = poploc_test,
             sploc_test_temp = sploc_test) %>%
      # remove parents that arent in the subfamily (three generation families)
      mutate_at(c('momloc_test_temp', 'poploc_test_temp','sploc_test_temp'),
                ~ ifelse(!.x %in% sporder_in_subfam[[1]],0,.x)) %>%
      mutate(is_married = ifelse(sploc_test_temp != 0,1,0),
             has_mother = ifelse(momloc_test_temp != 0,1,0),
             has_father = ifelse(poploc_test_temp != 0,1,0)) %>%
      group_by(serialno,subfam_test) %>%
      mutate(married = max(is_married),
             mother = max(has_mother),
             father = max(has_father)) %>%
      ungroup() %>%
      mutate(sftype_test = case_when(
        married == 1 & (mother == 1 | father == 1) ~ 1,
        married == 1 & mother == 0 & father == 0 ~ 2,
        married == 0 & father == 1 ~ 3,
        married == 0 & mother == 1 ~ 4)) %>%
      mutate(sftype_test = ifelse(famunit_test != 1, sftype_test + 4, sftype_test)) 
    
    subfams = subfams %>%
      arrange(serialno,has_mother,has_father,desc(is_married),sex,sporder) %>%
      group_by(serialno,subfam_test) %>%
      mutate(rank = row_number()) %>%
      ungroup() %>%
      mutate(sfrelate_test = case_when(
        rank == 1 ~ 1,
        is_married == 1 ~ 2,
        has_mother == 1 | has_father == 1 ~ 3
      )) 
  }
  
  df = bind_rows(rel_to_head,subfams)
  
  
  return(df)
  
}

get_subfamily_types = function(df){
  
  df = get_famunit(df)
  
  df = get_subfam(df)
  
  df = get_sftype_sfrelate(df)
  
  return(df)
  
}
# ==============================================================================
# 4. WORKFLOW FUNCTIONS
# ==============================================================================
edited = TRUE

write_error_message = function(nrow_start,nrow_end,yr,step='All'){
  
  if(nrow_end != nrow_start){
    message = paste0("Rows don't match for year: ",yr," and step: ",step,"\n")
    message = paste0(message,"Rows start: ",nrow_start," Rows end: ",nrow_end,"\n")
    cat(message, file = "data/derived/code_tests/error_log.txt", append = TRUE)
  }
  
  
}

get_ipums_subfam_vars = function(df,fix_sploc=FALSE,fix_parent_loc=FALSE){
  
  yr = unique(df$year)
  
  if(!edited){
    source('sploc.r')
  }else{
    source('sploc_edited.r')
  }
  
  print('Getting sploc...')
  
  nrow_start = nrow(df)
  df = get_sploc(df)
  nrow_end = nrow(df)
  write_error_message(nrow_start,nrow_end,yr,step='sploc')
  
  if(fix_sploc){
    df = df %>% mutate(sploc_test = sploc)
  }
  
  if(!edited){
    source('momloc_poploc.r')
  }else{
    source('momloc_poploc_edited.r')
  }
  
  print('Getting parent loc...')
  
  nrow_start = nrow(df)
  df = get_parent_loc(df)
  nrow_end = nrow(df)
  write_error_message(nrow_start,nrow_end,yr,step='parent_loc')
  
  if(fix_parent_loc){
    
    df = df %>%
      mutate(sploc_test = sploc,
             momloc_test = momloc,
             poploc_test = poploc,
             momloc2_test = momloc2,
             poploc2_test = poploc2)
    
  }
  
  source('subfamily_ids.r')
  print('Getting subfamily ids...')
  
  nrow_start = nrow(df)
  df = get_subfamily_types(df)
  nrow_end = nrow(df)
  write_error_message(nrow_start,nrow_end,yr,step='parent_loc')
  
  df = df %>%
    select(serialno,sporder,year,statefip,housing_unit_type,agep,mar,relshipp,sex,
           sploc,sploc_test,sploc_step,sploc_clarity,
           poploc,poploc_test,momloc,momloc_test,poploc2,poploc2_test,momloc2,momloc2_test,parent_loc_step,parent_loc_clarity,
           famunit,famunit_test,subfam,subfam_test,sftype,sftype_test,sfrelate,sfrelate_test)
  
  return(df)
  
  
}

years = c('2000')
for(yr in years){
  
  print(yr)
  
  filename =  paste0('data/intermediate/clean_acs/acs_',yr,'.csv')
  df = fread(filename) %>% mutate(serialno = as.character(serialno))
  
  tic.clear()
  tic.clearlog()
  tic()
  fips = unique(df$statefip)
  cl <- makeCluster(6)
  registerDoParallel(cl)
  
  nrow_start = nrow(df)
  
  df = foreach(fip = fips, .combine = rbind) %dopar% {
    library(tidyverse)
    #message = paste0("Starting: ",as.character(fip),"...\n")
    #cat(message, file = "data/derived/code_tests/error_log.txt", append = TRUE)
    df_fip = df %>% filter(statefip == fip)
    df_fip = get_ipums_subfam_vars(df_fip)
    #message = paste0("Success: ",as.character(fip),"...\n")
    #cat(message, file = "data/derived/code_tests/error_log.txt", append = TRUE)
    return(df_fip)
  }
  
  toc(log=T)
  
  nrow_end = nrow(df)
  write_error_message(nrow_start,nrow_end,yr,step='total')
  
  if(!edited){
    outfile = paste0('data/derived/acs_with_ipums_codes/acs_',yr,'.csv')
  }else{
    outfile = paste0('data/derived/acs_with_ipums_codes_edited/acs_',yr,'.csv')
  }
  fwrite(df,outfile) 
  
  elapsed = tic.log(format=F)[[1]]$toc - tic.log(format=F)[[1]]$tic
  time_row = data.frame('year'=c(yr),'state'=c('all'),
                        'time'=c(elapsed),type=c('us'))
  write.table(time_row,'data/derived/code_tests/process_times.csv',
              sep=",",col.names=F,row.names=F,append=T)
  
  
}

# postprocess:
yrs = as.character(2000:2023)

for(yr in yrs){
  file = paste0('data/derived/acs_with_ipums_codes_edited/acs_',yr,'.csv')
  df = fread(file)
  df = df %>%
    select(-sploc,-poploc,-momloc,-poploc2,-momloc2,
           -famunit,-subfam,-sftype,-sfrelate) %>%
    rename_with(~str_replace(.,'_test',''))
  root = 'data/derived/acs_final/'
  filename = ifelse(yr == 2000,'decennial_2000.csv',paste0('acs_',yr,'.csv'))
  outfile = paste0(root,filename)
  fwrite(df,outfile)
}

# ==============================================================================
# RENEE'S GLOBALS 
# ==============================================================================
# Birth cohorts to analyze
COHORT_RANGE <- c(1983:2000)

# Age filters (keep ages ≤17 OR ≥24, exclude 18-23)
AGE_FILTER_LOWER <- 17  # Children: age ≤ this
AGE_FILTER_UPPER <- 24  # Adults: age ≥ this

# Income interval for ranking (dollars)
INTERVAL <- 100  # Percentile ranks computed at $100 increments

# Income definitions to create
INCOME_DEFINITIONS <- tibble(
  name = c("xearn", "xemp", "xinc", "xcominc"),
  formula = c(
    "INCWAGE",
    "INCWAGE + INCBUS00",
    "INCTOT",
    "INCWAGE + INCBUS00 + INCSS + INCRETIR + INCINVST"
  )
)

# ==============================================================================
# RENEE'S HELPER FUNCTIONS
# ==============================================================================
# ==============================================================================
# 1. DATA LOADING
# ==============================================================================

#' Load IPUMS microdata from XML DDI file
#' 
#' @param filepath Path to IPUMS .xml DDI file (e.g., "usa_2010.xml")
#' @return data.table with IPUMS microdata
#' 
#' What it does:
#' - Reads the XML metadata file (.xml)
#' - Automatically finds the corresponding data file (.dat)
#' - Loads all variables and converts to data.table for fast processing
load_ipums_data <- function(filepath) {
  ddi <- read_ipums_ddi(filepath)
  dat_filepath <- sub("\\.xml$", ".dat", filepath)
  data <- read_ipums_micro(ddi, data_file = dat_filepath)
  setDT(data)
  return(data)
}

#' Load IPUMS microdata from .csv file
#' 
#' @param filepath Path to IPUMS .csv file (e.g., "acs_2005.csv")
#' @return data.table with IPUMS microdata
#' 
#' What it does:
#' - Reads the file
#' - Loads all variables and converts to data.table for fast processing
load_data_csv <- function(filepath) {
  data <- read_csv(filepath)
  setDT(data)
  return(data)
}

# ==============================================================================
# 2. DATA PREPARATION
# ==============================================================================

#' Code missing values in income variables
#' 
#' @param dt data.table with IPUMS data
#' @return Same data.table with missing codes replaced by NA
#' 
#' What it does:
#' - IPUMS uses special codes for missing (999999, 999998, etc.)
#' - This converts those codes to proper NA values
#' - Affects: INCWAGE, INCBUS00, INCINVST, INCRETIR, INCSS, INCTOT, etc.
code_missing_income <- function(dt) {
  dt[INCWAGE %in% c(999999, 999998), INCWAGE := NA]
  dt[INCBUS00 == 999999, INCBUS00 := NA]
  dt[INCINVST == 999999, INCINVST := NA]
  dt[INCRETIR == 999999, INCRETIR := NA]
  dt[INCSS == 99999, INCSS := NA]
  # dt[INCSUPP == 99999, INCSUPP := NA]
  # dt[INCWELFR == 99999, INCWELFR := NA]
  # dt[INCOTHER %in% c(99999, 99998), INCOTHER := NA]
  dt[INCTOT %in% c(9999999, 9999998), INCTOT := NA]
  return(dt)
}

#' Create a new income variable from a formula
#' 
#' @param dt data.table with IPUMS data
#' @param formula String expression (e.g., "INCWAGE + INCBUS00")
#' @param new_var_name Name for the new column
#' @return Same data.table with new income column added
#' 
#' What it does:
#' - Evaluates the formula to create a new income measure
#' - Example: "xemp" = INCWAGE + INCBUS00 (wage + business income)
create_income_variable <- function(dt, formula, new_var_name) {
  dt[, (new_var_name) := eval(parse(text = formula))]
  return(dt)
}

#' Create all income definitions at once
#' 
#' @param dt data.table with IPUMS data
#' @param definition_table Tibble with columns: name, formula
#' @return Same data.table with all income columns added
#' 
#' What it does:
#' - Loops through your income definitions table
#' - Creates each income measure (xearn, xemp, xinc, xcominc)
create_all_income_definitions <- function(dt, definition_table) {
  for (i in 1:nrow(definition_table)) {
    create_income_variable(
      dt, 
      formula = definition_table$formula[i], 
      new_var_name = definition_table$name[i]
    )
  }
  return(dt)
}

#' Make data long on ego
#' 
#' @param dt data.table with IPUMS data
#' @return data.table with one copy of all HH information per target ego, retaining all columns
#' What it does:
#' - Identifies a "target ego" and makes all information long.
make_data_long_on_ego <- function(dt) {
  
  # Step 1: Identify the target egos and their household information
  ego_data <- dt[is_ego == TRUE, .(CBSERIAL, ego_PERNUM = CBPERNUM, ego_AGE = AGE, ego_BIRTHYR = BIRTHYR)]
  
  # Step 2: Create copies of all household members for each ego
  long_data <- dt[ , .SD, .SDcols = names(dt)]  # This retains all original columns
  
  # Step 3: Merge ego data back into long_data to duplicate rows accordingly
  long_data <- merge(long_data, ego_data, by = c("CBSERIAL"), allow.cartesian = TRUE)
  
  # Mark if ego is an adult or a child based on the age threshold for all rows
  long_data[, is_child := ifelse(ego_AGE <= AGE_FILTER_LOWER, 1, 0)]
  long_data[, is_adult := ifelse(ego_AGE >= AGE_FILTER_UPPER, 1, 0)]
  
  # Return the long formatted data
  return(long_data)
}

#' Prepare data for aggregation
#' 
#' @param dt data.table with raw IPUMS data
#' @param cohort_range Vector of birth years to analyze (e.g., 1983:2000)
#' @param income_table Table with income definitions
#' @param age_filter_lower Keep ages ≤ this (default 17 for children)
#' @param age_filter_upper Keep ages ≥ this (default 24 for adults)
#' @param sample_n Optional: sample N households for testing
#' @return Filtered and prepared data.table
#' 
#' What it does:
#' 1. Optionally samples households (for testing)
#' 2. Keeps only households containing an "ego" (someone in cohort_range)
#' 3. Codes missing income values as NA
#' 4. Creates all income definitions
#' 5. Makes data long on every ego
#' 
#' Note: Ages 18-23 will be filtered out later during ranking
prep_for_aggregation <- function(dt, cohort_range, income_table, 
                                 age_filter_lower = 17, age_filter_upper = 24,
                                 sample_n = NULL) {
  # Optional sampling for testing
  if (!is.null(sample_n)) {
    cat("Sampling", sample_n, "households...\n")
    sampled_hh <- dt[, .(CBSERIAL = unique(CBSERIAL))][sample(.N, min(sample_n, .N))]
    dt <- dt[sampled_hh, on = "CBSERIAL"]
  }
  
  # Keep only households with an ego
  dt[, has_ego := any(BIRTHYR %in% cohort_range), by = CBSERIAL]
  dt <- dt[has_ego == TRUE]
  dt[, has_ego := NULL]
  
  # Identify target egos
  dt[, is_ego := BIRTHYR %in% COHORT_RANGE]
  
  # Clean income variables
  code_missing_income(dt)
  
  # Create all income measures
  create_all_income_definitions(dt, income_table)
  
  # Makes data long on target ego
  dt <- make_data_long_on_ego(dt)

  return(dt)
}

# ==============================================================================
# 3. AGGREGATION HELPERS
# ==============================================================================

#' Identify family members of target egos
#' @param dt data.table with IPUMS data
#' @return data.table with appended column: FAMILY
#' What it does:
#' - identifies which rows should be included for the FAMILY aggregation
#' - all family members (shares FAMUNIT) of target egos get T, all other rows get F
#' - Note that all target egos' famunit should include at least themselves.
#' - calculate regardless of target ego age.
identify_family <- function(dt) {
  dt[, FAMILY := (FAMUNIT == FAMUNIT[CBPERNUM == ego_PERNUM]),
     by = .(CBSERIAL, ego_PERNUM)]
  return(dt)
}

#' Identify target egos in adult years
#' @param dt data.table with IPUMS data
#' @return data.table with appended column: EGO
#' What it does:
#' - identifies which rows should be included for the EGO aggregation
#' - target egos in adult years get T, all other rows get F
#' - since data structure is ego-level, retain only one copy based on ego_PERNUM which identifies the focal ego for that household copy
identify_egos <- function(dt) {
  dt[, EGO := (CBPERNUM == ego_PERNUM & is_adult == TRUE)]
  return(dt)
}

#' Identify target egos' partners in adult years
#' @param dt data.table with IPUMS data
#' @return data.table with appended column: PARTNER
#' What it does:
#' - identifies which rows should be included for the PARTNER aggregation
#' - every row whose value for SPLOC is ego_PERNUM AND is_adult is true gets T, all else is F
identify_partners <- function(dt) {
  dt[, PARTNER := (SPLOC == ego_PERNUM & is_adult == TRUE)]
  return(dt)
}

#' Identify target egos' spouses in adult years
#' @param dt data.table with IPUMS data (assumes PARTNER exists in the data)
#' @return data.table with appended column: SPOUSE
#' What it does:
#' - identifies which rows should be included for the SPOUSE aggregation
#' - every row whose value for PARTNER is T AND MARTST == 1/2 all gets T, all else is F
identify_spouses <- function(dt) {
  dt[, SPOUSE := (PARTNER == TRUE & MARST %in% 1:2)]
  return(dt)
}

#' Identify target egos' guardians in childhood years
#' @param dt data.table with IPUMS data
#' @return data.table with appended column: GUARDIAN
#' What it does:
#' - identifies which rows should be included for the GUARDIANS and GUARDIANS_SUM aggregations
#' - every row who is identified as MOMLOC/MOMLOC2/POPLOC/POPLOC2 for target ego (via ego_PERNUM) gets T, all else gets F
identify_guardians <- function(dt) {
  # make all guardian pointers the same within household, which identifies the guardians
  # of target ego ONLY
  dt[, NMOMLOC := MOMLOC[CBPERNUM == ego_PERNUM],
     by = .(CBSERIAL, ego_PERNUM)]
  
  dt[, NMOMLOC2 := MOMLOC2[CBPERNUM == ego_PERNUM],
     by = .(CBSERIAL, ego_PERNUM)]
  
  dt[, NPOPLOC := POPLOC[CBPERNUM == ego_PERNUM],
     by = .(CBSERIAL, ego_PERNUM)]
  
  dt[, NPOPLOC2 := POPLOC2[CBPERNUM == ego_PERNUM],
     by = .(CBSERIAL, ego_PERNUM)]
  
  # set TRUE if is a guardian of target ego in childhood
  dt[, GUARDIAN := (CBPERNUM == NMOMLOC | CBPERNUM == NMOMLOC2 | CBPERNUM == NPOPLOC | CBPERNUM == NPOPLOC2) & (is_child == TRUE)]
  
  dt[, `:=`(NMOMLOC = NULL, NMOMLOC2 = NULL, NPOPLOC = NULL, NPOPLOC2 = NULL)]
  
  return(dt)
}

#' Identify target egos' married guardians in childhood years
#' @param dt data.table with IPUMS data, assumes GUARDIAN is a column in the data
#' @return data.table with appended column: MARRIED_GUARDIAN
#' What it does:
#' - identifies which rows should be included for the MARRIED_GUARDIANS and MARRIED_GUARDIANS_SUM aggregations
#' - excludes, from GUARDIANS column, any guardian whose partner is an unmarried HOH and where the ego's RELATED value is "child" only.
identify_married_guardians <- function(dt) {
  # Create a reference table for household-target ego variables
  hoh_info <- dt[RELATED == 101, .(hoh_MARST = unique(MARST), hoh_PERNUM = unique(CBPERNUM)), 
                 by = .(CBSERIAL, ego_PERNUM)] # Gather HOH info
  
  # Merge HOH info back to the original table
  dt[hoh_info, on = .(CBSERIAL, ego_PERNUM), 
     `:=`(hoh_MARST = i.hoh_MARST, hoh_PERNUM = i.hoh_PERNUM)]
  
  # Create household-targetego-level variable which takes on the RELATED value for that target ego
  dt[, ego_RELATED := RELATED[CBPERNUM == ego_PERNUM], by = .(CBSERIAL, ego_PERNUM)]
  
  # Use combined logical operation to create MARRIED_GUARDIAN
  dt[, MARRIED_GUARDIAN := (GUARDIAN) & !(SPLOC == hoh_PERNUM & (!hoh_MARST %in% 1:2) & ego_RELATED == 301)]
  
  dt[, `:=`(hoh_PERNUM = NULL, hoh_MARST = NULL, ego_RELATED = NULL, NPOPLOC2 = NULL)]
  
  return(dt)
}

#' Create all aggregation columns
#'
#' @param dt data.table after running prep_for_aggregation function
#' @return data.table with 7 new aggregation columns
#' 
#' What it does:
#' - applies all the helper functions to identify rows included for each aggregation type
create_aggregation_columns <- function(dt) {
  dt[, HOUSEHOLD := TRUE]
  identify_family(dt)
  identify_egos(dt)
  identify_partners(dt)
  identify_spouses(dt)
  identify_guardians(dt)
  identify_married_guardians(dt)
  
  return(dt)
}

#' Multiply income values by aggregation columns
#' @param dt data.table after running create_aggregation_columns function
#' @param income_def name of current income definition
#' @return data.table with 7 new appended aggregation columns
#' What it does:
#' - applies 7 aggregation types across given income column
#' - handles NAs versus 0s for each aggregation type.
create_income_columns <- function(dt, income_def, cols) {
  # Loop through the specified columns to create new columns based on income_def
  for (col in cols) {
    
    # Check if the column exists
    if (!col %in% names(dt)) {
      warning(paste(col, "not found in the dataset. Skipping."))
      next
    }
    # Create the new column name
    new_col_name <- paste0(income_def, "_", col)
    
    # Create the new binary column based on the specified logic
    dt[, (new_col_name) := ifelse(get(col) == TRUE, 
                                  ifelse(is.na(get(income_def)), 0, get(income_def)), 
                                  NA)]
  }
  
  # Return the modified data.table with the new columns
  return(dt)
}

#' Check if spouse < partner < ego
#' 
#' @param dt data.table of ego-level distribution result
#' @return T/F if check is passed.
#' What it does:
#' - Checks if number of valid spouse values <= valid partner values <= valid ego values
#' - And also if partner_sum == spouses_sum == ego
check_relationship_completeness <- function(dt) {
  # Get column names that contain "SPOUSE", "PARTNER", and "EGO"
  spouse_cols <- names(dt)[grepl("SPOUSE$", names(dt))]
  partner_cols <- names(dt)[grepl("PARTNER$", names(dt))]
  ego_cols <- names(dt)[grepl("EGO", names(dt))]
  spouses_cols <- names(dt)[grepl("SPOUSES", names(dt))]
  partners_cols <- names(dt)[grepl("PARTNERS", names(dt))]
  
  # Count non-NA values for SPOUSE columns
  spouse_non_na_count <- sum(sapply(spouse_cols, function(col) sum(!is.na(dt[[col]]))))
  
  # Count non-NA values for PARTNER columns
  partner_non_na_count <- sum(sapply(partner_cols, function(col) sum(!is.na(dt[[col]]))))
  
  # Count non-NA values for EGO columns
  ego_non_na_count <- sum(sapply(ego_cols, function(col) sum(!is.na(dt[[col]]))))
  
  # Count non-NA values for SPOUSES_SUM columns
  spouses_non_na_count <- sum(sapply(spouses_cols, function(col) sum(!is.na(dt[[col]]))))
  
  # Count non-NA values for PARTNERS_SUM columns
  partners_non_na_count <- sum(sapply(partners_cols, function(col) sum(!is.na(dt[[col]]))))
  
  # Check the conditions
  result <- spouse_non_na_count <= partner_non_na_count && partner_non_na_count <= ego_non_na_count && partners_non_na_count == spouses_non_na_count && spouses_non_na_count == ego_non_na_count
  
  print(paste0("Number of valid ego: ", ego_non_na_count))
  print(paste0("Number of valid partner: ", partner_non_na_count))
  print(paste0("Number of valid spouse: ", spouse_non_na_count))
  print(paste0("Number of valid partners: ", partners_non_na_count))
  print(paste0("Number of valid spouses: ", spouses_non_na_count))
  
  return(result)
}

#' Check if married-guardians <= guardians
#' 
#' @param dtg data.table of guardian-level distribution result
#' @param dtm data.table of married-guardian level distribution result
#' @return T/F if check is passed.
#' What it does:
#' - Checks if number of married-guardians <= guardians
check_guardians_count <- function(dtg, dtm) {
  print(paste0("Number of valid guardians: ", nrow(dtg)))
  print(paste0("Number of valid married guardians: ", nrow(dtm)))
  return(nrow(dtm) <= nrow(dtg))
}

#' Check if guardians_sum == guardians_married_sum
#' 
#' @param dt data.table of ego-level distribution result
#' @return T/F if check is passed.
check_guardians_sum_count <- function(dt) {
  gs_cols <- names(dt)[grepl("GUARDIANS_SUM", names(dt))]
  gs_non_na_count <- sum(sapply(gs_cols, function(col) sum(!is.na(dt[[col]]))))
  
  gm_cols <- names(dt)[grepl("GUARDIANS_MARRIED_SUM", names(dt))]
  gm_non_na_count <- sum(sapply(gm_cols, function(col) sum(!is.na(dt[[col]]))))
  
  print(paste0("Number of valid guardians_sum: ", gs_non_na_count))
  print(paste0("Number of valid guardians_married_sum: ", gm_non_na_count))
  
  return(gs_non_na_count == gm_non_na_count)
}

#' Check if household and family columnes are all complete
#' 
#' @param dt data.table of ego-level distribution result
#' @return T/F if check is passed.
#' What it does:
#' - Checks if there are no NA values in any household or family aggregation
check_complete_cases <- function(dt) {
  hh_cols <- names(dt)[grepl("HOUSEHOLD", names(dt))]
  hh_non_na_count <- sum(sapply(hh_cols, function(col) sum(!is.na(dt[[col]]))))
  
  family_cols <- names(dt)[grepl("FAMILY", names(dt))]
  fam_non_na_count <- sum(sapply(family_cols, function(col) sum(!is.na(dt[[col]]))))
  
  print(paste0("Number of valid households: ", hh_non_na_count))
  print(paste0("Number of valid families: ", fam_non_na_count))
  
  return(dt %>% 
           select(contains("HOUSEHOLD") | contains("FAMILY")) %>% 
           complete.cases() %>% all())
}

#' Check if logic for aggregation outputs are as expected
#' 
#' @param dt.list list of three distribution items to be returned in main aggregation function
#' @return null. Just prints the results of the checks
#' 
#' What it does:
#' - runs some logical checks for the results of the aggregation
check_all_cases <- function(dt.list) {
  guardians <- dt.list[[1]]
  mguardians <- dt.list[[2]]
  egos <- dt.list[[3]]
  
  # household and family
  if(check_complete_cases(egos)){
    cat("✓ Test 1 Passed\n")
  } else {
    cat("✗ Test 1 Failed\n")
  }
  
  # partner(s), spouse(s), ego
  if(check_relationship_completeness(egos)){
    cat("✓ Test 2 Passed\n")
  } else {
    cat("✗ Test 2 Failed\n")
  }
  
  # guardians and married guardians sum
  if(check_guardians_sum_count(egos)){
    cat("✓ Test 3 Passed\n")
  } else {
    cat("✗ Test 3 Failed\n")
  }
  
  # guardians and married guardians dist
  if(check_guardians_count(guardians, mguardians)){
    cat("✓ Test 4 Passed\n")
  } else {
    cat("✗ Test 4 Failed\n")
  }
}

# ==============================================================================
# 4. MAIN AGGREGATION FUNCTION
# ==============================================================================

#' Apply all aggregations efficiently
#' 
#' @param dt Prepared data (output from prep_for_aggregation)
#' @param income_defs Vector of income definition names as strings
#' @return List with: guardians (guardian-level distributions), married_guardians (married-guardians-level distributions), egos (ego-level with all other aggregations)
#' 
#' What it does:
#' For each income definition (xearn, xemp, xinc, xcominc):
#'   - household: sum across entire household
#'   - family: sum within family unit
#'   - ego: individual's own income (adult egos only)
#'   - partner: partner's income (if exists)
#'   - spouse: spouse's income (if married)
#'   - partners_sum: ego + partner
#'   - spouses_sum: ego + spouse
#'   - guardians_sum: sum of all guardian incomes
#'   - guardians_married_sum: sum of guardians incomes excluding non-married partners of HOHs
#'   - guardian: all guardians of target egos
#'   - married_guardian: all guardians of target egos excluding non-married partners of HOHs 
#' 
#' Results in 4 income defs × 11 aggregations = 44 new columns over 3 data.tables
apply_all_aggregations_optimized <- function(dt, income_defs) {
  
  cat("\n=== OPTIMIZED AGGREGATION ===\n")
  
  # create binary aggregation inclusion columns
  create_aggregation_columns(dt)
  
  # create results list
  # primed output of three items; guardians dist, married guardians dist, ego dist,
  guardians_dist <- dt[GUARDIAN == TRUE, .(CBSERIAL, ego_PERNUM, CBPERNUM, ego_BIRTHYR, PERWT)]
  
  mguardians_dist <- dt[MARRIED_GUARDIAN == TRUE, .(CBSERIAL, ego_PERNUM, CBPERNUM, ego_BIRTHYR, PERWT)]
  
  ego_dist <- dt[CBPERNUM == ego_PERNUM, .(CBSERIAL, CBPERNUM, ego_BIRTHYR, PERWT)]
  
  cols <- c("HOUSEHOLD", "FAMILY", "EGO", "PARTNER", "SPOUSE", "GUARDIAN", "MARRIED_GUARDIAN")
  
  # start looping through each income definition
  cat("Processing income definitions...\n")
  pb <- progress_bar$new(
    format = "  [:bar] :current/:total (:percent) eta: :eta",
    total = length(income_defs)
  )
  
  for (income_def in income_defs) {
    # 1: run create_income_columns to multiply aggregation inclusions by income values
    dt <- create_income_columns(dt, income_def, cols)
    
    # 2: process guardians distribution
    current <- dt[!is.na(paste0(income_def, "_GUARDIAN")), 
                  .(CBSERIAL, ego_PERNUM, CBPERNUM, income_value = get(paste0(income_def, "_GUARDIAN")))]
    
    setnames(current, "income_value", paste0(income_def, "_GUARDIAN"))    
    
    guardians_dist <- merge(guardians_dist, current, by = c("CBSERIAL", "ego_PERNUM", "CBPERNUM"), all.x = TRUE)
    
    # 3. process married guardians distribution
    current <- dt[!is.na(paste0(income_def, "_MARRIED_GUARDIAN")), 
                  .(CBSERIAL, ego_PERNUM, CBPERNUM, income_value = get(paste0(income_def, "_MARRIED_GUARDIAN")))]
    
    setnames(current, "income_value", paste0(income_def, "_MARRIED_GUARDIAN"))    
    
    mguardians_dist <- merge(mguardians_dist, current, by = c("CBSERIAL", "ego_PERNUM", "CBPERNUM"), all.x = TRUE)
    
    # 4. process all other ego-level distribution aggregations
    col_names <- paste0(income_def, "_", cols)
    
    current <- dt[, lapply(col_names, function(col) {
      if(all(is.na(get(col)))){ # if no one in the household-ego is to be included, return NA
        return(NA_real_)
      } else{
        # else, sum all valid values for the specified income column
        sum(get(col), na.rm = TRUE)
      }
    }), by = .(CBSERIAL, ego_PERNUM)]

    # Rename columns
    setnames(current, old = names(current)[-c(1, 2)], new = paste0(income_def, "_", cols))
    
    setnames(current, old = c(paste0(income_def, "_GUARDIAN"), paste0(income_def, "_MARRIED_GUARDIAN")),
             new = c(paste0(income_def, "_GUARDIANS_SUM"), paste0(income_def, "_GUARDIANS_MARRIED_SUM")))
    
    setnames(current, old = "ego_PERNUM", new = "CBPERNUM")

    ego_dist <- merge(ego_dist, current, by = c("CBSERIAL", "CBPERNUM"), all.x = TRUE)
    
    # 5. calculate partners_sum and spouses_sum
    ego_column <- paste0(income_def, "_EGO")
    partner_column <- paste0(income_def, "_PARTNER")
    spouse_column <- paste0(income_def, "_SPOUSE")
    
    ego_dist[, 
             paste0(income_def, "_PARTNERS_SUM") := 
               ifelse(
                 is.na(get(ego_column)) & is.na(get(partner_column)), 
                 NA_real_, 
                 rowSums(.SD, na.rm = TRUE)
               ),
             .SD = c(ego_column, partner_column)]
    
    ego_dist[, 
             paste0(income_def, "_SPOUSES_SUM") := 
               ifelse(
                 is.na(get(ego_column)) & is.na(get(spouse_column)), 
                 NA_real_, 
                 rowSums(.SD, na.rm = TRUE)
               ),
             .SD = c(ego_column, spouse_column)]
  }
  
  cat("\nAggregation complete!\n")
  
  # 6. Return results
  results <- list(guardians = guardians_dist,
                  married_guardians = mguardians_dist,
                  egos = ego_dist)
  
  # print if logical checks passed
  check_all_cases(results)

  return(results)
}

# ==============================================================================
# 5. RANKING FUNCTIONS
# ==============================================================================

#' Determine valid aggregations for a given age
#' 
#' @param age_in_year Age of person in the observation year
#' @return Vector of valid aggregation names
#' 
#' What it does:
#' - Ages ≤17 (children): household, family, guardians_sum, guardians, guardians_married
#' - Ages ≥24 (adults): household, family, partners_sum, married_sum, ego, partner, spouse
#' - Ages 18-23: excluded (return empty vector)
get_valid_aggregations <- function(age_in_year) {
  if (age_in_year <= AGE_FILTER_LOWER) {
    return(c("HOUSEHOLD", "FAMILY", "GUARDIANS_SUM", "GUARDIANS_MARRIED_SUM", "GUARDIAN", "MARRIED_GUARDIAN"))
  } else if (age_in_year >= AGE_FILTER_UPPER) {
    return(c("HOUSEHOLD", "FAMILY", "PARTNERS_SUM", "SPOUSES_SUM", "EGO", "PARTNER", "SPOUSE"))
  } else {
    return(character(0))
  }
}

#' Fast empirical ranking using weighted percentiles
#' 
#' @param data data.table with columns: income, ego_BIRTHYR, weights
#' @param income_var Name of income column
#' @param weight_var Name of weight column
#' @param interval Income interval size (dollars)
#' @return data.table: cohort, income_value, percentile_rank
#' 
#' What it does:
#' 1. For each birth cohort separately:
#' 2. Sort individuals by income
#' 3. Calculate cumulative weights to get empirical CDF
#' 4. Create income grid (every $100 from min to max)
#' 5. Interpolate to get percentile rank for each grid point
#' 
#' This is much faster than quantile regression and produces very similar results
rank_empirical_fast <- function(data, income_var, weight_var, interval = 100) {
  dt <- copy(data)
  dt[, `:=`(income_temp = get(income_var), weight_temp = get(weight_var))]
  dt <- dt[!is.na(income_temp)]
  
  if (nrow(dt) == 0) {
    return(data.table(cohort = numeric(), income_value = numeric(), percentile_rank = numeric()))
  }
  
  result <- dt[, {
    # Sort by income
    ord <- order(income_temp)
    inc_sorted <- income_temp[ord]
    wt_sorted <- weight_temp[ord]
    
    # Calculate cumulative percentiles
    cum_wt <- cumsum(wt_sorted)
    total_wt <- sum(wt_sorted)
    pct_rank <- cum_wt / total_wt
    
    # Create income grid
    min_inc <- floor(min(inc_sorted) / interval) * interval
    max_inc <- ceiling(max(inc_sorted) / interval) * interval
    income_grid <- seq(min_inc, max_inc, by = interval)
    
    # Interpolate percentiles for grid points
    grid_percentiles <- approx(
      x = inc_sorted,
      y = pct_rank,
      xout = income_grid,
      method = "linear",
      rule = 2
    )$y
    
    data.table(income_value = income_grid, percentile_rank = grid_percentiles)
    
  }, by = ego_BIRTHYR]
  
  setnames(result, "ego_BIRTHYR", "cohort")
  return(result)
}

#' Rank all income-aggregation combinations for a year
#' 
#' @param agg_result List from apply_all_aggregations_optimized
#' @param year_val Year being processed
#' @param cohort_range Vector of birth years
#' @param income_defs Vector of income definition names
#' @param weight_var Weight variable name
#' @param interval Income interval size
#' @param age_filter_lower Lower age threshold
#' @param age_filter_upper Upper age threshold
#' @return data.table with all results
#' 
#' What it does:
#' 1. Determine which cohort-aggregation combinations are valid
#'    (based on age filters)
#' 2. For each valid income-aggregation combination:
#'    - Extract the relevant data
#'    - Rank within each cohort
#' 3. Combine all results with metadata
#' 
#' Output columns: year, cohort, income_definition, aggregation_level, 
#'                 income_value, percentile_rank
rank_all_combinations_fast <- function(agg_result, year_val, cohort_range, income_defs, 
                                       weight_var, interval = 100,
                                       age_filter_lower = 17, age_filter_upper = 24) {
  
  egos_data <- agg_result$egos
  guardians_data <- agg_result$guardians
  mguardians_data <- agg_result$married_guardians
  
  all_results <- data.table()
  
  # Determine valid cohort-aggregation combinations
  valid_combinations <- data.table()
  
  for (cohort in cohort_range) {
    age_in_year <- year_val - cohort
    valid_aggs <- get_valid_aggregations(age_in_year)
    
    if (length(valid_aggs) > 0) {
      valid_combinations <- rbindlist(list(
        valid_combinations,
        data.table(cohort = cohort, aggregation = valid_aggs)
      ))
    }
  }
  
  # Get all income-aggregation combinations
  income_agg_cols <- CJ(
    income_def = income_defs,
    aggregation = unique(valid_combinations$aggregation)
  )[, col_name := paste0(income_def, "_", aggregation)]
  
  cat("Ranking income distributions...\n")
  pb <- progress_bar$new(
    format = "  [:bar] :current/:total (:percent) eta: :eta",
    total = nrow(income_agg_cols)
  )
  
  for (i in 1:nrow(income_agg_cols)) {
    income_def <- income_agg_cols$income_def[i]
    agg_level <- income_agg_cols$aggregation[i]
    col_name <- income_agg_cols$col_name[i]
    
    pb$tick()
    
    cohorts_for_agg <- valid_combinations[aggregation == agg_level, cohort]
    if (length(cohorts_for_agg) == 0) next
    
    # Special handling for "guardian" and "married_guardian aggregations
    if (agg_level == "GUARDIAN") {
      guardian_data <- guardians_data[[paste0(income_def, "_GUARDIAN")]]
      
      if (is.null(guardian_data) || length(guardian_data)==0) {
        next
      }
      
      data_subset <- guardians_data[ego_BIRTHYR %in% cohorts_for_agg, 
                                    .(ego_BIRTHYR, income = get(col_name), PERWT = PERWT)]
      
    } else if (agg_level == "MARRIED_GUARDIAN") {
      mguardian_data <- mguardians_data[[paste0(income_def, "_MARRIED_GUARDIAN")]]
      
      if (is.null(mguardian_data) || length(mguardian_data)==0) {
        next
      }
      
      data_subset <- mguardians_data[ego_BIRTHYR %in% cohorts_for_agg, 
                                   .(ego_BIRTHYR, income = get(col_name), PERWT = PERWT)]
      
    } else {
      # Regular aggregations (use main ego-level data)
      if (!col_name %in% names(egos_data)) {
        next
      }
      
      data_subset <- egos_data[ego_BIRTHYR %in% cohorts_for_agg, 
                               .(ego_BIRTHYR, income = get(col_name), PERWT = PERWT)]
    }
    
    if (nrow(data_subset) == 0 || all(is.na(data_subset$income))) {
      next
    }
    
    # Rank using fast empirical method
    rank_table <- rank_empirical_fast(data_subset, "income", "PERWT", interval)
    
    if (nrow(rank_table) == 0) {
      next
    }
    
    # Add metadata
    rank_table[, ':='(
      year = year_val,
      income_definition = income_def,
      aggregation_level = agg_level
    )]
    setcolorder(rank_table, c("year", "cohort", "income_definition", "aggregation_level", 
                              "income_value", "percentile_rank"))
    
    all_results <- rbindlist(list(all_results, rank_table))
  }
  
  return(all_results)
}

# ==============================================================================
# 6. WORKFLOW FUNCTIONS
# ==============================================================================

#' Process a single year
#' 
#' @param ipums_filepath Path to IPUMS .csv file
#' @param year_val Year (numeric)
#' @param cohort_range Birth years to analyze
#' @param income_definitions Table with income definitions
#' @param interval Income interval for ranking
#' @param age_filter_lower Lower age threshold
#' @param age_filter_upper Upper age threshold
#' @param sample_n Optional sample size for testing
#' @return data.table with all ranked results for this year
#' 
#' What it does (complete pipeline):
#' 1. Load raw IPUMS data
#' 2. Prepare data (filter, create income variables)
#' 3. Apply all aggregations
#' 4. Rank all combinations
#' 5. Return results
process_single_year <- function(ipums_filepath, year_val, cohort_range, income_definitions, 
                                interval = 100,
                                age_filter_lower = 17, age_filter_upper = 24,
                                sample_n = NULL) {
  cat("\n========================================\n")
  cat("Processing year:", year_val, "\n")
  if (!is.null(sample_n)) {
    cat("TESTING MODE: Sample of", sample_n, "households\n")
  }
  cat("========================================\n")
  
  # Load data
  cat("\nLoading data...\n")
  t1 <- Sys.time()
  raw_data <- load_data_csv(ipums_filepath)
  t2 <- Sys.time()
  cat("Loaded", nrow(raw_data), "rows in", round(difftime(t2, t1, units = "secs"), 1), "sec\n")
  
  # Prepare data
  cat("\nPreparing data...\n")
  t1 <- Sys.time()
  prepped_data <- prep_for_aggregation(raw_data, cohort_range, income_definitions,
                                       age_filter_lower, age_filter_upper, sample_n)
  t2 <- Sys.time()
  cat("Prepared", nrow(prepped_data), "rows in", round(difftime(t2, t1, units = "secs"), 1), "sec\n")
  
  # Apply aggregations
  cat("\n")
  t1 <- Sys.time()
  agg_result <- apply_all_aggregations_optimized(prepped_data, income_definitions$name)
  t2 <- Sys.time()
  cat("Aggregations completed in", round(difftime(t2, t1, units = "secs"), 1), "sec\n")
  
  # Rank all combinations
  cat("\n")
  t1 <- Sys.time()
  results <- rank_all_combinations_fast(
    agg_result = agg_result,
    year_val = year_val,
    cohort_range = cohort_range,
    income_defs = income_definitions$name,
    weight_var = "PERWT",
    interval = interval,
    age_filter_lower = age_filter_lower,
    age_filter_upper = age_filter_upper
  )
  t2 <- Sys.time()
  cat("Ranking completed in", round(difftime(t2, t1, units = "secs"), 1), "sec\n")
  
  cat("\nYear", year_val, "complete! Generated", nrow(results), "rows\n")
  
  return(results)
}

#' Process all years in parallel
#' 
#' @param ipums_files Named vector of file paths
#' @param output_dir Directory to save results
#' @param n_cores Number of cores (default: auto-detect)
#' @param ... Other parameters passed to process_single_year
#' @return List of summary info for each year
#' 
#' What it does:
#' 1. Sets up parallel processing workers
#' 2. Processes each year independently in parallel
#' 3. Saves results to compressed CSV files
#' 4. Returns summary statistics
process_all_years_parallel <- function(ipums_files, output_dir, 
                                       n_cores = NULL,
                                       cohort_range, income_definitions, 
                                       interval = 100,
                                       age_filter_lower = 17, age_filter_upper = 24) {
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  if (is.null(n_cores)) {
    n_cores <- parallel::detectCores() - 1
  }
  
  cat("========================================\n")
  cat("PARALLEL PROCESSING\n")
  cat("========================================\n")
  cat("Years:", length(ipums_files), "\n")
  cat("Cores:", n_cores, "\n")
  cat("Output:", output_dir, "\n")
  cat("========================================\n\n")
  
  # Set up parallel processing
  plan(multisession, workers = n_cores)
  
  overall_start <- Sys.time()
  
  # Process years in parallel
  results_list <- future_map(names(ipums_files), function(year_val) {
    filepath <- ipums_files[[year_val]]
    year_start <- Sys.time()
    
    # Process year
    results <- process_single_year(
      ipums_filepath = filepath,
      year_val = as.numeric(year_val),
      cohort_range = cohort_range,
      income_definitions = income_definitions,
      interval = interval,
      age_filter_lower = age_filter_lower,
      age_filter_upper = age_filter_upper,
      sample_n = NULL
    )
    
    year_end <- Sys.time()
    time_taken <- as.numeric(difftime(year_end, year_start, units = "mins"))
    
    # Save results
    output_file <- file.path(output_dir, paste0("income_ranks_", year_val, ".csv.gz"))
    fwrite(results, output_file)
    
    list(
      year = year_val,
      output_file = output_file,
      n_rows = nrow(results),
      time_minutes = time_taken
    )
  }, .options = furrr_options(seed = TRUE), .progress = TRUE)
  
  overall_end <- Sys.time()
  total_time <- as.numeric(difftime(overall_end, overall_start, units = "mins"))
  
  cat("\n========================================\n")
  cat("PARALLEL PROCESSING COMPLETE!\n")
  cat("Total time:", round(total_time, 1), "minutes\n")
  cat("========================================\n")
  
  # Close parallel workers
  plan(sequential)
  
  # Save summary
  summary_df <- data.frame(
    year = sapply(results_list, function(x) x$year),
    n_rows = sapply(results_list, function(x) x$n_rows),
    time_minutes = sapply(results_list, function(x) x$time_minutes),
    output_file = sapply(results_list, function(x) x$output_file)
  )
  
  summary_file <- file.path(output_dir, "processing_summary.csv")
  fwrite(summary_df, summary_file)
  cat("Summary saved to:", summary_file, "\n")
  
  return(results_list)
}

#' Create aggregations only
#' 
#' @param data original data containing columns for serial, pernum, family pointers, income, year etc.
#' @param income_definitions Table with income definitions
#' @param cohort_range Birth years to analyze
#' @param age_filter_lower Lower age threshold
#' @param age_filter_upper Upper age threshold
#' @return data.table with appended columns for all aggregation-definition pairs
#' 
#' What it does:
#' 1. Preps data and verifies presences of necessary columns
#' 2. Applies aggregations and multiplies by income definitions
#' 3. Returns original data with new appended columns for each aggregation-definition, linked by target ego
append_income_aggregations <- function(data, income_definitions, 
                                       cohort_range = 1983:2000,
                                       age_filter_lower = 17, age_filter_upper = 24){
  setDT(data)
  
  required_cols <- c("INCWAGE", "INCBUS00", "INCTOT", "INCSS", "INCRETIR", "INCINVST",
                     "CBSERIAL", "CBPERNUM", "BIRTHYR", "AGE", "YEAR", "MARST", "RELATED",
                     "FAMUNIT", "SPLOC", "MOMLOC", "POPLOC", "MOMLOC2", "POPLOC2", "PERWT")
  
  if (!all(required_cols %in% names(data))) {
    missing <- setdiff(required_cols, names(data))
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  }
  
  prepped_data <- prep_for_aggregation(data, cohort_range, income_definitions,
                                       age_filter_lower, age_filter_upper, sample_n=NULL)
  
  agg_result <- apply_all_aggregations_optimized(prepped_data, income_definitions$name)
  
  egos <- agg_result$egos %>% select(starts_with("CB") | starts_with("x"))
  
  guardians <- agg_result$guardians %>% select(CBSERIAL, CBPERNUM, ego_PERNUM, starts_with("x"))
  
  # reshape
  guardians[, pernum_seq := seq_len(.N), by = .(CBSERIAL, ego_PERNUM)]
  
  # Reshape wide
  guardians_w <- dcast(guardians,
                       CBSERIAL + ego_PERNUM ~ pernum_seq,
                       value.var = c("CBPERNUM", "xearn_GUARDIAN", "xemp_GUARDIAN", "xinc_GUARDIAN", "xcominc_GUARDIAN"))
  
  # rename values
  guardians_w %>% 
    rename(CBPERNUM = ego_PERNUM,
           GUARDIAN1 = CBPERNUM_1,
           GUARDIAN2 = CBPERNUM_2) -> guardians_w
  
  married_guardians <- agg_result$married_guardians %>% select(CBSERIAL, CBPERNUM, ego_PERNUM, starts_with("x"))
  
  married_guardians[, pernum_seq := seq_len(.N), by = .(CBSERIAL, ego_PERNUM)]
  
  mguardians_w <- dcast(married_guardians,
                        CBSERIAL + ego_PERNUM ~ pernum_seq,
                        value.var = c("CBPERNUM", "xearn_MARRIED_GUARDIAN", "xemp_MARRIED_GUARDIAN", "xinc_MARRIED_GUARDIAN", "xcominc_MARRIED_GUARDIAN"))
  
  # rename values
  mguardians_w %>% 
    rename(CBPERNUM = ego_PERNUM,
           MARRIED_GUARDIAN1 = CBPERNUM_1,
           MARRIED_GUARDIAN2 = CBPERNUM_2) -> mguardians_w
  
  # join back to original data
  result <- egos[data,
                 on = .(CBSERIAL, CBPERNUM)]
  
  result <- guardians_w[result,
                        on = .(CBSERIAL, CBPERNUM)]
  
  result <- mguardians_w[result,
                         on = .(CBSERIAL, CBPERNUM)]
  
  cat(paste0(ncol(result)-ncol(data), " income columns appended:"), 
      paste(setdiff(names(result), names(data)), 
            collapse = ", "), "\n")
  
  return(result)
}
