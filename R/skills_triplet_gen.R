

makeRootSkillCluster <- function(skill_base_name, base_difficulty) {
  skill_root_name <- paste0("rs-", skill_base_name)
  cluster_tibble <- tibble::tribble(~Origin, ~Relationship, ~Value,
                                    skill_root_name, "has.base", base_difficulty)
  return(cluster_tibble)
}

makeSpecificSkillCluster <- function(skill_spec, skill_base_name, skill_weight, skill_desc = NULL, skill_comp_date = NULL){
  skill_root_name <- paste0("rs-", skill_base_name)
  skill_name <- paste0("ss-", skill_spec)
  cluster_tibble <- tibble::tribble(~Origin, ~Relationship, ~Value,
                                    skill_name, "has.root", skill_root_name,
                                    skill_name, "has.weight", skill_weight)
  
  if(!is.null(skill_desc)){
    cluster_tibble <- rbind(cluster_tibble, tibble::tribble(~Origin, ~Relationship, ~Value,
                                                            skill_name, "has.desc", skill_desc))
  }
  if(!is.null(skill_comp_date)){
    cluster_tibble <- rbind(cluster_tibble, tibble::tribble(~Origin, ~Relationship, ~Value,
                                                            skill_name, "has.comp.date", skill_comp_date))
  }
  
  return(cluster_tibble)
}


require(magrittr)
makeRootSkillCluster("Rlang", 1) %>% 
  rbind(makeSpecificSkillCluster("Dplyr", "Rlang", 0.5, "Usage of the Dyplr Package to perform the full range of data manipulation", "2018-01-01")) %>% 
  rbind(makeSpecificSkillCluster("Data.table", "Rlang", 1)) ->
          test_data
  