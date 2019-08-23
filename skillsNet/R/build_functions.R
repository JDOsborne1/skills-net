

#' Make RS Cluster
#'
#' @param skill_base_name The base name of the skill, will be appended with the rs- prefix
#' @param base_difficulty The base difficulty of that type of skill
#'
#' @return
#' @export
#'
#' @examples
makeRootSkillCluster <- function(skill_base_name, base_difficulty) {
  skill_root_name <- paste0("rs-", skill_base_name)
  cluster_tibble <- tibble::tribble(~Origin, ~Relationship, ~Value,
                                    skill_root_name, "has.base", base_difficulty)
  return(cluster_tibble)
}

#' Make SS Cluster
#'
#' @param skill_spec The name of the specific skill
#' @param skill_base_name The name of the base skill
#' @param skill_weight The weight of the specific skill
#' @param skill_desc The description of the specific skill
#' @param skill_comp_date The date of completion for the specific skill
#'
#' @return
#' @export
#'
#' @examples
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



