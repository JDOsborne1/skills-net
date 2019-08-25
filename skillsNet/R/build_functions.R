

#' Make RS Cluster
#'
#' @param skill_base_name The base name of the skill, will be appended with the rs- prefix
#' @param added_date The date that that base skill was added
#' @param base_difficulty The base difficulty of that type of skill
#'
#' @return
#' @export
#'
#' @examples
makeRootSkillCluster <- function(skill_base_name, base_difficulty, added_date = NULL) {
  skill_root_name <- paste0("rs-", skill_base_name)
  base_difficulty_typed <- paste0("nu-", base_difficulty)

  added_date_typed <-paste0("da-", added_date)

  cluster_tibble <- tibble::tribble(~Origin, ~Relationship, ~Value,
                                    skill_root_name, "has.base", base_difficulty_typed)

  if(!is.null(added_date)){
    cluster_tibble <- rbind(cluster_tibble, tibble::tribble(~Origin, ~Relationship, ~Value,
                                                            skill_root_name, "added.date", added_date_typed))
  }

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
  skill_weight_typed <- paste0("nu-", skill_weight)
  skill_comp_date_typed <- paste0("da-", skill_comp_date)
  cluster_tibble <- tibble::tribble(~Origin, ~Relationship, ~Value,
                                    skill_name, "has.root", skill_root_name,
                                    skill_name, "has.weight", skill_weight_typed)

  if(!is.null(skill_desc)){
    cluster_tibble <- rbind(cluster_tibble, tibble::tribble(~Origin, ~Relationship, ~Value,
                                                            skill_name, "has.desc", skill_desc))
  }
  if(!is.null(skill_comp_date)){
    cluster_tibble <- rbind(cluster_tibble, tibble::tribble(~Origin, ~Relationship, ~Value,
                                                            skill_name, "has.comp.date", skill_comp_date_typed))
  }

  return(cluster_tibble)
}


#' Verify SS Cluster
#'
#' @param verify_by The Verifying party
#' @param skill_spec The name of the specific skill
#'
#' @return
#' @export
#'
#' @examples
verifySpecificSkillCluster <- function(skill_spec, verify_by){
  skill_name <- paste0("ss-", skill_spec)
  verify_by_name <- paste0("pa-", verify_by)
  cluster_tibble <- tibble::tribble(~Origin, ~Relationship, ~Value,
                                    skill_name, "verified.by", verify_by_name)

  return(cluster_tibble)
}
