get_parish_return_table <- function(census_data) {
    parish_return <- census_data %>% 
        distinct(Year, ParID, RegCnty, RegDist, SubDist, Parish, Population, MalePop, FemalePop, ParType) %>%
        mutate(ParType = as.integer(ParType)) %>% mutate(ParType = ifelse(is.na(ParType), 0, ParType))
    return(parish_return)
}

get_household_table <- function(census_data) {
    census_household <- census_data %>%
        mutate(H_ID = paste0(Year, '_', ParID, '_', H)) %>%
        mutate(across(c('HSS','tn_CFUs','H_CFU'), as.numeric)) %>%
        mutate(H_Age = as.integer(H_Age), H_Age_Band = as.integer(H_Age/5) * 5) %>%
            distinct (H_ID, Year, ParID, H, HSS, H_Sex, H_Age, H_Age_Band, H_Occ, H_CFU, tn_CFUs)
    return(census_household)
}

get_unit_table <- function(census_data) {
    census_cfu <- census_data %>% filter(CFU != 0) %>%
        mutate(H_ID = paste0(Year, '_', ParID, "_", H), UNIT_ID = paste0(Year, '_', ParID, "_", H, "_", CFU), IS_CFU = TRUE, IS_H_CFU = (CFU == H_CFU), CFUsize = as.integer(CFUsize)) %>%
        distinct (UNIT_ID, H_ID, Year, CFU, H_CFU, n_CFUs, CFUsize, HHD, IS_CFU, IS_H_CFU)
    census_non_cfu <- census_data %>% filter(CFU == 0) %>%
        mutate(H_ID = paste0(Year, '_', ParID, "_", H), UNIT_ID = paste0(Year, '_', ParID, "_", H, "_", CFU, "_", HHD), IS_CFU = FALSE, IS_H_CFU = FALSE) %>%
        group_by (UNIT_ID, H_ID, Year, CFU, H_CFU, n_CFUs, HHD, IS_CFU, IS_H_CFU) %>% summarise(CFUsize = n())
    census_unit <- bind_rows(census_cfu, census_non_cfu)
    return(census_unit)
}

get_person_table <- function(census_data) {
    census_person <- census_data %>%
        mutate(H_ID = paste0(Year, '_', ParID, "_", H),
                UNIT_ID = ifelse(CFU == 0, paste0(Year, '_', ParID, "_", H, "_", CFU, "_", HHD), paste0(Year, '_', ParID, "_", H, "_", CFU))) %>%
        mutate(PersonID = paste0(H_ID, "_", PID), Age = as.integer(Age)) %>%
        mutate(Age_Band = as.integer(Age/5) * 5) %>%
        mutate(Rela = as.integer(Rela)) %>%
        mutate(RelationGroup = as.integer(Rela / 1000) * 1000) %>%
        mutate(relationship = Rela - RelationGroup) %>%
        mutate(relationship = ifelse(relationship >= 200, Rela, relationship)) %>%
        mutate(relationship = ifelse(relationship >= 500 & relationship <= 581, relationship-500, relationship)) %>%
        select(Year, PersonID, RecID, PID, H_ID, UNIT_ID, Sex, Age, Age_Band, Cage, Cond, Mar, Relat, Rela,
               RelationGroup, relationship, Occ, Occode, HISCO, Inactive, Disab,
                DisCode1, DisCode2, Bpstring, BpCmty, Std_Par, 
                BpCnty, Cnti, Alt_Cnti, BpCtry, Ctry, Alt_Ctry)
    return(census_person)
}

get_relation_table <- function(census_data) {
    person_relation_parent <- census_data %>% 
        mutate(H_ID = paste0(Year, '_', ParID, "_", H)) %>%
        mutate(PersonID = paste0(H_ID, "_", PID)) %>%
        select(PersonID, Spouse, Mother, Father, H_ID) %>%
        pivot_longer(c("Spouse", "Mother", "Father"), names_to = "relation", values_to = "RelID") %>%
        filter(RelID != 0) %>%
        mutate(RelID = paste0(H_ID, "_", RelID)) %>% select(PersonID, relation, RelID)

    person_relation_child <- person_relation_parent %>% filter(relation == 'Mother' | relation == 'Father') %>%
                        mutate(NewPersonID = RelID, relation = 'Child', NewRelID = PersonID) %>%
                        select(PersonID = NewPersonID, relation, RelID = NewRelID)

    person_relation <- bind_rows(person_relation_parent, person_relation_child)

    return(person_relation)
}
