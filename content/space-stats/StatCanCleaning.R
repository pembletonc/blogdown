library(tidyverse)


CAN_11_AGE <- read_csv("./content/space-stats/CAN_11_CT_AGE.csv")
CAN_11_EDU <- read_csv("./content/space-stats/CAN_11_CT_EDU.csv")
CAN_11_LANG <- read_csv("./content/space-stats/CAN_11_CT_LANG.csv")
CAN_11_MFH <- read_csv("./content/space-stats/CAN_11_CT_MFH.csv")
CAN_11_MOB <- read_csv("./content/space-stats/CAN_11_CT_MOB.csv")
CAN_11_IMMI <- read_csv("./content/space-stats/CAN_11_CT_IMMI.csv")

glimpse(CAN_11_IMMI)


IMMI <- CAN_11_IMMI %>%
  select("GEOGRAPHY", "NORTH_AMERICAN_ABORIGINAL" = "NORTH_AMER", "CDN_CITIZEN" = "CANADIAN_C", "NOT_CITIZEN" = "NOT_CANADI",
         "NON_IMMIGRANTS" = "NON_IMMIGR", "IMMIGRANTS","NOT_A_VISI", "VISIBLE_MINORITY" = "TOTAL_VISI",
         "CHINESE", "BLACK", "SOUTH_ASIA", "LATIN_AMER", "FILIPINO", "ARAB",
         "SOUTHEAST_ASIAN" = "SOUTHEAST_", "WEST_ASIAN", "KOREAN", "JAPANESE",
         "BUDDHIST", "CHRISTIAN", "HINDU", "JEWISH", "MUSLIM", "SIKH", "ABORIGINAL_REL" = "TRADITIONA", "NO_REL" = "NO_RELIGIO")

MOB <- CAN_11_MOB %>%
  select("GEOGRAPHY", "INTERNAL_MIGRANTS" = "INTERNAL_M",
         "EXTERNAL_MIGRANTS" = "EXTERNAL_M")

LANG <- CAN_11_LANG %>%
  select("CTUID", "ENGLISH", "FRENCH", "ABORIGINAL" = "SELECTED_A",
         "OTHER_LANG" = "SELECTED_N")

MFH <- CAN_11_MFH %>%
  select("CTUID", "MARRIED_COMMON" = "MARRIED_OR", "NOT_MARRIED" = "NOT_MARRIE",
         "CHILDREN" = "TOTAL_CHIL", "CHILDREN_AVG" = "AVERAGE_NU",
         "SINGLE_DETACHED" = "SINGLE_DET", "APARTMENT_5_GREATER" = "APARTMENT_",
         "MOBILE_HOME" = "MOVABLE_DW", "SEMI_DET" = "SEMI_DETAC", "ROW_HOUSE",
         "APARTMENT1", "APARTMENT_5_LESS" = "APARTMEN2", "PPL_IN_HOUSEHOLD" = "AVERAGE_2")

EDU <-  CAN_11_EDU %>%
  select("GEOGRAPHY", "Participation" = "PARTICIPAT", "UNEMPLOYMENT" = "UNEMPLOYME",
         "CAR_TRUCK_", "CAR_TRUCK1", "COMMUTE_TRANSIT" = "PUBLIC_TRA", "COMMUTE_WALK" = "WALKED",
         "COMMUTE_CYCLE" = "BICYCLE", "COMMUTE_TIME" = "MEDIAN_COM" ,"NO_EDU" = "NO_CERTIFI", "HIGHSCHOOL" = "HIGH_SCHOO", 
         "APPRENTICE", "COLLEGE_CE", "UNIVERSIT1", "UNIVERSITY", "UNIVERSI2" ) %>%
  mutate(COMMUTE_CAR = CAR_TRUCK_ + CAR_TRUCK1,
         COLLEGE = APPRENTICE + COLLEGE_CE,
         UNIVERSITY = UNIVERSIT1 + UNIVERSITY + UNIVERSI2) %>%
  select(-c("CAR_TRUCK_", "CAR_TRUCK1", "APPRENTICE", "COLLEGE_CE",
            "UNIVERSIT1", "UNIVERSI2"))


Census_2011 <- CAN_11_AGE %>%
  select("CTUID", "GEOGRAPHY", "CMANAME", "PRNAME", `2011` = "POPULATION","POP_PERC_CHANGE" = "POPULATI2",  
         "POP_DENSITY_KM2" = "POPULATI3","PERC_POP_OVER_15" = "OF_THE_POP" , "MEDIAN_AGE") %>%
  gather(`2011`, key = "YEAR", value = "Population") %>%
  left_join(EDU, by = "GEOGRAPHY") %>%
  left_join(MFH, by = "CTUID") %>%
  left_join(LANG, by = "CTUID") %>%
  left_join(MOB, by = "GEOGRAPHY") %>%
  left_join(IMMI, by = "GEOGRAPHY")






