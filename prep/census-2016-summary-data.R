


library(Census2016.DataPack)

ced_boundaries

d1 <- CED__Age  %>%
  group_by(CED_NAME16) %>%
  summarise(young_persons = sum(persons[Age %in% c("0-4", "5-14", "14-19")]) / sum(persons))

d2 <- CED__BornAust %>%
  group_by(CED_NAME16) %>%
  summarise(born_aust = sum(persons[BornAust]) / sum(persons))

d3 <- CED__IndigenousStatus %>%
  group_by(CED_NAME16) %>%
  summarise(indigenous = 1- sum(persons[IndigenousStatus == "Non-indigenous"]) / sum(persons))
  
d4 <- CED__medianRent %>%
  select(CED_NAME16, median_rent = medianRent)
  
d5 <- CED__MaxSchoolingCompleted %>%
  group_by(CED_NAME16) %>%
  summarise(adults_less_than_year_10_school = sum(adults[MaxSchoolingCompleted %in% c("Did not go to school",
                                                                                      "Year 8", "Year 9")]) / 
              sum(adults))

d6 <-  CED__medianTotalPersonalIncome

d7 <- CED__OnlyEnglishSpokenHome %>%
  group_by(CED_NAME16) %>%
  summarise(only_english_spoken_home = sum(persons[OnlyEnglishSpokenHome]) / sum(persons))

ced_data_2016 <- d1 %>%
  left_join(d2, by = "CED_NAME16") %>%
  left_join(d3, by = "CED_NAME16") %>%
  left_join(d4, by = "CED_NAME16") %>%
  left_join(d5, by = "CED_NAME16") %>%
  left_join(d6, by = "CED_NAME16") %>%
  left_join(d7, by = "CED_NAME16") %>%
  rename(ced_name = CED_NAME16)


save(ced_data_2016, file = "pkg/data/ced_data_2016.rda")
