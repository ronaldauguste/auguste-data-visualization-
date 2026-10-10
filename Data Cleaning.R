library(tidyverse)
library(foreign)
library(haven)
library(psych)

#Importing Tyler's data
Tyler_Parent_Knowledge <- read_sav(file = "./Parent Knowledge Data.sav")

Tyler_Teacher_Knowledge <- read_sav(file = "./Teacher Knowledge Data (Local).sav")

#Importing Ronald's data
Ronald_Parent_Teacher_Knowledge <- read_csv(file = "./reduced_dataset.csv")
#n.b. review original knowledge scores later on

#Data Inspection
glimpse(Tyler_Parent_Knowledge)
glimpse(Tyler_Teacher_Knowledge)

#Adding data source, and parent/teacher identifiers to Tyler's data
Tyler_Parent_Knowledge <- Tyler_Parent_Knowledge |> 
  mutate(
    Role = "parent",
    across(starts_with("Knowledge_Score"), as.numeric)) |> #haven import created a weird "haven labelled" vector type
  relocate(Role, .after = Gender)

Tyler_Teacher_Knowledge <- Tyler_Teacher_Knowledge |> 
  mutate(Role = "teacher",
         across(starts_with("Knowledge_Score"), as.numeric)) |> #haven import created a weird "haven labelled" vector type
  relocate(Role, .after = Gender)

#Selecting relevant columns
Tyler_Parent_Reduced <- Tyler_Parent_Knowledge |> 
  select(ProlificID, Age, Gender, Role, Country_Residence, Knowledge_Score01:Knowledge_Score25, Knowledge_Score27)
Tyler_Parent_Reduced

Tyler_Teacher_Reduced <- Tyler_Teacher_Knowledge |> 
  select(ProlificID_, Age, Gender, Role, Country_Residence, Knowledge_Score01:Knowledge_Score25, Knowledge_Score27) |> 
  rename(ProlificID = ProlificID_)

Tyler_Teacher_Reduced

#Merging Tyler's data
Tyler_PT_Combined <- full_join(Tyler_Parent_Reduced, Tyler_Teacher_Reduced)

Tyler_PT_Combined

#Creating comparable Knowledge score (T&T version excluded Q26)
Knowledge_keys <- list(Knowledge_Total = paste0("Knowledge_Score", sprintf("%02d", c(1:25, 27))))
#needed sprintf() to "zero-pad" the numbers to 2 digits
Knowledge_keys

Knowledge_scores <- scoreItems(Knowledge_keys, Tyler_PT_Combined, impute = "none", min = 0, max = 1, totals = T)

#Converting list to dataframe
Knowledge_scores.df <- as.data.frame(Knowledge_scores$scores)

#Combining dataframes
Tyler_PT_w_Totals <- bind_cols(Tyler_PT_Combined, Knowledge_scores.df)

#Cleaning Ronald's data
glimpse(Ronald_Parent_Teacher_Knowledge)

Ronald_Reduced <- Ronald_Parent_Teacher_Knowledge |> 
  select(ID_Key, Age, Gender, Teacher_YN, Country, KQ_1:KQ_26)

Ronald_Reduced <- Ronald_Reduced |> 
  mutate(
    Role = case_when(
      Teacher_YN == "No" ~ "parent",   #creating matching "Role" column like in Tyler's data
      Teacher_YN == "Yes" ~ "teacher"
      ),
    Country = case_when(
      Country == 1 ~ "Canada",
      Country == 2 ~ "Trinidad & Tobago"
    ),
    across(starts_with("KQ_"),  #fixing KQ_'s so 1's = True and 0's are False/IDK
           ~case_when(
             .x == "1" ~ 1,
             .x %in% c("2", "I Don't Know") ~ 0
           )
           )
    ) |> 
  relocate(Role, .after = Gender)


#Creating matching Knowledge columns
Ronald_Reduced <- Ronald_Reduced |>
  rename_with(~ sprintf("Knowledge_Score%02d", 1:25), KQ_1:KQ_25)   

Ronald_Reduced <- Ronald_Reduced |>
  rename(Knowledge_Score27 = KQ_26)
  
#recreate knowledge totals
Ronald_Knowledge_scores <- scoreItems(Knowledge_keys, Ronald_Reduced, impute = "none", min = 0, max = 1, totals = T)
  
Ronald_Knowledge_scores

#Converting list to dataframe
Ronald_Knowledge_scores.df <- as.data.frame(Ronald_Knowledge_scores$scores)

#Combining dataframes
Ronald_PT_w_Totals <- bind_cols(Ronald_Reduced, Ronald_Knowledge_scores.df)

#Reducing/renaming both df's to identical columns
Ronald_PT_w_Totals <- Ronald_PT_w_Totals |> 
  select(!Teacher_YN) |> 
  rename(Country_Residence = Country)

Ronald_PT_w_Totals <- Ronald_PT_w_Totals |> 
  mutate(Data_source = "Ronald", .after = ID_Key)

Tyler_PT_w_Totals <- Tyler_PT_w_Totals |> 
  rename(ID_Key = ProlificID) |> 
  mutate(Data_source = "Tyler", .after = ID_Key)

#Tyler's Age col is char, so.....
Tyler_PT_w_Totals <- Tyler_PT_w_Totals |>
  mutate(Age = parse_number(Age))

#Need to fix both Gender columns
Ronald_PT_w_Totals <- Ronald_PT_w_Totals |> 
  mutate(
    Gender = fct_infreq(Gender),
    Gender = case_when(Gender == "Male" ~ "Male",
                       Gender == "Female" ~ "Female",
                       TRUE ~ "Other Gender")) 

Tyler_PT_w_Totals <- Tyler_PT_w_Totals |> 
  mutate(
         Gender = case_when(Gender == 1 ~ "Male",
                            Gender == 2~ "Female",
                            TRUE ~ "Other Gender"),
         Gender = fct_infreq(Gender)) 

#Combining Tyler's and Ronald's data frames
Tyl_Ron_Full_DF <- full_join(Tyler_PT_w_Totals, Ronald_PT_w_Totals)

#Creating % Correct Column

Tyl_Ron_Full_DF <-  Tyl_Ron_Full_DF |> 
  mutate(Knowledge_Corr_Percent = round((Knowledge_Total / 26)*100, 2))

#Creating % Incorrect Column
Tyl_Ron_Full_DF <- Tyl_Ron_Full_DF |> 
  mutate(Knowledge_Incorr_Percent = 100 - Knowledge_Corr_Percent)

#Exporting cleaned dataframe
write_csv(Tyl_Ron_Full_DF, "Complete_ADHD_Knowledge.csv")

Tyl_Ron_Full_DF
describeBy(Tyl_Ron_Full_DF$Knowledge_Total, Tyl_Ron_Full_DF$Country_Residence)


Quick_comp_plot <- Tyl_Ron_Full_DF |> 
  ggplot(aes(x = Country_Residence, y = Knowledge_Corr_Percent)) +
  geom_bar(stat = "summary", fun = "mean") +
  stat_summary(
    geom = "text", fun = "mean",
    aes(label = sprintf("%.2f", after_stat(y))),
    vjust = -0.5) +
  facet_wrap(~Role, ncol = 1) +
  theme_minimal()

ggsave(
  "Quick_comp_plot.jpeg", 
  plot = Quick_comp_plot,
  width = 16, height = 8, units = "in",
  dpi = 300)
         