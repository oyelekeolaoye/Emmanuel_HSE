library(foreign)
library(stringr)
library(dplyr)
library(eq5d)
library(tidyverse)
library(table1)

#read raw data and assign NAs to values as defined by the authors
hse_2000_raw <- read.table("data/hse00ai.tab", header = T, sep = "\t", na.strings = c('-1', '-2', '-9', '-8'))

#select variables of interest and filter for desired age range
hse_2000 <- hse_2000_raw %>%
  filter(age > 15 & age < 65) %>%
  mutate(econact = fct_recode(as.factor(activb), `In employment` = '2', `Unemployment` = '3', `Unemployment` ='4',
                              `Unemployment` ='5' , `Unemployment` = '6',`Retired` = '8', `Retired` ='7', 
                              `Other economically inactive` = '1', `Retired` ='9', `Other economically inactive` ='11', `Other economically inactive` ='10'), 
         year = "2000") %>%
  select(year, pserial, age, sex, weight, econact, topqual3, ghqg2, genhelf2, 
         ghqconc, ghqdecis, ghqsleep, ghqconfi, ghquse, ghqstrai, ghqover, ghqenjoy, ghqface, ghqunhap, ghqworth, ghqhappy)

# define the columns to recode to likert scoring 0-36
start_col <- which(names(hse_2000) == "ghqconc")
end_col <- which(names(hse_2000) == "ghqhappy")

# Now use these indices with the `:` operator to select the columns
cols <- names(hse_2000)[start_col:end_col]

# recode scores fit make 0-36 likert single score across the 12 questions
hse_2000 <- hse_2000 %>%
  mutate(across(all_of(cols), ~case_when(. == 1 ~ 0,
                                         . == 2 ~ 1,
                                         . == 3 ~ 2,
                                         . == 4 ~ 3),
                .names = "{.col}"))
hse_2000$ghq36scr <- rowSums(hse_2000[, start_col:end_col])

#Dealing with GHQG2 
#Authors have recoded as follows VALUE LABELS ghqg2 ///1 'Score 0' /// 2 'Score 1-3' /// 3 'Score 4+'.
# Recode 1 and 2 as No case and 3 as Case
hse_2000 <- hse_2000 %>%
  mutate(ghqg2 = fct_recode(as.factor(ghqg2), `No case` = '1', `No case` = '2', `Case Mental Health` = '3'))


# Recode variables
hse_2000 <- hse_2000 %>%
  mutate(Sex = fct_recode(as.factor(sex), `Male` = '1', `Female` = '2'),
         `Employment_Status` = fct_recode(as.factor(econact)),
         `Self_Rated_Health` = fct_recode(as.factor(hse_2000$genhelf2), `Very good/good` = '1',
                                          Fair = '2',
                                          `Bad/very bad` = '3'),
         `Highest_Edu_Attain` = fct_recode(as.factor(hse_2000$topqual3),
                                           `Degree and equivalent` = '1',
                                           `Below degree` = '2',
                                           `Below degree` = '3',
                                           `Below degree` = '4',
                                           `Below degree` = '5',
                                           `No degree` = '6',
                                           `No degree` = '7'))

# Drop unusused variables
start_col <- which(names(hse_2000) == "ghqconc")
end_col <- which(names(hse_2000) == "ghqhappy")
cols_to_exclude <- names(hse_2000)[start_col:end_col]
hse_2000 <- hse_2000[, !(names(hse_2000) %in% cols_to_exclude)]

#define breakpoints for agegroups
breakpoints <- c(15,24,34,44,54,64) #15 will not be included in the interval, observed the open interval notation in labels

#retain complete cases//// 1.6k observations removed
hse_2000_complete <- na.omit(hse_2000) %>%
  select(-sex) %>%
  rename(GHQg2 = ghqg2, Age = age, Weight = weight, GHQ36Scr = ghq36scr) %>%
  mutate(Age = cut(Age, breakpoints)) %>%
  mutate(Age = factor(Age, labels = c("16-24","25-34", "35-44", "45-54","55-64")),
         )



caption  <- "2000 descriptive summary table using Employment Status as explanatory variable"

# Table 1 (all outcome variables included to a table)
table1::table1(~ factor(year) + Age + Sex + Weight + Highest_Edu_Attain + factor(Self_Rated_Health) + GHQg2 + GHQ36Scr | Employment_Status, data = hse_2000_complete, caption=caption)

######################################################################################################################################################################################

#read raw data and assign NAs to values as defined by the authors

hse_2001_raw <- read.table("data/hse01ai.tab", fill = TRUE, header = T, sep = "\t", na.strings = c('-1', '-2', '-9', '-8'))

#select variables of interest and filter for desired age range
hse_2001 <- hse_2001_raw %>%
  filter(age > 15 & age < 65) %>%
  mutate(econact = fct_recode(as.factor(activb), `In employment` = '2', `Unemployment` = '3', `Unemployment` ='4',
                              `Unemployment` ='5' , `Unemployment` = '6',`Retired` = '8', `Retired` ='7', 
                              `Other economically inactive` = '1', `Retired` ='9', `Other economically inactive` ='11', `Other economically inactive` ='10'), 
         year = "2001") %>%
  select(year, pserial, age, sex, weight, econact, topqual3, ghqg2, genhelf2, 
         ghqconc, ghqdecis, ghqsleep, ghqconfi, ghquse, ghqstrai, ghqover, ghqenjoy, ghqface, ghqunhap, ghqworth, ghqhappy)

# define the columns to recode to likert scoring 0-36
start_col <- which(names(hse_2001) == "ghqconc")
end_col <- which(names(hse_2001) == "ghqhappy")

# Now use these indices with the `:` operator to select the columns
cols <- names(hse_2001)[start_col:end_col]

# recode scores fit make 0-36 likert single score across the 12 questions
hse_2001 <- hse_2001 %>%
  mutate(across(all_of(cols), ~case_when(. == 1 ~ 0,
                                         . == 2 ~ 1,
                                         . == 3 ~ 2,
                                         . == 4 ~ 3),
                .names = "{.col}"))
hse_2001$ghq36scr <- rowSums(hse_2001[, start_col:end_col])

#Dealing with GHQG2 
#Authors have recoded as follows VALUE LABELS ghqg2 ///1 'Score 0' /// 2 'Score 1-3' /// 3 'Score 4+'.
# Recode 1 and 2 as No case and 3 as Case
hse_2001 <- hse_2001 %>%
  mutate(ghqg2 = fct_recode(as.factor(ghqg2), `No case` = '1', `No case` = '2', `Case Mental Health` = '3'))


# Recode variables
hse_2001 <- hse_2001 %>%
  mutate(Sex = fct_recode(as.factor(sex), `Male` = '1', `Female` = '2'),
         `Employment_Status` = fct_recode(as.factor(econact)),
         `Self_Rated_Health` = fct_recode(as.factor(hse_2001$genhelf2), `Very good/good` = '1',
                                          Fair = '2',
                                          `Bad/very bad` = '3'),
         `Highest_Edu_Attain` = fct_recode(as.factor(hse_2001$topqual3),
                                           `Degree and equivalent` = '1',
                                           `Below degree` = '2',
                                           `Below degree` = '3',
                                           `Below degree` = '4',
                                           `Below degree` = '5',
                                           `No degree` = '6',
                                           `No degree` = '7'))

# Drop unusused variables
start_col <- which(names(hse_2001) == "ghqconc")
end_col <- which(names(hse_2001) == "ghqhappy")
cols_to_exclude <- names(hse_2001)[start_col:end_col]
hse_2001 <- hse_2001[, !(names(hse_2001) %in% cols_to_exclude)]

#retain complete cases//// 1.6k observations removed
hse_2001_complete <- na.omit(hse_2001) %>%
  select(-sex) %>%
  rename(GHQg2 = ghqg2, Age = age, Weight = weight, GHQ36Scr = ghq36scr) %>%
  mutate(Age = cut(Age, breakpoints)) %>%
  mutate(Age = factor(Age, labels = c("16-24","25-34", "35-44", "45-54","55-64")),
  )
caption  <- "2001 descriptive summary table using Employment Status as explanatory variable"

# Table 1 (all outcome variables included to a table)
table1::table1(~ factor(year) + Age + Sex + Weight + Highest_Edu_Attain + factor(Self_Rated_Health) + GHQg2 + GHQ36Scr | Employment_Status, data = hse_2001_complete, caption=caption)

###################################################################################################################################################################################

#read raw data and assign NAs to values as defined by the authors
hse_2002_raw <- read.table("data/hse02ai.tab", header = T, sep = "\t", na.strings = c('-1', '-2', '-9', '-8'))


#select variables of interest and filter for desired age range
hse_2002 <- hse_2002_raw %>%
  filter(age > 15 & age < 65) %>%
  mutate(econact = fct_recode(as.factor(activb), `In employment` = '2', `Unemployment` = '3', `Unemployment` ='4',
                              `Unemployment` ='5' , `Unemployment` = '6',`Retired` = '8', `Retired` ='7', 
                              `Other economically inactive` = '1', `Retired` ='9', `Other economically inactive` ='11', `Other economically inactive` ='10'), 
         year = "2002") %>%
  select(year, pserial, age, sex, weight, econact, topqual3, ghqg2, genhelf2, 
         ghqconc, ghqdecis, ghqsleep, ghqconfi, ghquse, ghqstrai, ghqover, ghqenjoy, ghqface, ghqunhap, ghqworth, ghqhappy)

# define the columns to recode to likert scoring 0-36
start_col <- which(names(hse_2002) == "ghqconc")
end_col <- which(names(hse_2002) == "ghqhappy")

# Now use these indices with the `:` operator to select the columns
cols <- names(hse_2002)[start_col:end_col]

# recode scores fit make 0-36 likert single score across the 12 questions
hse_2002 <- hse_2002 %>%
  mutate(across(all_of(cols), ~case_when(. == 1 ~ 0,
                                         . == 2 ~ 1,
                                         . == 3 ~ 2,
                                         . == 4 ~ 3),
                .names = "{.col}"))
hse_2002$ghq36scr <- rowSums(hse_2002[, start_col:end_col])

#Dealing with GHQG2 
#Authors have recoded as follows VALUE LABELS ghqg2 ///1 'Score 0' /// 2 'Score 1-3' /// 3 'Score 4+'.
# Recode 1 and 2 as No case and 3 as Case
hse_2002 <- hse_2002 %>%
  mutate(ghqg2 = fct_recode(as.factor(ghqg2), `No case` = '1', `No case` = '2', `Case Mental Health` = '3'))


# Recode variables
hse_2002 <- hse_2002 %>%
  mutate(Sex = fct_recode(as.factor(sex), `Male` = '1', `Female` = '2'),
         `Employment_Status` = fct_recode(as.factor(econact)),
         `Self_Rated_Health` = fct_recode(as.factor(hse_2002$genhelf2), `Very good/good` = '1',
                                          Fair = '2',
                                          `Bad/very bad` = '3'),
         `Highest_Edu_Attain` = fct_recode(as.factor(hse_2002$topqual3),
                                           `Degree and equivalent` = '1',
                                           `Below degree` = '2',
                                           `Below degree` = '3',
                                           `Below degree` = '4',
                                           `Below degree` = '5',
                                           `No degree` = '6',
                                           `No degree` = '7'))

# Drop unusused variables
start_col <- which(names(hse_2002) == "ghqconc")
end_col <- which(names(hse_2002) == "ghqhappy")
cols_to_exclude <- names(hse_2002)[start_col:end_col]
hse_2002 <- hse_2002[, !(names(hse_2002) %in% cols_to_exclude)]


#retain complete cases//// 1.6k observations removed
hse_2002_complete <- na.omit(hse_2002) %>%
  rename(GHQg2 = ghqg2, Age = age, Weight = weight, GHQ36Scr = ghq36scr) %>%
  mutate(Age = cut(Age, breakpoints)) %>%
  mutate(Age = factor(Age, labels = c("16-24","25-34", "35-44", "45-54","55-64")),
  ) %>%
  select(-sex)
caption  <- "2002 descriptive summary table using Employment Status as explanatory variable"

# Table 1 (all outcome variables included to a table)
table1::table1(~ factor(year) + Age + Sex + Weight + Highest_Edu_Attain + factor(Self_Rated_Health) + GHQg2 + GHQ36Scr | Employment_Status, data = hse_2002_complete, caption=caption)

#####################################################################################################################################################################################

#read raw data and assign NAs to values as defined by the authors
hse_2003_raw <- read.table("data/hse03ai.tab", header = T, sep = "\t", na.strings = c('-1', '-2', '-9', '-8'))


#select variables of interest and filter for desired age range
hse_2003 <- hse_2003_raw %>%
  filter(age > 15 & age < 65) %>%
  mutate(econact = fct_recode(as.factor(activb), `In employment` = '2', `Unemployment` = '3', `Unemployment` ='4',
                              `Unemployment` ='5' , `Unemployment` = '6',`Retired` = '8', `Retired` ='7', 
                              `Other economically inactive` = '1', `Retired` ='9', `Other economically inactive` ='11', `Other economically inactive` ='10'), 
         year = "2003") %>%
  select(year, pserial, age, weight, sex, int_wt, econact, topqual3, ghqg2, genhelf2, 
         ghqconc, ghqdecis, ghqsleep, ghqconfi, ghquse, ghqstrai, ghqover, ghqenjoy, ghqface, ghqunhap, ghqworth, ghqhappy, mobility, selfcare, usualact, pain, anxiety)

# define the columns to recode to likert scoring 0-36
start_col <- which(names(hse_2003) == "ghqconc")
end_col <- which(names(hse_2003) == "ghqhappy")

# Now use these indices with the `:` operator to select the columns
cols <- names(hse_2003)[start_col:end_col]

# recode scores fit make 0-36 likert single score across the 12 questions
hse_2003 <- hse_2003 %>%
  mutate(across(all_of(cols), ~case_when(. == 1 ~ 0,
                                         . == 2 ~ 1,
                                         . == 3 ~ 2,
                                         . == 4 ~ 3),
                .names = "{.col}"))
hse_2003$ghq36scr <- rowSums(hse_2003[, start_col:end_col])

#Dealing with GHQG2 
#Authors have recoded as follows VALUE LABELS ghqg2 ///1 'Score 0' /// 2 'Score 1-3' /// 3 'Score 4+'.
# Recode 1 and 2 as No case and 3 as Case
hse_2003 <- hse_2003 %>%
  mutate(ghqg2 = fct_recode(as.factor(ghqg2), `No case` = '1', `No case` = '2', `Case Mental Health` = '3'))


#Dealing with EQ5 - which is on 3 levels based on unique values of the 5 variables associated
#make a string for each individual score across all 5 dimensions
scores <- str_c(hse_2003$mobility, hse_2003$selfcare, hse_2003$usualact, hse_2003$pain, hse_2003$anxiety)

# Calculate the EQ-5D-3L score using the UK TTO value set
hse_2003$EQMEAN <- eq5d(scores=scores, country="UK", version="3L", type="TTO", ignore.invalid = TRUE)


# Recode variables
hse_2003 <- hse_2003 %>%
  mutate(Sex = fct_recode(as.factor(sex), `Male` = '1', `Female` = '2'),
         Employment_Status = fct_recode(as.factor(econact)),
         Self_Rated_Health = fct_recode(as.factor(hse_2003$genhelf2), `Very good/good` = '1',
                                        Fair = '2',
                                        `Bad/very bad` = '3'),
         Highest_Edu_Attain = fct_recode(as.factor(hse_2003$topqual3),
                                         `Degree and equivalent` = '1',
                                         `Below degree` = '2',
                                         `Below degree` = '3',
                                         `Below degree` = '4',
                                         `Below degree` = '5',
                                         `No degree` = '6',
                                         `No degree` = '7'))
# Drop unusused variables
start_col <- which(names(hse_2003) == "ghqconc")
end_col <- which(names(hse_2003) == "anxiety")
cols_to_exclude <- names(hse_2003)[start_col:end_col]
hse_2003 <- hse_2003[, !(names(hse_2003) %in% cols_to_exclude)]

#retain complete cases//// 1.6k observations removed
hse_2003_complete <- na.omit(hse_2003) %>%
  rename(GHQg2 = ghqg2, Int_Weight = int_wt, Age = age, GHQ36Scr = ghq36scr, Weight = weight) %>%
  mutate(Age = cut(Age, breakpoints)) %>%
  mutate(Age = factor(Age, labels = c("16-24","25-34", "35-44", "45-54","55-64")),
  ) %>%
  select(-sex)
caption  <- "2003 descriptive summary table using Employment Status as explanatory variable"

# Table 1 (all outcome variables included to a table)
table1::table1(~ factor(year) + Age + Sex + Int_Weight + Highest_Edu_Attain + factor(Self_Rated_Health) + GHQg2 + GHQ36Scr + EQMEAN | Employment_Status, data = hse_2003_complete, caption=caption)

####################################################################################################################################################################################################

#read raw data and assign NAs to values as defined by the authors
hse_2004_raw <- read.table("data/hse04etha.tab", header = T, sep = "\t", na.strings = c('-1', '-2', '-9', '-8'))

#select variables of interest and filter for desired age range
hse_2004 <- hse_2004_raw %>%
  filter(age > 15 & age < 65) %>%
  mutate(econact = fct_recode(as.factor(activb), `In employment` = '2', `Unemployment` = '3', `Unemployment` ='4',
                              `Unemployment` ='5' , `Unemployment` = '6',`Retired` = '8', `Retired` ='7', 
                              `Other economically inactive` = '1', `Retired` ='9', `Other economically inactive` ='95', `Other economically inactive` ='10'), 
         year = "2004") %>%
  select(year, pserial, age, sex, weight, wt_int, econact, topqual3, GHQg2, genhelf2, 
         ghqconc, ghqdecis, ghqsleep, ghqconfi, ghquse, ghqstrai, ghqover, ghqenjoy, ghqface, ghqunhap, ghqworth, ghqhappy, mobility, selfcare, usualact, pain, anxiety)


# define the columns to recode to likert scoring 0-36
start_col <- which(names(hse_2004) == "ghqconc")
end_col <- which(names(hse_2004) == "ghqhappy")

# Now use these indices with the `:` operator to select the columns
cols <- names(hse_2004)[start_col:end_col]

# recode scores fit make 0-36 likert single score across the 12 questions
hse_2004 <- hse_2004 %>%
  mutate(across(all_of(cols), ~case_when(. == 1 ~ 0,
                                         . == 2 ~ 1,
                                         . == 3 ~ 2,
                                         . == 4 ~ 3),
                .names = "{.col}"))
hse_2004$ghq36scr <- rowSums(hse_2004[, start_col:end_col])

#Dealing with GHQG2 
#Authors have recoded as follows VALUE LABELS ghqg2 ///1 'Score 0' /// 2 'Score 1-3' /// 3 'Score 4+'.
# Recode 1 and 2 as No case and 3 as Case
hse_2004 <- hse_2004 %>%
  mutate(GHQg2 = fct_recode(as.factor(GHQg2), `No case` = '1', `No case` = '2', `Case Mental Health` = '3'))


#Dealing with EQ5 - which is on 3 levels based on unique values of the 5 variables associated
#make a string for each individual score across all 5 dimensions
scores <- str_c(hse_2004$mobility, hse_2004$selfcare, hse_2004$usualact, hse_2004$pain, hse_2004$anxiety)

# Calculate the EQ-5D-3L score using the UK TTO value set
hse_2004$EQMEAN <- eq5d(scores=scores, country="UK", version="3L", type="TTO", ignore.invalid = TRUE)


# Recode variables
hse_2004 <- hse_2004 %>%
  mutate(Sex = fct_recode(as.factor(sex), `Male` = '1', `Female` = '2'),
         `Employment_Status` = fct_recode(as.factor(econact)),
         `Self_Rated_Health` = fct_recode(as.factor(hse_2004$genhelf2), `Very good/good` = '1',
                                          Fair = '2',
                                          `Bad/very bad` = '3'),
         `Highest_Edu_Attain` = fct_recode(as.factor(hse_2004$topqual3),
                                           `Degree and equivalent` = '1',
                                           `Below degree` = '2',
                                           `Below degree` = '3',
                                           `Below degree` = '4',
                                           `Below degree` = '5',
                                           `No degree` = '6',
                                           `No degree` = '7'))

# Drop unusused variables
start_col <- which(names(hse_2004) == "ghqconc")
end_col <- which(names(hse_2004) == "anxiety")
cols_to_exclude <- names(hse_2004)[start_col:end_col]
hse_2004 <- hse_2004[, !(names(hse_2004) %in% cols_to_exclude)]


#retain complete cases//// 1.6k observations removed
hse_2004_complete <- na.omit(hse_2004) %>%
  rename(GHQg2 = GHQg2, Int_Weight = wt_int, Age = age, GHQ36Scr = ghq36scr, Weight = weight) %>%
  mutate(Age = cut(Age, breakpoints)) %>%
  mutate(Age = factor(Age, labels = c("16-24","25-34", "35-44", "45-54","55-64")),
  ) %>%
  select(-sex)
caption  <- "2004 descriptive summary table using Employment Status as explanatory variable"

# Table 1 (all outcome variables included to a table)
table1::table1(~ factor(year) + Age + Sex + Int_Weight + Highest_Edu_Attain + factor(Self_Rated_Health) + GHQg2 + GHQ36Scr + EQMEAN | Employment_Status, data = hse_2004_complete, caption=caption)

###################################################################################################################################################################################################

#read raw data and assign NAs to values as defined by the authors
hse_2005_raw <- read.table("data/hse05ai.tab", header = T, sep = "\t", na.strings = c('-1', '-2', '-9', '-8'))

#select variables of interest and filter for desired age range
hse_2005 <- hse_2005_raw %>%
  filter(age > 15 & age < 65) %>%
  mutate(econact = fct_recode(as.factor(activb), `In employment` = '2', `Unemployment` = '3', `Unemployment` ='4',
                              `Unemployment` ='5' , `Unemployment` = '6',`Retired` = '8', `Retired` ='7', 
                              `Other economically inactive` = '1', `Retired` ='9', `Other economically inactive` ='95', `Other economically inactive` ='10'), 
         year = "2005") %>%
  select(year, pserial, age, sex, weight, wt_int, econact, topqual3, GHQg2, genhelf2, 
         ghqconc, ghqdecis, ghqsleep, ghqconfi, ghquse, ghqstrai, ghqover, ghqenjoy, ghqface, ghqunhap, ghqworth, ghqhappy, mobility, selfcare, usualact, pain, anxiety)


# define the columns to recode to likert scoring 0-36
start_col <- which(names(hse_2005) == "ghqconc")
end_col <- which(names(hse_2005) == "ghqhappy")

# Now use these indices with the `:` operator to select the columns
cols <- names(hse_2005)[start_col:end_col]

# recode scores fit make 0-36 likert single score across the 12 questions
hse_2005 <- hse_2005 %>%
  mutate(across(all_of(cols), ~case_when(. == 1 ~ 0,
                                         . == 2 ~ 1,
                                         . == 3 ~ 2,
                                         . == 4 ~ 3),
                .names = "{.col}"))
hse_2005$ghq36scr <- rowSums(hse_2005[, start_col:end_col])

#Dealing with GHQG2 
#Authors have recoded as follows VALUE LABELS ghqg2 ///1 'Score 0' /// 2 'Score 1-3' /// 3 'Score 4+'.
# Recode 1 and 2 as No case and 3 as Case
hse_2005 <- hse_2005 %>%
  mutate(GHQg2 = fct_recode(as.factor(GHQg2), `No case` = '1', `No case` = '2', `Case Mental Health` = '3'))


#Dealing with EQ5 - which is on 3 levels based on unique values of the 5 variables associated
#make a string for each individual score across all 5 dimensions
scores <- str_c(hse_2005$mobility, hse_2005$selfcare, hse_2005$usualact, hse_2005$pain, hse_2005$anxiety)

# Calculate the EQ-5D-3L score using the UK TTO value set
hse_2005$EQMEAN <- eq5d(scores=scores, country="UK", version="3L", type="TTO", ignore.invalid = TRUE)


# Recode variables
hse_2005 <- hse_2005 %>%
  mutate(Sex = fct_recode(as.factor(sex), `Male` = '1', `Female` = '2'),
         `Employment_Status` = fct_recode(as.factor(econact)),
         `Self_Rated_Health` = fct_recode(as.factor(hse_2005$genhelf2), `Very good/good` = '1',
                                          Fair = '2',
                                          `Bad/very bad` = '3'),
         `Highest_Edu_Attain` = fct_recode(as.factor(hse_2005$topqual3),
                                           `Degree and equivalent` = '1',
                                           `Below degree` = '2',
                                           `Below degree` = '3',
                                           `Below degree` = '4',
                                           `Below degree` = '5',
                                           `No degree` = '6',
                                           `No degree` = '7'))
# Drop unusused variables
start_col <- which(names(hse_2005) == "ghqconc")
end_col <- which(names(hse_2005) == "anxiety")
cols_to_exclude <- names(hse_2005)[start_col:end_col]
hse_2005 <- hse_2005[, !(names(hse_2005) %in% cols_to_exclude)]


#retain complete cases//// 1.6k observations removed
hse_2005_complete <- na.omit(hse_2005) %>%
  rename(GHQg2 = GHQg2, Int_Weight = wt_int, Age = age, GHQ36Scr = ghq36scr, Weight = weight) %>%
  mutate(Age = cut(Age, breakpoints)) %>%
  mutate(Age = factor(Age, labels = c("16-24","25-34", "35-44", "45-54","55-64")),
  ) %>%
  select(-sex)
caption  <- "2005 descriptive summary table using Employment Status as explanatory variable"

# Table 1 (all outcome variables included to a table)
table1::table1(~ factor(year) + Age + Sex + Int_Weight + Highest_Edu_Attain + factor(Self_Rated_Health) + GHQg2 + GHQ36Scr + EQMEAN | Employment_Status, data = hse_2005_complete, caption=caption)

###################################################################################################################################################################################################


#read raw data and assign NAs to values as defined by the authors
hse_2006_raw <- read.table("data/hse06ai.tab", header = T, sep = "\t", na.strings = c('-1', '-2', '-9', '-8'))


#select variables of interest and filter for desired age range
hse_2006 <- hse_2006_raw %>%
  filter(age > 15 & age < 65) %>%
  mutate(econact = fct_recode(as.factor(activb), `In employment` = '2', `Unemployment` = '3', `Unemployment` ='4',
                              `Unemployment` ='5' , `Unemployment` = '6',`Retired` = '8', `Retired` ='7', 
                              `Other economically inactive` = '1', `Retired` ='9', `Other economically inactive` ='95', `Other economically inactive` ='10'), 
         year = "2006") %>%
  select(year, pserial, age, sex, weight, wt_int, econact, topqual3, GHQg2, genhelf2, 
         ghqconc, ghqdecis, ghqsleep, ghqconfi, ghquse, ghqstrai, ghqover, ghqenjoy, ghqface, ghqunhap, ghqworth, ghqhappy, mobility, selfcare, usualact, pain, anxiety)


# define the columns to recode to likert scoring 0-36
start_col <- which(names(hse_2006) == "ghqconc")
end_col <- which(names(hse_2006) == "ghqhappy")

# Now use these indices with the `:` operator to select the columns
cols <- names(hse_2006)[start_col:end_col]

# recode scores fit make 0-36 likert single score across the 12 questions
hse_2006 <- hse_2006 %>%
  mutate(across(all_of(cols), ~case_when(. == 1 ~ 0,
                                         . == 2 ~ 1,
                                         . == 3 ~ 2,
                                         . == 4 ~ 3),
                .names = "{.col}"))
hse_2006$ghq36scr <- rowSums(hse_2006[, start_col:end_col])

#Dealing with GHQG2 
#Authors have recoded as follows VALUE LABELS ghqg2 ///1 'Score 0' /// 2 'Score 1-3' /// 3 'Score 4+'.
# Recode 1 and 2 as No case and 3 as Case
hse_2006 <- hse_2006 %>%
  mutate(GHQg2 = fct_recode(as.factor(GHQg2), `No case` = '1', `No case` = '2', `Case Mental Health` = '3'))

#Dealing with EQ5 - which is on 3 levels based on unique values of the 5 variables associated
#make a string for each individual score across all 5 dimensions
scores <- str_c(hse_2006$mobility, hse_2006$selfcare, hse_2006$usualact, hse_2006$pain, hse_2006$anxiety)

# Calculate the EQ-5D-3L score using the UK TTO value set
hse_2006$EQMEAN <- eq5d(scores=scores, country="UK", version="3L", type="TTO", ignore.invalid = TRUE)


# Recode variables
hse_2006 <- hse_2006 %>%
  mutate(Sex = fct_recode(as.factor(sex), `Male` = '1', `Female` = '2'),
         `Employment_Status` = fct_recode(as.factor(econact)),
         `Self_Rated_Health` = fct_recode(as.factor(hse_2006$genhelf2), `Very good/good` = '1',
                                          Fair = '2',
                                          `Bad/very bad` = '3'),
         `Highest_Edu_Attain` = fct_recode(as.factor(hse_2006$topqual3),
                                           `Degree and equivalent` = '1',
                                           `Below degree` = '2',
                                           `Below degree` = '3',
                                           `Below degree` = '4',
                                           `Below degree` = '5',
                                           `No degree` = '6',
                                           `No degree` = '7'))

# Drop unusused variables
start_col <- which(names(hse_2006) == "ghqconc")
end_col <- which(names(hse_2006) == "anxiety")
cols_to_exclude <- names(hse_2006)[start_col:end_col]
hse_2006 <- hse_2006[, !(names(hse_2006) %in% cols_to_exclude)]


#retain complete cases//// 1.6k observations removed
hse_2006_complete <- na.omit(hse_2006) %>%
  rename(GHQg2 = GHQg2, Int_Weight = wt_int, Age = age, GHQ36Scr = ghq36scr, Weight = weight) %>%
  mutate(Age = cut(Age, breakpoints)) %>%
  mutate(Age = factor(Age, labels = c("16-24","25-34", "35-44", "45-54","55-64")),
  ) %>%
  select(-sex)
caption  <- "2006 descriptive summary table using Employment Status as explanatory variable"

# Table 1 (all outcome variables included to a table)
table1::table1(~ factor(year) + Age + Sex + Int_Weight + Highest_Edu_Attain + factor(Self_Rated_Health) + GHQg2 + GHQ36Scr + EQMEAN | Employment_Status, data = hse_2006_complete, caption=caption)

###################################################################################################################################################################################################

#read raw data and assign NAs to values as defined by the authors
hse_2007_raw <- read.table("data/hse07ai.tab", header = T, sep = "\t", na.strings = c('-1', '-2', '-9', '-8'))


#select variables of interest and filter for desired age range
hse_2007 <- hse_2007_raw %>%
  filter(age > 15 & age < 65) %>%
  mutate(econact = fct_recode(as.factor(activb), `In employment` = '2', `Unemployment` = '3', `Unemployment` ='4',
                              `Unemployment` ='5' , `Unemployment` = '6',`Retired` = '8', `Retired` ='7', 
                              `Other economically inactive` = '1', `Retired` ='9', `Other economically inactive` ='95', `Other economically inactive` ='10'), 
         year = "2007") %>%
  select(year, pserial, age, sex, weight, wt_int, econact, topqual3, genhelf2)

# Recode variables
hse_2007 <- hse_2007 %>%
  mutate(Sex = fct_recode(as.factor(sex), `Male` = '1', `Female` = '2'),
         `Employment_Status` = fct_recode(as.factor(econact)),
         `Self_Rated_Health` = fct_recode(as.factor(hse_2007$genhelf2), `Very good/good` = '1',
                                          Fair = '2',
                                          `Bad/very bad` = '3'),
         `Highest_Edu_Attain` = fct_recode(as.factor(hse_2007$topqual3),
                                           `Degree and equivalent` = '1',
                                           `Below degree` = '2',
                                           `Below degree` = '3',
                                           `Below degree` = '4',
                                           `Below degree` = '5',
                                           `No degree` = '6',
                                           `No degree` = '7'))

#retain complete cases//// 1.6k observations removed
hse_2007_complete <- na.omit(hse_2007) %>%
  rename(Int_Weight = wt_int, Age = age, Weight = weight) %>%
  mutate(Age = cut(Age, breakpoints)) %>%
  mutate(Age = factor(Age, labels = c("16-24","25-34", "35-44", "45-54","55-64")),
  ) %>%
  select(-sex)
caption  <- "2007 descriptive summary table using Employment Status as explanatory variable"

# Table 1 (all outcome variables included to a table)
table1::table1(~ factor(year) + Age + Sex + Int_Weight + Highest_Edu_Attain + factor(Self_Rated_Health) | Employment_Status, data = hse_2007_complete, caption=caption)

##########################################################################################################################################################################

#read raw data and assign NAs to values as defined by the authors
hse_2008_raw <- read.table("data/hse08ai.tab", header = T, sep = "\t", na.strings = c('-1', '-2', '-9', '-8'))

#select variables of interest and filter for desired age range
hse_2008 <- hse_2008_raw %>%
  filter(age > 15 & age < 65) %>%
  mutate(econact = fct_recode(as.factor(activb), `In employment` = '2', `Unemployment` = '3', `Unemployment` ='4',
                              `Unemployment` ='5' , `Unemployment` = '6',`Retired` = '8', `Retired` ='7', 
                              `Other economically inactive` = '1', `Retired` ='9', `Other economically inactive` ='95', `Other economically inactive` ='10'), 
         year = "2008") %>%
  select(year, pserial, age, sex, weight, wt_int, econact, topqual3, GHQg2, genhelf2, 
         ghqconc, ghqdecis, ghqsleep, ghqconfi, ghquse, ghqstrai, ghqover, ghqenjoy, ghqface, ghqunhap, ghqworth, ghqhappy, mobility, selfcare, usualact, pain, anxiety)

# define the columns to recode to likert scoring 0-36
start_col <- which(names(hse_2008) == "ghqconc")
end_col <- which(names(hse_2008) == "ghqhappy")

# Now use these indices with the `:` operator to select the columns
cols <- names(hse_2008)[start_col:end_col]

# recode scores fit make 0-36 likert single score across the 12 questions
hse_2008 <- hse_2008 %>%
  mutate(across(all_of(cols), ~case_when(. == 1 ~ 0,
                                         . == 2 ~ 1,
                                         . == 3 ~ 2,
                                         . == 4 ~ 3),
                .names = "{.col}"))
hse_2008$ghq36scr <- rowSums(hse_2008[, start_col:end_col])

#Dealing with GHQG2 
#Authors have recoded as follows VALUE LABELS ghqg2 ///1 'Score 0' /// 2 'Score 1-3' /// 3 'Score 4+'.
# Recode 1 and 2 as No case and 3 as Case
hse_2008 <- hse_2008 %>%
  mutate(GHQg2 = fct_recode(as.factor(GHQg2), `No case` = '1', `No case` = '2', `Case Mental Health` = '3'))


#Dealing with EQ5 - which is on 3 levels based on unique values of the 5 variables associated
#make a string for each individual score across all 5 dimensions
scores <- str_c(hse_2008$mobility, hse_2008$selfcare, hse_2008$usualact, hse_2008$pain, hse_2008$anxiety)

# Calculate the EQ-5D-3L score using the UK TTO value set
hse_2008$EQMEAN <- eq5d(scores=scores, country="UK", version="3L", type="TTO", ignore.invalid = TRUE)


# Recode variables
hse_2008 <- hse_2008 %>%
  mutate(Sex = fct_recode(as.factor(sex), `Male` = '1', `Female` = '2'),
         Employment_Status = fct_recode(as.factor(econact)),
         Self_Rated_Health = fct_recode(as.factor(hse_2008$genhelf2), `Very good/good` = '1',
                                        Fair = '2',
                                        `Bad/very bad` = '3'),
         Highest_Edu_Attain = fct_recode(as.factor(hse_2008$topqual3),
                                         `Degree and equivalent` = '1',
                                         `Below degree` = '2',
                                         `Below degree` = '3',
                                         `Below degree` = '4',
                                         `Below degree` = '5',
                                         `No degree` = '6',
                                         `No degree` = '7'))
# Drop unusused variables
start_col <- which(names(hse_2008) == "ghqconc")
end_col <- which(names(hse_2008) == "anxiety")
cols_to_exclude <- names(hse_2008)[start_col:end_col]
hse_2008 <- hse_2008[, !(names(hse_2008) %in% cols_to_exclude)]

#retain complete cases//// 1.6k observations removed
hse_2008_complete <- na.omit(hse_2008) %>%
  rename(GHQg2 = GHQg2, Int_Weight = wt_int, Age = age, GHQ36Scr = ghq36scr, Weight = weight) %>%
  mutate(Age = cut(Age, breakpoints)) %>%
  mutate(Age = factor(Age, labels = c("16-24","25-34", "35-44", "45-54","55-64")),
  ) %>%
  select(-sex)
caption  <- "2008 descriptive summary table using Employment Status as explanatory variable"

# Table 1 (all outcome variables included to a table)
table1::table1(~ factor(year) + Age + Sex + Int_Weight + Highest_Edu_Attain + factor(Self_Rated_Health) + GHQg2 + GHQ36Scr + EQMEAN | Employment_Status, data = hse_2008_complete, caption=caption)

###################################################################################################################################################################################################

#read raw data and assign NAs to values as defined by the authors
hse_2009_raw <- read.table("data/hse09ai.tab", header = T, sep = "\t", na.strings = c('-1', '-2', '-9', '-8'))


#select variables of interest and filter for desired age range
hse_2009 <- hse_2009_raw %>%
  filter(age > 15 & age < 65) %>%
  mutate(econact = fct_recode(as.factor(activb), `In employment` = '2', `Unemployment` = '3', `Unemployment` ='4',
                              `Unemployment` ='5' , `Unemployment` = '6',`Retired` = '8', `Retired` ='7', 
                              `Other economically inactive` = '1', `Retired` ='9', `Other economically inactive` ='95', `Other economically inactive` ='10'), 
         year = "2009") %>%
  select(year, pserial, age, sex, weight, wt_int, econact, topqual3, GHQg2, genhelf2, 
         ghqconc, ghqdecis, ghqsleep, ghqconfi, ghquse, ghqstrai, ghqover, ghqenjoy, ghqface, ghqunhap, ghqworth, ghqhappy)


# define the columns to recode to likert scoring 0-36
start_col <- which(names(hse_2009) == "ghqconc")
end_col <- which(names(hse_2009) == "ghqhappy")

# Now use these indices with the `:` operator to select the columns
cols <- names(hse_2009)[start_col:end_col]

# recode scores fit make 0-36 likert single score across the 12 questions
hse_2009 <- hse_2009 %>%
  mutate(across(all_of(cols), ~case_when(. == 1 ~ 0,
                                         . == 2 ~ 1,
                                         . == 3 ~ 2,
                                         . == 4 ~ 3),
                .names = "{.col}"))
hse_2009$ghq36scr <- rowSums(hse_2009[, start_col:end_col])

#Dealing with GHQG2 
#Authors have recoded as follows VALUE LABELS ghqg2 ///1 'Score 0' /// 2 'Score 1-3' /// 3 'Score 4+'.
# Recode 1 and 2 as No case and 3 as Case
hse_2009 <- hse_2009 %>%
  mutate(GHQg2 = fct_recode(as.factor(GHQg2), `No case` = '1', `No case` = '2', `Case Mental Health` = '3'))


# Recode variables
hse_2009 <- hse_2009 %>%
  mutate(Sex = fct_recode(as.factor(sex), `Male` = '1', `Female` = '2'),
         Employment_Status = fct_recode(as.factor(econact)),
         Self_Rated_Health = fct_recode(as.factor(hse_2009$genhelf2), `Very good/good` = '1',
                                        Fair = '2',
                                        `Bad/very bad` = '3'),
         Highest_Edu_Attain = fct_recode(as.factor(hse_2009$topqual3),
                                         `Degree and equivalent` = '1',
                                         `Below degree` = '2',
                                         `Below degree` = '3',
                                         `Below degree` = '4',
                                         `Below degree` = '5',
                                         `No degree` = '6',
                                         `No degree` = '7'))

# Drop unusused variables
start_col <- which(names(hse_2009) == "ghqconc")
end_col <- which(names(hse_2009) == "ghqhappy")
cols_to_exclude <- names(hse_2009)[start_col:end_col]
hse_2009 <- hse_2009[, !(names(hse_2009) %in% cols_to_exclude)]

#retain complete cases//// 1.6k observations removed
hse_2009_complete <- na.omit(hse_2009) %>%
  rename(GHQg2 = GHQg2, Int_Weight = wt_int, Age = age, GHQ36Scr = ghq36scr, Weight = weight) %>%
  mutate(Age = cut(Age, breakpoints)) %>%
  mutate(Age = factor(Age, labels = c("16-24","25-34", "35-44", "45-54","55-64")),
  ) %>%
  select(-sex)
caption  <- "2009 descriptive summary table using Employment Status as explanatory variable"

# Table 1 (all outcome variables included to a table)
table1::table1(~ factor(year) + Age + Sex + Int_Weight + Highest_Edu_Attain + factor(Self_Rated_Health) + GHQg2 + GHQ36Scr | Employment_Status, data = hse_2009_complete, caption=caption)

##########################################################################################################################################################################################

#read raw data and assign NAs to values as defined by the authors
hse_2010_raw <- read.table("data/hse10ai.tab", header = T, sep = "\t", na.strings = c('-1', '-2', '-9', '-8', '-90'))

#select variables of interest and filter for desired age range
hse_2010 <- hse_2010_raw %>%
  filter(age > 15 & age < 65) %>%
  mutate(econact = fct_recode(as.factor(activb), `In employment` = '2', `Unemployment` = '3', `Unemployment` ='4',
                              `Unemployment` ='5' , `Unemployment` = '6',`Retired` = '8', `Retired` ='7', 
                              `Other economically inactive` = '1', `Retired` ='9', `Other economically inactive` ='95', `Other economically inactive` ='10'), 
         year = "2010") %>%
  select(year, pserial, age, sex, weight, wt_int, econact, topqual3, ghqg2, genhelf2, 
         ghqconc, ghqdecis, ghqsleep, ghqconfi, ghquse, ghqstrai, ghqover, ghqenjoy, ghqface, ghqunhap, ghqworth, ghqhappy, mobility, selfcare, usualact, pain, anxiety)

# define the columns to recode to likert scoring 0-36
start_col <- which(names(hse_2010) == "ghqconc")
end_col <- which(names(hse_2010) == "ghqhappy")

# Now use these indices with the `:` operator to select the columns
cols <- names(hse_2010)[start_col:end_col]

# recode scores fit make 0-36 likert single score across the 12 questions
hse_2010 <- hse_2010 %>%
  mutate(across(all_of(cols), ~case_when(. == 1 ~ 0,
                                         . == 2 ~ 1,
                                         . == 3 ~ 2,
                                         . == 4 ~ 3),
                .names = "{.col}"))
hse_2010$ghq36scr <- rowSums(hse_2010[, start_col:end_col])

#Dealing with ghqg2 
#Authors have recoded as follows VALUE LABELS ghqg2 ///1 'Score 0' /// 2 'Score 1-3' /// 3 'Score 4+'.
# Recode 1 and 2 as No case and 3 as Case
hse_2010 <- hse_2010 %>%
  mutate(ghqg2 = fct_recode(as.factor(ghqg2), `No case` = '1', `No case` = '2', `Case Mental Health` = '3'))


#Dealing with EQ5 - which is on 3 levels based on unique values of the 5 variables associated
#make a string for each individual score across all 5 dimensions
scores <- str_c(hse_2010$mobility, hse_2010$selfcare, hse_2010$usualact, hse_2010$pain, hse_2010$anxiety)
# Calculate the EQ-5D-3L score using the UK TTO value set
hse_2010$EQMEAN <- eq5d(scores=scores, country="UK", version="3L", type="TTO", ignore.invalid = TRUE)


# Recode variables
hse_2010 <- hse_2010 %>%
  mutate(Sex = fct_recode(as.factor(sex), `Male` = '1', `Female` = '2'),
         `Employment_Status` = fct_recode(as.factor(econact)),
         `Self_Rated_Health` = fct_recode(as.factor(hse_2010$genhelf2), `Very good/good` = '1',
                                          Fair = '2',
                                          `Bad/very bad` = '3'),
         `Highest_Edu_Attain` = fct_recode(as.factor(hse_2010$topqual3),
                                           `Degree and equivalent` = '1',
                                           `Below degree` = '2',
                                           `Below degree` = '3',
                                           `Below degree` = '4',
                                           `Below degree` = '5',
                                           `No degree` = '6',
                                           `No degree` = '7'))


# Drop unusused variables
start_col <- which(names(hse_2010) == "ghqconc")
end_col <- which(names(hse_2010) == "anxiety")
cols_to_exclude <- names(hse_2010)[start_col:end_col]
hse_2010 <- hse_2010[, !(names(hse_2010) %in% cols_to_exclude)]


#retain complete cases//// 1.4k observations removed
hse_2010_complete <- na.omit(hse_2010) %>%
  rename(GHQg2 = ghqg2, Int_Weight = wt_int, Age = age, GHQ36Scr = ghq36scr, Weight = weight) %>%
  mutate(Age = cut(Age, breakpoints)) %>%
  mutate(Age = factor(Age, labels = c("16-24","25-34", "35-44", "45-54","55-64")),
  ) %>%
  select(-sex)
caption  <- "2010 descriptive summary table using Employment Status as explanatory variable"

# Table 1 (all outcome variables included to a table)
table1::table1(~ factor(year) + Age + Sex + Int_Weight + Highest_Edu_Attain + factor(Self_Rated_Health) + GHQg2 + GHQ36Scr | Employment_Status, data = hse_2010_complete, caption=caption)

##########################################################################################################################################################################################

#read raw data and assign NAs to values as defined by the authors
hse_2011_raw <- read.table("data/hse2011ai.tab", header = T, sep = "\t", na.strings = c('-1', '-2', '-9', '-8'))


#select variables of interest and filter for desired age range
hse_2011 <- hse_2011_raw %>%
  filter(Age > 15 & Age < 65) %>%
  mutate(econact = fct_recode(as.factor(activb), `In employment` = '2', `Unemployment` = '3', `Unemployment` ='4',
                              `Unemployment` ='5' , `Unemployment` = '6',`Retired` = '8', `Retired` ='7', 
                              `Other economically inactive` = '1', `Retired` ='9', `Other economically inactive` ='95', `Other economically inactive` ='10'), 
         year = "2011") %>%
  select(year, pserial, Age, Sex, Weight, wt_int, econact, topqual3, genhelf2, 
         Mobility, Selfcare, UsualAct, Pain, Anxiety)

# Now use these indices with the `:` operator to select the columns
cols <- names(hse_2011)[start_col:end_col]

#Dealing with EQ5 - which is on 3 levels based on unique values of the 5 variables associated
#make a string for each individual score across all 5 dimensions
scores <- str_c(hse_2011$Mobility, hse_2011$Selfcare, hse_2011$UsualAct, hse_2011$Pain, hse_2011$Anxiety)

# Calculate the EQ-5D-3L score using the UK TTO value set
hse_2011$EQMEAN <- eq5d(scores=scores, country="UK", version="3L", type="TTO", ignore.invalid = TRUE)


# Recode variables
hse_2011 <- hse_2011 %>%
  mutate(Sex = fct_recode(as.factor(Sex), `Male` = '1', `Female` = '2'),
         Employment_Status = fct_recode(as.factor(econact)),
         Self_Rated_Health = fct_recode(as.factor(hse_2011$genhelf2), `Very good/good` = '1',
                                        Fair = '2',
                                        `Bad/very bad` = '3'),
         Highest_Edu_Attain = fct_recode(as.factor(hse_2011$topqual3),
                                         `Degree and equivalent` = '1',
                                         `Below degree` = '2',
                                         `Below degree` = '3',
                                         `Below degree` = '4',
                                         `Below degree` = '5',
                                         `No degree` = '6',
                                         `No degree` = '7'))
# Drop unusused variables
start_col <- which(names(hse_2011) == "Mobility")
end_col <- which(names(hse_2011) == "Anxiety")
cols_to_exclude <- names(hse_2011)[start_col:end_col]
hse_2011 <- hse_2011[, !(names(hse_2011) %in% cols_to_exclude)]

#retain complete cases//// 1.6k observations removed
hse_2011_complete <- na.omit(hse_2011) %>%
  rename(Int_Weight = wt_int) %>%
  mutate(Age = cut(Age, breakpoints)) %>%
  mutate(Age = factor(Age, labels = c("16-24","25-34", "35-44", "45-54","55-64")),
  )

caption  <- "2011 descriptive summary table using Employment Status as explanatory variable"

# Table 1 (all outcome variables included to a table)
table1::table1(~ factor(year) + Age + Sex + Int_Weight + Highest_Edu_Attain + factor(Self_Rated_Health) + EQMEAN | Employment_Status, data = hse_2011_complete, caption=caption)

################################################################################################################################################################################

#read raw data and assign NAs to values as defined by the authors
hse_2012_raw <- read.table("data/hse2012ai.tab", header = T, sep = "\t", na.strings = c('-1', '-2', '-9', '-8'))


#select variables of interest and filter for desired age range
hse_2012 <- hse_2012_raw %>%
  filter(Age > 15 & Age < 65) %>%
  mutate(econact = fct_recode(as.factor(activb), `In employment` = '2', `Unemployment` = '3', `Unemployment` ='4',
                              `Unemployment` ='5' , `Unemployment` = '6',`Retired` = '8', `Retired` ='7', 
                              `Other economically inactive` = '1', `Retired` ='9', `Other economically inactive` ='95', `Other economically inactive` ='10'), 
         year = "2012") %>%
  select(year, pserial, Age, Sex, Weight, wt_int, econact, topqual3, GHQg2, genhelf2, 
         GHQCONC, GHQDECIS, GHQSLEEP, GHQCONFI, GHQUSE, GHQSTRAI, GHQOVER, GHQENJOY, GHQFACE, GHQUNHAP, GHQWORTH, GHQHAPPY, Mobility, Selfcare, UsualAct, Pain, Anxiety)

# define the columns to recode to likert scoring 0-36
start_col <- which(names(hse_2012) == "GHQCONC")
end_col <- which(names(hse_2012) == "GHQHAPPY")

# Now use these indices with the `:` operator to select the columns
cols <- names(hse_2012)[start_col:end_col]

# recode scores fit make 0-36 likert single score across the 12 questions
hse_2012 <- hse_2012 %>%
  mutate(across(all_of(cols), ~case_when(. == 1 ~ 0,
                                         . == 2 ~ 1,
                                         . == 3 ~ 2,
                                         . == 4 ~ 3),
                .names = "{.col}"))
hse_2012$ghq36scr <- rowSums(hse_2012[, start_col:end_col])

#Dealing with GHQG2 
#Authors have recoded as follows VALUE LABELS ghqg2 ///1 'Score 0' /// 2 'Score 1-3' /// 3 'Score 4+'.
# Recode 1 and 2 as No case and 3 as Case
hse_2012 <- hse_2012 %>%
  mutate(GHQg2 = fct_recode(as.factor(GHQg2), `No case` = '1', `No case` = '2', `Case Mental Health` = '3'))


#Dealing with EQ5 - which is on 3 levels based on unique values of the 5 variables associated
#make a string for each individual score across all 5 dimensions
scores <- str_c(hse_2012$Mobility, hse_2012$Selfcare, hse_2012$UsualAct, hse_2012$Pain, hse_2012$Anxiety)

# Calculate the EQ-5D-3L score using the UK TTO value set
hse_2012$EQMEAN <- eq5d(scores=scores, country="UK", version="3L", type="TTO", ignore.invalid = TRUE)


# Recode variables
hse_2012 <- hse_2012 %>%
  mutate(Sex = fct_recode(as.factor(Sex), `Male` = '1', `Female` = '2'),
         `Employment_Status` = fct_recode(as.factor(econact)),
         `Self_Rated_Health` = fct_recode(as.factor(hse_2012$genhelf2), `Very good/good` = '1',
                                          Fair = '2',
                                          `Bad/very bad` = '3'),
         `Highest_Edu_Attain` = fct_recode(as.factor(hse_2012$topqual3),
                                           `Degree and equivalent` = '1',
                                           `Below degree` = '2',
                                           `Below degree` = '3',
                                           `Below degree` = '4',
                                           `Below degree` = '5',
                                           `No degree` = '6',
                                           `No degree` = '7'))
# Drop unusused variables
start_col <- which(names(hse_2012) == "GHQCONC")
end_col <- which(names(hse_2012) == "Anxiety")
cols_to_exclude <- names(hse_2012)[start_col:end_col]
hse_2012 <- hse_2012[, !(names(hse_2012) %in% cols_to_exclude)]

#retain complete cases//// 1.6k observations removed
hse_2012_complete <- na.omit(hse_2012) %>%
  rename(GHQg2 = GHQg2, Int_Weight = wt_int, GHQ36Scr = ghq36scr) %>%
  mutate(Age = cut(Age, breakpoints)) %>%
  mutate(Age = factor(Age, labels = c("16-24","25-34", "35-44", "45-54","55-64")),
  )
caption  <- "2012 descriptive summary table using Employment Status as explanatory variable"

# Table 1 (all outcome variables included to a table)
table1::table1(~ factor(year) + Age + Sex + Int_Weight + Highest_Edu_Attain + factor(Self_Rated_Health) + GHQg2 + GHQ36Scr + EQMEAN | Employment_Status, data = hse_2012_complete, caption=caption)

###################################################################################################################################################################################################

#read raw data and assign NAs to values as defined by the authors
hse_2013_raw <- read.table("data/hse2013ai.tab", header = T, sep = "\t", na.strings = c('-1', '-2', '-9', '-8'))


#select variables of interest and filter for desired age range
hse_2013 <- hse_2013_raw %>%
  filter(Age > 15 & Age < 65) %>%
  mutate(econact = fct_recode(as.factor(Activb), `In employment` = '2', `Unemployment` = '3', `Unemployment` ='4',
                              `Unemployment` ='5' , `Retired` = '8', `Retired` ='7', 
                              `Other economically inactive` = '1', `Other economically inactive` = '6', 
                              `Other economically inactive` ='9', `Other economically inactive` ='95', `Other economically inactive` ='10'), 
         year = "2013") %>%
  select(year, pserial, Age, Sex, Weight, wt_int, econact, topqual3, genhelf2)

# Recode variables
hse_2013 <- hse_2013 %>%
  mutate(Sex = fct_recode(as.factor(Sex), `Male` = '1', `Female` = '2'),
         `Employment_Status` = fct_recode(as.factor(econact)),
         `Self_Rated_Health` = fct_recode(as.factor(hse_2013$genhelf2), `Very good/good` = '1',
                                          Fair = '2',
                                          `Bad/very bad` = '3'),
         `Highest_Edu_Attain` = fct_recode(as.factor(hse_2013$topqual3),
                                           `Degree and equivalent` = '1',
                                           `Below degree` = '2',
                                           `Below degree` = '3',
                                           `Below degree` = '4',
                                           `Below degree` = '5',
                                           `No degree` = '6',
                                           `No degree` = '7'))

#retain complete cases//// 1.6k observations removed
hse_2013_complete <- na.omit(hse_2013) %>%
  rename(Int_Weight = wt_int) %>%
  mutate(Age = cut(Age, breakpoints)) %>%
  mutate(Age = factor(Age, labels = c("16-24","25-34", "35-44", "45-54","55-64")),
  )
caption  <- "2013 descriptive summary table using Employment Status as explanatory variable"

# Table 1 (all outcome variables included to a table)
table1::table1(~ factor(year) + Age + Sex + Int_Weight + Highest_Edu_Attain + factor(Self_Rated_Health) | Employment_Status, data = hse_2013_complete, caption=caption)

#######################################################################################################################################################################

#read raw data and assign NAs to values as defined by the authors

hse_2014_raw <- read.table("data/hse2014ai.tab", fill = TRUE, header = T, sep = "\t", na.strings = c('-1', '-2', '-9', '-8'))

#select variables of interest and filter for desired age range
hse_2014 <- hse_2014_raw %>%
  filter(Age90 > 15 & Age90 < 65) %>%
  mutate(econact = fct_recode(as.factor(Activb), `In employment` = '2', `Unemployment` = '3', `Unemployment` ='4',
                              `Unemployment` ='5' , `Retired` = '8', `Retired` ='7', 
                              `Other economically inactive` = '1', `Other economically inactive` = '6', 
                              `Other economically inactive` ='9', `Other economically inactive` ='95', `Other economically inactive` ='10')) %>%
  mutate(year = "2014") %>%
  select(year, pserial, Age90, Sex, Weight, wt_int, econact, topqual3, GHQg2, genhelf2, 
         GHQCONC, GHQDECIS, GHQSLEEP, GHQCONFI, GHQUSE, GHQSTRAI, GHQOVER, GHQENJOY, GHQFACE, GHQUNHAP, GHQWORTH, GHQHAPPY, Mobility, Selfcare, UsualAct, Pain, Anxiety)

# define the columns to recode to likert scoring 0-36
start_col <- which(names(hse_2014) == "GHQCONC")
end_col <- which(names(hse_2014) == "GHQHAPPY")

# Now use these indices with the `:` operator to select the columns
cols <- names(hse_2014)[start_col:end_col]

# recode scores fit make 0-36 likert single score across the 12 questions
hse_2014 <- hse_2014 %>%
  mutate(across(all_of(cols), ~case_when(. == 1 ~ 0,
                                         . == 2 ~ 1,
                                         . == 3 ~ 2,
                                         . == 4 ~ 3),
                .names = "{.col}"))
hse_2014$ghq36scr <- rowSums(hse_2014[, start_col:end_col])

#Dealing with GHQG2 
#Authors have recoded as follows VALUE LABELS ghqg2 ///1 'Score 0' /// 2 'Score 1-3' /// 3 'Score 4+'.
# Recode 1 and 2 as No case and 3 as Case
hse_2014 <- hse_2014 %>%
  mutate(GHQg2 = fct_recode(as.factor(GHQg2), `No case` = '1', `No case` = '2', `Case Mental Health` = '3'))


#Dealing with EQ5 - which is on 3 levels based on unique values of the 5 variables associated
#make a string for each individual score across all 5 dimensions
scores <- str_c(hse_2014$Mobility, hse_2014$Selfcare, hse_2014$UsualAct, hse_2014$Pain, hse_2014$Anxiety)

# Calculate the EQ-5D-3L score using the UK TTO value set
hse_2014$EQMEAN <- eq5d(scores=scores, country="UK", version="3L", type="TTO", ignore.invalid = TRUE)


# Recode variables
hse_2014 <- hse_2014 %>%
  mutate(Sex = fct_recode(as.factor(Sex), `Male` = '1', `Female` = '2'),
         Employment_Status = fct_recode(as.factor(econact)),
         Self_Rated_Health = fct_recode(as.factor(hse_2014$genhelf2), `Very good/good` = '1',
                                        Fair = '2',
                                        `Bad/very bad` = '3'),
         Highest_Edu_Attain = fct_recode(as.factor(hse_2014$topqual3),
                                         `Degree and equivalent` = '1',
                                         `Below degree` = '2',
                                         `Below degree` = '3',
                                         `Below degree` = '4',
                                         `Below degree` = '5',
                                         `No degree` = '7',
                                         `No degree` ='6'))
# Drop unusused variables
start_col <- which(names(hse_2014) == "GHQCONC")
end_col <- which(names(hse_2014) == "Anxiety")
cols_to_exclude <- names(hse_2014)[start_col:end_col]
hse_2014 <- hse_2014[, !(names(hse_2014) %in% cols_to_exclude)]

#retain complete cases//// 1.6k observations removed
hse_2014_complete <- na.omit(hse_2014) %>%
  rename(GHQg2 = GHQg2, Int_Weight = wt_int, Age = Age90, GHQ36Scr = ghq36scr) %>%
  mutate(Age = cut(Age, breakpoints)) %>%
  mutate(Age = factor(Age, labels = c("16-24","25-34", "35-44", "45-54","55-64")),
  )
caption  <- "2014 descriptive summary table using Employment Status as explanatory variable"

# Table 1 (all outcome variables included to a table)
table1::table1(~ factor(year) + Age + Sex + Int_Weight + Highest_Edu_Attain + factor(Self_Rated_Health) + GHQg2 + GHQ36Scr + EQMEAN | Employment_Status, data = hse_2014_complete, caption=caption)

###################################################################################################################################################################################################

#read raw data and assign NAs to values as defined by the authors

hse_2015_raw <- read.table("data/hse2015ai.tab", fill = TRUE, header = T, sep = "\t", na.strings = c('-1', '-2', '-9', '-8'))

#select variables of interest and filter for desired age range
hse_2015 <- hse_2015_raw %>%
  filter (ag16g10 %in% c(1,2,3,4,5)) %>%
  mutate(econact = fct_recode(as.factor(Activb2), `In employment` = '2', `Unemployment` = '3', `Unemployment` ='4',
                              `Unemployment` ='5' , `Retired` = '8', `Retired` ='7', 
                              `Other economically inactive` = '1', `Other economically inactive` = '6', 
                              `Other economically inactive` ='9', `Other economically inactive` ='95'), 
         year = "2015",
         Age = fct_recode(as.factor(ag16g10), `16-24` = '1', `25-34` = '2', `35-44` = '3', `45-54` = '4', `55-64` = '5')) %>%
  select(year, SerialA, Age, Sex, Weight, wt_int, topqual3, genhelf2, econact)


# Recode variables
hse_2015 <- hse_2015 %>%
  mutate(Sex = fct_recode(as.factor(Sex), `Male` = '1', `Female` = '2'),
         Employment_Status = fct_recode(as.factor(econact)),
         Self_Rated_Health = fct_recode(as.factor(genhelf2), `Very good/good` = '1',
                                        Fair = '2',
                                        `Bad/very bad` = '3'),
         Highest_Edu_Attain = fct_recode(as.factor(topqual3),
                                         `Degree and equivalent` = '1',
                                         `Below degree` = '2',
                                         `Below degree` = '3',
                                         `Below degree` = '4',
                                         `Below degree` = '5',
                                         `No degree` = '7',
                                         `No degree` ='6'))


#retain complete cases//// 1.6k observations removed
hse_2015_complete <- na.omit(hse_2015) %>%
  rename(Int_Weight = wt_int)

caption  <- "2015 descriptive summary table using Employment Status as explanatory variable"

# Table 1 (all outcome variables included to a table)
table1::table1(~ factor(year) + Grp_Age + Sex + Int_Weight + Highest_Edu_Attain + factor(Self_Rated_Health) | Employment_Status, data = hse_2015_complete, caption=caption)

###########################################################################################################################################################################

#read raw data and assign NAs to values as defined by the authors
hse_2016_raw <- read.table("data/hse2016_eul.tab", header = T, sep = "\t", na.strings = c('-1', '-2', '-9', '-8'))


#select variables of interest and filter for desired age range
hse_2016 <- hse_2016_raw %>%
  filter (ag16g10 %in% c(1,2,3,4,5)) %>%
  mutate(econact = fct_recode(as.factor(Activb2), `In employment` = '2', `Unemployment` = '3', `Unemployment` ='4',
                              `Unemployment` ='5' , `Retired` = '8', `Retired` ='7', 
                              `Other economically inactive` = '1', `Other economically inactive` = '6', 
                              `Other economically inactive` ='9', `Other economically inactive` ='95'), 
         year = "2016",
         Age = fct_recode(as.factor(ag16g10), `16-24` = '1', `25-34` = '2', `35-44` = '3', `45-54` = '4', `55-64` = '5')) %>%
  select(year, SerialA, Age, Sex, Weight, wt_int, econact, topqual3, GHQg2, genhelf2, 
         GHQCONC, GHQDECIS, GHQSLEEP, GHQCONFI, GHQUSE, GHQSTRAI, GHQOVER, GHQENJOY, GHQFACE, GHQUNHAP, GHQWORTH, GHQHAPPY)

# define the columns to recode to likert scoring 0-36
start_col <- which(names(hse_2016) == "GHQCONC")
end_col <- which(names(hse_2016) == "GHQHAPPY")

# Now use these indices with the `:` operator to select the columns
cols <- names(hse_2016)[start_col:end_col]

# recode scores fit make 0-36 likert single score across the 12 questions
hse_2016 <- hse_2016 %>%
  mutate(across(all_of(cols), ~case_when(. == 1 ~ 0,
                                         . == 2 ~ 1,
                                         . == 3 ~ 2,
                                         . == 4 ~ 3),
                .names = "{.col}"))
hse_2016$ghq36scr <- rowSums(hse_2016[, start_col:end_col])

#Dealing with GHQG2 
#Authors have recoded as follows VALUE LABELS ghqg2 ///1 'Score 0' /// 2 'Score 1-3' /// 3 'Score 4+'.
# Recode 1 and 2 as No case and 3 as Case
hse_2016 <- hse_2016 %>%
  mutate(GHQg2 = fct_recode(as.factor(GHQg2), `No case` = '1', `No case` = '2', `Case Mental Health` = '3'))

# Recode variables
hse_2016 <- hse_2016 %>%
  mutate(Sex = fct_recode(as.factor(Sex), `Male` = '1', `Female` = '2'),
         Employment_Status = fct_recode(as.factor(econact)),
         Self_Rated_Health = fct_recode(as.factor(hse_2016$genhelf2), `Very good/good` = '1',
                                        Fair = '2',
                                        `Bad/very bad` = '3'),
         Highest_Edu_Attain = fct_recode(as.factor(hse_2016$topqual3),
                                         `Degree and equivalent` = '1',
                                         `Below degree` = '2',
                                         `Below degree` = '3',
                                         `Below degree` = '4',
                                         `Below degree` = '5',
                                         `No degree` = '6',
                                         `No degree` = '7'))

# Drop unusused variables
start_col <- which(names(hse_2016) == "GHQCONC")
end_col <- which(names(hse_2016) == "GHQHAPPY")
cols_to_exclude <- names(hse_2016)[start_col:end_col]
hse_2016 <- hse_2016[, !(names(hse_2016) %in% cols_to_exclude)]

#retain complete cases//// 1.6k observations removed
hse_2016_complete <- na.omit(hse_2016) %>%
  rename(GHQg2 = GHQg2, Int_Weight = wt_int,  GHQ36Scr = ghq36scr)
caption  <- "2016 descriptive summary table using Employment Status as explanatory variable"

# Table 1 (all outcome variables included to a table)
table1::table1(~ factor(year) + Grp_Age + Sex + Int_Weight + Highest_Edu_Attain + factor(Self_Rated_Health) + GHQg2 + GHQ36Scr | Employment_Status, data = hse_2016_complete, caption=caption)

#########################################################################################################################################################

#read raw data and assign NAs to values as defined by the authors

hse_2017_raw <- read.table("data/hse17i_eul_v1.tab", fill = TRUE, header = T, sep = "\t", na.strings = c('-1', '-2', '-9', '-8'))

#select variables of interest and filter for desired age range
hse_2017 <- hse_2017_raw %>%
  filter (ag16g10 %in% c(1,2,3,4,5)) %>%
  mutate(econact = fct_recode(as.factor(Activb2), `In employment` = '2', `Unemployment` = '3', `Unemployment` ='4',
                              `Unemployment` ='5' , `Retired` = '8', `Retired` ='7', 
                              `Other economically inactive` = '1', `Other economically inactive` = '6', 
                              `Other economically inactive` ='9', `Other economically inactive` ='95'), 
         year = "2017",
         Age = fct_recode(as.factor(ag16g10), `16-24` = '1', `25-34` = '2', `35-44` = '3', `45-54` = '4', `55-64` = '5')) %>%
  select(year, SerialA, Age, Sex, Weight, wt_int, econact, topqual3, genhelf2, 
         Mobil17, SelfCa17, UsualA17, Pain17, Anxiet17)

#Dealing with EQ5 - which is on 3 levels based on unique values of the 5 variables associated
#make a string for each individual score across all 5 dimensions
scores <- str_c(hse_2017$Mobil17, hse_2017$SelfCa17, hse_2017$UsualA17, hse_2017$Pain17, hse_2017$Anxiet17)

# Calculate the EQ-5D-3L score using the UK TTO value set
hse_2017$EQMEAN <- eq5d(scores=scores, country="UK", version="3L", type="TTO", ignore.invalid = TRUE)


# Recode variables
hse_2017 <- hse_2017 %>%
  mutate(Sex = fct_recode(as.factor(Sex), `Male` = '1', `Female` = '2'),
         Employment_Status = fct_recode(as.factor(econact)),
         Self_Rated_Health = fct_recode(as.factor(hse_2017$genhelf2), `Very good/good` = '1',
                                        Fair = '2',
                                        `Bad/very bad` = '3'),
         Highest_Edu_Attain = fct_recode(as.factor(hse_2017$topqual3),
                                         `Degree and equivalent` = '1',
                                         `Below degree` = '2',
                                         `Below degree` = '3',
                                         `Below degree` = '4',
                                         `Below degree` = '5',
                                         `No degree` = '6',
                                         `No degree` = '7'))
# Drop unusused variables
start_col <- which(names(hse_2017) == "Mobil17")
end_col <- which(names(hse_2017) == "Anxiet17")
cols_to_exclude <- names(hse_2017)[start_col:end_col]
hse_2017 <- hse_2017[, !(names(hse_2017) %in% cols_to_exclude)]

#retain complete cases//// 1.6k observations removed
hse_2017_complete <- na.omit(hse_2017) %>%
  rename(Int_Weight = wt_int,  Sex=Sex)
caption  <- "2017 descriptive summary table using Employment Status as explanatory variable"

# Table 1 (all outcome variables included to a table)
table1::table1(~ factor(year) + Grp_Age + Sex + Int_Weight + Highest_Edu_Attain + factor(Self_Rated_Health) + EQMEAN | Employment_Status, data = hse_2017_complete, caption=caption)

#####################################################################################################################################################################################


#read raw data and assign NAs to values as defined by the authors
hse_2018_raw <- read.table("data/hse_2018_eul_15082022.tab", header = T, sep = "\t", na.strings = c('-1', '-2', '-9', '-8'))


#select variables of interest and filter for desired age range
hse_2018 <- hse_2018_raw %>%
  filter (ag16g10 %in% c(1,2,3,4,5)) %>%
  mutate(econact = fct_recode(as.factor(Activb2), `In employment` = '2', `Unemployment` = '3', `Unemployment` ='4',
                              `Unemployment` ='5' , `Retired` = '8', `Retired` ='7', 
                              `Other economically inactive` = '1', `Other economically inactive` = '6', 
                              `Other economically inactive` ='9', `Other economically inactive` ='95'), 
         year = "2018",
         Age = fct_recode(as.factor(ag16g10), `16-24` = '1', `25-34` = '2', `35-44` = '3', `45-54` = '4', `55-64` = '5')) %>%
  select(year, Seriala, Age, Sex, Weight, wt_int, econact, topqual3, GHQG2, GenHelf2, 
         GHQCONC, GHQDECIS, GHQSLEEP, GHQCONFI, GHQUSE, GHQSTRAI, GHQOVER, GHQENJOY, GHQFACE, GHQUNHAP, GHQWORTH, GHQHAPPY, Mobil17, SelfCa17, UsualA17, Pain17, Anxiet17)

# define the columns to recode to likert scoring 0-36
start_col <- which(names(hse_2018) == "GHQCONC")
end_col <- which(names(hse_2018) == "GHQHAPPY")

# Now use these indices with the `:` operator to select the columns
cols <- names(hse_2018)[start_col:end_col]

# recode scores fit make 0-36 likert single score across the 12 questions
hse_2018 <- hse_2018 %>%
  mutate(across(all_of(cols), ~case_when(. == 1 ~ 0,
                                         . == 2 ~ 1,
                                         . == 3 ~ 2,
                                         . == 4 ~ 3),
                .names = "{.col}"))
hse_2018$ghq36scr <- rowSums(hse_2018[, start_col:end_col])

#Dealing with GHQG2 
#Authors have recoded as follows VALUE LABELS ghqg2 ///1 'Score 0' /// 2 'Score 1-3' /// 3 'Score 4+'.
# Recode 1 and 2 as No case and 3 as Case
hse_2018 <- hse_2018 %>%
  mutate(GHQG2 = fct_recode(as.factor(GHQG2), `No case` = '1', `No case` = '2', `Case Mental Health` = '3'))


#Dealing with EQ5 - which is on 3 levels based on unique values of the 5 variables associated
#make a string for each individual score across all 5 dimensions
scores <- str_c(hse_2018$Mobil17, hse_2018$SelfCa17, hse_2018$UsualA17, hse_2018$Pain17, hse_2018$Anxiet17)

# Calculate the EQ-5D-3L score using the UK TTO value set
hse_2018$EQMEAN <- eq5d(scores=scores, country="UK", version="3L", type="TTO", ignore.invalid = TRUE)


# Recode variables
hse_2018 <- hse_2018 %>%
  mutate(Sex = fct_recode(as.factor(Sex), `Male` = '1', `Female` = '2'),
         Employment_Status = fct_recode(as.factor(econact)),
         Self_Rated_Health = fct_recode(as.factor(hse_2018$GenHelf2), `Very good/good` = '1',
                                        Fair = '2',
                                        `Bad/very bad` = '3'),
         Highest_Edu_Attain = fct_recode(as.factor(hse_2018$topqual3),
                                         `Degree and equivalent` = '1',
                                         `Below degree` = '2',
                                         `Below degree` = '3',
                                         `Below degree` = '4',
                                         `Below degree` = '5',
                                         `No degree` = '6',
                                         `No degree` = '7'))
# Drop unusused variables
start_col <- which(names(hse_2018) == "GHQCONC")
end_col <- which(names(hse_2018) == "Anxiet17")
cols_to_exclude <- names(hse_2018)[start_col:end_col]
hse_2018 <- hse_2018[, !(names(hse_2018) %in% cols_to_exclude)]

#retain complete cases//// 1.6k observations removed
hse_2018_complete <- na.omit(hse_2018) %>%
  rename(GHQg2 = GHQG2, Int_Weight = wt_int, GHQ36Scr = ghq36scr, genhelf2 = GenHelf2)
caption  <- "2018 descriptive summary table using Employment Status as explanatory variable"

# Table 1 (all outcome variables included to a table)
table1::table1(~ factor(year) + Grp_Age + Sex + Int_Weight + Highest_Edu_Attain + factor(Self_Rated_Health) + GHQg2 + GHQ36Scr + EQMEAN | Employment_Status, data = hse_2018_complete, caption=caption)

#######################################################################################################################################################################################################


#read raw data and assign NAs to values as defined by the authors
hse_2019_raw <- read.table("data/hse_2019_eul_20211006.tab", header = T, sep = "\t", na.strings = c('-1', '-2', '-9', '-8'))


#select variables of interest and filter for desired age range
hse_2019 <- hse_2019_raw %>%
  filter (ag16g10 %in% c(1,2,3,4,5)) %>%
  mutate(econact = fct_recode(as.factor(Activb2), `In employment` = '2', `Unemployment` = '3', `Unemployment` ='4',
                              `Unemployment` ='5' , `Retired` = '8', `Retired` ='7', 
                              `Other economically inactive` = '1', `Other economically inactive` = '6', 
                              `Other economically inactive` ='9', `Other economically inactive` ='95'), 
         year = "2019",
         Age = fct_recode(as.factor(ag16g10), `16-24` = '1', `25-34` = '2', `35-44` = '3', `45-54` = '4', `55-64` = '5')) %>%
  select(year, SerialA, Age, Sex, Weight, wt_int, econact, topqual3, GenHelf2) 

# Recode variables
hse_2019 <- hse_2019 %>%
  mutate(Sex = fct_recode(as.factor(Sex), `Male` = '1', `Female` = '2'),
         Employment_Status = fct_recode(as.factor(econact)),
         Self_Rated_Health = fct_recode(as.factor(hse_2019$GenHelf2), `Very good/good` = '1',
                                        Fair = '2',
                                        `Bad/very bad` = '3'),
         Highest_Edu_Attain = fct_recode(as.factor(hse_2019$topqual3),
                                         `Degree and equivalent` = '1',
                                         `Below degree` = '2',
                                         `Below degree` = '3',
                                         `Below degree` = '4',
                                         `Below degree` = '5',
                                         `No degree` = '6',
                                         `No degree` = '7'))

#retain complete cases//// 1.6k observations removed
hse_2019_complete <- na.omit(hse_2019) %>%
  rename(Int_Weight = wt_int, genhelf2 = GenHelf2)
caption  <- "2019 descriptive summary table using Employment Status as explanatory variable"

# Table 1 (all outcome variables included to a table)
table1::table1(~ factor(year) + Age + Sex + Int_Weight + Highest_Edu_Attain + factor(Self_Rated_Health) | Employment_Status, data = hse_2019_complete, caption=caption)

###########################################################################################################################################################################



all_years <- bind_rows(hse_2000_complete, hse_2001_complete, hse_2002_complete, hse_2003_complete, 
                   hse_2004_complete, hse_2005_complete, hse_2006_complete, hse_2007_complete, 
                   hse_2008_complete, hse_2009_complete, hse_2010_complete, hse_2011_complete, 
                   hse_2012_complete, hse_2013_complete, hse_2014_complete, hse_2015_complete,
                   hse_2016_complete, hse_2017_complete, hse_2018_complete, hse_2019_complete)

caption  <- " Years 2000 to 2019 Combined descriptive summary table using Year as explanatory variable"

# Table 1 (all outcome variables included to a table)
table1::table1(~ Age + factor(Grp_Age) + Sex + Int_Weight + Highest_Edu_Attain + factor(Self_Rated_Health) + GHQg2 + GHQ36Scr + EQMEAN + Employment_Status| factor(year), data = all_years, caption=caption)

write.csv(all_years, "allyears.csv")
