#starting from here to do the final cleaning of the lab dataset 
#loading the pavckages 
library(haven)
library(stringr)
library(dplyr)
library(lubridate)
#setting the working directory 
setwd("C:/LAB_19_10_2026")
#choosing a file
library(haven)
LAB <- read_dta("C:/LAB_19_10_2026/LAB.dta")

# Browsing and cleaning record_id and study_id 
LAB %>%
select(record_id, study_id) %>%
View()
#Checking for the duplicates
LAB$record_id <- gsub("\\s+", "", trimws(as.character(LAB$record_id)))
#checking for the duplicates
anyDuplicated(LAB$record_id)

#checking for the study id 
#firstly removing the spaces
LAB$study_id <- gsub("\\s+", "", trimws(as.character(LAB$study_id)))
#checking for the duplicates
anyDuplicated(LAB$study_id)
#Checking where study id is not equal to record id 
LAB %>%
filter(!is.na(study_id),!is.na(record_id),study_id != as.character(record_id)) %>%
select(record_id, study_id, regno, ancnumber) %>%
View()
#correcting theses ids with extensions
LAB$record_id <- sub("--.*$", "", as.character(LAB$record_id))
#CONFIRMING AGAIN
LAB %>%
filter(!is.na(study_id),!is.na(record_id),study_id != as.character(record_id)) %>%
select(record_id, study_id, regno, ancnumber) %>%
View()
#Correcting that particular id
LAB$study_id[LAB$study_id=="71_R18_24M"]<-"71_R18_24F"
#confirming again that record id and study id are equal
LAB %>%
filter(!is.na(study_id),!is.na(record_id),study_id != as.character(record_id)) %>%
select(record_id, study_id, regno, ancnumber) %>%
View()
# Drop record_id because its same as study id
LAB <- LAB %>%
select(-record_id)
# Check study_id and cleaning it
LAB %>%
select(study_id) %>%
View()
#labelling the study id
attr(LAB$study_id, "label") <- "Unique Patient ID"

# Frequency table for ANC number
table(LAB$ancnumber, useNA = "ifany")
#Finding all anc numbers containing some characters
library(dplyr)
library(stringr)
#browising the record with texts as anc numbers
LAB %>%
filter(!is.na(ancnumber),str_trim(ancnumber) != "",str_detect(str_trim(ancnumber), "[^0-9]")) %>%
select(study_id, regno, ancnumber) %>%
View()
#correcting the anc number using the row position
library(dplyr)

# 1. Correct the ANC numbers using the original row positions
LAB <- LAB %>%
mutate(ancnumber = case_when(row_number() == 2416 & ancnumber == "May-22"  ~ "5",row_number() == 5848 & ancnumber == "571/23"  ~ "571",row_number() == 998  & ancnumber == "160/20"  ~ "160",row_number() == 2019 & ancnumber == "19/23"   ~ "19",row_number() == 7359 & ancnumber == "002/23"  ~ "002",row_number() == 7761 & ancnumber == "1011/24" ~ "1011",TRUE ~ ancnumber))
#confirming after removing them 
LAB %>%
  filter(!is.na(ancnumber),str_trim(ancnumber) != "",str_detect(str_trim(ancnumber), "[^0-9]")) %>%
  select(study_id, regno, ancnumber) %>%
  View()
#correcting this record that was not corrected 
LAB$ancnumber[LAB$study_id == "18_R1_23M"] <- "5"

# Remove leading, trailing and repeated internal spaces in ANC number  
LAB <- LAB %>%
mutate(ancnumber = str_squish(ancnumber))
#labelng the anc number
attr(LAB$ancnumber, "label") <- "ANC number"
# Inspect maternity numbers
table(LAB$mat_nmbr, useNA = "ifany")
#checking if there are characters in mat number
LAB %>%
filter(!is.na(mat_nmbr),str_trim(mat_nmbr) != "",str_detect(str_trim(mat_nmbr), "[^0-9]")) %>%
select(study_id, regno, mat_nmbr) %>%
View()
#correcting these maternity number because the upper ones are the ones considered to be mat numbers 
LAB$mat_nmbr[LAB$study_id %in% c("105_R6_23M", "27_R46_24M")] <- c("50", "244")
#confirming after correcting them 
LAB %>%
filter(!is.na(mat_nmbr),str_trim(mat_nmbr) != "",str_detect(str_trim(mat_nmbr), "[^0-9]")) %>%
select(study_id, regno, mat_nmbr) %>%
View()
#renaming the mat_number
LAB<-LAB%>%
rename(maternity_number=mat_nmbr)
#labeling the maternity number 
attr(LAB$maternity_number, "label") <- "Maternity number"
#cleaning the facility variable
#label valuing the facility
attr(LAB$facilty, "label") <- "Facility name"
#coding the facilty
#Label defining the facility
LAB$facilty<-dplyr::case_when(LAB$facilty=="Mayuge"~ 1,LAB$facilty=="Buluba"~2,TRUE~NA_real_)
#attaching the labels
LAB$facilty<-haven::labelled(LAB$facilty,labels = c("Mayuge"=1,"Buluba"=2),label="facility")
#checking 
#attaching the label
attr(LAB$facilty,"label")
#checking the labels
attr(LAB$facilty,"labels")

#cleaning the registernumber
attr(LAB$regno, "label") <- "Register Number"
#confirming 
attr(LAB$regno, "label")
#labellingthe entry date in the register 
attr(LAB$entry_date, "label") <- "Date of entry in register"
#working on the entry date in the database 
attr(LAB$date_2, "label") <- "Date of entry in database"
#cleaning the lab number 
#  Tabulate lab_no
table(LAB$lab_no, useNA = "ifany")
#checking if we have non numeric lab_nos for correction
LAB %>%
filter(!is.na(lab_no),str_trim(lab_no) != "",str_detect(str_trim(lab_no), "[^0-9]")) %>%
select(study_id, regno, lab_no) %>%
View()
#correcting the one displayed 48_R32_24M to lab no 82
LAB$lab_no[LAB$study_id == "48_R32_24M"] <- "82"
#confirming that the lab_no are corrected
LAB %>%
filter(!is.na(lab_no),str_trim(lab_no) != "",str_detect(str_trim(lab_no), "[^0-9]")) %>%
select(study_id, regno, lab_no) %>%
View()
#renaming the lab_no to laboratory number 
LAB<-LAB%>%
rename(laboratory_no=lab_no)
#labelling the lab no 
attr(LAB$laboratory_no,"label")<-"Lab number"
#refining the mother's name
# Check missing values
sum(is.na(LAB$mothers_name) | trimws(LAB$mothers_name) == "")
# Remove leading/trailing spaces and extra spaces between names
LAB$mothers_name <- trimws(gsub("\\s+", " ", as.character(LAB$mothers_name)))
# Convert blank strings to NA
LAB$mothers_name[LAB$mothers_name == ""] <- NA
# Apply variable label
install.packages("sjlabelled")
library(sjlabelled)
set_label(LAB$mothers_name) <- "Mother's name"
# Check it
label(LAB$mothers_name)
#turning them to proper lower cases
LAB <- LAB %>%mutate(mothers_name = str_to_title(str_squish(str_to_lower(mothers_name))))
# Verify
LAB %>%
  select(study_id, regno, mothers_name) %>%
  View()
#Cleaning the age of the mother
table(LAB$mother_age, useNA = "ifany")
summary(LAB$mother_age)
# Setting all ages <= 9 to missing
LAB$mother_age[
  !is.na(LAB$mother_age) & LAB$mother_age <= 9
] <- NA_real_
#labelling the age 
attr(LAB$mother_age, "label") <- "Age of the mother"
#summarising the age after removing the outliers
summary(LAB$mother_age)
#cleaning the malaria microscopy
table(LAB$mal_microscopy, useNA = "ifany")
#Label defining the varible
LAB$mal_microscopy<-dplyr::case_when(LAB$mal_microscopy=="mps++"~ 2,LAB$mal_microscopy=="No mps seen"~1,TRUE~NA_real_)
#attaching the labels
LAB$mal_microscopy<-haven::labelled(LAB$mal_microscopy,labels = c("No mps seen"=1,"mps++"=2),label="Malaria Microscopy")
#checking 
table(LAB$mal_microscopy, useNA = "ifany")
#checking for labels
attr(LAB$mal_microscopy,"label")
#checking for the labels
attr(LAB$mal_microscopy, "labels")
###cleaning the rtds_mal
table(LAB$rdts_mal, useNA = "ifany")
#labeling the rdts mals
LAB$rdts_mal<-dplyr::case_when(LAB$rdts_mal=="Positive"~1,LAB$rdts_mal=="Negative"~2,TRUE~NA_real_)
#attaching the labels(
LAB$rdts_mal<-haven::labelled(LAB$rdts_mal,labels = c("Positive"=1,"Negative"=2),label="Malaria RDTS")
#checking whether the labels were attached
attr(LAB$rdts_mal,"label")
#tabulating the rtds variable
table(LAB$rdts_mal, useNA = "ifany")
#checking the labels
attr(LAB$rdts_mal,"labels")
table(LAB$rdts_mal, useNA = "ifany")

#cleaning the protein results 
#Checking the labels for the protein results
unique(LAB$protein_results)
#grouping and categorising the results 
LAB <- LAB %>%
mutate(Protein_testResults = case_when(protein_results %in% c("Nil", "Non reactive") ~ "Non reactive",protein_results == "Reactive" ~ "Reactive",TRUE ~ NA_character_))
#then dropping protein_results after categorising it 
LAB <- LAB %>%
select(-protein_results)
#labeling the protein results and attaching the labels
attr(LAB$Protein_testResults,"labels")
#attaching the labels
LAB$Protein_testResults <- haven::labelled(dplyr::case_when(trimws(tolower(as.character(LAB$Protein_testResults))) == "reactive" ~ 1,trimws(tolower(as.character(LAB$Protein_testResults))) == "non reactive" ~ 2,TRUE ~ NA_real_),labels = c("Reactive" = 1, "Non reactive" = 2),label = "Protein results")
#checking whether the labels have been attached
haven::print_labels(LAB$Protein_testResults)
unique(LAB$Protein_testResults)
#cleaning the sugar levels 
LAB <- LAB %>%
mutate(sugar_testresults = case_when(sugar_results %in% c("Nil", "Non reactive") ~ "Non reactive",sugar_results == "Reactive" ~ "Reactive",TRUE ~ NA_character_))
#dropping the sugar_results variable after cleaning 
LAB <- LAB %>%
select(-sugar_results)
#checkng for the sugar_testresults
unique(LAB$sugar_testresults)
#label defining the variable
LAB$sugar_testresults<-dplyr::case_when(LAB$sugar_testresults=="Reactive"~ 1,LAB$sugar_testresults=="Non reactive"~2,TRUE~NA_real_)
#attaching the labels
LAB$sugar_testresults<-haven::labelled(LAB$sugar_testresults,labels = c("Reactive"=1,"Non reactive"=2),label="Sugar results")
#confirming that labels have been attached
haven::print_labels(LAB$sugar_testresults)
#checking the frequencies
table(haven::as_factor(LAB$sugar_testresults), useNA = "always")
#cleaning the sugar results 
unique(LAB$sugar_levels)
#labelling sugar levels
attr(LAB$sugar_levels,"label")<-"Sugar levels"

#checking for the rbs
unique(LAB$rbs_9da2cc)
#labelling the rbs variable
attr(LAB$rbs_9da2cc,"label")<-"RBS"
#labeling the missing questions #aquestion that asks whether there are missing questions in the register 
attr(LAB$missing_questions,"label")<-"Were any of the questions not answered?"
#labeling the variable showing the missing questions 
attr(LAB$list_questions,"label")<-"List the missing questions"
#checking for the questions that were missing 
unique(LAB$list_questions)
#checking for the entrant variable
attr(LAB$entered_by,"label")<-"Data entered by"
#checking the frequencies
unique(LAB$entered_by)
#correcting mariaamt to mariamt
LAB$entered_by[LAB$entered_by == "mariaamt"] <- "mariamt"
#checking for the status of register completeness
attr(LAB$lab_form_v2_complete,"label")<-"Complete"
#checking the frequesncies
table(LAB$lab_form_v2_complete,useNA = "always")

#selecting the final dataset for analysis
lab_analysis <- LAB %>%
  select(study_id, 
         ancnumber,
         maternity_number,
         facilty,
         entry_date,
         laboratory_no,
         mothers_name,
         mother_age,
         mal_microscopy,
         rdts_mal,
         Protein_testResults,
         sugar_testresults,
         sugar_levels,
         rbs_9da2cc)

lab_analysis <- lab_analysis %>%
mutate(across(where(haven::is.labelled), haven::as_factor))
#Saving it as csv file to export it
write.csv(lab_analysis,"C:/LAB_19_10_2026/lab_analysis.csv",row.names = FALSE,na = "")




















