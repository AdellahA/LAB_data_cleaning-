*setting the working dirctory
cd "C:/LAB_19_10_2026"
*loading the data
use "C:/LAB_19_10_2026/LAB.dta", clear
* Inspecting and cleaning the two IDs
browse record_id study_id
* Remove leading/trailing spaces and all internal whitespace
replace record_id = ustrregexra(strtrim(record_id), "[[:space:]]+", "") if !missing(record_id)
* Check duplicate record IDs
duplicates report record_id
duplicates tag record_id, gen(dup_record_id)
tabulate dup_record_id, missing
* Remove the temporary duplicate indicator
drop dup_record_id
* Clean study_id in the same way
replace study_id = ustrregexra(strtrim(study_id), "[[:space:]]+", "") if !missing(study_id)

* Check duplicate study IDs
duplicates report study_id
duplicates tag study_id, gen(dup_study_id)
tabulate dup_study_id, missing
drop dup_study_id
* Check records where study_id differs from record_id
browse record_id study_id regno ancnumber if !missing(study_id) & !missing(record_id) & study_id != record_id
* Remove extensions beginning with --
replace record_id = regexr(record_id, "--.*$", "") if !missing(record_id)

* Confirm mismatches after removing extensions
browse record_id study_id regno ancnumber if !missing(study_id) & !missing(record_id) & study_id != record_id

* Correct the particular study ID
replace study_id = "71_R18_24F" if study_id == "71_R18_24M"

* Confirm that the IDs now agree
browse record_id study_id regno ancnumber if !missing(study_id) & !missing(record_id) & study_id != record_id

* Drop record_id because it duplicates study_id
drop record_id

* Inspect and label study_id
browse study_id
label variable study_id "Unique Patient ID"

* 3. Clean ANC number
* Frequency table, including missing values
tabulate ancnumber, missing

* Browse ANC numbers containing non-numeric characters
browse study_id regno ancnumber if !missing(ancnumber) & strtrim(ancnumber) != "" & ustrregexm(strtrim(ancnumber), "[^0-9]")

* Correct ANC numbers using their original observation positions
replace ancnumber = "5"    if _n == 2416 & ancnumber == "May-22"
replace ancnumber = "571"  if _n == 5848 & ancnumber == "571/23"
replace ancnumber = "160"  if _n == 998  & ancnumber == "160/20"
replace ancnumber = "19"   if _n == 2019 & ancnumber == "19/23"
replace ancnumber = "002"  if _n == 7359 & ancnumber == "002/23"
replace ancnumber = "1011" if _n == 7761 & ancnumber == "1011/24"

* Confirm remaining ANC numbers containing non-numeric characters
browse study_id regno ancnumber if !missing(ancnumber) & strtrim(ancnumber) != "" & ustrregexm(strtrim(ancnumber), "[^0-9]")

* Correct the remaining particular record
replace ancnumber = "5" if study_id == "18_R1_23M"

* Remove leading, trailing and repeated internal spaces
replace ancnumber = itrim(strtrim(ancnumber)) if !missing(ancnumber)
* Label ANC number
label variable ancnumber "ANC number"

* 4. Clean maternity number
tabulate mat_nmbr, missing

* Browse maternity numbers containing non-numeric characters
browse study_id regno mat_nmbr if !missing(mat_nmbr) & strtrim(mat_nmbr) != "" & ustrregexm(strtrim(mat_nmbr), "[^0-9]")
* Correct the two identified maternity numbers
replace mat_nmbr = "50"  if study_id == "105_R6_23M"
replace mat_nmbr = "244" if study_id == "27_R46_24M"

* Confirm remaining non-numeric maternity numbers
browse study_id regno mat_nmbr if !missing(mat_nmbr) & strtrim(mat_nmbr) != "" & ustrregexm(strtrim(mat_nmbr), "[^0-9]")
* Rename and label maternity number
rename mat_nmbr maternity_number
label variable maternity_number "Maternity number"

* 5. Clean and label facility
* Inspect the original facility values
codebook facilty
* Recode facility: Mayuge=1, Buluba=2
generate byte facility_temp = .
replace facility_temp = 1 if strtrim(facilty) == "Mayuge"
replace facility_temp = 2 if strtrim(facilty) == "Buluba"

* Check the recoding before replacing the original variable
tabulate facilty facility_temp, missing
drop facilty
rename facility_temp facilty
* Attach value labels and variable label
label define facility_lbl 1 "Mayuge" 2 "Buluba", replace
label values facilty facility_lbl
label variable facilty "facility"

* Check facility codes and labels
tabulate facilty, missing
label list facility_lbl


* 6. Label register number and dates
label variable regno "Register Number"
label variable entry_date "Date of entry in register"
label variable date_2 "Date of entry in database"


* 7. Clean laboratory number
tabulate lab_no, missing
* Browse laboratory numbers containing non-numeric characters
browse study_id regno lab_no if !missing(lab_no) & strtrim(lab_no) != "" & ustrregexm(strtrim(lab_no), "[^0-9]")
* Correct the identified laboratory number
replace lab_no = "82" if study_id == "48_R32_24M"

* Confirm remaining non-numeric laboratory numbers
browse study_id regno lab_no if !missing(lab_no) & strtrim(lab_no) != "" & ustrregexm(strtrim(lab_no), "[^0-9]")
* Rename and label laboratory number
rename lab_no laboratory_no
label variable laboratory_no "Lab number"

* 8. Clean mother's name
* Count missing and blank names before cleaning
count if missing(mothers_name) | strtrim(mothers_name) == ""

* Remove leading/trailing spaces and repeated internal spaces
replace mothers_name = itrim(strtrim(mothers_name)) if !missing(mothers_name)

* Convert blank strings to Stata string missing
replace mothers_name = "" if strtrim(mothers_name) == ""
* Convert names to proper/title case
replace mothers_name = strproper(strlower(mothers_name)) if !missing(mothers_name)

* Attach variable label
label variable mothers_name "Mother's name"

* Verify the names
browse study_id regno mothers_name

* 9. Clean mother's age
tabulate mother_age, missing
summarize mother_age
* Set ages less than or equal to 9 to missing
replace mother_age = . if mother_age <= 9 & !missing(mother_age)
* Label and summarise age after cleaning
label variable mother_age "Age of the mother"
summarize mother_age

* 10. Clean malaria microscopy
tabulate mal_microscopy, missing
* Recode microscopy results
generate byte mal_microscopy_new = .
replace mal_microscopy_new = 2 if strtrim(mal_microscopy) == "mps++"
replace mal_microscopy_new = 1 if strtrim(mal_microscopy) == "No mps seen"

* Attach value labels
label define microscopy_lbl 1 "No mps seen" 2 "mps++", replace
label values mal_microscopy_new microscopy_lbl
label variable mal_microscopy_new "Malaria Microscopy"
* Check the new variable
tabulate mal_microscopy_new, missing
label list microscopy_lbl
* Replace the original variable with the cleaned variable
drop mal_microscopy
rename mal_microscopy_new mal_microscopy


* 11. Clean malaria RDT results
tabulate rdts_mal, missing
* Recode RDT results
generate byte rdts_mal_new = .
replace rdts_mal_new = 1 if strtrim(rdts_mal) == "Positive"
replace rdts_mal_new = 2 if strtrim(rdts_mal) == "Negative"

* Attach value labels
label define rdts_lbl 1 "Positive" 2 "Negative", replace
label values rdts_mal_new rdts_lbl
label variable rdts_mal_new "Malaria RDTS"
* Check the new variable
tabulate rdts_mal_new, missing
label list rdts_lbl

* Replace the original variable with the cleaned variable
drop rdts_mal
rename rdts_mal_new rdts_mal


* 12. Clean protein results
* Inspect original protein results
tabulate protein_results, missing

* Group Nil and Non reactive together
generate str20 Protein_testResults = ""
replace Protein_testResults = "Non reactive" if inlist(strtrim(protein_results), "Nil", "Non reactive")
replace Protein_testResults = "Reactive" if strtrim(protein_results) == "Reactive"

* Check the grouped results
tabulate Protein_testResults, missing

* Drop original variable after checking
drop protein_results

* Convert grouped results to numeric codes
generate byte Protein_testResults_num = .
replace Protein_testResults_num = 1 if lower(strtrim(Protein_testResults)) == "reactive"
replace Protein_testResults_num = 2 if lower(strtrim(Protein_testResults)) == "non reactive"

* Attach value labels and variable label
label define protein_lbl 1 "Reactive" 2 "Non reactive", replace
label values Protein_testResults_num protein_lbl
label variable Protein_testResults_num "Protein results"

* Replace the string grouped variable with the labelled numeric variable
drop Protein_testResults
rename Protein_testResults_num Protein_testResults
* Check labels and frequencies
tabulate Protein_testResults, missing
label list protein_lbl

* 13. Clean sugar test results
tabulate sugar_results, missing

* Group Nil and Non reactive together
generate str20 sugar_testresults = ""
replace sugar_testresults = "Non reactive" if inlist(strtrim(sugar_results), "Nil", "Non reactive")
replace sugar_testresults = "Reactive" if strtrim(sugar_results) == "Reactive"

* Check grouped sugar results
tabulate sugar_testresults, missing

* Drop original variable after checking
drop sugar_results

* Convert grouped results to numeric codes
generate byte sugar_testresults_num = .
replace sugar_testresults_num = 1 if strtrim(sugar_testresults) == "Reactive"
replace sugar_testresults_num = 2 if strtrim(sugar_testresults) == "Non reactive"

* Attach value labels and variable label
label define sugar_lbl 1 "Reactive" 2 "Non reactive", replace
label values sugar_testresults_num sugar_lbl
label variable sugar_testresults_num "Sugar results"

* Replace string variable with labelled numeric variable
drop sugar_testresults
rename sugar_testresults_num sugar_testresults

* Confirm labels and frequencies
tabulate sugar_testresults, missing
label list sugar_lbl

* 14. Label sugar levels and RBS
label variable sugar_levels "Sugar levels"
tabulate rbs_9da2cc, missing
label variable rbs_9da2cc "RBS"

* 15. Label missing-question variables
label variable missing_questions "Were any of the questions not answered?"
label variable list_questions "List the missing questions"
tabulate list_questions, missing

* 16. Label and correct data entrant
label variable entered_by "Data entered by"
tabulate entered_by, missing
* Correct the spelling
replace entered_by = "mariamt" if entered_by == "mariaamt"
* Confirm correction
tabulate entered_by, missing
* 17. Register completeness
label variable lab_form_v2_complete "Complete"
tabulate lab_form_v2_complete, missing

