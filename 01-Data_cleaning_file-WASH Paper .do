*************: WASH AND COVID19 Effect Analysis :******************************
********************:     Data Cleaning 	:**********************************
cd "~/Year 11/Publications/Milestone3 - Publications/Datasets"

****: Creating pre-COVID19 dataset
use "Pre_covid19_dataset.dta", clear

*: Preprocessing
gen group = 1
order group
drop survey
drop MoE_code school_type sex_pupils headteacher headteacher_phone deputy_headteacher deputy_headteacher_phone
egen number_boys_enrolled = rowtotal(A1_ECD_enrolled_boys A1_P1_enrolled_boys A1_P2_enrolled_boys A1_P3_enrolled_boys A1_P4_enrolled_boys A1_P5_enrolled_boys A1_P6_enrolled_boys A1_P7_enrolled_boys A1_P8_enrolled_boys)
drop A1_ECD_enrolled_boys A1_P1_enrolled_boys A1_P2_enrolled_boys A1_P3_enrolled_boys A1_P4_enrolled_boys A1_P5_enrolled_boys A1_P6_enrolled_boys A1_P7_enrolled_boys A1_P8_enrolled_boys

egen number_girls_enrolled = rowtotal(A2_ECD_enrolled_girls A2_P1_enrolled_girls A2_P2_enrolled_girls A2_P3_enrolled_girls A2_P4_enrolled_girls A2_P5_enrolled_girls A2_P6_enrolled_girls A2_P7_enrolled_girls A2_P8_enrolled_girls)
drop A2_ECD_enrolled_girls A2_P1_enrolled_girls A2_P2_enrolled_girls A2_P3_enrolled_girls A2_P4_enrolled_girls A2_P5_enrolled_girls A2_P6_enrolled_girls A2_P7_enrolled_girls A2_P8_enrolled_girls

order number_boys_enrolled number_girls_enrolled, after(Longitude)
drop A3_ECD_present_boys A3_P1_present_boys A3_P2_present_boys A3_P3_present_boys A3_P4_present_boys A3_P5_present_boys A3_P6_present_boys A3_P7_present_boys A3_P8_present_boys A4_ECD_present_girls A4_P1_present_girls A4_P2_present_girls A4_P3_present_girls A4_P4_present_girls A4_P5_present_girls A4_P6_present_girls A4_P7_present_girls A4_P8_present_girls
egen number_teachers = rowtotal(A5_male_teachers A6_female_teachers)
order number_teachers, after(number_girls_enrolled)
egen number_children_enrolled = rowtotal(number_boys_enrolled number_girls_enrolled)
order number_children_enrolled, before(number_boys_enrolled)

drop A5_male_teachers A6_female_teachers
gen pupils_teacher_ratio = (number_children_enrolled/number_teachers)
order pupils_teacher_ratio, after(number_teachers)

replace water_sanitation_program  = 0 if water_sanitation_program == 2
order water_sanitation_program, after(pupils_teacher_ratio)
replace deworming_program = 0 if deworming_program == 2
order deworming_program, after(water_sanitation_program)
gen teacher_trained_deworming = 1 if deworming_teachers == 1
replace teacher_trained_deworming = 0 if deworming_teachers == 2
order teacher_trained_deworming, after(deworming_program)
gen Deworming_IEC_material = 1 if (DW_posters_class == 1 | DW_posters_HT == 1 | DW_booklets_library == 1 | DW_other == 1)
replace Deworming_IEC_material = 0 if Deworming_IEC_material != 1
order Deworming_IEC_material, after(teacher_trained_deworming)
drop DW_posters_class DW_posters_HT DW_booklets_library DW_other DW_details

gen improved_water_source_s = 1 if (source_water == 1 | source_water == 2 | source_water == 3 | source_water == 6)
replace improved_water_source_s = 0 if improved_water_source_s != 1
order improved_water_source_s, after(Deworming_IEC_material)

gen water_drinking_s = 1 if water_for_drinking_s == 1
replace water_drinking_s = 0 if water_for_drinking_s != 1
order water_drinking_s, after(improved_water_source_s)
drop water_for_drinking_s

replace hw_near_toilets_s = 0 if hw_near_toilets_s == 2
order hw_near_toilets_s, after(water_drinking_s)
gen hw_water_near_toilets_s = 1 if (hw_near_toilets_s == 1 &  water_in_hw_s == 1)
replace hw_water_near_toilets_s = 0 if hw_water_near_toilets_s != 1
gen hw_water_soap_near_toilets_s = 1 if (hw_near_toilets_s == 1 &  water_in_hw_s == 1 & soap_in_hw_s == 1)
replace hw_water_soap_near_toilets_s = 0 if hw_water_soap_near_toilets_s != 1
order hw_water_near_toilets_s hw_water_soap_near_toilets_s, after(hw_near_toilets_s)

order number_latrine_block, after(separate_toilets_s)
gen number_latrines = (number_latrine_block*latrines_per_block_count)
order latrine, after(months_without_water)
order number_latrines, after(number_latrine_block)
replace separate_toilets_s = 2 if separate_toilets_s == .
replace number_latrine_block = 0 if number_latrine_block == .
replace number_latrines = 0 if number_latrines == .

gen pupil_latrine_ratio_girls = (number_girls_enrolled/number_latrines)
gen pupil_latrine_ratio_boys = (number_boys_enrolled/number_latrines)
gen pupil_latrine_ratio_overall = (number_children_enrolled/number_latrines)
order pupil_latrine_ratio_girls pupil_latrine_ratio_boys pupil_latrine_ratio_overall, after(number_latrines)
drop pupil_latrine_ratio_females pupil_latrine_ratio_males total_females total_males
replace feeding_program = 0 if feeding_program == 2
order feeding_program, after(water_sanitation_program)
drop water_in_hw_s soap_in_hw_s first_aid_s first_aid_what_s

gen hw_school = 1 if handwashing_type != .
replace hw_school = 0 if hw_school == .
order hw_school, after(water_drinking_s)
replace hw_near_toilets_s = 0 if hw_near_toilets_s == .

drop handwashing_type other_hw_type
drop source_water
drop other_source_water
drop feeding_HW_after deworming_who deworming_drugs deworming_teachers who_trained_teachers other_programme latrines_per_block_count total_pupils total_latrines pupil_latrine_ratio improved_water_source hw_water_soap_available
order region, after(date_visit)

replace teacher_trained_deworming =  0 if teacher_trained_deworming == .
replace months_without_water = 1 if months_without_water == .
replace latrine = 0 if latrine == 2
replace separate_toilets_s = 0 if separate_toilets_s == 2

*: Variable labelling
label variable group "Group"
label variable date_visit "Date visit"
label variable region "Number of regions"
label variable countycode "Number of counties"
label variable subcounty_code "Number of subcounties"
label variable school_code "Number of schools"
label variable Latitude "Latitude"
label variable Longitude "Longitude"
label variable number_children_enrolled "Average number of children per school"
label variable number_boys_enrolled "Average number of boys per school"
label variable number_girls_enrolled "Average number of girls per school"
label variable number_teachers "Average number of teachers per school"
label variable pupils_teacher_ratio "Pupils per teacher ratio"
label variable water_sanitation_program "Average number of schools with water and sanitation programme"
label variable feeding_program "Average number of schools with feeding programme"
label variable deworming_program "Average number of schools with deworming programme"
label variable teacher_trained_deworming "Average number of schools with any teacher(s) trained on deworming in the past 6 months"
label variable Deworming_IEC_material "Average number of schools with deworming IEC materials displayed at various places in the school"
label variable improved_water_source_s "Proportion of schools with an improved water source at school"
label variable water_drinking_s "Proportion of schools with drinking water always available to children at school"
label variable hw_school "Proportion of schools with handwashing facility anywhere in the school"
label variable hw_near_toilets_s "Proportion of schools with handwashing facility near latrine"
label variable hw_water_near_toilets_s "Proportion of schools with handwashing facility near latrine equipped with water only"
label variable hw_water_soap_near_toilets_s "Proportion of schools with handwashing facility near latrine equipped with water and soap"
label variable months_without_water "Average number of months without water in the school"
label variable latrine "Proportion of schools with latrine (any type)"
label variable separate_toilets_s "Proportion of schools with latrine (any type) separate for boys and girls"
label variable number_latrine_block "Average number of latrine blocks in the school"
label variable number_latrines "Average number of latrines in the school"
label variable pupil_latrine_ratio_girls "Pupils per latrine ratio, girls"
label variable pupil_latrine_ratio_boys "Pupils per latrine ratio, boys"
label variable pupil_latrine_ratio_overall "Pupils per latrine ratio, overall"


*: Latrine dataset
gen latrine_unhygienic = max(num_feces, num_feces_outside, num_smell, num_flies)
gen latrine_structure = num_good_structure
gen latrine_clean = num_clean
gen latrine_improved_s = 1 if (type_latrine == 1 | type_latrine == 2)
replace latrine_improved_s = 0 if latrine_improved_s != 1

collapse (median)latrine_unhygienic (median)latrine_structure (median)latrine_clean (sum)latrine_improved_s, by(latrine_school_code)

replace latrine_school_code = 0 if latrine_school_code == .
replace latrine_unhygienic = 0 if latrine_unhygienic == .
replace latrine_structure = 0 if latrine_structure == .
replace latrine_clean = 0 if latrine_clean == .
replace latrine_improved_s = 0 if latrine_improved_s == .

rename latrine_school_code school_code

merge 1:1 school_code using "/Users/mac/Library/CloudStorage/OneDrive-Personal/EvidenceAction Program/Year 11/Publications/Milestone3 - Publications/Datasets/Y6_latrine_data_summ.dta"

drop _merge

order latrine_improved_s, after(separate_toilets_s)

label variable latrine_improved_s "Proportion of schools with improved latrine"
label variable latrine_unhygienic "Proportion of schools with unhygienic latrines"
label variable latrine_structure "Proportion of schools with good latrine structural integrity"
label variable latrine_clean "Proportion of schools with clean latrines"


*: Individual dataset
replace eat_soil = 0 if eat_soil == 2
replace shoe_wearing = 0 if shoe_wearing == 2
replace treatment = 0 if (treatment == 2 | treatment == 3)
replace improved_water_source_h = 0 if improved_water_source_h == 2
replace toilet_home = 0 if toilet_home == 2

replace eat_soil = 0 if eat_soil == .
replace shoe_wearing = 0 if shoe_wearing == .
replace treatment = 0 if treatment == .
replace toilet_home = 0 if toilet_home == .

order eat_soil shoe_wearing  
order treatment, after(shoe_wearing)
order sthinfect sthinfect asinfect hkinfect trinfect schistoinfect sminfect shaeminfect, after(treatment)

gen wash_water_soap_home = 1 if (wash_place_home == 1 & (water_available_home == 1 | soap_available_home == 1))
replace wash_water_soap_home = 0 if wash_water_soap_home != 1
order wash_water_soap_home, after(shaeminfect)
order improved_water_source_h, after(wash_water_soap_home)
order toilet_home, after(improved_water_source_h)

gen anal_cleansing_s = 1 if (school_provide_anal_cleansing == 1 | school_provide_anal_cleansing == 2)
replace anal_cleansing_s = 0 if anal_cleansing_s != 1
order anal_cleansing_s, after(toilet_home)

gen wash_hands_school = 1 if (wash_school == 1 | wash_school == 2)
replace wash_hands_school = 0 if wash_hands_school != 1
order wash_hands_school, after(anal_cleansing_s)

gen anal_cleansing_h = 1 if (anal_cleansing_home == 1 | anal_cleansing_home == 2)
replace anal_cleansing_h = 0 if anal_cleansing_h != 1
order anal_cleansing_h, after(wash_hands_school)

gen wash_hands_home = 1 if (wash_home_usual == 1 | wash_home_usual == 2)
replace wash_hands_home = 0 if wash_hands_home != 1
order wash_hands_home, after(anal_cleansing_h)

order school_provide_anal_cleansing, after(toilet_home)

*Generation of household socio-economic status
tab1 education household_members wall floor roof Car Motorbike Bicycle Mobile_Phone Radio Television Sofa_set Electricity

replace education = 0 if (education == 1 | education == 5)
replace education = 1 if (education == 2 | education == 3)
replace education = 2 if education == 4
replace education = 0 if education == .

gen household_size = 0 if (household_members > 10)
replace household_size = 1 if (household_members >= 5 & household_members <= 10)
replace household_size = 2 if (household_members < 5)

gen household_structure = 1 if (wall == 1 & floor == 1 & (roof == 1 | roof == 2))
replace household_structure = 0 if household_structure != 1

replace Car = 0 if Car == 2 | Car == .
replace Motorbike = 0 if Motorbike == 2 | Motorbike == .
replace Bicycle = 0 if Bicycle == 2 | Bicycle == .
replace Mobile_Phone = 0 if Mobile_Phone == 2 | Mobile_Phone == .
replace Radio = 0 if Radio == 2 | Radio == .
replace Television = 0 if Television == 2 | Television == .
replace Sofa_set = 0 if Sofa_set == 2 | Sofa_set == .
replace Electricity = 0 if Electricity == 2 | Electricity == .

pca education household_size household_structure Car Motorbike Bicycle Mobile_Phone Radio Television Sofa_set Electricity

pca education household_size household_structure Car Motorbike Bicycle Mobile_Phone Radio Television Sofa_set Electricity, components(2) vce(normal)

testparm education household_size household_structure Car Motorbike Bicycle Mobile_Phone Radio Television Sofa_set Electricity, equal eq(Comp1)

pca // To redisplay the principal-component output after performing pca 
screeplot // To obtain a scree plot of the eigenvalues
screeplot, yline(1) ci(het) // Adds a line across the y-axis at 1 and adds heteroskedastic bootstrap confidence intervals.
predict pc1 pc2 pc3 pc4 pc5 pc6, score // To obtain the scores of the first 6 components (creates new variables)

predict pc1, score // We decided to obtain the scores of the first component only (creates new variable)

*xtile quart = pc1, nq(3) // To create tertiles

rename pc1 SES_index
order SES_index, after(wash_hands_home)



* Creating school level data from the individual level data
collapse (count) id (sum) eat_soil shoe_wearing treatment sthinfect asinfect hkinfect trinfect schistoinfect sminfect shaeminfect wash_water_soap_home improved_water_source_h toilet_home school_provide_anal_cleansing anal_cleansing_s wash_hands_school anal_cleansing_h wash_hands_home (mean) SES_index, by(schoolcode)

rename schoolcode school_code

merge 1:1 school_code using "/Users/mac/Library/CloudStorage/OneDrive-Personal/EvidenceAction Program/Year 11/Publications/Milestone3 - Publications/Datasets/Y6_individual_level_data_summ.dta"
drop _merge

order eat_soil shoe_wearing SES_index, after(pupils_teacher_ratio)
order treatment, after(Deworming_IEC_material)
order sth_prev as_prev hk_prev tr_prev schisto_prev sm_prev shaem_prev, after(treatment)
order wash_water_soap_home improved_water_source_h, after(months_without_water)
order toilet_home anal_cleansing_s wash_hands_school anal_cleansing_h wash_hands_home, after(latrine_clean)

label variable eat_soil "Average number of children reporting soil-eating behaviours"
label variable shoe_wearing "Average number of children wearing shoes at school"
label variable SES_index "Average household socio-economic status"
label variable treatment "Average number of children reporting taking deworming medicines in the last year"
label variable sth_prev "Any STH prevalence (%)"
label variable as_prev "A. lumbricoides prevalence (%)"
label variable hk_prev "Hookworm prevalence (%)"
label variable tr_prev "T. trichiura prevalence (%)"
label variable schisto_prev "Any schistosomiasis prevalence (%)"
label variable sm_prev "S. mansoni prevalence (%)"
label variable shaem_prev "S. haematobium prevalence (%)"
label variable wash_water_soap_home "Proportion of children reporting availability of handwashing facility equipped with soap and water at home"
label variable improved_water_source_h "Proportion of children reporting availability of improved water source at home"
label variable toilet_home "Proportion of children reporting availability of latrine (any type) at home"
label variable anal_cleansing_s "Proportion of children reporting availability of anal cleansing material at school"
label variable wash_hands_school "Proportion of children reporting washing hands with soap and water at school"
label variable anal_cleansing_h "Proportion of children reporting availability of anal cleansing material at home"
label variable wash_hands_home "Proportion of children reporting washing hands with soap and water at home"
label variable school_provide_anal_cleansing "Proportion of schools with anal cleansing material always available to children at school"




********************************************************************************
****: Creating post-COVID19 dataset
use "Post_covid19_dataset.dta", clear


gen region1 = ""
replace region1 = "1" if region == "Coast"
replace region1 = "2" if region == "Nyanza"
replace region1 = "3" if region == "Western"
replace region1 = "4" if region == "Rift Valley"
replace region1 = "5" if region == "Eastern"
replace region1 = "6" if region == "North Eastern"
replace region1 = "7" if region == "Central"
destring region1, replace
label define regi2 1"Coast" 2"Nyanza" 3"Western" 4"Rift Valley" 5"Eastern" 6"North Eastern" 7"Central"
label values region1 regi2
order region1, after(region)
drop region
rename region1 region

rename countyname countycode

replace countycode = "1" if countycode == "BOMET"
replace countycode = "2" if countycode == "BUNGOMA" 
replace countycode = "3" if countycode == "BUSIA"
replace countycode = "4" if countycode == "GARISSA"
replace countycode = "5" if countycode == "HOMABAY"
replace countycode = "6" if countycode == "KAKAMEGA" 
replace countycode = "7" if countycode == "KERICHO" 
replace countycode = "8" if countycode == "KILIFI"
replace countycode = "21" if countycode == "KIRINYAGA"
replace countycode = "9" if countycode == "KISII"
replace countycode = "10" if countycode == "KISUMU"
replace countycode = "11" if countycode == "KITUI"
replace countycode = "12" if countycode == "KWALE"
replace countycode = "22" if countycode == "LAMU"
replace countycode = "23" if countycode == "MACHAKOS"
replace countycode = "13" if countycode == "MAKUENI" 
replace countycode = "15" if countycode == "MIGORI"
replace countycode = "16" if countycode == "MOMBASA"
replace countycode = "24" if countycode == "NANDI"
replace countycode = "17" if countycode == "NAROK"
replace countycode = "18" if countycode == "NYAMIRA"
replace countycode = "25" if countycode == "SIAYA"
replace countycode = "19" if countycode == "TAITA TAVETA"
replace countycode = "26" if countycode == "TANA RIVER"
replace countycode = "27" if countycode == "TRANS NZOIA"
replace countycode = "20" if countycode == "VIHIGA"
replace countycode = "14" if countycode == "WAJIR"

destring countycode, replace

label define cn 1"Bomet" 2"Bungoma" 3"Busia" 4"Garissa" 5"Homabay" 6"Kakamega" 7"Kericho" 8"Kilifi" 9"Kisii" 10"Kisumu" 11"Kitui" 12"Kwale" 13"Makueni" 14"Wajir" 15"Migori" 16"Mombasa" 17"Narok" 18"Nyamira" 19"T. Taveta" 20"Vihiga" 21"Kirinyaga" 22"Lamu" 23"Machakos" 24"Nandi" 25"Siaya" 26"T. River" 27"T. Nzoia"
label values countycode cn


replace subcounty_code = "505" if subcounty_code == "Ainamoi"
replace subcounty_code = "211" if subcounty_code == "Aldai"
replace subcounty_code = "217" if subcounty_code == "Alego Usonga"
replace subcounty_code = "711" if subcounty_code == "Awendo"
replace subcounty_code = "703" if subcounty_code == "Bomachoge Borabu"
replace subcounty_code = "501" if subcounty_code == "Bomet Central"
replace subcounty_code = "502" if subcounty_code == "Bomet East"
replace subcounty_code = "867" if subcounty_code == "Bonchari"
replace subcounty_code = "218" if subcounty_code == "Bondo"
replace subcounty_code = "606" if subcounty_code == "Budalangi"
replace subcounty_code = "601" if subcounty_code == "Bumula"
replace subcounty_code = "223" if subcounty_code == "Bura"
replace subcounty_code = "506" if subcounty_code == "Buret"
replace subcounty_code = "608" if subcounty_code == "Butula"
replace subcounty_code = "510" if subcounty_code == "Chepalungu"
replace subcounty_code = "226" if subcounty_code == "Cherangany"
replace subcounty_code = "212" if subcounty_code == "Chesumei"
replace subcounty_code = "100" if subcounty_code == "Dadaab"
replace subcounty_code = "101" if subcounty_code == "Dujis"
replace subcounty_code = "120" if subcounty_code == "Eldas"
replace subcounty_code = "213" if subcounty_code == "Emgwen"
replace subcounty_code = "227" if subcounty_code == "Endebess"
replace subcounty_code = "870" if subcounty_code == "Fafi"
replace subcounty_code = "609" if subcounty_code == "Funyula"
replace subcounty_code = "224" if subcounty_code == "Galole"
replace subcounty_code = "868" if subcounty_code == "Ganze"
replace subcounty_code = "225" if subcounty_code == "Garsen"
replace subcounty_code = "219" if subcounty_code == "Gem"
replace subcounty_code = "200" if subcounty_code == "Gichugu"
replace subcounty_code = "620" if subcounty_code == "Hamisi"
replace subcounty_code = "103" if subcounty_code == "Ijara"
replace subcounty_code = "104" if subcounty_code == "Jomvu"
replace subcounty_code = "716" if subcounty_code == "Kabondo Kasipul"
replace subcounty_code = "602" if subcounty_code == "Kabuchai"
replace subcounty_code = "180" if subcounty_code == "Kaiti"
replace subcounty_code = "203" if subcounty_code == "Kangundo"
replace subcounty_code = "869" if subcounty_code == "Karachuonyo"
replace subcounty_code = "204" if subcounty_code == "Kathiani"
replace subcounty_code = "800" if subcounty_code == "Khwisero"
replace subcounty_code = "801" if subcounty_code == "Kibwezi East"
replace subcounty_code = "802" if subcounty_code == "Kibwezi West"
replace subcounty_code = "509" if subcounty_code == "Kilgoris"
replace subcounty_code = "106" if subcounty_code == "Kilifi North"
replace subcounty_code = "803" if subcounty_code == "Kilome"
replace subcounty_code = "804" if subcounty_code == "Kimilili"
replace subcounty_code = "805" if subcounty_code == "Kiminini"
replace subcounty_code = "806" if subcounty_code == "Kinango"
replace subcounty_code = "807" if subcounty_code == "Kipkelion East"
replace subcounty_code = "808" if subcounty_code == "Kipkelion West"
replace subcounty_code = "708" if subcounty_code == "Kisumu East"
replace subcounty_code = "188" if subcounty_code == "Kitui Central"
replace subcounty_code = "809" if subcounty_code == "Kitui East"
replace subcounty_code = "810" if subcounty_code == "Kitui Rural"
replace subcounty_code = "811" if subcounty_code == "Kitui South"
replace subcounty_code = "189" if subcounty_code == "Kitui West"
replace subcounty_code = "812" if subcounty_code == "Konoin"
replace subcounty_code = "710" if subcounty_code == "Kuria East"
replace subcounty_code = "813" if subcounty_code == "Kuria West"
replace subcounty_code = "814" if subcounty_code == "Kwanza"
replace subcounty_code = "815" if subcounty_code == "Lamu West"
replace subcounty_code = "102" if subcounty_code == "Likoni"
replace subcounty_code = "816" if subcounty_code == "Likuyani"
replace subcounty_code = "817" if subcounty_code == "Lugari"
replace subcounty_code = "113" if subcounty_code == "Lunga Lunga"
replace subcounty_code = "610" if subcounty_code == "Lurambi"
replace subcounty_code = "818" if subcounty_code == "Machakos Town"
replace subcounty_code = "105" if subcounty_code == "Magarini"
replace subcounty_code = "819" if subcounty_code == "Makueni"
replace subcounty_code = "107" if subcounty_code == "Malindi"
replace subcounty_code = "820" if subcounty_code == "Masinga"
replace subcounty_code = "821" if subcounty_code == "Matayos"
replace subcounty_code = "108" if subcounty_code == "Matuga"
replace subcounty_code = "822" if subcounty_code == "Matungu"
replace subcounty_code = "823" if subcounty_code == "Matungulu"
replace subcounty_code = "824" if subcounty_code == "Mavoko"
replace subcounty_code = "190" if subcounty_code == "Mbooni"
replace subcounty_code = "825" if subcounty_code == "Mosop"
replace subcounty_code = "826" if subcounty_code == "Muhoroni"
replace subcounty_code = "827" if subcounty_code == "Mumias East"
replace subcounty_code = "828" if subcounty_code == "Mwala"
replace subcounty_code = "829" if subcounty_code == "Mwingi East"
replace subcounty_code = "830" if subcounty_code == "Mwingi North"
replace subcounty_code = "831" if subcounty_code == "Mwingi West"
replace subcounty_code = "832" if subcounty_code == "Nambale"
replace subcounty_code = "833" if subcounty_code == "Nandi Hills"
replace subcounty_code = "834" if subcounty_code == "Narok East"
replace subcounty_code = "835" if subcounty_code == "Narok North"
replace subcounty_code = "836" if subcounty_code == "Narok South"
replace subcounty_code = "837" if subcounty_code == "Narok West"
replace subcounty_code = "715" if subcounty_code == "Ndhiwa"
replace subcounty_code = "838" if subcounty_code == "Ndia"
replace subcounty_code = "839" if subcounty_code == "North Mugirango"
replace subcounty_code = "840" if subcounty_code == "Nyakach"
replace subcounty_code = "707" if subcounty_code == "Nyando"
replace subcounty_code = "713" if subcounty_code == "Nyaribari Masaba"
replace subcounty_code = "841" if subcounty_code == "Nyatike"
replace subcounty_code = "842" if subcounty_code == "Rangwe"
replace subcounty_code = "843" if subcounty_code == "Rarieda"
replace subcounty_code = "844" if subcounty_code == "Saboti"
replace subcounty_code = "845" if subcounty_code == "Seme"
replace subcounty_code = "503" if subcounty_code == "Sigowet/Soin"
replace subcounty_code = "846" if subcounty_code == "Sirisia"
replace subcounty_code = "847" if subcounty_code == "Sotik"
replace subcounty_code = "848" if subcounty_code == "South Mugirango"
replace subcounty_code = "849" if subcounty_code == "Suba North"
replace subcounty_code = "850" if subcounty_code == "Suba South"
replace subcounty_code = "851" if subcounty_code == "Suna East"
replace subcounty_code = "852" if subcounty_code == "Suna West"
replace subcounty_code = "853" if subcounty_code == "Tarbaj"
replace subcounty_code = "854" if subcounty_code == "Taveta"
replace subcounty_code = "855" if subcounty_code == "Teso North"
replace subcounty_code = "856" if subcounty_code == "Teso South"
replace subcounty_code = "857" if subcounty_code == "Tinderet"
replace subcounty_code = "858" if subcounty_code == "Tongaren"
replace subcounty_code = "859" if subcounty_code == "Ugenya"
replace subcounty_code = "860" if subcounty_code == "Ugunja"
replace subcounty_code = "861" if subcounty_code == "Uriri"
replace subcounty_code = "110" if subcounty_code == "Voi"
replace subcounty_code = "862" if subcounty_code == "Wajir East"
replace subcounty_code = "863" if subcounty_code == "Wajir North"
replace subcounty_code = "864" if subcounty_code == "Wajir South"
replace subcounty_code = "604" if subcounty_code == "Webuye East"
replace subcounty_code = "865" if subcounty_code == "West Mugirango"
replace subcounty_code = "111" if subcounty_code == "Wundanyi"
replace subcounty_code = "866" if subcounty_code == "Yatta"

destring subcounty_code, replace

label define sbcn 505"Ainamoi" 211"Alego Usonga" 711"Awendo" 703"Bomachoge Borabu" 501"Bomet Central" 502"Bomet East" 867"Bonchari" 218"Bondo" 606"Budalangi" 601"Bumula" 223"Bura" 506"Buret" 608"Butula" 510"Chepalungu" 226"Cherangany" 212"Chesumei" 100"Dadaab" 101"Dujis" 120"Eldas" 213"Emgwen" 227"Endebess" 870"Fafi" 609"Funyula" 224"Galole" 868"Ganze" 225"Garsen" 219"Gem" 200"Gichugu" 620"Hamisi" 103"Ijara" 104"Jomvu" 716"Kabondo Kasipul" 602"Kabuchai" 180"Kaiti" 203"Kangundo" 869"Karachuonyo" 204"Kathiani" 800"Khwisero" 801"Kibwezi East" 802"Kibwezi West" 509"Kilgoris" 106"Kilifi North" 803"Kilome" 804"Kimilili" 805"Kiminini" 806"Kinango" 807"Kipkelion East" 808"Kipkelion West" 708"Kisumu East" 188"Kitui Central" 809"Kitui East" 810"Kitui Rural" 811"Kitui South" 189"Kitui West" 812"Konoin" 710"Kuria East" 813"Kuria West" 814"Kwanza" 815"Lamu West" 102"Likoni" 816"Likuyani" 817"Lugari" 113"Lunga Lunga" 610"Lurambi" 818"Machakos Town" 105"Magarini" 819"Makueni" 107"Malindi" 820"Masinga" 821"Matayos" 108"Matuga" 822"Matungu" 823"Matungulu" 824"Mavoko" 190"Mbooni" 825"Mosop" 826"Muhoroni" 827"Mumias East" 828"Mwala" 829"Mwingi East" 830"Mwingi North" 831"Mwingi West" 832"Nambale" 833"Nandi Hills" 834"Narok East" 835"Narok North" 836"Narok South" 837"Narok West" 715"Ndhiwa" 838"Ndia" 839"North Mugirango" 840"Nyakach" 707"Nyando" 713"Nyaribari Masaba" 841"Nyatike" 842"Rangwe" 843"Rarieda" 844"Saboti" 845"Seme" 503"Sigowet/Soin" 846"Sirisia" 847"Sotik" 848"South Mugirango" 849"Suba North" 850"Suba South" 851"Suna East" 852"Suna West" 853"Tarbaj" 854"Taveta" 855"Teso North" 856"Teso South" 857"Tinderet" 858"Tongaren" 859"Ugenya" 860"Ugunja" 861"Uriri" 110"Voi" 862"Wajir East" 863"Wajir North" 864"Wajir South" 604"Webuye East" 865"West Mugirango" 111"Wundanyi" 866"Yatta"

label values subcounty_code sbcn

label define sn 501060"MOGOIWET" 501061"MUIYWEK" 502074"CHEBISIAN" 502075"KORARA" 503042"MWOKYOT" 504086"NDARAWETA" 500220"KAPLOMBOI" 601050"BUMULA" 601051"ST ELIZABETH MALINDA RC PRI" 602115"MAROBO FAM PRI SCH" 603074"LUTONYI PRI SCH" 604005"NAMBUYA ACK PRI SCH" 604006"TOLOSO PRI SCH" 605036"KAPCHONGE DEB PRI SCH" 606011"LUTACHO FYM PRI SCH" 606012"SITIKHO PRI SCH" 614108"SHIONGO PRI SCH" 615197"NANGILI PRI SCH" 616189"SUNRISE VALLEY ACADEMY PRI" 617125"MITAHO PRI SCH" 617126"NABONGO PRI SCH" 618177"MUNGORE PRI SCH" 619150"EMAKHWALE PRI SCH" 619151"NAMUNDERA PRI SCH" 505029"KERICHO TOWNSHIP PRI SCH" 506033"KAPKARIN PRI SCH" 507072"BENDITAI PRI SCH" 508097"IMBARAGAI PRI SCH" 509075"CHEBIRIR PRI SCH" 620064"JEBRONGO PRI SCH" 211106"KUBOJOI PRI SCH" 212107"AIC ITIGO MIXED SEC SCH" 213108"ACK KALEL ECD" 214109"ACK SIRWO PRI SCH" 215110"KAPTIEN PRI SCH" 216111"AIC METEITEI PRI SCH" 226123"MARURA PRI SCH" 226124"NOIGAM PRI SCH" 227125"KIMONDO PRI SCH" 228126"SANGO PRI SCH" 229127"BIRUNDA PRI SCH" 229128"PENGI PRI SCH" 230129"GITWAMBA PRI SCH" 230130"KINYORO PRI SCH" 607023"MUDEMBI PRI SCH" 608021"MUNGAMBWA PRI SCH" 609029"LAKESIDE ACADEMY PRI SCH" 610002"BUYENDE PRI" 611022"SUNSHINE PRI" 612107"KAESET PRI SCH" 613191"APATIT PRI SCH" 715094"WANG'APALA PRI SCH" 716104"SANDA PRI SCH" 716105"WIKONDIEK PRI SCH" 717070"STARLITE PRI SCH" 718067"NYAMBARE PRI SCH" 719052"GOD JOPE PRI" 719053"WASARIA PRIMARY" 720005"KITAWA PRI" 720006"KOGA PRI" 106107"BOGAMACHUKO PRI" 106108"MWANGEA PRI" 106109"PALAKUMI PRI" 107143"JEZA ZHOMU PRI" 107144"DABASO PRIMARY" 107145"KILIMO PRIMARY" 108182"SHAKAHOLA PRIMARY" 109132"KHOMBENI PRI SCH" 702204"MOSOBETI DOK PRI" 703109"KENYORORA PRI" 704152"NYAMERAKO PRIMARY" 705126"NYAKIOGIRO PRIMARY" 708070"CHIGA PRI SCH" 709062"CHEMELIL BI PRI" 707056"KODUM PRIMARY" 706081"RABUOR PRIMARY" 706082"UGWE PRI SCH" 701107"GUMO PRI SCH" 701108"KWOYO KOWE PRIMARY" 701109"RABONGI PRI" 112045"MBITA PRI SCH" 112046"NDAVAYA PRI SCH" 113001"GODO PRI SCH" 113002"KALALAMI PRI SCH" 113003"MWANGAZA ACADEMY PRI SCH" 114003"VUGA PRI SCH" 711009"SONY SUGAR PRI" 710032"KWIBANCHA PRI SCH" 712038"ST. JOHN'S NYANGITI PRI SCH" 713019"ST. FRANCIS XAVIER SCH" 713020"SCHAP COMMUNITY PRIMARY" 714064"ONYALO PRI" 725006"GIRIBE PRI SCH" 726062"KODUOGO PRI" 104059"MIRITINI WORLD BANK PRI SCH" 105026"JAMVI LA WAGENI PRI SCH" 510037"INDONYO PRI" 510038"OSERO PRI" 510039"POROKO PRI" 510040"SOSIANA PRI" 511051"EOR-ENKULE PRI SCH" 512054"MURUA PRI SCH" 512055"EMPOPONGI PRIMARY" 512056"LENANA PRIMARY" 513095"EOR EWUASO PRI SCH" 513096"ILKIMATE PRI SCH" 513097"KUNTAI PRI" 513098"NAAPOLOSA PRI" 513099"NKOBEN PRI SCH" 513100"NKOSESIA PRI" 513101"OLMUSAKWA PRI" 514094"MOGOIYUET PRI SCH" 514095"KOITAMUGUL PRIMARY" 721145"CHAINA PRI" 721146"GITWEBE PRIMARY" 722080"NYAMIRA PRIMARY" 111108"CHOKAA PRIMARY SCH" 111109"ST. PATRICK KIMALA PRIMARY" 110106"KIRUMBI PRIMARY" 110107"KISIMENYI PRIMARY" 115056"KITUMBI PRIMARY" 100163"ILANGO PRIMARY" 101086"IFTIN PRIMARY" 102052"RAYA PRIMARY" 103014"GUYO PRIMARY" 188056"KWA WEWA PRI SCH" 188057"MAAINI PRI SCH" 189060"ITUUSYA PRIMARY" 189061"KIANGINI PRIMARY" 189062"KINAKONI PRIMARY" 190028"KANYANGI PRIMARY" 190029"KWA KILYA PRIMARY" 191076"NDILILI PRIMARY" 191077"MONGUNI  PRI SCH" 191078"MWAMBAISYUKO PRI SCH" 191079"YANGATHI PRI SCH" 192010"KOMU PRIMARY" 192011"MIKUYU MIKYA PRIMARY" 193152"KATHOKA PRI" 193153"KAVOKO PRIMARY" 194128"ITUNGUNI PRI" 194129"KATAMA PRIMARY SCHOOL" 194130"KATUUNI PRIMARY" 194131"KIMU PRI SCH" 194132"NGOMANO PRI" 195160"KAIRUNGU PRI" 195161"MBAU PRIMARY SCHOOL" 180131"IIUNI PRI SCH" 180132"KALONGO PRI SCH" 181161"IVIANI DEB PRI" 181162"KIKUNDUKU PRI SCH" 181163"KYAMBUSYA PRI SCH" 182570"KATHYAKA PRI SCH" 182571"MBIUNI PRI SCH" 183511"ENGULI PRI" 184601"KATEIKO PRI SCH" 184602"MATHANGATHINI PRI" 184603"SYALWE PRI SCH" 185574"KANGO PRI SCH" 185575"KASOONI PRI SCH" 185576"KATIKOMU PRI SCH" 120671"ELNUR PRI SCH" 120672"TULATULA PRIMARY" 121594"TARBAJ PRI SCH" 122590"LAFALEY PRIMARY" 123640"LEHELEY PRIMARY" 124690"BOA PRIMARY" 124691"EL ADOW PRIMARY" 124692"KULAALEY PRIMARY" 200607"KIANYAMBO PRI" 201076"UPPER BARICHO PRI" 202082"KATSAKAKAIRU PRI SCH" 202083"SIKOMANI PRI SCH" 202084"ST. JOHN'S MISSION PRIMARY" 203094"KYENI ACADEMY PRI SCH" 204010"MBEE PRIMARY" 205093"KATUMANI PRI" 205094"KINOI PRIMARY" 206095"KALULUINI PRI SCH" 206096"KATHUI PRI SCH" 206097"MASAKU PRIMARY" 206098"MIANGENI PRI SCH" 207099"KILIKU PRIMARY" 208100"KATHIANI PRIMARY" 208101"NZOIANI PRI SCH" 208102"ISYOKANI PRIMARY" 209103"KWAKISAU PRI" 209104"NGULUNI PRI" 210105"KANGOKO PRIMARY SCHOOL" 217112"ADUWA PRIMARY" 218113"MITUNDU PRI SCH" 218114"NYAMUANGA PRI SCH" 219115"NYAWARA PRIMARY" 220116"MITURI PRI SCH" 221117"YENGA PRIMARY" 222118"SIMERRO PRIMARY" 223119"RUKO PRIMARY" 223120"TUNE PRIMARY" 224121"BAKISANO PRIMARY" 225122"BILISA PRIMARY"

label values school_code sn

rename (latitude longitude) (Latitude Longitude)

destring deworming_teachers, replace

destring handwashing_type, replace

gen latrine = latrine_block

order sth_prev as_prev hk_prev tr_prev sm_prev shaem_prev, after(Deworming_IEC_material)

drop num_examined hkepg asepg trepg smepg sthepg shaeme hkinfect asinfect trinfect sminfect sthinfect shaeminfect total_children_enrolled total_children_present children_per_latrine

destring COVID19_sensitization, replace
replace COVID19_sensitization = 0 if COVID19_sensitization == 2
order COVID19_sensitization, after(pupil_latrine_ratio_overall)
replace COVID19_sensitization = 0 if COVID19_sensitization == .

destring cov19_teachers_trained, replace
replace cov19_teachers_trained = 0 if (cov19_teachers_trained == 2 | cov19_teachers_trained == .)
order cov19_teachers_trained, after(COVID19_sensitization)

destring cov19_students_trained, replace
replace cov19_students_trained = 0 if (cov19_students_trained == 2 | cov19_students_trained == .)
order cov19_students_trained, after(cov19_teachers_trained)

replace cov1_handwashing_stations = 0 if (cov1_handwashing_stations == 2 | cov1_handwashing_stations == .)
order cov1_handwashing_stations, after(cov19_students_trained)

replace cov6_handwashing_soap = 1 if (cov6_handwashing_soap == 1 | cov6_handwashing_soap == 2)
replace cov6_handwashing_soap = 0 if (cov6_handwashing_soap == 3 | cov6_handwashing_soap == .)
replace cov7_handwashing_water = 1 if (cov7_handwashing_water == 1 | cov7_handwashing_water == 2)
replace cov7_handwashing_water = 0 if (cov7_handwashing_water == 3 | cov7_handwashing_water == .)
gen cov6_hws_water_soap = 1 if (cov1_handwashing_stations == 1 & (cov6_handwashing_soap == 1 | cov7_handwashing_water == 1))
replace cov6_hws_water_soap = 0 if cov6_hws_water_soap != 1
order cov6_hws_water_soap, after(cov1_handwashing_stations)

replace cov19_IEC_material = "0" if cov19_IEC_material == ""
destring cov19_IEC_material, replace
order cov19_IEC_material, after(cov6_hws_water_soap)

label variable sth_prev "Any STH prevalence (%)"
label variable as_prev "A. lumbricoides prevalence (%)"
label variable hk_prev "Hookworm prevalence (%)"
label variable tr_prev "T. trichiura prevalence (%)"
label variable sm_prev "S. mansoni prevalence (%)"
label variable shaem_prev "S. haematobium prevalence (%)"
label variable COVID19_sensitization "Average number of schools with COVID-19 sensitization and prevention programme"
label variable cov19_teachers_trained "Average number of schools with any teacher(s) trained on COVID-19 prevention practices"
label variable cov19_students_trained "Average number of schools which trained children on COVID-19 prevention practices"
label variable cov1_handwashing_stations "Proportion of schools with handwashing station(s) specifically for COVID-19 prevention"
label variable cov6_hws_water_soap "Average number of handwashing stations with soap and water specifically for COVID-19 prevention in school"
label variable cov19_IEC_material "Average number of schools with COVID-19 IEC materials displayed at various places in school"
label variable cov_knowledge "Proportion of children reporting knowledge about COVID-19, transmission, and protection measures"
label variable cov_wash_hands_school "Proportion of children reporting washing hands with soap and water at the handwashing stations specifically for COVID-19 prevention at school"
label variable cov_hws_ws_home "Proportion of children reporting availability of handwashing stations with soap and water specifically for COVID-19 prevention at home"
label variable cov_wash_hands_home "Proportion of children reporting washing hands with soap and water at the handwashing stations specifically for COVID-19 prevention at home"

*: Latrine dataset
drop if school_code == 0
drop _merge

order latrine_improved_s, after(separate_toilets_s)
order latrine_unhygienic latrine_structure latrine_clean, after(pupil_latrine_ratio_overall)


*: Individual dataset
gen shoe_wearing = 1 if (shoes == 1 | shoes == 2)
replace shoe_wearing = 0 if shoe_wearing != 1

gen improved_water_source_h = 1 if (water_source_home == 1 | water_source_home == 2 | water_source_home == 3 | water_source_home == 5)
replace improved_water_source_h = 0 if improved_water_source_h != 1

gen cov_knowledge = 1 if G1_heard_covid19 == 1
replace cov_knowledge = 0 if cov_knowledge != 1
order cov_knowledge, after(SES_index)

gen cov_wash_hands_school = 1 if G17_school_wash_hands == 1
replace cov_wash_hands_school = 0 if cov_wash_hands_school != 1
order cov_wash_hands_school, after(cov_knowledge)

gen cov_hws_ws_home = 1 if (G8_home_handwashing == 1 & G9_home_water_handwashing == 1 & G10_home_soap_handwashing == 1)
replace cov_hws_ws_home = 0 if cov_hws_ws_home != 1
order cov_hws_ws_home, after(cov_wash_hands_school)

gen cov_wash_hands_home = 1 if G11_home_wash_hands == 1
replace cov_wash_hands_home = 0 if cov_wash_hands_home != 1
order cov_wash_hands_home, after(cov_hws_ws_home)

replace school_provide_anal_cleansing = 1 if (school_provide_anal_cleansing== 1 | school_provide_anal_cleansing == 2)
replace school_provide_anal_cleansing = 0 if school_provide_anal_cleansing != 1

* Creating school level data from the individual level data
collapse (count) unique_id (sum) eat_soil shoe_wearing treatment sthinfect asinfect hkinfect trinfect schistoinfect sminfect shaeminfect wash_water_soap_home improved_water_source_h toilet_home school_provide_anal_cleansing anal_cleansing_s wash_hands_school anal_cleansing_h wash_hands_home cov_knowledge cov_wash_hands_school cov_hws_ws_home cov_wash_hands_home (mean) SES_index, by(schoolcode)

rename schoolcode school_code

gen sth_prev = (sthinfect/id)*100 
gen as_prev = (asinfect/id)*100
gen hk_prev = (hkinfect/id)*100 
gen tr_prev = (trinfect/id)*100 
gen schisto_prev = (schistoinfect/id)*100
gen sm_prev = (sminfect/id)*100
gen shaem_prev = (shaeminfect/id)*100

drop id sthinfect asinfect hkinfect trinfect schistoinfect sminfect shaeminfect

order sth_prev as_prev hk_prev tr_prev schisto_prev sm_prev shaem_prev, after(treatment)

merge 1:1 school_code using "/Users/mac/Library/CloudStorage/OneDrive-Personal/EvidenceAction Program/Year 11/Publications/Milestone3 - Publications/Datasets/Y9_individual_level_data_summ.dta"

rename (cov_knowledge COVID19_sensitization cov19_teachers_trained cov19_students_trained cov1_handwashing_stations cov6_hws_water_soap cov_wash_hands_school cov19_IEC_material cov_hws_ws_home cov_wash_hands_home) (cov19_knowledge cov19_sensitization cov19_teachers_trained cov19_students_trained cov19_handwashing_stations cov19_hws_water_soap cov19_wash_hands_school cov19_IEC_material cov19_hws_ws_home cov19_wash_hands_home)

drop xxx water_for_drinking_s cov2_number_handwashing cov3_handwashing_pupils cov3_handwashing_teachers cov3_handwashing_shared cov4_handwashing_locations cov5_handwashing_type cov6_handwashing_soap cov7_handwashing_water feeding_HW_after deworming_who deworming_drugs deworming_teachers who_trained_teachers cov19_conducted_programme cov19_provided_handwashing other_programme DW_posters_class DW_posters_HT DW_booklets_library cov19_COVID19_poster_classroom cov19_COVID19_poster_office cov19_COVID19_booklet_library DW_other DW_other_specify DW_details latrine_block latrines_per_block_count rf_improved_water_source rf_hw_water_soap_available rf_cov_hw_water_soap_available _merge



********************************************************************************
****: Appending the two datasets
append using "/Users/mac/Library/CloudStorage/OneDrive-Personal/EvidenceAction Program/Year 11/Publications/Milestone3 - Publications/Datasets/Post_covid19_dataset.dta", nolabel force

use "01_Pre_post_covid19_dataset.dta", clear




******************************************************************************
**: Creating summarized dataset using PCA

/* I this dataset, first the all the variables were dichotomized, next all positive direction categories were coded as one and all negative direction categories coded as zero. */

use "02_PCA_dataset.dta", clear


* Demographic conditions
pca education household_size household_structure Car Motorbike Bicycle Mobile_Phone Radio Television Sofa_set Electricity

pca education household_size household_structure Car Motorbike Bicycle Mobile_Phone Radio Television Sofa_set Electricity, components(2) vce(normal)

testparm education household_size household_structure Car Motorbike Bicycle Mobile_Phone Radio Television Sofa_set Electricity, equal eq(Comp1)

pca // To redisplay the principal-component output after performing pca 
screeplot // To obtain a scree plot of the eigenvalues
screeplot, yline(1) ci(het) // Adds a line across the y-axis at 1 and adds heteroskedastic bootstrap confidence intervals.
predict pc1, score // We decided to obtain the scores of the first component only since it contains the best scores (creates new variable)

xtile quart = pc1, nq(2) // To create tertiles

rename pc1 demographic_conditions
rename quart demographic_conditions_tertiles

* Treatment related conditions
tab1 rf_water_sanitation_program rf_feeding_program rf_deworming_program rf_teacher_trained_deworming rf_Deworming_IEC_material rf_treatment


* Water related conditions

tab1 rf_improved_water_source_s rf_water_drinking_s rf_hw_school rf_hw_near_toilets_s rf_hw_water_near_toilets_s rf_hw_water_soap_near_toilets_s rf_months_without_water rf_wash_water_soap_home rf_improved_water_source_h

* Sanitation related conditions
tab1 rf_latrine rf_separate_toilets_s rf_latrine_improved_s rf_number_latrines pupil_latrine_ratio_girls pupil_latrine_ratio_boys pupil_latrine_ratio_overall rf_latrine_unhygienic rf_latrine_structure rf_latrine_clean rf_toilet_home

* Hygeine related conditions
tab1 rf_school_provide_anal_cleansing rf_anal_cleansing_s rf_wash_hands_school rf_anal_cleansing_h rf_wash_hands_home


***************************************************************************
**: Creating individual level data of the PCA dataset to increase sample size

use "02_PCA_dataset_individual_level.dta", clear

merge m:m school_code using "/Users/mac/Library/CloudStorage/OneDrive-Personal/EvidenceAction Program/Year 11/Publications/Milestone3 - Publications/Datasets/list_schools_numb_children.dta"

drop survey _merge

rename no_children no_children_per_school

expand no_children_per_school // code to create repeated observations as per the number of children per school

bysort school_code: gen id = _n // listing the number of children per school

label define gp 1"pre" 2"post"
label values group gp

