*************: WASH AND COVID19 Effect Analysis :******************************
*************:    Data Analysis and Modelling 	:******************************
cd "~/Year 11/Publications/Milestone3 - Publications/Datasets"

use "01_Pre_post_covid19_dataset.dta", clear

*: Further cleaning
replace latrine_improved_s = 1 if (latrine_improved_s != 0 &latrine_improved_s != .)
replace latrine_unhygienic = 1 if (latrine_unhygienic != 0 & latrine_unhygienic != .)
replace latrine_structure = 1 if (latrine_structure != 0 & latrine_structure != .)
replace latrine_clean = 1 if (latrine_clean != 0 & latrine_clean != .)
replace school_provide_anal_cleansing = 1 if (school_provide_anal_cleansing != 0 & school_provide_anal_cleansing != .)

****: Table 1: Summary of water, sanitation, and hygiene characteristics before and after the COVID-19 pandemic as assessed during 2018 and 2021 surveys respectively


* Demographic conditions
bysort group: tab region
bysort group: tab countycode 
bysort group: tab subcounty_code
bysort group: tab school_code
bysort group: su number_children_enrolled number_boys_enrolled number_girls_enrolled number_teachers pupils_teacher_ratio eat_soil shoe_wearing SES_index, detail

median number_children_enrolled, by(group)
median number_boys_enrolled, by(group)
median number_girls_enrolled, by(group)
median number_teachers, by(group)
median pupils_teacher_ratio, by(group)
median eat_soil, by(group)
median shoe_wearing, by(group)
median SES_index, by(group)

* Treatment related conditions
bysort group: tab water_sanitation_program
bysort group: tab feeding_program
bysort group: tab deworming_program
bysort group: tab teacher_trained_deworming
bysort group: tab Deworming_IEC_material
bysort group: su treatment

prtest water_sanitation_program, by(group)
prtest feeding_program, by(group)
prtest deworming_program, by(group)
prtest teacher_trained_deworming, by(group)
prtest Deworming_IEC_material, by(group)
median treatment, by(group)


* Infection status conditions
bysort group: su sth_prev as_prev hk_prev tr_prev schisto_prev sm_prev shaem_prev

ttest sth_prev, by(group)
ttest as_prev, by(group)
ttest hk_prev, by(group)
ttest tr_prev, by(group)
ttest schisto_prev, by(group)
ttest sm_prev, by(group)
ttest shaem_prev, by(group)

* Water related conditions
bysort group: tab1 improved_water_source_s water_drinking_s hw_school hw_near_toilets_s hw_water_near_toilets_s hw_water_soap_near_toilets_s 
bysort group: su months_without_water, detail
bysort group: su wash_water_soap_home improved_water_source_h, detail

prtest improved_water_source_s, by(group)
prtest water_drinking_s, by(group)
prtest hw_school, by(group)
prtest hw_near_toilets_s, by(group)
prtest hw_water_near_toilets_s, by(group)
prtest hw_water_soap_near_toilets_s, by(group)
median months_without_water, by(group)
median wash_water_soap_home, by(group)
median improved_water_source_h, by(group)

* Sanitation related conditions
bysort group: tab1 latrine separate_toilets_s latrine_improved_s
bysort group: su number_latrine_block number_latrines pupil_latrine_ratio_girls pupil_latrine_ratio_boys pupil_latrine_ratio_overall, detail
bysort group: tab1 latrine_unhygienic latrine_structure latrine_clean
bysort group: su toilet_home, detail

prtest latrine, by(group)
prtest separate_toilets_s, by(group)
prtest latrine_improved_s, by(group)
median number_latrine_block, by(group)
median number_latrines, by(group)
median pupil_latrine_ratio_girls, by(group) 
median pupil_latrine_ratio_boys, by(group)
median pupil_latrine_ratio_overall, by(group)
prtest latrine_unhygienic, by(group)
prtest latrine_structure, by(group)
prtest latrine_clean, by(group)
median toilet_home, by(group)

* Hygiene related conditions
bysort group: tab school_provide_anal_cleansing
bysort group: su anal_cleansing_s wash_hands_school anal_cleansing_h wash_hands_home, detail

prtest school_provide_anal_cleansing, by(group)
median anal_cleansing_s, by(group)
median wash_hands_school, by(group) 
median anal_cleansing_h, by(group)
median wash_hands_home, by(group)

* COVID-19 related conditions
bysort group: su cov19_knowledge, detail
bysort group: tab1 cov19_sensitization cov19_teachers_trained cov19_students_trained cov19_handwashing_stations cov19_hws_water_soap
bysort group: su cov19_wash_hands_school, detail
bysort group: tab cov19_IEC_material
bysort group: su cov19_hws_ws_home cov19_wash_hands_home, detail



****: Table 2: Univariable analysis of water, sanitation, and hygiene (WASH) impact on any soil-transmitted helminthiasis (STH) before and after the COVID-19 pandemic as assessed using multilevel linear mixed effects modeling

***: Creating the necessary variables
* Demographic conditions
recode number_children_enrolled (min/500 = 1 "≤500") (501/max = 2 ">500"), gen(rf_number_children_enrolled)
recode number_teachers (min/12 = 1 "≤12") (13/max = 2 ">12"), gen(rf_number_teachers) label(teachers)
recode pupils_teacher_ratio (min/25 = 1 "≤25.0") (25.1/max = 2 ">25.0"), gen(rf_pupils_teacher_ratio) label(pt_ratio)
recode eat_soil (0 = 0 "0") (1/max = 1 "≥1"), gen(rf_eat_soil1)
recode shoe_wearing (min/50 = 1 "≤50") (51/max = 2 ">50"), gen(rf_shoe_wearing) 
xtile rf_SES_index = SES_index, nq(2)

*********************************
*Any STH
*********************************
*Before COVID
meglm sth_prev ib2.rf_number_children_enrolled if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib1.rf_number_teachers if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib2.rf_pupils_teacher_ratio if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib1.rf_eat_soil if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib1.rf_shoe_wearing if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib1.rf_SES_index if group == 1 || school_code:, iter(1)  startg() family(nb)


*After COVID
meglm sth_prev 1b2.rf_number_children_enrolled if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib1.rf_number_teachers if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib2.rf_pupils_teacher_ratio if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib1.rf_eat_soil if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib1.rf_shoe_wearing if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib1.rf_SES_index if group == 2 || school_code:, iter(1)  startg() family(nb)

*Effect estimation
meglm sth_prev 1b2.rf_number_children_enrolled i.group if (group==1 | group==2) || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib1.rf_number_teachers i.group if (group==1 | group==2) || school_code:, iter(1)  startg() family(nb)



*********************************
*Any SCH
*********************************
*Before COVID
meglm schisto_prev ib2.rf_number_children_enrolled if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib1.rf_number_teachers if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib2.rf_pupils_teacher_ratio if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib1.rf_eat_soil if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib1.rf_shoe_wearing if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib1.rf_SES_index if group == 1 || school_code:, iter(1)  startg() family(nb)


*After COVID
meglm schisto_prev 1b2.rf_number_children_enrolled if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib1.rf_number_teachers if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib2.rf_pupils_teacher_ratio if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib1.rf_eat_soil if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib1.rf_shoe_wearing if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib1.rf_SES_index if group == 2 || school_code:, iter(1)  startg() family(nb)



estimates store m1
estimates store m2
estimates stats m1 m2



* Treatment related conditions
gen rf_water_sanitation_program = water_sanitation_program
gen rf_feeding_program = feeding_program
gen rf_deworming_program = deworming_program
gen rf_teacher_trained_deworming = teacher_trained_deworming
gen rf_Deworming_IEC_material = Deworming_IEC_material
recode treatment (min/50 = 1 "≤50") (51/max = 2 ">50"), gen(rf_treatment) 

*********************************
*Any STH
*********************************
*Before COVID
meglm sth_prev ib0.rf_water_sanitation_program if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib0.rf_feeding_program if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib0.rf_deworming_program if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib0.rf_teacher_trained_deworming if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib0.rf_Deworming_IEC_material if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib1.rf_treatment if group == 1 || school_code:, iter(1)  startg() family(nb)

*After COVID
meglm sth_prev ib0.rf_water_sanitation_program if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib0.rf_feeding_program if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib0.rf_deworming_program if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib0.rf_teacher_trained_deworming if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib0.rf_Deworming_IEC_material if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib1.rf_treatment if group == 2 || school_code:, iter(1)  startg() family(nb)


*********************************
*Any SCH
*********************************
*Before COVID
meglm schisto_prev ib0.rf_water_sanitation_program if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib0.rf_feeding_program if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib0.rf_deworming_program if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib0.rf_teacher_trained_deworming if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib0.rf_Deworming_IEC_material if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib1.rf_treatment if group == 1 || school_code:, iter(1)  startg() family(nb)


*After COVID
meglm schisto_prev ib0.rf_water_sanitation_program if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib0.rf_feeding_program if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib0.rf_deworming_program if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib0.rf_teacher_trained_deworming if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib0.rf_Deworming_IEC_material if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib1.rf_treatment if group == 2 || school_code:, iter(1)  startg() family(nb)


* Water related conditions
gen rf_improved_water_source_s = improved_water_source_s
gen rf_water_drinking_s = water_drinking_s
gen rf_hw_school = hw_school
gen rf_hw_near_toilets_s = hw_near_toilets_s
gen rf_hw_water_near_toilets_s = hw_water_near_toilets_s
gen rf_hw_water_soap_near_toilets_s = hw_water_soap_near_toilets_s
recode months_without_water (0 = 0 "0") (1/max = 1 "≥1"), gen(rf_months_without_water)
recode wash_water_soap_home (min/50 = 0 "≤50") (51/max = 1 ">50"), gen(rf_wash_water_soap_home)
recode improved_water_source_h (min/50 = 0 "≤50") (51/max = 1 ">50"), gen(rf_improved_water_source_h)

*********************************
*Any STH
*********************************
*Before COVID
meglm sth_prev ib0.rf_improved_water_source_s if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib0.rf_water_drinking_s if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib0.rf_hw_school if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib0.rf_hw_near_toilets_s if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib0.rf_hw_water_near_toilets_s if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib0.rf_hw_water_soap_near_toilets_s if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib1.rf_months_without_water if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib0.rf_wash_water_soap_home if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib0.rf_improved_water_source_h if group == 1 || school_code:, iter(1)  startg() family(nb)


*After COVID
meglm sth_prev ib0.rf_improved_water_source_s if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib0.rf_water_drinking_s if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib0.rf_hw_school if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib0.rf_hw_near_toilets_s if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib0.rf_hw_water_near_toilets_s if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib0.rf_hw_water_soap_near_toilets_s if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib1.rf_months_without_water if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib0.rf_wash_water_soap_home if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib0.rf_improved_water_source_h if group == 2 || school_code:, iter(1)  startg() family(nb)

*********************************
*Any SCH
*********************************
*Before COVID
meglm schisto_prev ib0.rf_improved_water_source_s if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib0.rf_water_drinking_s if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib0.rf_hw_school if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib0.rf_hw_near_toilets_s if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib0.rf_hw_water_near_toilets_s if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib0.rf_hw_water_soap_near_toilets_s if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib1.rf_months_without_water if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib0.rf_wash_water_soap_home if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib0.rf_improved_water_source_h if group == 1 || school_code:, iter(1)  startg() family(nb)


*After COVID
meglm schisto_prev ib0.rf_improved_water_source_s if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib0.rf_water_drinking_s if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib0.rf_hw_school if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib0.rf_hw_near_toilets_s if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib0.rf_hw_water_near_toilets_s if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib0.rf_hw_water_soap_near_toilets_s if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib1.rf_months_without_water if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib0.rf_wash_water_soap_home if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib0.rf_improved_water_source_h if group == 2 || school_code:, iter(1)  startg() family(nb)

* Sanitation related conditions
gen rf_latrine = latrine
gen rf_separate_toilets_s = separate_toilets_s
gen rf_latrine_improved_s = latrine_improved_s
recode number_latrines (min/4 = 0 "≤4") (5/max = 1 ">4"), gen(rf_number_latrines)
recode pupil_latrine_ratio_overall (min/25.0 = 0 "≤25.0") (25.01/max = 1 ">25.0"), gen(rf_pupil_latrine_ratio_overall)
gen rf_latrine_unhygienic = latrine_unhygienic
gen rf_latrine_structure = latrine_structure
gen rf_latrine_clean = latrine_clean
recode toilet_home (min/50 = 0 "≤50") (51/max = 1 ">50"), gen(rf_toilet_home)

*********************************
*Any STH
*********************************
*Before COVID
meglm sth_prev ib0.rf_latrine if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib0.rf_separate_toilets_s if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib0.rf_latrine_improved_s if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib0.rf_number_latrines if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib1.rf_pupil_latrine_ratio_overall if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib1.rf_latrine_unhygienic if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib0.rf_latrine_structure if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib0.rf_latrine_clean if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib0.rf_toilet_home if group == 1 || school_code:, iter(1)  startg() family(nb)


*After COVID
meglm sth_prev ib0.rf_latrine if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib0.rf_separate_toilets_s if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib0.rf_latrine_improved_s if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib0.rf_number_latrines if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib1.rf_pupil_latrine_ratio_overall if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib1.rf_latrine_unhygienic if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib0.rf_latrine_structure if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib0.rf_latrine_clean if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib0.rf_toilet_home if group == 2 || school_code:, iter(1)  startg() family(nb)


*********************************
*Any SCH
*********************************
*Before COVID
meglm schisto_prev ib0.rf_latrine if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib0.rf_separate_toilets_s if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib0.rf_latrine_improved_s if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib0.rf_number_latrines if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib1.rf_pupil_latrine_ratio_overall if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib1.rf_latrine_unhygienic if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib0.rf_latrine_structure if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib0.rf_latrine_clean if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib0.rf_toilet_home if group == 1 || school_code:, iter(1)  startg() family(nb)


*After COVID
meglm schisto_prev ib0.rf_latrine if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib0.rf_separate_toilets_s if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib0.rf_latrine_improved_s if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib0.rf_number_latrines if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib1.rf_pupil_latrine_ratio_overall if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib1.rf_latrine_unhygienic if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib0.rf_latrine_structure if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib0.rf_latrine_clean if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib0.rf_toilet_home if group == 2 || school_code:, iter(1)  startg() family(nb)


* Hygiene related conditions
gen rf_school_provide_anal_cleansing = school_provide_anal_cleansing
recode anal_cleansing_s (min/50 = 0 "≤50") (51/max = 1 ">50"), gen(rf_anal_cleansing_s)
recode wash_hands_school (min/50 = 0 "≤50") (51/max = 1 ">50"), gen(rf_wash_hands_school)
recode anal_cleansing_h (min/50 = 0 "≤50") (51/max = 1 ">50"), gen(rf_anal_cleansing_h)
recode wash_hands_home (min/50 = 0 "≤50") (51/max = 1 ">50"), gen(rf_wash_hands_home)

*********************************
*Any STH
*********************************
*Before COVID
meglm sth_prev ib0.rf_school_provide_anal_cleansing if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib0.rf_anal_cleansing_s if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib0.rf_wash_hands_school if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib0.rf_anal_cleansing_h if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib0.rf_wash_hands_home if group == 1 || school_code:, iter(1)  startg() family(nb)


*After COVID
meglm sth_prev ib0.rf_school_provide_anal_cleansing if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib0.rf_anal_cleansing_s if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib0.rf_wash_hands_school if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib0.rf_anal_cleansing_h if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib0.rf_wash_hands_home if group == 2 || school_code:, iter(1)  startg() family(nb)

*********************************
*Any SCH
*********************************
*Before COVID
meglm schisto_prev ib0.rf_school_provide_anal_cleansing if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib0.rf_anal_cleansing_s if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib0.rf_wash_hands_school if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib0.rf_anal_cleansing_h if group == 1 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib0.rf_wash_hands_home if group == 1 || school_code:, iter(1)  startg() family(nb)


*After COVID
meglm schisto_prev ib0.rf_school_provide_anal_cleansing if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib0.rf_anal_cleansing_s if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib0.rf_wash_hands_school if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib0.rf_anal_cleansing_h if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib0.rf_wash_hands_home if group == 2 || school_code:, iter(1)  startg() family(nb)


* COVID-19 related conditions
recode cov19_knowledge (min/50 = 0 "≤50") (51/max = 1 ">50"), gen(rf_cov19_knowledge)
gen rf_cov19_sensitization = cov19_sensitization
gen rf_cov19_teachers_trained = cov19_teachers_trained
gen rf_cov19_students_trained = cov19_students_trained
gen rf_cov19_handwashing_stations = cov19_handwashing_stations
gen rf_cov19_hws_water_soap = cov19_hws_water_soap
recode cov19_wash_hands_school (min/50 = 0 "≤50") (51/max = 1 ">50"), gen(rf_cov19_wash_hands_school)
gen rf_cov19_IEC_material = cov19_IEC_material
recode cov19_hws_ws_home (min/50 = 0 "≤50") (51/max = 1 ">50"), gen(rf_cov19_hws_ws_home)
recode cov19_wash_hands_home (min/50 = 0 "≤50") (51/max = 1 ">50"), gen(rf_cov19_wash_hands_home)

*********************************
*Any STH
*********************************
*Before COVID
***

*After COVID
meglm sth_prev ib0.rf_cov19_knowledge if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib0.rf_cov19_sensitization if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib0.rf_cov19_teachers_trained if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib0.rf_cov19_students_trained if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib0.rf_cov19_handwashing_stations if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib0.rf_cov19_hws_water_soap if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib0.rf_cov19_wash_hands_school if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib0.rf_cov19_IEC_material if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib0.rf_cov19_hws_ws_home if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm sth_prev ib0.rf_cov19_wash_hands_home if group == 2 || school_code:, iter(1)  startg() family(nb)


*********************************
*Any SCH
*********************************
*Before COVID
***

*After COVID
meglm schisto_prev ib0.rf_cov19_knowledge if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib0.rf_cov19_sensitization if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib0.rf_cov19_teachers_trained if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib0.rf_cov19_students_trained if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib0.rf_cov19_handwashing_stations if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib0.rf_cov19_hws_water_soap if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib0.rf_cov19_wash_hands_school if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib0.rf_cov19_IEC_material if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib0.rf_cov19_hws_ws_home if group == 2 || school_code:, iter(1)  startg() family(nb)
meglm schisto_prev ib0.rf_cov19_wash_hands_home if group == 2 || school_code:, iter(1)  startg() family(nb)


******************************************************************************
****: Table 1:

use "02_PCA_dataset.dta", clear

* Before performing PCA, we computed covariance matrix to identify any significant correlations
pwcorr pca*

* Demographic conditions
pca pca_number_children_enrolled pca_number_teachers pca_pupils_teacher_ratio pca_eat_soil pca_shoe_wearing pca_SES_index, vce(normal)

pca // To redisplay the principal-component output after performing pca 
screeplot // To obtain a scree plot of the eigenvalues
screeplot, yline(1) ci(het) // Adds a line across the y-axis at 1 and adds heteroskedastic bootstrap confidence intervals.

predict pc1, score // We decided to obtain the scores of the first component only since it contains the best scores (creates new variable)

xtile quart = pc1, nq(2) // To create tertiles

rename pc1 demographic_conditions
rename quart demographic_conditions_tertiles

* Treatment related conditions
pca pca_water_sanitation_program pca_feeding_program pca_deworming_program pca_teacher_trained_deworming pca_Deworming_IEC_material pca_treatment, vce(normal)

pca // To redisplay the principal-component output after performing pca 
screeplot // To obtain a scree plot of the eigenvalues
screeplot, yline(1) ci(het) // Adds a line across the y-axis at 1 and adds heteroskedastic bootstrap confidence intervals.

predict pc1, score // We decided to obtain the scores of the first component only since it contains the best scores (creates new variable)

xtile quart = pc1, nq(2) // To create tertiles

rename pc1 treatment_conditions
rename quart treatment_conditions_tertiles

* Water related conditions
pca pca_improved_water_source_s pca_water_drinking_s pca_hw_school pca_hw_near_toilets_s pca_hw_water_near_toilets_s pca_hw_water_soap_near_toilets_s pca_months_without_water pca_wash_water_soap_home pca_improved_water_source_h

pca // To redisplay the principal-component output after performing pca 
screeplot // To obtain a scree plot of the eigenvalues
screeplot, yline(1) ci(het) // Adds a line across the y-axis at 1 and adds heteroskedastic bootstrap confidence intervals.

predict pc1, score // We decided to obtain the scores of the first component only since it contains the best scores (creates new variable)

xtile quart = pc1, nq(2) // To create tertiles

rename pc1 water_conditions
rename quart water_conditions_tertiles

* Sanitation related conditions
pca pca_latrine pca_separate_toilets_s pca_latrine_improved_s pca_number_latrines pca_pupil_latrine_ratio_girls pca_pupil_latrine_ratio_boys pca_pupil_latrine_ratio_overall pca_latrine_unhygienic pca_latrine_structure pca_latrine_clean pca_toilet_home

pca // To redisplay the principal-component output after performing pca 
screeplot // To obtain a scree plot of the eigenvalues
screeplot, yline(1) ci(het) // Adds a line across the y-axis at 1 and adds heteroskedastic bootstrap confidence intervals.

predict pc1, score // We decided to obtain the scores of the first component only since it contains the best scores (creates new variable)

xtile quart = pc1, nq(2) // To create tertiles

rename pc1 sanitation_conditions
rename quart sanitation_conditions_tertiles

* Hygiene related conditions
pca pca_school_provide_anal_cleans pca_anal_cleansing_s pca_wash_hands_school pca_anal_cleansing_h pca_wash_hands_home

pca // To redisplay the principal-component output after performing pca 
screeplot // To obtain a scree plot of the eigenvalues
screeplot, yline(1) ci(het) // Adds a line across the y-axis at 1 and adds heteroskedastic bootstrap confidence intervals.

predict pc1, score // We decided to obtain the scores of the first component only since it contains the best scores (creates new variable)

xtile quart = pc1, nq(2) // To create tertiles

rename pc1 hygiene_conditions
rename quart hygiene_conditions_tertiles

* COVID-19 related conditions
pca pca_cov19_knowledge pca_cov19_sensitization pca_cov19_teachers_trained pca_cov19_students_trained pca_cov19_handwashing_stations pca_cov19_hws_water_soap pca_cov19_wash_hands_school pca_cov19_IEC_material pca_cov19_hws_ws_home pca_cov19_wash_hands_home

pca // To redisplay the principal-component output after performing pca 
screeplot // To obtain a scree plot of the eigenvalues
screeplot, yline(1) ci(het) // Adds a line across the y-axis at 1 and adds heteroskedastic bootstrap confidence intervals.

predict pc1, score // We decided to obtain the scores of the first component only since it contains the best scores (creates new variable)

xtile quart = pc1, nq(2) // To create tertiles

rename pc1 covid19_conditions
rename quart covid19_conditions_tertiles


****: Table 2:

*********************************
*Any STH
*********************************
*Before COVID
meglm sth_prev demographic_conditions if group == 1 || school_code:, startg() family(nb)
meglm sth_prev treatment_conditions if group == 1 || school_code:, startg() family(nb)
meglm sth_prev water_conditions if group == 1 || school_code:, startg() family(nb)
meglm sth_prev sanitation_conditions if group == 1 || school_code:, startg() family(nb)
meglm sth_prev hygiene_conditions if group == 1 || school_code:, startg() family(nb)
meglm sth_prev covid19_conditions if group == 1 || school_code:, startg() family(nb)
*After COVID
meglm sth_prev demographic_conditions if group == 2 || school_code:, startg() family(nb)
meglm sth_prev treatment_conditions if group == 2 || school_code:, startg() family(nb)
meglm sth_prev water_conditions if group == 2 || school_code:, startg() family(nb)
meglm sth_prev sanitation_conditions if group == 2 || school_code:, startg() family(nb)
meglm sth_prev hygiene_conditions if group == 2 || school_code:, startg() family(nb)
meglm sth_prev covid19_conditions if group == 2 || school_code:, startg() family(nb)
*DID (Effect estimation)
meglm sth_prev i.group##i.demographic_conditions_tertiles || school_code:, startg() 
meglm sth_prev i.group##i.treatment_conditions_tertiles || school_code:, startg() 
meglm sth_prev i.group##i.water_conditions_tertiles || school_code:, startg() 
meglm sth_prev i.group##i.sanitation_conditions_tertiles || school_code:, startg() 
meglm sth_prev i.group##i.hygiene_conditions_tertiles || school_code:, startg() 
meglm sth_prev i.group##i.covid19_conditions_tertiles || school_code:, startg() 


*********************************
*Any SCH
*********************************
*Before COVID
meglm schisto_prev demographic_conditions if group == 1 || school_code:, startg() family(nb)
meglm schisto_prev treatment_conditions if group == 1 || school_code:, startg() family(nb)
meglm schisto_prev water_conditions if group == 1 || school_code:, startg() family(nb)
meglm schisto_prev sanitation_conditions if group == 1 || school_code:, startg() family(nb)
meglm schisto_prev hygiene_conditions if group == 1 || school_code:, startg() family(nb)
meglm schisto_prev covid19_conditions if group == 1 || school_code:, startg() family(nb)
*After COVID
meglm schisto_prev demographic_conditions if group == 2 || school_code:, startg() family(nb)
meglm schisto_prev treatment_conditions if group == 2 || school_code:, startg() family(nb)
meglm schisto_prev water_conditions if group == 2 || school_code:, startg() family(nb)
meglm schisto_prev sanitation_conditions if group == 2 || school_code:, startg() family(nb)
meglm schisto_prev hygiene_conditions if group == 2 || school_code:, startg() family(nb)
meglm schisto_prev covid19_conditions if group == 2 || school_code:, startg() family(nb)
*DID (Effect estimation)
meglm schisto_prev i.group##i.demographic_conditions_tertiles || school_code:, startg() 
meglm schisto_prev i.group##i.treatment_conditions_tertiles || school_code:, startg() 
meglm schisto_prev i.group##i.water_conditions_tertiles || school_code:, startg() 
meglm schisto_prev i.group##i.sanitation_conditions_tertiles || school_code:, startg() 
meglm schisto_prev i.group##i.hygiene_conditions_tertiles || school_code:, startg() 
meglm schisto_prev i.group##i.covid19_conditions_tertiles || school_code:, startg() 



****: Table 3:

*********************************
*Any STH
*********************************
*Before COVID
meglm sth_prev demographic_conditions treatment_conditions water_conditions  sanitation_conditions hygiene_conditions if group == 1 || school_code:, startg() family(nb)

*After COVID
meglm sth_prev demographic_conditions treatment_conditions water_conditions  sanitation_conditions hygiene_conditions covid19_conditions if group == 2 || school_code:, startg() family(nb)

*DID (Effect estimation)
meglm sth_prev i.group##i.demographic_conditions_tertiles i.group##i.treatment_conditions_tertiles i.group##i.water_conditions_tertiles i.group##i.sanitation_conditions_tertiles i.group##i.hygiene_conditions_tertiles || school_code:, startg()

*********************************
*Any SCH
*********************************
*Before COVID
meglm schisto_prev demographic_conditions treatment_conditions water_conditions  sanitation_conditions hygiene_conditions if group == 1 || school_code:, startg() family(nb)

*After COVID
meglm schisto_prev demographic_conditions treatment_conditions water_conditions  sanitation_conditions hygiene_conditions covid19_conditions if group == 2 || school_code:, startg() family(nb)

*DID (Effect estimation)
meglm schisto_prev i.group##i.demographic_conditions_tertiles i.group##i.treatment_conditions_tertiles i.group##i.water_conditions_tertiles i.group##i.sanitation_conditions_tertiles i.group##i.hygiene_conditions_tertiles 



*****************************************************************************
**: Using individual level data

use "02_PCA_dataset_individual_level.dta", clear

****: Table 2:

*********************************
*Any STH
*********************************
*Before COVID
meglm sth_prev demographic_conditions if group == 1 || subcounty_code: || school_code:, iter(0) startg() family(nb)
meglm sth_prev treatment_conditions if group == 1 || subcounty_code: || school_code:, iter(0) startg() family(nb)
meglm sth_prev water_conditions if group == 1 || subcounty_code: || school_code:, iter(0) startg() family(nb)
meglm sth_prev sanitation_conditions if group == 1 || subcounty_code: || school_code:, iter(0) startg() family(nb)
meglm sth_prev hygiene_conditions if group == 1 || subcounty_code: || school_code:, iter(0) startg() family(nb)
meglm sth_prev covid19_conditions if group == 1 || subcounty_code: || school_code:, iter(0) startg() family(nb)
*After COVID
meglm sth_prev demographic_conditions if group == 2 || subcounty_code: || school_code:, iter(0) startg() family(nb)
meglm sth_prev treatment_conditions if group == 2 || subcounty_code: || school_code:, iter(0) startg() family(nb)
meglm sth_prev water_conditions if group == 2 || subcounty_code: || school_code:, iter(0) startg() family(nb)
meglm sth_prev sanitation_conditions if group == 2 || subcounty_code: || school_code:, iter(0) startg() family(nb)
meglm sth_prev hygiene_conditions if group == 2 || subcounty_code: || school_code:, iter(0) startg() family(nb)
meglm sth_prev covid19_conditions if group == 2 || subcounty_code: || school_code:, iter(0) startg() family(nb)
*DID (Effect estimation)
meglm sth_prev i.group##i.demographic_conditions_tertiles || subcounty_code: || school_code:, iter(0) startg() 
meglm sth_prev i.group##i.treatment_conditions_tertiles || subcounty_code: || school_code:, iter(0) startg() 
meglm sth_prev i.group##i.water_conditions_tertiles || subcounty_code: || school_code:, iter(0) startg() 
meglm sth_prev i.group##i.sanitation_conditions_tertiles || subcounty_code: || school_code:, iter(0) startg() 
meglm sth_prev i.group##i.hygiene_conditions_tertiles || subcounty_code:, iter(0) startg() 
meglm sth_prev i.group##i.covid19_conditions_tertiles || subcounty_code: || school_code:, iter(0) startg() 

*********************************
*Any SCH
*********************************
*Before COVID
meglm schisto_prev demographic_conditions if group == 1 || subcounty_code: || school_code:, iter(0) startg() family(nb)
meglm schisto_prev treatment_conditions if group == 1 || subcounty_code: || school_code:, iter(0) startg() family(nb)
meglm schisto_prev water_conditions if group == 1 || subcounty_code: || school_code:, iter(0) startg() family(nb)
meglm schisto_prev sanitation_conditions if group == 1 || subcounty_code: || school_code:, iter(0) startg() family(nb)
meglm schisto_prev hygiene_conditions if group == 1 || subcounty_code: || school_code:, iter(0) startg() family(nb)
meglm schisto_prev covid19_conditions if group == 1 || subcounty_code: || school_code:, iter(0) startg() family(nb)
*After COVID
meglm schisto_prev demographic_conditions if group == 2 || subcounty_code: || school_code:, iter(0) startg() family(nb)
meglm schisto_prev treatment_conditions if group == 2 || subcounty_code: || school_code:, iter(0) startg() family(nb)
meglm schisto_prev water_conditions if group == 2 || subcounty_code: || school_code:, iter(0) startg() family(nb)
meglm schisto_prev sanitation_conditions if group == 2 || subcounty_code: || school_code:, iter(0) startg() family(nb)
meglm schisto_prev hygiene_conditions if group == 2 || subcounty_code: || school_code:, iter(0) startg() family(nb)
meglm schisto_prev covid19_conditions if group == 2 || subcounty_code: || school_code:, iter(0) startg() family(nb)
*DID (Effect estimation)
meglm schisto_prev i.group##i.demographic_conditions_tertiles || subcounty_code:, iter(0) startg() 
meglm schisto_prev i.group##i.treatment_conditions_tertiles || subcounty_code:, iter(0) startg() 
meglm schisto_prev i.group##i.water_conditions_tertiles || subcounty_code:, iter(0) startg() 
meglm schisto_prev i.group##i.sanitation_conditions_tertiles || subcounty_code:, iter(0) startg() 
meglm schisto_prev i.group##i.hygiene_conditions_tertiles || subcounty_code:, iter(0) startg() 
meglm schisto_prev i.group##i.covid19_conditions_tertiles || subcounty_code:, iter(0) startg() 

****: Table 3:

*********************************
*Any STH
*********************************
*Before COVID
meglm sth_prev demographic_conditions treatment_conditions water_conditions  sanitation_conditions hygiene_conditions if group == 1 || subcounty_code: || school_code:, iter(0) startg() family(nb)

*After COVID
meglm sth_prev demographic_conditions treatment_conditions water_conditions  sanitation_conditions hygiene_conditions covid19_conditions if group == 2 || subcounty_code: || school_code:, iter(0) startg() family(nb)

*DID (Effect estimation)
meglm sth_prev i.group##i.demographic_conditions_tertiles i.group##i.treatment_conditions_tertiles i.group##i.water_conditions_tertiles i.group##i.sanitation_conditions_tertiles i.group##i.hygiene_conditions_tertiles || subcounty_code:, iter(0) startg()

*********************************
*Any SCH
*********************************
*Before COVID
meglm schisto_prev demographic_conditions treatment_conditions water_conditions  sanitation_conditions hygiene_conditions if group == 1 || subcounty_code: || school_code:, iter(0) startg() family(nb)

*After COVID
meglm schisto_prev demographic_conditions treatment_conditions water_conditions  sanitation_conditions hygiene_conditions covid19_conditions if group == 2 || subcounty_code: || school_code:, iter(0) startg() family(nb)

*DID (Effect estimation)
meglm schisto_prev i.group##i.demographic_conditions_tertiles i.group##i.treatment_conditions_tertiles i.group##i.water_conditions_tertiles i.group##i.sanitation_conditions_tertiles i.group##i.hygiene_conditions_tertiles || subcounty_code:, iter(0) startg()


******************************************************************************
****: Revised Table 2:

*********************************
*Any STH
*********************************
*Before COVID
meglm sth_prev demographic_conditions if group == 1 || subcounty_code: || school_code: || id:, iter(0) startg() family(nb)
meglm sth_prev treatment_conditions if group == 1 || subcounty_code: || school_code:, iter(0) startg() family(nb)
meglm sth_prev water_conditions if group == 1 || subcounty_code: || school_code:, iter(0) startg() family(nb)
meglm sth_prev sanitation_conditions if group == 1 || subcounty_code: || school_code:, iter(0) startg() family(nb)
meglm sth_prev hygiene_conditions if group == 1 || subcounty_code: || school_code:, iter(0) startg() family(nb)
meglm sth_prev covid19_conditions if group == 1 || subcounty_code: || school_code:, iter(0) startg() family(nb)
*After COVID
meglm sth_prev demographic_conditions if group == 2 || subcounty_code: || school_code:, iter(0) startg() family(nb)
meglm sth_prev treatment_conditions if group == 2 || subcounty_code: || school_code:, iter(0) startg() family(nb)
meglm sth_prev water_conditions if group == 2 || subcounty_code: || school_code:, iter(0) startg() family(nb)
meglm sth_prev sanitation_conditions if group == 2 || subcounty_code: || school_code:, iter(0) startg() family(nb)
meglm sth_prev hygiene_conditions if group == 2 || subcounty_code: || school_code:, iter(0) startg() family(nb)
meglm sth_prev covid19_conditions if group == 2 || subcounty_code: || school_code:, iter(0) startg() family(nb)
*DID (Effect estimation)
meglm sth_prev i.group##i.demographic_conditions_tertiles || subcounty_code: || school_code: || id:, iter(0) startg() 
meglm sth_prev i.group##i.treatment_conditions_tertiles || subcounty_code: || school_code: || id:, iter(0) startg() 
meglm sth_prev i.group##i.water_conditions_tertiles || subcounty_code: || school_code: || id:, iter(0) startg() 
meglm sth_prev i.group##i.sanitation_conditions_tertiles || subcounty_code: || school_code: || id:, iter(0) startg() 
meglm sth_prev i.group##i.hygiene_conditions_tertiles || subcounty_code: || id:, iter(0) startg() 
meglm sth_prev i.group##i.covid19_conditions_tertiles || subcounty_code: || school_code: || id:, iter(0) startg() 

*********************************
*Any SCH
*********************************
*Before COVID
meglm schisto_prev demographic_conditions if group == 1 || subcounty_code: || school_code:, iter(0) startg() family(nb)
meglm schisto_prev treatment_conditions if group == 1 || subcounty_code: || school_code:, iter(0) startg() family(nb)
meglm schisto_prev water_conditions if group == 1 || subcounty_code: || school_code:, iter(0) startg() family(nb)
meglm schisto_prev sanitation_conditions if group == 1 || subcounty_code: || school_code:, iter(0) startg() family(nb)
meglm schisto_prev hygiene_conditions if group == 1 || subcounty_code: || school_code:, iter(0) startg() family(nb)
meglm schisto_prev covid19_conditions if group == 1 || subcounty_code: || school_code:, iter(0) startg() family(nb)
*After COVID
meglm schisto_prev demographic_conditions if group == 2 || subcounty_code: || school_code:, iter(0) startg() family(nb)
meglm schisto_prev treatment_conditions if group == 2 || subcounty_code: || school_code:, iter(0) startg() family(nb)
meglm schisto_prev water_conditions if group == 2 || subcounty_code: || school_code:, iter(0) startg() family(nb)
meglm schisto_prev sanitation_conditions if group == 2 || subcounty_code: || school_code:, iter(0) startg() family(nb)
meglm schisto_prev hygiene_conditions if group == 2 || subcounty_code: || school_code:, iter(0) startg() family(nb)
meglm schisto_prev covid19_conditions if group == 2 || subcounty_code: || school_code:, iter(0) startg() family(nb)
*DID (Effect estimation)
meglm schisto_prev i.group##i.demographic_conditions_tertiles || subcounty_code:, iter(0) startg() 
meglm schisto_prev i.group##i.treatment_conditions_tertiles || subcounty_code:, iter(0) startg() 
meglm schisto_prev i.group##i.water_conditions_tertiles || subcounty_code:, iter(0) startg() 
meglm schisto_prev i.group##i.sanitation_conditions_tertiles || subcounty_code:, iter(0) startg() 
meglm schisto_prev i.group##i.hygiene_conditions_tertiles || subcounty_code:, iter(0) startg() 
meglm schisto_prev i.group##i.covid19_conditions_tertiles || subcounty_code:, iter(0) startg() 

****: Revised Table 3:

*********************************
*Any STH
*********************************
*Before COVID
meglm sth_prev demographic_conditions treatment_conditions water_conditions  sanitation_conditions hygiene_conditions if group == 1 || subcounty_code: || school_code:, iter(0) startg() family(nb)

*After COVID
meglm sth_prev demographic_conditions treatment_conditions water_conditions  sanitation_conditions hygiene_conditions covid19_conditions if group == 2 || subcounty_code: || school_code:, iter(0) startg() family(nb)

*DID (Effect estimation)
meglm sth_prev i.group##i.demographic_conditions_tertiles i.group##i.treatment_conditions_tertiles i.group##i.water_conditions_tertiles i.group##i.sanitation_conditions_tertiles i.group##i.hygiene_conditions_tertiles || countycode: || id:, iter(0) startg()

*********************************
*Any SCH
*********************************
*Before COVID
meglm schisto_prev demographic_conditions treatment_conditions water_conditions  sanitation_conditions hygiene_conditions if group == 1 || subcounty_code: || school_code:, iter(0) startg() family(nb)

*After COVID
meglm schisto_prev demographic_conditions treatment_conditions water_conditions  sanitation_conditions hygiene_conditions covid19_conditions if group == 2 || subcounty_code: || school_code:, iter(0) startg() family(nb)

*DID (Effect estimation)
meglm schisto_prev i.group##i.demographic_conditions_tertiles i.group##i.treatment_conditions_tertiles i.group##i.water_conditions_tertiles i.group##i.sanitation_conditions_tertiles i.group##i.hygiene_conditions_tertiles || countycode: || id:, iter(0) startg()


********************************************************************************
* Revised individual level analysis (reporting odds ratios)

use "03_pre_post_individual_data_rev.dta", clear

*********************************
*Any STH
*********************************
*Before COVID
meglm sthinfect demographic_conditions if group == 1 || subcounty_code: || school_code:, iter(0) startg() family(bi) or
meglm sthinfect treatment_conditions if group == 1 || subcounty_code: || school_code:, iter(0) startg() family(bi) or
meglm sthinfect water_conditions if group == 1 || subcounty_code: || school_code:, iter(0) startg() family(bi) or
meglm sthinfect sanitation_conditions if group == 1 || subcounty_code: || school_code:, iter(0) startg() family(bi) or
meglm sthinfect hygiene_conditions if group == 1 || subcounty_code: || school_code:, iter(0) startg() family(bi) or
meglm sthinfect covid19_conditions if group == 1 || subcounty_code: || school_code:, iter(0) startg() family(bi) or
*After COVID
meglm sthinfect demographic_conditions if group == 2 || subcounty_code: || school_code:, iter(0) startg() family(bi) or
meglm sthinfect treatment_conditions if group == 2 || subcounty_code: || school_code:, iter(0) startg() family(bi) or
meglm sthinfect water_conditions if group == 2 || subcounty_code: || school_code:, iter(0) startg() family(bi) or
meglm sthinfect sanitation_conditions if group == 2 || subcounty_code: || school_code:, iter(0) startg() family(bi) or
meglm sthinfect hygiene_conditions if group == 2 || subcounty_code: || school_code:, iter(0) startg() family(bi) or
meglm sthinfect covid19_conditions if group == 2 || subcounty_code: || school_code:, iter(0) startg() family(bi) or
*DID (Effect estimation)
meglm sthinfect i.group##c.demographic_conditions || subcounty_code: || school_code: || id:, iter(0) startg() family(bi) or
meglm sthinfect i.group##c.treatment_conditions || subcounty_code: || school_code: || id:, iter(0) startg() family(bi) or
meglm sthinfect i.group##c.water_conditions || subcounty_code: || school_code: || id:, iter(0) startg() family(bi) or
meglm sthinfect i.group##c.sanitation_conditions || subcounty_code: || school_code: || id:, iter(0) startg() family(bi) or
meglm sthinfect i.group##c.hygiene_conditions || subcounty_code: || id:, iter(0) startg() family(bi) or
meglm sthinfect i.group##c.covid19_conditions || subcounty_code: || school_code: || id:, iter(0) startg() family(bi) or

* Mixed-effects logistic regression
melogit sthinfect i.group##c.demographic_conditions || subcounty_code: || school_code: unique_id, or
melogit sthinfect i.group##c.treatment_conditions || subcounty_code: || school_code:, or
melogit sthinfect i.group##c.water_conditions || subcounty_code: || school_code:, or
melogit sthinfect i.group##c.sanitation_conditions || subcounty_code: || school_code:, or
melogit sthinfect i.group##c.hygiene_conditions || subcounty_code: || school_code:, or
melogit sthinfect i.group##c.covid19_conditions || subcounty_code: || school_code:, or






