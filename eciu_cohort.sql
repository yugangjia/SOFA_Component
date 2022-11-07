with 
sofa24 as (select patientunitstayid as s24_id,sofa_resp as resp_24,sofa_gcs as cns_24,sofa_circ as cv_24,sofa_liver as liver_24,sofa_hematology as coag_24, sofa_renal as renal_24   from `icu-elos.eicu_sepsis.itu_sofa_day` where day = 1 ),
sofa168 as (select patientunitstayid as s168_id,sofa_resp as resp_168,sofa_gcs as cns_168,sofa_circ as cv_168,sofa_liver as liver_168,sofa_hematology as coag_168, sofa_renal as renal_168   from `icu-elos.eicu_sepsis.itu_sofa_day` where day = 7 ),
vent24 as (select patientunitstayid as mv24_id from `icu-elos.eicu_sepsis.invasive` where starttime<24*60 and endtime>24*60),
vent168 as (select patientunitstayid as mv168_id from `icu-elos.eicu_sepsis.invasive` where starttime<168*60 and endtime>168*60),
patient as (select patientunitstayid as p_id, hospitaldischargelocation, unitdischargestatus, hospitaldischargestatus from `physionet-data.eicu_crd.patient` ),

cabg_adm as (select distinct patientunitstayid as cabg_id from `physionet-data.eicu_crd.patient`
where apacheadmissiondx in ("CABG alone, coronary artery bypass grafting","CABG redo with valve repair/replacement",
"CABG with mitral valve replacement","CABG alone, redo","CABG with double valve repair/replacement",
"CABG with aortic valve replacement","CABG with other operation","CABG with mitral valve repair","CABG redo with other operation","CABG with pulmonic or tricuspid valve repair or replacement ONLY.","CABG, minimally invasive; mid-CABG")),

cirrhosis as (select distinct * from (
select distinct patientunitstayid as cirrhosis_id  from `physionet-data.eicu_crd.pasthistory` 
where pasthistorypath in (
"notes/Progress Notes/Past History/Organ Systems/Gastrointestinal (R)/Cirrhosis/jaundice",
"notes/Progress Notes/Past History/Organ Systems/Gastrointestinal (R)/Cirrhosis/UGI bleeding",
"notes/Progress Notes/Past History/Organ Systems/Gastrointestinal (R)/Cirrhosis/encephalopathy",
"notes/Progress Notes/Past History/Organ Systems/Gastrointestinal (R)/Cirrhosis/ascites",
"notes/Progress Notes/Past History/Organ Systems/Gastrointestinal (R)/Cirrhosis/varices",
"notes/Progress Notes/Past History/Organ Systems/Gastrointestinal (R)/Cirrhosis/biopsy proven",
"notes/Progress Notes/Past History/Organ Systems/Gastrointestinal (R)/Cirrhosis/clinical diagnosis",
"notes/Progress Notes/Past History/Organ Systems/Gastrointestinal (R)/Cirrhosis/coma")
union all
select distinct patientunitstayid from `physionet-data.eicu_crd.apachepredvar`
where cirrhosis=1
union all
select distinct patientunitstayid from `physionet-data.eicu_crd.diagnosis`  where diagnosisstring in ("gastrointestinal|hepatic disease|hepatic dysfunction|with cirrhosis",
"gastrointestinal|hepatic disease|hepatic dysfunction|with cirrhosis|biliary", "gastrointestinal|hepatic disease|hepatic dysfunction|with cirrhosis|alcoholic", "gastrointestinal|hepatic disease|hepatic dysfunction|with cirrhosis|cryptogenic")
)),
esrd as(
select distinct patientunitstayid as esrd_id from (
select distinct patientunitstayid from `physionet-data.eicu_crd.pasthistory` 
where pasthistorypath in ("notes/Progress Notes/Past History/Organ Systems/Renal  (R)/Renal Failure/renal failure - hemodialysis")
union all
select distinct patientunitstayid from `physionet-data.eicu_crd.diagnosis`  where diagnosisstring in ("renal|disorder of kidney|ESRD (end stage renal disease)"))
)
 
select distinct * from  `physionet-data.eicu_crd_derived.icustay_detail` as cohort 
left join sofa24 on cohort.patientunitstayid = sofa24.s24_id
left join sofa168 on cohort.patientunitstayid = sofa168.s168_id
left join vent24 on cohort.patientunitstayid = vent24.mv24_id
left join vent168 on cohort.patientunitstayid = vent168.mv168_id
left join patient on cohort.patientunitstayid = patient.p_id
left join cabg_adm on cohort.patientunitstayid = cabg_adm.cabg_id
left join cirrhosis on cohort.patientunitstayid = cirrhosis.cirrhosis_id
left join esrd on cohort.patientunitstayid = esrd.esrd_id