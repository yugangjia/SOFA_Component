with cabg_adm as
(select  distinct hadm_id as cabg_id from `physionet-data.mimic_hosp.drgcodes` 
where drg_code in ("231","232" ,"233" , "234" ,"235" ,"236")),
cirrhosis as (select  distinct hadm_id as cirrhosis_id from physionet-data.mimic_hosp.diagnoses_icd
where icd_code in ("K7030","K7031", "K717", "K743", "K744", "K745", "K7460", "K7469","5712", "5715", "5716")),
esrd as (select  distinct hadm_id as ESRD_id from physionet-data.mimic_hosp.diagnoses_icd
where icd_code in ("N186", "5856")),
maxhr as (select stay_id,max(hr) as max_hr from physionet-data.mimic_derived.sofa group by stay_id),
sofa24 as (select stay_id,respiration_24hours as resp_24,coagulation_24hours as coag_24,liver_24hours as liver_24,
cardiovascular_24hours as cv_24,cns_24hours as cns_24,renal_24hours as renal_24 from physionet-data.mimic_derived.sofa where hr = 24 ),
sofa168 as (select stay_id,respiration_24hours as resp_168,coagulation_24hours as coag_168,liver_24hours as liver_168,
cardiovascular_24hours as cv_168,cns_24hours as cns_168,renal_24hours as renal_168 from physionet-data.mimic_derived.sofa where hr = 168),
sofalast as (select s1.stay_id,maxhr.max_hr,respiration_24hours as resp_last,coagulation_24hours as coag_last,liver_24hours as liver_last,
cardiovascular_24hours as cv_last,cns_24hours as cns_last,renal_24hours as renal_last from physionet-data.mimic_derived.sofa as s1 right join maxhr on s1.hr=maxhr.max_hr and s1.stay_id = maxhr.stay_id),
vent24 as (select s.stay_id,v.ventilation_status as vent_24 from (select * from physionet-data.mimic_derived.sofa where hr=24)  as s inner join 
(select * from `physionet-data.mimic_derived.ventilation` where ventilation_status != "None") as v 
on s.stay_id = v.stay_id  and s.starttime >= DATETIME_TRUNC(v.starttime, HOUR) and s.endtime<=DATETIME_TRUNC(v.endtime,HOUR)),
vent168 as (select s.stay_id,v.ventilation_status as vent_168 from (select * from physionet-data.mimic_derived.sofa where hr=168)  as s inner join 
(select * from `physionet-data.mimic_derived.ventilation` where ventilation_status != "None") as v 
on s.stay_id = v.stay_id  and s.starttime >= DATETIME_TRUNC(v.starttime, HOUR) and s.endtime<=DATETIME_TRUNC(v.endtime,HOUR)),
ventlast as  (select v.stay_id, v.ventilation_status as vent_last from (select sf.stay_id,sf.starttime,sf.endtime,sf.hr from physionet-data.mimic_derived.sofa as sf 
inner join maxhr on maxhr.stay_id = sf.stay_id and maxhr.max_hr=sf.hr+24) 
as s left join 
(select * from `physionet-data.mimic_derived.ventilation` where ventilation_status = "InvasiveVent") as v 
on s.stay_id = v.stay_id  and s.starttime <= v.endtime),
adm as (select hadm_id, deathtime,admission_type,admission_location,discharge_location from `physionet-data.mimic_core.admissions`)
select distinct * from  `physionet-data.mimic_derived.icustay_detail` as cohort 
left join sofa24 on cohort.stay_id = sofa24.stay_id 
left join sofa168 on cohort.stay_id = sofa168.stay_id
left join sofalast on cohort.stay_id = sofalast.stay_id
left join vent24 on cohort.stay_id = vent24.stay_id
left join vent168 on cohort.stay_id = vent168.stay_id
left join ventlast on ventlast.stay_id = cohort.stay_id
left join adm on cohort.hadm_id = adm.hadm_id
left join physionet-data.mimic_derived.sepsis3 as s3 on cohort.stay_id = s3.stay_id
left join cabg_adm on cohort.hadm_id = cabg_adm.cabg_id
left join cirrhosis on cohort.hadm_id = cirrhosis.cirrhosis_id
left join esrd on cohort.hadm_id = esrd.esrd_id