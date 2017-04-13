/*Pilot analysis with one percent data*/

/*All data*/
libname all 'S:\data\Optum\full_dataset\original_files\SAS';

/*One percent data*/
libname one 'S:\data\Optum\SAS\onepct_sample';

/*Merge the following on patid*/
/*Also check number of patid within each*/

/*ses_ses: D_Race_Code, D_Household_Income_Range_Code, D_Education_Level_Code, patid*/
/*Has race, hh income, education data*/
/*538k observations (x100 = ~55m people, about right?)*/
data ses;
set one.Sample1pct_ses_ses;
keep patid d_race_code d_household_income_range_code d_education_level_code;
run; quit; 


/*ses_mbr: GDR_CD, yrdob, patid, eligeff, eligend*/
/*Has DOB (age), gender, beginning and end of eligibility info*/
/*694k observations (x100 = ~70m people, I think about right)*/
data mbr;
set one.Sample1pct_ses_mbr;
keep patid gdr_cd yrdob eligeff eligend;
run; quit; 


/*ses_r: Ahfsclss, Brnd_Nm, Charge, Chk_Dt, Copay, Days_Sup, Deduct, Dispfee, Fill_Dt, 
 Fst_Fill, Ndc, patid, Rfl_Nbr, Std_Cost, Std_Cost_Yr, Strength, yr, quarter*/
/*Has info on drugs - date, name, category, charge, copay, days supply, etc.*/
/*18.7m observations*/
data r;
set one.Sample1pct_ses_r;
keep patid ahfsclss brnd_nm charge chk_dt copay days_sup deduct dispfee fill_dt fst_fill
     ndc rfl_nbr std_cost std_cost_yr strength year quarter;
run; quit; 


/*ses_m: charge, coins, copay, deduct, DIAG1, DIAG2, DIAG3, DIAG4, DIAG5, drg, dstatus,
 enctr, FST_DT, ICD_FLAG, LOC_CD, patid, pos, provcat, STD_COST, STD_COST_YR, TOS_CD, 
 year, quarter*/
/*has info on visit date, diagnosis codes, charge, copay, provider category - OUTPATIENT?*/
/*38.6m observations*/
data m;
set one.Sample1pct_ses_m;
keep patid charge coins copay deduct DIAG1 DIAG2 DIAG3 DIAG4 DIAG5 drg dstatus
 enctr FST_DT ICD_FLAG LOC_CD pos provcat STD_COST STD_COST_YR TOS_CD 
 year quarter;
run; quit; 


/*ses_c: ADMIT_DATE, charge, coins, copay, deduct, DIAG1, DIAG2, DIAG3, DIAG4, DIAG5, 
 drg, dstatus, ICD_FLAG, patid, pos, STD_COST, STD_COST_YR, TOS_CD, year, quarter*/
/*has info on visit date, diagnosis codes, charge, copay - INPATIENT?*/
/*144K observations*/
data c;
set one.Sample1pct_ses_c;
keep patid ADMIT_DATE charge coins copay deduct DIAG1 DIAG2 DIAG3 DIAG4 DIAG5 
 drg dstatus ICD_FLAG pos STD_COST STD_COST_YR TOS_CD year quarter;
run; quit; 

/*Pick out inpatient (c) and outpatient (m) records with diabetes onset*/
/*Then, keep ALL longitudinal records for those patids*/
/*Inpatient data*/
data c; set c; 
diab=0;
array DXs diag1-diag5;
do i=1 to 5;
if DXs(i) in ('250', '25000', '25002', '250.00', '250.02') then diab=1;
end; 
run; quit; 

proc freq data=c; /*about 7.65% of records have diab=1*/
tables diab;
run; quit; 

/*Outpatient data*/
data m; set m; 
diab=0;
array DXs diag1-diag5;
do i=1 to 5;
if DXs(i) in ('250', '25000', '25002', '250.00', '250.02') then diab=1;
end; 
run; quit; 

proc freq data=m; /*about 5.07% of records have diab=1*/
tables diab;
run; quit; 

/*Pick out all observations from c (inpatient) with diab=1*/
proc sql; /*11k records w/ diab=1 -> 25k total rows*/
create table c2 as
select *
from c
group by patid
having sum(diab=1) > 0
order by patid;
quit; 

/*Pick out all observations from m (outpatient) with diab=1*/
proc sql; /*1.9m records w/ diab=1 -> 10.6m total rows*/
create table m2 as
select *
from m
group by patid
having sum(diab=1) > 0
order by patid;
quit; 

/*Concatenate the inpatient and outpatient data*/
data c2;
set c2;
inhosp = 1;
run; quit; 

data m2;
set m2;
inhosp = 0;
run; quit; 

data all;
set c2 (RENAME=(ADMIT_DATE=FST_DT)) m2;
run; quit; 

/*Define outcomes*/
data all; set all; 
renal = 0;
opth = 0;
neuro = 0;
circ = 0;
ulcer = 0;
ihd = 0;
stroke = 0;
hf = 0;
array DXs diag1-diag5;
do i=1 to 5;
if DXs(i) in ('25040', '25042', '250.40', '250.42') then renal = 1;
if DXs(i) in ('25050', '25052', '250.50', '250.52') then opth = 1;
if DXs(i) in ('25060', '25062', '250.60', '250.62') then neuro = 1;
if DXs(i) in ('25070', '25072', '250.70', '250.72') then circ = 1;
if DXs(i) in ('7078', '7079', '707.8', '707.9', '70710', '70711', '70712', '70713',
              '70714', '70715', '70719', '707.10', '707.11', '707.12', '707.13',
              '707.14', '707.15', '707.19') then ulcer = 1;
if DXs(i) in ('4149', '414.9') then ihd = 1;
if DXs(i) in ('43491', '434.91') then stroke = 1;
if DXs(i) in ('4280', '428.0') then hf = 1; 
end; 
run; quit; 

/*Look at data set*/
proc contents data=all;
run; quit; 

proc freq data=all;
tables renal opth neuro circ ulcer ihd stroke hf;
run; quit; 

/*Merge with covariates from ses and mbr by patid*/
proc sort data=all;
by patid FST_DT;
run; quit; 

proc sort data=ses;
by patid;
run; quit; 

data all2;
merge all (in=x) ses (in=y);
by patid;
if x=1; 
run; quit; 

proc sort data=all2;
by patid FST_DT;
run; quit; 

proc sort data=mbr;
by patid;
run; quit; 

data all3;
merge all2 (in=x) mbr (in=y);
by patid;
if x=1; 
run; quit; 

/*Merge with drugs data set*/
data r2;
set r (RENAME=(chk_dt=FST_DT));
run; quit; 

proc sort data=r2;
by patid FST_DT;
run; quit; 

proc sort data=all3;
by patid FST_DT;
run; quit; 

data all4;
set all3 r2;
by patid; 
run; quit; 

/*Only keep those IDs in all4 that have diabetes*/
proc sql; /*10.6m records w/ diab=1 supplemented w/ drug data -> 16.9m total rows*/
create table all5 as
select *
from all4
group by patid
having sum(diab=1) > 0
order by patid;
quit; 

/*Sort data and only keep those records corresponding to or after diabetes diagnosis*/
proc sort data=all5;
by patid diab FST_DT;
run; quit; 

data all5a;
set all5;
where diab=1; 
keep patid diab FST_DT;
run; quit; 

/*Get first date of diab and re-merge*/
data fdiab;
set all5a;
by patid FST_DT;
firstDiab = first.patid;
run; quit; 

/*Only keep first diab and re-merge to all5*/
data f2;
set fdiab (RENAME=(FST_DT=diab_date));
where firstDiab=1; 
keep patid diab_date;
run; quit; 

/*Merge*/
data all6;
merge all5 (in=x) f2 (in=y);
by patid;
if x=1; 
run; quit; 

/*Compute time differences and only keep positives*/
data all6;
set all6;
diff = FST_DT-diab_date;
run; quit; 

data all7;
set all6; 
where diff>=0;  /*13M of 16M records are post-diab diagnosis for about 47k obs = approx 300 records per person?*/
run; quit; 

/*Quick count of how many IDs*/
proc sql;
select count(distinct patid) as CNT_patid
from work.all7; /*47k obs*/
quit; 

/*Sort by patid and time*/
proc sort data=all7;
by patid diff;
run; quit; 

/*Write a .csv file for later use*/
proc export data=all7
    outfile='\\phs-isilon.private\phs\users\jrigdon\T2DM\diab_optum1.csv'
    dbms=csv
    replace;
run;


/*Now prepare summary tables for SB - using R?*/







/*STOP HERE - look at "critical event" code?*/
data all5;
set all5;
drp = 0;
by patid;
do until (diab = 1);
drp = 1;
end;
run; quit; 

proc print data=all5 (obs=200);
run; quit;

proc freq data=all5;
tables drp;
run; quit; 

/**
if first.patid then first_id = 1; else first_id = 0;
if first.diab = 1 then first_diab = 1; 
if last.patid then last_id = 1; else last_id = 0;
run; quit;/ 

































