** APPENDIX 1b (for evaluating categorical and ordinal variables);
data inset_categorical; 
set &libdata..&inset(keep=&target &vartxt); 
%macro tempchar;
%do i=1 %to 10; 
if ranuni(123) < 0.5 then insertchartemp&i='A'; else insertchartemp&i='B'; 
%end; 
%mend; 
%tempchar; 
attrib _all_ label=''; run; 

/*
ods output nlevels=checkfreq; 
proc freq data=inset_categorical nlevels; 
tables &vartxt insertchartemp1-insertchartemp10 / noprint; run; 
ods output close; 
*/



data check_contents; 
retain &vartxt; 
set inset_categorical(keep=&vartxt insertchartemp: obs=1); run; 

proc contents data=check_contents varnum out=check_contents2 noprint; run; 
proc sort data=check_contents2(keep=name varnum) out=checkfreq(rename=(name=tablevar)); by varnum; run; 


data varcnt; set checkfreq; varcnt+1; run;

proc univariate data=varcnt noprint;
      var varcnt;
      output out=pctscore pctlpts=0 10 20 30 40 50 60 70 80 90 100
          pctlpre=pct_; run; 

data _null_; 
set pctscore; 
call symputx('start1', 1); 
call symputx('end1', int(pct_10)-1);
call symputx('start2', int(pct_10));
call symputx('end2', int(pct_20)-1);
call symputx('start3', int(pct_20));
call symputx('end3', int(pct_30)-1);
call symputx('start4', int(pct_30));
call symputx('end4', int(pct_40)-1);
call symputx('start5', int(pct_40));
call symputx('end5', int(pct_50)-1);
call symputx('start6', int(pct_50));
call symputx('end6', int(pct_60)-1);
call symputx('start7', int(pct_60));
call symputx('end7', int(pct_70)-1);
call symputx('start8', int(pct_70));
call symputx('end8', int(pct_80)-1);
call symputx('start9', int(pct_80));
call symputx('end9', int(pct_90)-1);
call symputx('start10', int(pct_90));
call symputx('end10', pct_100); run; 

proc sql noprint; select tablevar into :varmore separated by ' ' from varcnt; quit; 
proc sql noprint; create table vcnt as select count(*) as vcnt from varcnt; quit; 
data _null_; set vcnt; call symputx('vmcnt', vcnt); run; 
proc sql noprint; select tablevar into :v1-:v&vmcnt from varcnt; quit; 
proc sql noprint; select max(varcnt), compress('&x'||put(varcnt, 10.))
into :varcount, :tempvar separated by ' ' from varcnt order by varcnt; quit;
proc sql noprint; select tablevar into :x1-:x&end10 from varcnt; quit; 
proc sql noprint; select count(*) into :obscnt from inset_categorical; quit; 

%macro stkchar;
%do i=1 %to &vmcnt;
data v&i; 
length tablevar $ 32; 
set inset_categorical(keep=&&v&i rename=(&&v&i=origvalue));
tablevar="&&v&i";
format tablevar $32.; run; 

proc sql; 
create table vmoreranked&i as
select tablevar, origvalue as rankvmore, count(*) as totalcnt
from v&i
group by 1, 2; quit; 

data vmoreranked&i; length rankvmore $ 12; set vmoreranked&i; run; %end; 
%mend; 
%stkchar; 

data stackorig;
length rankvmore $ 32; 
set vmoreranked1-vmoreranked&vmcnt; 
format rankvmore $32.;
informat rankvmore $32.; run; 

%macro outshell; 
%do i=1 %to &varcount; 
%macro ybinamax; 
%if &ytype=binary %then %do; 
proc univariate data=inset_categorical noprint;
      var &target;
     output out=pctoutlier(keep=pcty_100 rename=(pcty_100=capy)) 
        pctlpts=100 
        pctlpre=pcty_; run; %end; 
%else %do; 
proc univariate data=inset_categorical noprint;
      var &target;
     output out=pctoutlier(keep=pcty_&capy rename=(pcty_&capy=capy)) 
        pctlpts=&capy 
        pctlpre=pcty_; run; %end; 
%mend; 
%ybinamax; 

data try_model; 
length &&x&i $ 32; 
if _n_=1 then set pctoutlier; 
set inset_categorical; 
if &&x&i=' ' then &&x&i="&charmiss";
if &target > capy then &target=capy; 
format &&x&i $32.;
informat &&x&i $32.; run; 

proc sql noprint; select count(*) into :nonmiss from try_model where &&x&i ne "&charmiss"; quit; 

%macro binary; 
proc sql noprint; 
select sum(case when &target=1 then 1 else 0 end), sum(case when &target=0 then 1 else 0 end), count(*)
       into :tot_bad, :tot_good, :tot_both
from try_model; quit; 

proc sql; 
create table woe&i as 
(select "&&x&i" as tablevar, 
        &&x&i as tier, 
        count(*) as cnt,
		count(*)/&tot_both as cnt_pct,
        sum(case when &target=0 then 1 else 0 end) as sum_good,
        sum(case when &target=0 then 1 else 0 end)/&tot_good as dist_good,
        sum(case when &target=1 then 1 else 0 end) as sum_bad,
        sum(case when &target=1 then 1 else 0 end)/&tot_bad as dist_bad,
        log((sum(case when &target=0 then 1 else 0 end)/&tot_good)/(sum(case when &target=1 then 1 else 0 end)/&tot_bad))*100 as woe,
        ((sum(case when &target=0 then 1 else 0 end)/&tot_good)-(sum(case when &target=1 then 1 else 0 end)/&tot_bad))
			*log((sum(case when &target=0 then 1 else 0 end)/&tot_good)/(sum(case when &target=1 then 1 else 0 end)/&tot_bad)) as pre_iv,
		sum(case when &target=1 then 1 else 0 end)/count(*) as outcomesum			
   from try_model
   group by "&&x&i", &&x&i)
order by &&x&i; quit; 

ods results off; 
ods listing close;
/* ods output chisq=CHISQDAT_temp;
proc freq data=try_model noprint; 
tables &&x&i*&target / NOCOL NOPERCENT CHISQ noprint; run;
ods listing;

data chisquare&i(keep=tablevar value rename=(value=chisquare));
set chisqdat_temp;
IF STATISTIC='Chi-Square';
table=substr(Table,7,49);
tablevar=substr(table, 1, index(table, "*")-2); run;  */
%mend; 

%macro continu; 
proc means data=try_model mean median; 
var &target; 
output out=check_mean_median(drop=_type_ _freq_)
      mean=ymeanvalue
	  median=ymedianvalue; run; 

data mean_median_combo(keep=ymeanmedvalue); 
set check_mean_median;  
ymeanmedvalue=sum(&mean_median_combo*ymeanvalue, (1-&mean_median_combo)*ymedianvalue); 
call symputx('ymeanmed', ymeanmedvalue); run; 

data check_mod;
if _N_ = 1 then set mean_median_combo;
set try_model;

if &target > ymeanmedvalue then do; ytarget_good=&target-ymeanmedvalue; ytarget_bad=0; end; 
else if &target le ymeanmedvalue then do; ytarget_bad=ymeanmedvalue-&target; ytarget_good=0; end; run;

proc sql noprint; select sum(ytarget_bad), sum(ytarget_good), count(*) 
     into :tot_bad, :tot_good, :tot_both from check_mod; quit; 

proc sql; 
create table woe&i as 
(select "&&x&i" as tablevar, 
    &&x&i as tier, 
    count(*) as cnt,
    count(*)/&tot_both as cnt_pct,
    sum(ytarget_bad) as sum_bad,
    sum(ytarget_bad)/&tot_bad as dist_bad,
    log((sum(ytarget_bad)/&tot_bad)/(sum(ytarget_good)/&tot_good))*100 as woe,
    (sum(ytarget_bad)/&tot_bad-sum(ytarget_good)/&tot_good) *log((sum(ytarget_bad)/&tot_bad)/(sum(ytarget_good)/&tot_good)) as pre_iv,      
		sum(&target)/count(*) as outcomesum			
   from check_mod
   group by "&&x&i", &&x&i)
order by &&x&i; quit; 

data woe&i; 
length tier $ 32; 
set woe&i; 
format tier $32.; 
informat tier $32.; 
run;

proc sort data=woe&i; by &&x&i; run; 
data check_for_anova(keep=&&x&i); set check_mod; run; 

proc contents data=check_for_anova out=check_contents(keep=name); run; 
proc sql noprint; select name into :anovalab from check_contents; quit; 


ods results on; 
/* PROC ANOVA DATA=check_mod
     outstat=chisquaretemp(where=(tablevar ne 'ERROR') 
      keep=_source_ F rename=(_source_=tablevar F=chisquare) 
      );
    CLASS &&x&i;
    MODEL &target=&&x&i;
    MEANS &&x&i/TUKEY CLDIFF; run; 

data chisquare&i; 
length tablevar $ 32.; 
set chisquaretemp; 
tablevar="&anovalab"; 
format tablevar $32.; 
informat tablevar $32.; run;  */
%mend; 
%&ytype; 

proc sql; 
create table iv&i as select "&&x&i" as tablevar,
                            sum(pre_iv) as iv, 
                           (1-&nonmiss/&obscnt) as pct_missing
from woe&i; quit; 

** remedy missing woe;  
proc sql noprint; select min(woe) into :minwoe from woe&i where woe ne .; quit; 
proc sql noprint; select max(woe) into :maxwoe from woe&i where woe ne .; quit;
proc sql noprint; select min(outcomesum) into :minpery from woe&i where woe ne .; quit; 
proc sql noprint; select max(outcomesum) into :maxpery from woe&i where woe ne .; quit; 

data woe&i; set woe&i; 
if woe=. and outcomesum < &minpery then woe=&maxwoe; 
if woe=. and outcomesum > &maxpery then woe=&minwoe; 
run;  
** remedy ends; 

%end;
%mend outshell; 
%outshell; 

%macro stackset; 
%do j=1 %to 10; 
data tempiv&j; 
length tablevar $ 32; 
set iv&&start&j-iv&&end&j;
format tablevar $32.; run; 

data tempwoe&j; 
length tablevar $ 32; 
length tier $ 32; 
set woe&&start&j-woe&&end&j; 
format tablevar $32.; 
format tier $32.; run; 

/* data tempchisquare&j; 
length tablevar $ 32; 
set chisquare&&start&j-chisquare&&end&j; 
format tablevar $32.; run; */
%end; 
%mend; 
%stackset;

data &libdata..ivall; set tempiv1-tempiv10; if substr(tablevar, 1, 14)='insertchartemp' then delete; run; 
data &libdata..woeall; set tempwoe1-tempwoe10; if substr(tablevar, 1, 14)='insertchartemp' then delete; run; 
/* data &libdata..chisquareall; set tempchisquare1-tempchisquare10; if substr(tablevar, 1, 14)='insertchartemp' then delete; run; 

proc sort data=&libdata..chisquareall nodupkey out=chisquare_nodup(keep=chisquare); by descending chisquare; run; 
data chisquare_nodup; set chisquare_nodup; power_order+1; run; 
proc sort data=chisquare_nodup; by chisquare; run; 
proc sort data=&libdata..chisquareall; by chisquare; run; 

data &libdata..chisquareall;
merge &libdata..chisquareall chisquare_nodup;
by chisquare; run; */

proc sort data=&libdata..ivall; by descending iv; run; 
data &libdata..ivall; set &libdata..ivall; ivrank+1; run; 
proc sort data=&libdata..ivall nodupkey out=ivtemp(keep=iv); by descending iv; run; 
data ivtemp; set ivtemp; ivtier+1; run; 
proc sort data=ivtemp; by iv; run; 
proc sort data=&libdata..ivall; by iv; run; 

data ivouttmp; 
length type $ 4; 
merge &libdata..ivall ivtemp; by iv; 
ivrank=ivtier;
type='char'; 
format type $4.; 
informat type $4.; 
drop ivtier; run; 

proc sort data=ivouttmp nodupkey; by tablevar; run; 
proc sort data=&libdata..woeall; by tablevar; run;
/* proc sort data=&libdata..chisquareall nodupkey; by tablevar; run;  */
proc sort data=&libdata..ivall nodupkey; by tablevar; run; 

/* old code; */
/* data &libdata..ivout; 
merge ivouttmp &libdata..chisquareall;
by tablevar; run; 
*/

/* new code; */
data &libdata..ivout; 
set ivouttmp; run; 


proc sort data=&libdata..ivall; by tablevar; run; 

data &libdata..iv_woe_all; 
length tablevar $ 32; 
merge &libdata..ivall &libdata..woeall /* &libdata..chisquareall */; 
by tablevar; 
type="char"; run; 

proc sort data=&libdata..iv_woe_all; by tablevar tier; run;  
proc sort data=stackorig; by tablevar rankvmore; run; 

data &libdata..iv_woe_all2; 
length tablevar $ 32; 
merge &libdata..iv_woe_all(in=t) stackorig(in=s rename=(rankvmore=tier));
by tablevar tier; if t; run; 

proc sort data=&libdata..iv_woe_all2; by ivrank tier; run; 

%let retvar=tablevar iv ivrank tier cnt cnt_pct dist_bad woe outcomesum pct_missing type chisquare power_order seqnum;
data &libdata..&woeout(keep=&retvar); 
retain &retvar; 
set &libdata..iv_woe_all2; 

format binmin &formtall; 
format binmax &formtall;
informat binmin &formtall; 
informat binmax &formtall;

label tablevar="Variable";
label iv="IV";
label ivrank="IV Rank";
label tier="Tier/Bin";
label cnt="# Customers";
label cnt_pct="% Custoemrs";
label woe="Weight of Evidence";
label outcomesum="&outname";
label pct_missing="% Missing Values"; run; 

proc sort data=&libdata..ivout; by tablevar; run; 
data &libdata..&ivout; set &libdata..ivout; by tablevar; run; 
proc sort data=&libdata..&woeout; by tablevar; run; 
data &libdata..&woeout; set &libdata..&woeout; by tablevar; 
if first.tablevar=1 then seqnum=0; seqnum+1; output; run; 
proc sort data=&libdata..&woeout out=&woeout._char; by ivrank tablevar; run; 
proc sort data=&libdata..&ivout out=&ivout._char(keep=iv ivrank pct_missing tablevar type chisquare power_order); by ivrank; run; 

