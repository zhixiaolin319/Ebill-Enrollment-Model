** APPENDIX 1a (for evaluating numerical variables);
data num_modified; 
set &libdata..&inset(keep=&target &varnum); 
%macro tempnum;
%do i=1 %to 10; 
if ranuni(123) < 0.5 then insertnumetemp&i=1; else insertnumetemp&i=0; 
%end; 
%mend; 
%tempnum; 
attrib _all_ label=''; run; 

/*
ods output nlevels=checkfreq; 
proc freq data=num_modified nlevels; 
tables &varnum insertnumetemp1-insertnumetemp10 / noprint; run; 
ods output close; 
*/


data check_contents; 
retain &varnum; 
set num_modified(keep=&varnum insertnumetemp: obs=1); run; 

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
proc sql; create table vcnt as select count(*) as vcnt from varcnt; quit; 
data _null_; set vcnt; call symputx('vmcnt', vcnt); run; 

proc sql noprint; select tablevar into :v1-:v&vmcnt from varcnt; quit; 
proc sql noprint; select max(varcnt), compress('&x'||put(varcnt, 10.)) 
into :varcount, :tempvar separated by ' '  from varcnt order by varcnt; quit;
proc sql noprint; select tablevar into :x1-:x&end10 from varcnt; quit; 
proc sql noprint; select count(*) into :obscnt from num_modified; quit; 


%macro stkorig;
%do i=1 %to &vmcnt;
data v&i; 
length tablevar $ 32; 
set num_modified(keep=&&v&i rename=(&&v&i=origvalue));
tablevar="&&v&i";
format tablevar $32.; run; 

proc rank data=v&i  groups=&tiermax out=v&i; 
by tablevar; 
var origvalue; 
ranks rankvmore; run; 

proc means data=v&i median mean min max nway noprint;
class tablevar rankvmore; 
var origvalue; 
output out=vmoreranked&i(drop=_type_ _freq_)
       median=med_origv 
       mean=mean_origv
       min=binmin
       max=binmax; run; %end; 
%mend; 
%stkorig;

data stackorig; set vmoreranked1-vmoreranked&vmcnt; run; 

proc rank data=num_modified groups=&tiermax out=try_model(keep=&tempvar &target); 
var &varmore; ranks &varmore; run; 

%macro outshell; 
%do i=1 %to &varcount; 
%macro ybinamax; 
%if &ytype=binary %then %do; 
proc univariate data=num_modified noprint;
      var &target;
     output out=pctoutlier(keep=pcty_100 rename=(pcty_100=capy)) 
        pctlpts=100 
        pctlpre=pcty_; run; %end; 
%else %do; 
proc univariate data=num_modified noprint;
      var &target;
     output out=pctoutlier(keep=pcty_&capy rename=(pcty_&capy=capy)) 
        pctlpts=&capy 
        pctlpre=pcty_; run; %end; 
%mend; 
%ybinamax; 

data try_model; 
if _n_=1 then set pctoutlier; 
set try_model; 
if &&x&i=. then &&x&i=&tempmiss;
if &target > capy then &target=capy; run; 

proc sql noprint; select count(*) into :nonmiss from try_model where &&x&i ne &tempmiss; quit; 

%macro binary; 
/* proc sql noprint; 
select sum(case when &target=1 then 1 else 0 end), sum(case when &target=0 then 1 else 0 end), count(*)
       into :tot_bad, :tot_good, :tot_both
from try_model; quit; 
*/

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

ytarget_good_temp=&target - ymeanmedvalue; 
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
        (sum(ytarget_bad)/&tot_bad-sum(ytarget_good)/&tot_good)
			*log((sum(ytarget_bad)/&tot_bad)/(sum(ytarget_good)/&tot_good)) as pre_iv,      
		sum(&target)/count(*) as outcomesum
   from check_mod
   group by "&&x&i", &&x&i)
order by &&x&i; quit; 

data woe&i; set woe&i; withinsqm+1; run; 
proc sort data=woe&i; by &&x&i; run; 
%mend; 

%&ytype; 

proc rank data=try_model groups=&tiermax out=temp; var &&x&i; ranks rank_x; run; 

proc means data=temp sum mean nway noprint; 
class rank_x; 
var &target; 
output out=temp2(drop=_type_ rename=(_freq_=cntweight))
       sum=ycount
      mean=ymean; run; 

proc sort data=temp2; by rank_x; run; 

data table; set temp2;
retain ysum weightsum;
ysum+ycount; weightsum+cntweight; rank_x_negative=-1*rank_x; run; 

proc sort data=table; by rank_x_negative; run;

data table; set table;
by rank_x_negative;
if _n_=1 then do; ytotal=ysum; counttotal=weightsum; end;
retain ytotal counttotal; run;
proc sort data=table; by rank_x; run; 

data table; set table; 
y_cum_pct=ysum/ytotal; pop_cum_pct=weightsum/counttotal; run; 

data ginidat; set table;
xlag = lag(pop_cum_pct); ylag = lag(y_cum_pct);
columna =y_cum_pct*xlag; columnb =pop_cum_pct*ylag;
retain suma sumb;
suma + columna; sumb + columnb;
gini=abs(suma-sumb); run;

data gini_x(keep=gini); set ginidat; if y_cum_pct=1; run; 

data gini&i;
if _N_ = 1 then set gini_x;
set gini_x;
tablevar="&&x&i"; run;

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

/* proc sort data=woe&i; by &&x&i; run; */
proc sort data=woe&i; by tier; run;
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
set woe&&start&j-woe&&end&j; 
format tablevar $32.; run; 

data tempgini&j;
length tablevar $ 32; 
set gini&&start&j-gini&&end&j;
format tablevar $32.; run; 
%end; 
%mend; 
%stackset;

data &libdata..ivall; set tempiv1-tempiv10; if substr(tablevar, 1, 14)='insertnumetemp' then delete;  run; 
data &libdata..woeall; set tempwoe1-tempwoe10; if substr(tablevar, 1, 14)='insertnumetemp' then delete; run; 
data &libdata..giniall; set tempgini1-tempgini10; if substr(tablevar, 1, 14)='insertnumetemp' then delete; run; 
proc sort data=&libdata..giniall; by descending gini; run; 

proc sort data=&libdata..ivall; by descending iv; run; 
data &libdata..ivall; set &libdata..ivall; ivrank+1; run; 
proc sort data=&libdata..ivall nodupkey out=ivtemp(keep=iv); by descending iv; run; 
data ivtemp; set ivtemp; ivtier+1; run; 
proc sort data=ivtemp; by iv; run; 
proc sort data=&libdata..ivall; by iv; run; 
data &ivout; merge &libdata..ivall ivtemp; by iv; run; 
proc sort data=&ivout; by tablevar; run; 
proc sort data=&libdata..woeall; by tablevar; run;
proc sort data=&libdata..giniall; by tablevar; run;

data &libdata..iv_woe_all; 
length tablevar $ 32; 
merge &ivout &libdata..woeall &libdata..giniall; 
by tablevar; run; 

proc sort data=&libdata..iv_woe_all; by tablevar tier; run;  
proc sort data=stackorig; by tablevar rankvmore; run; 

data &libdata..iv_woe_all2; 
length tablevar $ 32; 
merge &libdata..iv_woe_all(in=t) stackorig(in=s rename=(rankvmore=tier));
by tablevar tier;
if t; 
if s then tier=med_origv; run; 

proc sort data=&libdata..iv_woe_all2; by ivrank tier; run; 

%let retvar=tablevar iv ivrank ivtier tier cnt cnt_pct dist_bad woe outcomesum pct_missing binmin binmax;

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
label pct_missing="% Missing Values";
label binmin="bin min";
label binmax="bin max"; run; 

proc sort data=&ivout nodupkey; by tablevar; run; 
proc sort data=&libdata..giniall nodupkey; by tablevar; run; 
proc sort data=&libdata..&woeout; by tablevar; run; 

data &libdata..&ivout; 
length tablevar $ 32; 
merge &ivout(in=t keep=tablevar iv ivrank ivtier pct_missing)
      &libdata..giniall(keep=tablevar gini);
by tablevar; 
if t; run; 

proc sort data=&libdata..&ivout; by descending gini; run; 
proc sort data=&libdata..&ivout nodupkey out=ivtemp(keep=gini); by descending gini; run; 
data ivtemp; set ivtemp; ginitier+1; run; 
proc sort data=ivtemp; by gini; run; 
proc sort data=&libdata..&ivout; by gini; run; 
data &ivout; merge &libdata..&ivout ivtemp; by gini; run; 
proc sort data=&ivout; by tablevar; run; 

data &libdata..&ivout; 
length type $ 4; 
set &ivout; 
by tablevar; 
type="num "; 
format type $4.; 
informat type $4.; run; 

proc sort data=&libdata..&ivout(drop=ivrank) out=&ivout._num(rename=(ivtier=ivrank ginitier=power_order)); by ivtier; run; 
data &ivout._num(keep=iv ivrank pct_missing tablevar type gini power_order); set &ivout._num; run; 

proc sort data=&ivout._num; by tablevar; run; 
proc sort data=&libdata..&woeout; by tablevar; run; 

data &libdata..&woeout._t;
merge &libdata..&woeout(in=t)
      &ivout._num(keep=tablevar power_order);
by tablevar; 
if t; run; 
proc sort data=&libdata..&woeout._t(drop=ivrank) out=&woeout._num(rename=(ivtier=ivrank)); by ivtier tablevar; run; 

%macro to_excel(data_sum);
PROC EXPORT DATA=&data_sum
            OUTFILE="&libout/&data_sum"
            label DBMS=tab REPLACE; run; 
%mend; 

proc sort data=&woeout._num; by tablevar tier; run; 
data &woeout._num(rename=(tier_txt=tier)); 
set &woeout._num;
by tablevar; 
if first.tablevar=1 then seqnum=0; seqnum+1; tier_txt=STRIP(PUT(tier, best32.)); tier_num=tier; type="num "; output; 
drop tier; run; 


** for treating missing values only; 
data &woeout._num;
set &woeout._num;

if tier=.A then tier=-4;
else if tier=.B then tier=-3; 
else if tier=.C then tier=-2; 
else if tier=.D then tier=-1; 
run; 

