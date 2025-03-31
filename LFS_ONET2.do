  
  ///////////////////////
  //LFS Survey - 1983(2012)-2022
  ///////////////////////
  //https://webgate.ec.europa.eu/s-circabc/ui/denied
  //password: ESTATmicro23RP87
   
  //Code written for De Economicst paper "In Europe, Arduous Jobs Fall On First-Generation Migrants. But Later Generations Benefit From Improved Opportunities"
  //Focus on 1st and 2nd generation (non-EU) immigrants
  //Pooled analysis (with country fixed effects, distinct analysis by GDP class ...and possible importance of immigration pool)
  
  //Last update: March 2025
  


 global odr "C:\Users\adminv\OneDrive - UCL\"
  global dpath "$odr\Eurostat LFS\Data_request2023\Data_\YEAR_DATA"
  global rpath "$odr\Eurostat LFS\Data_request2023\Results\"
  
  cd "$odr\Eurostat LFS\Data_request2023\Statawk_"

  /////////////////////////////////
**# Bookmark #1 Part 1- Prepare LFS data: isco3 occupation and background variables (isced, tenure...)
  /////////////////////////////////

{  //***core code ==> lfsdb
	 include "C:\Users\adminv\OneDrive - UCL\Eurostat LFS\Data_request2023\LFS_core.do"

}


  /////////////////////////////////
**# Bookmark #2 Part 2- Prepare O*NET data : arduousness by isco3 occupation
  /////////////////////////////////
  

clear all
set more off

/*   O*NET preparation
 { //From SOC 10 (O*NET) to ISCO-08 4/3/2 digits (LFS) ==>soc_isco
////////////////////////////
import excel using "$odr\ONet\isco_soc_crosswalk.xls", sheet("soc10isco08") firstrow clear
rename *, lower
rename isco08code isco08
rename soccode soc10
keep isco08 soc10
des, f
list soc10 isco08 in 1/20
replace soc10=subinstr(soc10, "-", "", 1) 
destring soc10, replace
destring isco08, replace //4 digits
rename isco08 isco4 //label
iscolbl   isco08 isco4
iscogen isco3=mino(isco4) //go for 3 digit + label
label save isco3 using isco3f , replace //!!! we will need to reload this one after
iscogen isco2=submajor(isco4) //go for 2 digit + label
iscogen isco1=major(isco4) //go for 1 digit + label

	foreach var of varlist _all { //remove all variable labels
	   label var `var' ""
	  }
save soc_isco, replace
 des, f
 }

{ // O*Net model WORK CONTEXT ==> ardu
////////////////////////////////
import excel using "$odr\SHARE\Data\ONET\db_24_3_excel\Work Context.xlsx", firstrow clear
rename *, lower
gen Origin = "4"
keep scaleid elementname datavalue onetsoccode Origin

*Keep only the needed measurements 
keep if scaleid=="IM" | scaleid=="CX" | scaleid == "CT"
drop scaleid

*Simplify values and names when reshaping 
rename datavalue s
replace elementname=subinstr(elementname, " ", "_", .) 
replace elementname=subinstr(elementname, ",", "", .) 
replace elementname=subinstr(elementname, "-", "", .) 
replace elementname=subinstr(elementname, "/", "", .) 
replace elementname=subinstr(elementname, "'", "", .) 
replace elementname = substr(elementname,1,30)
replace elementname =  Origin + elementname 
drop Origin
tab  elementname

reshape wide s, i(onetsoccode) j(elementname) string
sort onetsoccode
rename onetsoccode onetsoc10
tab onetsoc10
save onet_, replace

*Transforming O*Net: computation of arduousness index 
use onet_, replace
pca s4*, component(3) blank(0.12) //principal component
   
predict ardu , score
qui: su ardu
replace ardu = ardu/r(sd)

*Final cleaning + transforming to ISCO08

keep ardu onetsoc10

*From O*NET-SOC 10 to SOC 10
replace onetsoc10 = subinstr(onetsoc10, "-", "", 1)
destring onetsoc10, replace
gen soc10=int(onetsoc10)
drop onetsoc10
 *Inject SOC 10 to ISCO-08 corresondance table
recode soc10 (191020=191021) //some missing found
duplicates drop soc10, force
	/////////////////////////////////////////
merge 1:m soc10 using soc_isco   //inject isco4/3/2/1 code
	//////////////////////////
drop _merge
bysort isco4: egen ardu4=mean(ardu)
bysort isco3: egen ardu3=mean(ardu)
bysort isco2: egen ardu2=mean(ardu)
bysort isco1: egen ardu1=mean(ardu)
save ardu, replace
}


 { //Typical graphs/tables put in the Appendix

use onet_, replace
pca s4*, component(2) blank(0.12) //principal component


/*Eigen vector & R2*/
screeplot    , neigen(20) xtitle ("Rank of principal component") xlabel(1(2)20) ///
        title("") 
graph save figA1_pca, replace
graph export "$rpath\figA1_pca.eps", replace
 
/*Loading factors [L] for first PC*/
*predict penib penib_, score 
esttab e(L, fmt(%9.2f)) using "$rpath\tab_dstat0.tex",  replace    ///
  addnote("Source: O*NET 2020")

		/*Fig - O*NET ISCO 3 arduouness index (random sample)*/

 use ardu, clear
 bysort isco3: keep if _n==1
 sample 30
 numlabel , add
 graph hbar ardu3, over(isco3, sort(1) lab(labs(1.8) )) ///
ylabel(, labsize(vsmall)) ///
 ytitle("O*Net arduous. index (ISCO2)", size(vsmall)) ///
 note("Source: O*NET 2021", size(vsmall)) 
 
 graph save fig1, replace
graph export "$rpath\Fig1.eps", replace
 }


  /////////////////////////////////
**# Bookmark #3 Part3- Assemble LFS and O*NET (+ PENN WORLD)
  /////////////////////////////////

 {  //Penn World GDP per head ==> gdp_
 * list of countries present in LFS
use lfsdb, clear
bysort cnt2: keep if _n==1
keep cnt2
decode cnt2, gen(cnt2_)
replace cnt2_=substr(cnt2_,-2,.)
save lfs_clist, replace
 tab cnt2_

 *Penn data
use "C:\Users\adminv\OneDrive - UCL\\PennW_\penn_", clear
replace cnt2="UK" if cnt2=="GB"
replace cnt2="GR" if cnt2=="EL"
tab cnt2
 keep cnt2 year gdph
 rename cnt2 cnt2_
keep if year >=2015 // average 2015-2019
collapse gdph, by(cnt2_)
merge 1:1 cnt2_ using lfs_clist
keep if _merge==3
drop _merge
xtile gdphq= gdph , nq(4) //construct quartile
	tostring gdphq, gen(x)
 gen cnt2_gdp=x+"_"+cnt2_ 
 drop x
 save gdp_, replace
 }
 
 */
 
 
  { //** [isco] [isco_pr] codes list in LFS + merge with O*NET ==> isco_pr_ardu isco_ardu }
  local vl isco isco_pr //current occupation, previous occupation

 quiet  foreach i in `vl' { 
 

use lfsdb, clear
drop if `i'==999
bysort `i': keep if _n==1
rename `i' isco3
keep isco3
save isco3_, replace

//Merge at isco level 3,2, 1
use isco3_,clear
gen isco2=.
replace isco2=isco3 if isco3<100
replace isco2=int(isco3/10) if isco3>=100
gen isco1=.
replace isco1=isco2 if isco2<10
replace isco1=int(isco2/10) if isco2>=10

merge 1:m isco3 using ardu, keepusing(isco3 soc10 ardu3)
*tab isco3 if _merge==1
drop _merge
merge m:m isco2 using ardu, keepusing(isco2 soc10 ardu2)
*tab isco2 if _merge==1
drop _merge
merge m:m isco1 using ardu, keepusing(isco1 soc10 ardu1)
*tab isco1 if _merge==1, missing
drop _merge
//select final arduousness index (if not available at isco3 level, go for isco2, if.. for isco1)
gen ardu=.
replace ardu=ardu3 if !missing(ardu3)
replace ardu=ardu2 if missing(ardu3) & !missing(ardu2)
replace ardu=ardu1 if missing(ardu3) & missing(ardu2) & !missing(ardu1)
rename isco3 `i' //back to the original variable name
sort `i'
by `i' : keep if _n==1
keep `i' ardu
save `i'_ardu, replace
  }
  }

{ //**Inject [isco[_pr]] x O*NET lists, PENN into LFS main file ==>> lfsdb_ardu
use lfsdb , clear
decode cnt2, gen(cnt2_)
replace cnt2_=substr(cnt2_,-2,.)
merge m:1 isco_pr using isco_pr_ardu  //previous job arduousness
rename ardu ardu_pr //not to counfound it with current job arduousness
drop _merge 
merge m:1 isco using isco_ardu //current job arduousness
drop _merge 
  run isco3f.do  //recall isco3 value labels
 label values isco isco_pr isco3 // reapply isco3 value label
 numlabel isco3, add
//inject gdp per head
merge m:1 cnt2_ using gdp_
drop _merge
save lfsdb_ardu, replace
}

//make copy
copy lfsdb_ardu.dta lfsdb_ardu_bup.dta, replace



	//////////////////////////////////////////////////////////////////	
**# Bookmark #3 Part4 Econometric Analysis : immigration an₫ (conditional) arduouness gap, 
 //////////////////////////////////////////////
  
  {  //** Export key agregate characteristics of countries (immigrant share, gdph)
 use lfsdb_ardu, clear
 keep if year>=2018 //so we keep uk on board
  bysort cnt2_ fem: egen sNEU=mean(imm1) //share of 1st generation non-EU migrants
  bysort cnt2_ fem: egen sEU_NEU=mean(imm1x) //share of 1st generation non-EU + EU migrants
  bysort cnt2_ fem: gen sEU= sEU_NEU - sNEU //share 1st genratioin EU migrants
  bysort cnt2_ fem: keep if _n==1
  replace sNEU=. if sNEU==0
    replace  sEU_NEU=. if  sEU_NEU==0
  keep cnt2_ fem sNEU sEU_NEU sEU
  sort cnt2_ 
  save imm_s, replace
  

   use lfsdb_ardu, clear
  bysort cnt2_ (year): keep if _n==_N //latest year
  keep cnt2_ gdph  year
  sort cnt2_
  save gdph_, replace
  
 use gdph_, clear
 br
  }
  
  /*i) `Native-born with native background' (i.e. people born in Belgium from Belgian-born parents)
    ii) `Second-generation immigrants' (i.e. people born in Belgium with at least one foreign-born parent)
  iii) `First-generation immigrants' (i.e. people born outside Belgium). 
The reference category, in the regression analysis, is the group
of `Native-born people with a native background' (further referred to as `Natives').
*/

	/* identify, for each variable the oldest wave available 
	   local vlist ardu imm1 isced durwk_  age year fem isco
	  foreach i in `vlist' {
    use lfsdb_ardu if empl_ilo==1 , clear
	keep year `i'
       keep if !missing(`i')
	sort year
	keep if  _n==1
	list  `i' year 
	  }
	 ==isco is the constraint */

  { // *sample selection (1st generation or no) + arduouness quantiles ==> smpl_

    use lfsdb_ardu   if empl_ilo==1, clear
	merge m:1 cnt2_ using imm_s
	keep if !missing(sNEU) //drop countries that do not report the NATIVE/EU/NEU breakdown
	tab cnt2_
quiet xi: reg ardu imm1_ isced durwk_  yresid age year fem isco nace
keep if e(sample)
save smpl_, replace

  }

  /*
use smpl_, clear
twoway (kdensity ardu  if imm1==0, lc(blue)) ///
          (kdensity ardu if imm1==1, lc(red))
*/
	

 {  // *sample selection (1st & 2nd generation) ==> smpl_12

    use lfsdb_ardu   if empl_ilo==1, clear
	merge m:1 cnt2_ using imm_s
	keep if !missing(sNEU) //drop countries that do not report the NATIVE/EU/NEU breakdown
	tab cnt2_
quiet xi: reg ardu immxx isced durwk_ yresid age year fem ardu nace
keep if e(sample)
save smpl_12, replace
 }

 
 /*
use smpl_12, clear
twoway (kdensity ardu  if imm==0, lc(blue)) ///
          (kdensity ardu if imm==1, lc(red)) ///
		       (kdensity ardu if imm==2, lc(green)) 

*/

**# Bookmark #4.1 Descriptive Statistics/first visualisation
  ////////////////////////////////////////////////////////
  

  
 { //** Descriptive statistics (by gender x country) [to be included in paper]
   *[mean] ardu imm1 isced durwk_  age year
use smpl_ , clear
foreach var of varlist _all {
	label var `var' ""
}
*des, f
 collect clear
 collect dir
	//step 1: produce your table 
table (cnt2) () (fem), ///
        statistic(mean  ardu ) nformat(%6.2f  mean, basestyle) ///
		        statistic(sd  ardu )  nformat(%6.3f  sd) ///
					statistic(mean i.imm1_) nformat(%6.4f  mean, basestyle) ///
		        statistic(mean  isced durwk_  age ) nformat(%6.4f  mean)  ///		
		 statistic(min  year) statistic(max  year) nformat(%6.0f max min)  ///
		 nototals
	
	//step 2: modify appearance   

collect dir 
collect rename Table dstat_  //rename collection
 collect label levels result  mean      "Mean"  , modify
  collect label levels result  sd      "Std" , modify
    collect label levels result  min      "Min" , modify
	    collect label levels result  max      "Max" , modify
 collect preview  
 collect style cell border_block, border(right, pattern(nil)) //remove border to the right of first column cell
  collect preview 
  
   //step 3 : Export (latex)
   
 collect dir
				  				  
	collect export "$rpath\dstat_", ///
                  as(tex) name(dstat_)  tableonly  replace
 }		 
	
	 { //** Descriptive statistics: for GITHUB APPENDIX: https://github.com/VVandenberghe/ISCO3/blob/main/Migrants_arduousness_ISCO3.pdf]
	 
	{ //isco3 list + labels
	 
	 use smpl_ , clear
	 bysort isco: keep if _n==1
	asdocx list isco ,label save("$rpath\dstat_isco_list.tex")   fhc(\b) replace
	}
	
	{ // isco3: freq by country (by gender)  
use smpl_ , clear
label values isco .
des isco, f
 collect clear
 collect dir
	//step 1: produce your table (gross income statistics by gender and educational attainemnt)
table (isco) (cnt2) (fem), ///
        statistic(percent, across(isco) ) ///
		nformat(%6.2f percent) totals(fem#cnt2) 
	
	//step 2: modify its appearance   

collect dir 
collect rename Table dstat_isco  //rename collection
 collect label levels result  percent      "Freq." ///" , modify
 collect preview  
 collect style cell border_block, border(right, pattern(nil)) //remove border to the right of first column cell
  collect preview 
  
 
   //step 3 : Export ( latex)
   
 collect dir
				  				  
	collect export "$rpath\dstat_isco", ///
                  as(tex) name(dstat_isco)  tableonly  replace
				  
	}
 }



  
**# Bookmark #4.2  Econometric analysis  [by gender]
////////////////////////////////////////////////////////////////  


est clear

{ //** 1. GAP (1 gen) Regression

	{ //B.1 1st generation all countries
   use smpl_ , clear
   foreach var of varlist _all {
	label var `var' ""
}
    //male
 /*quiet:*/ eststo  M_p : reg ardu  i.imm1_  i.isced durwk_  i.age  i.cnt2 i.year i.nace if  fem==0  , vce(cluster isco) 
  regsave 1.imm1_ 2.imm1_ using gapM, pval  replace  //recuperate gap estimates as a Stata database
  //female
/*quiet:*/ eststo  F_p  :  reg ardu  i.imm1_  i.isced durwk_  i.age i.cnt2 i.year i.nace if  fem==1 , vce(cluster isco)
  regsave 1.imm1_ 2.imm1_ using gapF, pval replace //recuperate gap estimates as a Stata database
 }

 /*
	{ //B.1 1st generation all countries: breakdown by GDP cat
  use smpl_ , clear
     foreach var of varlist _all {
	label var `var' ""
        }
   levelsof gdphq, local(clist) clean

  foreach c in  `clist' { //loop of countries
 use smpl_ if gdphq==`c', clear
    //male
quiet: eststo  M_p`c' : reg ardu  i.imm1_ i.isced durwk_  age i.year i.cnt2 i.nace if  fem==0  , vce(cluster isco)     
  //female
quiet: eststo  F_p`c'  :  reg ardu  i.imm1_ i.isced durwk_  age i.year i.cnt2 i.nace if  fem==1 , vce(cluster isco)
 }
 }
 */
 
	{ //B.1 1st generation all countries: breakdown by education level (yresid<=5)
  use smpl_ , clear
     foreach var of varlist _all {
	label var `var' ""
        }
   levelsof sisced, local(clist) clean

  foreach c in  `clist' { //loop of countries
 use smpl_ if sisced==`c' & yresid<=5, clear
    //male
quiet: eststo  M_p_edu`c'yresid5 : reg ardu  i.imm1_  durwk_  i.age i.year i.cnt2 i.nace if  fem==0  , vce(cluster isco)     
  //female
quiet: eststo  F_p_edu`c'yresid5  :  reg ardu  i.imm1_  durwk_  i.age i.year i.cnt2 i.nace if  fem==1 , vce(cluster isco)
 }
 }
  
}
  est dir
 { //** 1. GAP (1 gen) LATEX
 
  
   *male vs female (1st generation only)
  est dir
    esttab  M_p F_p  using "$rpath\immMF.tex", replace ///
 cell(b(star fmt(3)) se(par fmt(3))) stats(N r2, fmt(%9.0fc %9.2fc) labels("N" "R2"))  ///
noobs  nonumber compress nogaps label  ///
 mlabels(, titles) ///
 drop( _cons 0.imm1_ ) ///
   rename("1.imm1_" "EU (1st gen.)" "2.imm1_" "NEU (1st gen.)") ///
   indicate( "Education=*isced" ///
             "Tenure=durwk_" ///
		    "Age=*age"  ///
		   "Sector(NACE1d)=*nace"  ///
		   "Country x Year=*cnt2*" ///
		   "Year=*year")  ///
 starlevels(* 0.10 ** 0.05 *** 0.01)  ///
  addnote("Source: EU-LFS, O*NET")
  
     * breakdown by educational attaiment (1st generation only)
  est dir
    esttab  M_p_edu* F_p_edu*   using "$rpath\immFM_educ.tex", replace ///
 cell(b(star fmt(3)) se(par fmt(3))) stats(N r2, fmt(%9.0fc %9.2fc) labels("N" "R2"))  ///
noobs  nonumber compress nogaps label  ///
 mlabels(, titles) ///
 drop( _cons 0.imm1_ ) ///
   rename("1.imm1_" "EU (1st gen.)" "2.imm1_" "NEU (1st gen.)" ) ///
   indicate( "Tenure=durwk_" ///
		    "Age=*age"  ///
		   "Sector(NACE1d)=*nace"  ///
		   "Country x Year=*cnt2*" ///
		   "Year=*year")  ///
 starlevels(* 0.10 ** 0.05 *** 0.01)  ///
  addnote("Source: EU-LFS, O*NET")
  
   * breakdown by educational attaiment (1st generation only, yresid<=5)
 est dir
    esttab  M_p_edu*yresid5 F_p_edu*yresid5  using "$rpath\immFM_educ_yresid5.tex", replace ///
 cell(b(star fmt(3)) se(par fmt(3))) stats(N r2, fmt(%9.0fc %9.2fc) labels("N" "R2"))  ///
noobs  nonumber compress nogaps label  ///
 mlabels(, titles) ///
 drop( _cons 0.imm1_ ) ///
   rename("1.imm1_" "EU (1st gen.)" "2.imm1_" "NEU (1st gen.)" ) ///
   indicate( "Tenure=durwk_" ///
		    "Age=*age"  ///
		   "Sector(NACE1d)=*nace"  ///
		   "Country x Year=*cnt2*" ///
		   "Year=*year")  ///
 starlevels(* 0.10 ** 0.05 *** 0.01)  ///
  addnote("Source: EU-LFS, O*NET")

 }  

 
 ///Expressing gaps relative to the standard-deviation of arduousness
   use smpl_ if fem==0, clear
  su ardu, detail
  scalar ardu_sd_M= r(sd)
  dis ardu_sd_M
  dis 0.093/ardu_sd_M //EU     .10343421
    dis 0.127/ardu_sd_M //NEU   .14124886

  
     use smpl_ if fem==1, clear
  su ardu, detail
  scalar ardu_sd_F= r(sd)
  dis ardu_sd_F
  dis .117/ardu_sd_F  //EU 		.17069359
dis .171/ardu_sd_F //NEU 		.24947524
 //////////////////////////////////////////////////////////
 est clear
 
 { //** 2. CONVERGENCE (1 gen) Regression:  [yresid] interacted (only 1st generation)

   use smpl_ , clear
   foreach var of varlist _all {
	label var `var' ""
        }
    //male
 quiet: eststo  M_pyresid : reg ardu  i.imm1_ c.yresid#i.imm1_ i.isced durwk_  ib25.age  i.cnt2 i.year i.nace if  fem==0  , vce(cluster isco) 
 estadd scalar EUyconv  _b[1.imm1_]/-_b[yresid#1.imm1_]
 testnl  _b[1.imm1_]/-_b[yresid#1.imm1_]=0
estadd scalar EUpvalue=r(p)
 estadd scalar NEUyconv  _b[2.imm1_]/-_b[yresid#2.imm1_]
  testnl  _b[2.imm1_]/-_b[yresid#2.imm1_]=0
estadd scalar NEUpvalue=r(p)
 //female
quiet: eststo  F_pyresid  :  reg ardu  i.imm1_ c.yresid#i.imm1_ i.isced durwk_  i.age i.cnt2 i.year i.nace if  fem==1 , vce(cluster isco)
 estadd scalar EUyconv  _b[1.imm1_]/-_b[yresid#1.imm1_]
 testnl  _b[1.imm1_]/-_b[yresid#1.imm1_]=0
estadd scalar EUpvalue=r(p)
 estadd scalar NEUyconv  _b[2.imm1_]/-_b[yresid#2.imm1_]
  testnl  _b[1.imm1_]/-_b[yresid#1.imm1_]=0
estadd scalar NEUpvalue=r(p)
 }
   
 
 { //**  2. CONVERGENCE (1 gen) LATEX: [yresid] interacted (only 1st generation)

    esttab  M_pyresid F_pyresid using "$rpath\imm_conv.tex", replace ///
	b  r2  ///
 scalar(N EUyconv EUpvalue NEUyconv NEUpvalue) sfmt(%9.0fc %9.3fc) ///
noobs  nonumber compress nogaps label  ///
 mlabels(, titles) ///
 drop( _cons) ///
   indicate( "Education=*isced" ///
             "Tenure=durwk_" ///
		    "Age=*age"  ///
		   "Sector(NACE1d)=*nace"  ///
		   "Country =*cnt2*" ///
		   "Year=*year")  ///
 starlevels(* 0.10 ** 0.05 *** 0.01)  ///
  addnote("Source: EU-LFS, O*NET")
  
 }
  

 est clear
{ //** 3.  GAP (1 & 2 gen) Regression:

	{ //B.1 1st, 2nd generation vs natives (only 2021-22)
 
   use smpl_12 , clear
      foreach var of varlist _all {
	label var `var' ""
        }
    //male
/*quiet:*/ eststo  M12_p : reg ardu  i.immxx i.isced durwk_  i.age i.year i.nace i.cnt2 if  fem==0  , vce(cluster isco) coeflegend
  //female
/*quiet:*/ eststo  F12_p  :  reg ardu  i.immxx i.isced durwk_  i.age i.year i.nace i.cnt2 if  fem==1  , vce(cluster isco)
  }

/*	{ //B.1 1st, 2nd generation vs natives (only 2021-22): breakdown by GDP cat
 
   use smpl_12 , clear
   foreach var of varlist _all {
	label var `var' ""
        }
   levelsof gdphq, local(clist) clean

  foreach c in  `clist' { //loop of countries
 use smpl_12 if gdphq==`c', clear
    //male
quiet: eststo  M12_p`c' : reg ardu  i.immxx i.isced durwk_  i.age i.year i.nace i.cnt2 if  fem==0  , vce(cluster isco) coeflegend
  //female
quiet: eststo  F12_p`c'  :  reg ardu  i.immxx i.isced durwk_  i.age i.year i.nace i.cnt2 if  fem==1  , vce(cluster isco)
  }
 } */
  
/*	{ //B.1 1st, 2nd generation vs natives (only 2021-22):  breakdown by education level
 
  use smpl_12 , clear
     foreach var of varlist _all {
	label var `var' ""
        }
   levelsof sisced, local(clist) clean

  foreach c in  `clist' { //loop of countries
 use smpl_12 if sisced==`c', clear
    //male
quiet: eststo  M12_p_edu`c' : reg ardu  i.immxx durwk_  age i.year i.cnt2 i.nace if  fem==0  , vce(cluster isco)     
  //female
quiet: eststo  F12_p_edu`c'  :  reg ardu  i.immxx  durwk_  age i.year i.cnt2 i.nace if  fem==1 , vce(cluster isco)
 }
 } */
 
}
	 	 
{ //** 3.  GAP (1 & 2 gen) LATEX

 *male & female (1st & 2nd generations)
    esttab  M12_p F12_p using "$rpath\imm12M.tex", replace ///
 cell(b(star fmt(3)) se(par fmt(3))) stats(N r2, fmt(%9.0fc %9.2fc) labels("N" "R2"))  ///
noobs  nonumber compress nogaps label  ///
 mlabels(, titles) ///
 drop( _cons 0.immxx) ///
   rename("1.imm1_" "EU (1st gen.)" "2.imm1_" "NEU (1st gen.)" ///
   "1.immxx" "EU (1st gen.)" "2.immxx" "NEU (1st gen.)"  "3.immxx" "EU (2nd gen.)" "4.immxx" "NEU (2nd gen.)")  ///
   indicate( "Education=*isced" ///
             "Tenure=durwk_" ///
		    "Age=*age"  ///
		   "Sector(NACE1d)=*nace"  ///
		   "Country x Year=*cnt2*" ///
		   "Year=*year")  ///
 starlevels(* 0.10 ** 0.05 *** 0.01)  ///
  addnote("Source: EU-LFS, O*NET")
  

}


  **# Bookmark #4.3  Econometric analysis  [by country x gender] : country heterogeneity
////////////////////////////////////////////////////////////////////////////////////////

 
{ //** Estimate models [country x gender] and collect coefficients 	===> db_imm_yresid_coef

use smpl_, clear 
     foreach var of varlist _all {
	label var `var' ""
        }
 levelsof cnt2_, local(clist) clean
 *local clist  AT BG
  levelsof fem, local(slist) clean
  
   foreach s in `slist' {
     foreach c in  `clist' {
	 	  tempfile x`c'`s'
 use smpl_ if cnt2_=="`c'" &  fem==`s' , clear //migrants only
 quiet: reg  ardu  i.imm1_  i.isced durwk_  i.age  i.year i.nace 
 regsave 1.imm1_ 2.imm1_ using `x`c'`s'', pval  addlabel(cnt2_,"`c'") replace
 }
 }
 
 clear  //import to avoid appending to open file
 gen cnt2_=""
 gen fem=.
 foreach s in `slist' { //append
    foreach c in  `clist' {  
		 append using `x`c'`s'', force
		 replace cnt2_=substr("`c'",3,2) if cnt2_==""
		 replace fem=`s' if fem==.
	 }
  }
 /*encode cnt2, gen(cnt2_)
	  //put non significant coefficients to zero
	gen coef_yres= coef //in anticipation of merge
	gen coef_yres_= coef
	replace coef_yres_=0 if pval>=.1
	rename pval pval_yres*/
		gen var_=1 if var=="1.imm1_"
		replace var_=2 if var=="2.imm1_"
		replace coef=0 if pval>.1
		merge m:1 cnt2_ fem using imm_s  //inject immigrants shares
		keep if _merge==3
		drop _merge
		merge m:1 cnt2_ using gdph_   //inject immigrants shares
		keep if _merge==3
		drop _merge	
save db_imm_coef, replace
	keep var_ cnt2 fem coef stderr pval N r2
	reshape wide coef stderr pval N r2 , i(cnt2_ fem) j(var_)  //wide version
		label var coef1  "EU"
		label var coef2 "NEU"

save db_imm_coefw, replace
}


	   //graphs illustrating the heterogeneity across country x gender !!!


		   *Arduousness gap (yresid=0), Annual rate of convergence

		  { //**male
		*recup pooled estimated gap
use smpl_ , clear
 quiet:  reg ardu  i.imm1_  i.isced durwk_  i.age  i.cnt2 i.year i.nace if  fem==0  , vce(cluster isco) 
local  betaEU = _b[1.imm1_]
local betaEUf : display %4.2f `betaEU'
local betaNEU = _b[2.imm1_]
local betaNEUf : display %4.2f `betaNEU'
         *plot country by countries coefficients
 use db_imm_coefw if fem==0 , clear
merge m:1 cnt2_ using gdph_   //inject immigrants shares
 gen x="/"
 egen cnt2_gdph=concat(cnt2_ x gdph), format(%9.0fc)
graph dot coef1 coef2, linegap(20) ///
                 lines(lp(shortdash) lw(thin))  ///
                 yline(`betaEU', lcol(green) lp(solid) lw(medium)) ///
				      yline(`betaNEU', lcol(red) lp(solid) lw(medium)) ///
					   yline(0, lcol(gs6) lp(solid) lw(thin)) ///
					   text(`betaEU' 0 "EU pooled (`betaEUf')", size(vsmall) col(green)) ///
					   	   text(`betaNEU' -2.5 "NEU pooled (`betaNEUf')", size(vsmall) col(red)) ///
					   linetype(line) ///
						lines(lwidth(vthin)) ///
                        over(cnt2_gdph, sort(1) descending label(labsize(*.6)) ) /// 
				      ytitle("Arduousness gap by country (M)", size(small)) ///
							marker(1,  msize(small)  mcolor(green) )	///
								marker(2, m(+) msize(small)  mcolor(red) )	///
					legend(label(1 "EU") label(2 "NEU") rows(1) size(small))
			 graph save gapcnt2M, replace
		  }
		  
		  
		  { //**female
		*recup pooled estimated gap
use smpl_ , clear
 quiet: reg ardu  i.imm1_  i.isced durwk_  i.age  i.cnt2 i.year i.nace if  fem==1  , vce(cluster isco) 
local  betaEU = _b[1.imm1_]
local betaEUf : display %4.2f `betaEU'
local betaNEU = _b[2.imm1_]
local betaNEUf : display %4.2f `betaNEU'
         *plot country by countries coefficients
 use db_imm_coefw if fem==1 , clear 
  merge m:1 cnt2_ using gdph_   //inject immigrants shares
 gen x="/"
 egen cnt2_gdph=concat(cnt2_ x gdph), format(%9.0fc)
graph dot coef1 coef2, linegap(20) ///
                 lines(lp(shortdash) lw(thin))  ///
                 yline(`betaEU', lcol(green) lp(solid) lw(medium)) ///
				      yline(`betaNEU', lcol(red) lp(solid) lw(medium)) ///
					 yline(0, lcol(gs6) lp(solid) lw(thin)) ///
					   text(`betaEU' 0 "EU pooled (`betaEUf')", size(vsmall) col(green)) ///
					   	   text(`betaNEU' -2.5 "NEU pooled (`betaNEUf')", size(vsmall) col(red)) ///
					   linetype(line) ///
						lines(lwidth(vthin)) ///
                        over(cnt2_gdph, sort(1) descending label(labsize(*.6)) ) /// 
				      ytitle("Arduousness gap by country (F)", size(small)) ///
							marker(1,  msize(small)  mcolor(green) )	///
								marker(2, m(+) msize(small)  mcolor(red) )	///
					legend(label(1 "EU") label(2 "NEU") rows(1) size(small))
			 graph save gapcnt2F, replace
		  }


 grc1leg  gapcnt2M.gph  gapcnt2F.gph,  legendfrom(gapcnt2M.gph) position(6)  ycommon xcommon 
save fig3, replace
graph export "$rpath\gapcnt2.png", replace
 
 
 *some regressions
 
 use db_imm_coef, clear
 gen NEU=0 if var=="1.imm1_"
 replace NEU=1 if var=="2.imm1_"
 gen gdph_10th=gdph/10000
 
 eststo cnthet : reg coef  fem  NEU  gdph_10th c.fem#c.gdph_10th  c.NEU# c.gdph_10th

 
  est dir
    esttab  cnthet using "$rpath\cnthet.tex", replace ///
 cell(b(star fmt(3)) se(par fmt(3))) stats(N r2, fmt(%9.0fc %9.2fc) labels("N" "R2"))  ///
noobs  nonumber compress nogaps label  ///
 mlabels(, titles) ///
 starlevels(* 0.10 ** 0.05 *** 0.01)  ///
  addnote("Source: EU-LFS, O*NET")