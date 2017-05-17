/* 11/02/2014  Debut d'exploitation  de la base acess avec un fichier .csv principal  */
/* 03/02/2015  Exploitation base meteo   */
/* 26/02/2015 : Rectification d'une erreur sur le merge (2 cles : Region et annee !) */
/* 20/04/2017 : Reprise programme EpurationOptilait.do    */

clear
global root "d:/progs/Optilait"
*global root "c:/chris/progs/Optilait"

cd $root

insheet using data\OptiSO22A_18ans_RegionOK-destring.csv, delimit(";")

des   /* 63861  obs.*/ 
  
/*
save $root\data\OptiSO22A_18ans_RegionOK.dta, replace

/* On utilise el fichier stata */
use $root\data\OptiSO22A_18ans_RegionOK.dta, clear
*/

/*  Cleaning */ 

drop anne /* prendre variable annnee) */

/* Analyse des missing sur sau pour decider quel  echantillon d'annees a garder  */ 
preserve
keep ident annee sau
drop if annee <1996
drop if annee > 2006
reshape wide sau,  i(ident) j(annee )
misstable pat sau*
restore

 /* On s'apercoit que les pattern de missing les plus courants 
 sont  en fin de periode et au debut */
 
 /* on a 84% de l'echantillon avec toutes les variables renseignes sur ces annees */

 /* On deccide de garder de 1996 a2006  */
drop if annee <1996 | annee >2006 

/* Calcul des individus presents sur les annees  */

bysort ident : gen NbreAn = _N
*bysort ident : keep if _n==1
*bysort NbreAn : count

keep if NbreAn == 11
distinct ident   /* 2589  individus  */


/*  detection des dynamiques aberrantes   TODO */
xtset ident annee
xtline sau if sau >300
count if sau >300
xtline sau if sau >300 , t(annee) i(ident ) overlay
xtline sau if ident == 9094019, t(annee) i(ident ) overlay
xtline  sfp if ident == 9094019, t(annee) i(ident ) overlay

/* Premiers essais de modelisation */ 


/* modele a tester en NP  TODO */
* Vu avec Stephane : prixlaiteriemoyen variable a utiliser !
regress laitproduit  sau sfp  eqvltotal hasfpirri charpot  prixlaiteriemoyen quota   concvlr  I.rgion if annee ==2006


regress laitproduit  sau sfp  eqvltotal hasfpirri charpot  prixlaiteriemoyen quota   concvlr   if annee ==2006

/* Sortie pour R */
* update 28/04/2014 : we keep also prixconcvlr as w1 

outsheet ident annee sau laitproduit  sau sfp  eqvltotal hasfpirri charpot  prixlaiteriemoyen quota ///
		concvlr  prixconcvlr rgion using "data/Optilait.csv", delimit(",") replace

/* Sortie Stata pour plus tard  */
keep ident annee sau laitproduit  sau sfp  eqvltotal hasfpirri charpot  prixlaiteriemoyen quota ///
		concvlr prixconcvlr rgion 
save $root\data\Optilait.dta, replace



/* ====== Exploitation base meteo ========  */
clear
insheet using data\ToutMeteoDiese_prAnalyse.csv, delimit(";")

des
*tab annee  /* il y a des annees 1960- > 2100 !!*/
tab lieu

save $root\data\ToutMeteoDiese_prAnalyse.dta, replace

/* Comme pour les donnees Optilait , On deccide de garder de 1996 a 2006  */
drop if annee <1996 | annee >2006 

/* Creation des regions cf "Zones/cartes reion avec villes.xls"  */

gen region =.
replace region = 1 if lieu =="MILLA"
replace region = 2 if lieu =="VILLE"
replace region = 3 if lieu =="GOURD"
replace region = 4 if lieu =="ALBI_"
replace region = 5 if lieu =="STGIR"
replace region = 6 if lieu =="PAU"

/* Creation deficit hydrique = precipitation - etp   */

gen DeltaH =  p - etp

/* Creation Moyennes par an et par region  */

bysort annee region : egen Tyear = mean(tmoy)
bysort annee region : egen ETPyear = mean(etp)
bysort annee region : egen DELTAyear = mean(DeltaH)

/* Creation Moyennes sur periode production   */
gen tmoytemp = tmoy
replace tmoytemp =. if jr_1erjanv <120 | jr_1erjanv >240

gen etptemp = etp
replace etptemp =. if jr_1erjanv <120 | jr_1erjanv >240

gen Deltatemp = DeltaH
replace Deltatemp =. if jr_1erjanv <120 | jr_1erjanv >240

bysort annee region : egen Tprod = mean(tmoytemp)
bysort annee region : egen ETPprod = mean(etptemp)
bysort annee region : egen DELTAprod = mean(Deltatemp)

capture drop  *temp 
/* Quelques stats des ...!!*/

sum *year if annee == 2003

parcord2 *year if annee ==2003,  by(region) 

parcord2 *year ,  by(region) 

/* On ne garde plus qu'une information par an et par region */

keep if region !=.
bysort annee region : keep if _n==1
keep annee region *year *prod

save $root\data\BaseMeteo11Ans.dta, replace
count
 
/* ===   MERGING the two database ===  */

use $root\data\Optilait.dta, clear
drop if annee <1996 | annee >2006 
count
tab rgion
rename rgion region
merge n:n region annee using $root\data\BaseMeteo11Ans.dta
 
 
/* Corrections : */
/* changement unites  1996-2002  (2/02/2016 S&C) */
 
replace  prixlaiteriemoyen = (prixlaiteriemoyen * 1000) /6.55957 if annee <=2001

replace  prixconcvlr = (prixconcvlr * 1000) /6.55957 if annee <=2001

/* Correction prixlait a zero   */
 sum prixlaiteriemoyen  if region ==2
 count if prixlaiteriemoyen ==0
 replace prixlaiteriemoyen = r(mean) if prixlait ==0


/* Sauvegarde fichier de travail pour R  et stata */
 drop _merge
/* Cgt d'unite pour le lait */
/* supprime pour etre en accord avec les ,nouvelles unites  */

*replace laitproduit = laitproduit /1000   
*replace quotalait  = quotalait  /1000
outsheet using "data/OptilaitMerge2.csv", delimit(",") replace

save $root\data\OptilaitMerge2.dta, replace


/* ====== 02/2016 ===========*/ 
/* Correction on some outliers concerning production or  heads (identified by laitpercows */

use $root\data\OptilaitMerge2.dta, clear
gen laitpercow = laitproduit /eqvltotal
gen suprod= (laitpercow > 6500)
distinct ident if suprod ==1
edit  if  laitpercow >10000

*edit eqvltotal laitproduit laitpercow  annee suprod  if ident ==64460005
*edit eqvltotal laitproduit laitpercow annee suprod  if ident == 64093133

/* L'individu passe de 50 à 19 vaches en 2000 et 2001 sans baisser de production */ 
bysort ident: egen IsSuper = sum(suprod)
edit eqvltotal laitproduit laitpercow  annee suprod IsSuper ident if IsSuper == 1

drop if suprod ==1  /* 174 obs removed  */



/* 17/02/2016 : Computiong profit and sd(profit) HERE !!!! */
/* As in the R program, w2 = 15 (fixed)  */ 
gen Profit =  prixlaiteriemoyen * laitproduit/1000 - prixconcvlr *(concvlr*eqvltotal)/1000 - 15*hasfpirri 

bysort ident : egen SigmaProf = sd(Profit)   /* la variation de profit est donc intra annuelle/individu  */

save $root\data\OptilaitMerge3.dta, replace    /* <<----- Fichier à utiliser dans R */



 
 /* Stats  Des */
count
corr *year
 
corr laitproduit quota
bysort annee : sum  Tyear  /* Pas de variation sensible de la temperature, ni ETP, ni DELTA en 2003 !!*/

 
 
 /* petit test  */
count if annee ==2006
regress laitproduit  sau sfp  eqvltotal hasfpirri charpot  prixlaiteriemoyen quota  ///
					concvlr DELTAyear Tyear  ETPyear if annee ==2006
					
/* si l'on introsuit la region, on garde la significativite des variables meteo  */
xi :regress laitproduit  sau sfp  eqvltotal hasfpirri charpot  prixlaiteriemoyen quota  ///
					concvlr DELTAyear Tyear  ETPyear i.region if annee ==2006














/* Inutile pour notre analyse --
levelsof lieu

gen commune=.
label variable commune "Code postal commune"

replace commune = 81000   if lieu =="ALBI_"
replace commune = 9140   if lieu =="AULUS"
replace commune = 15000   if lieu =="AURIL"
replace commune = 31320   if lieu =="AUZEV"
replace commune = 46127   if lieu =="GOURD"
replace commune = 12210   if lieu =="LAGUIOLE"
replace commune = 12210   if lieu =="LAGUIOLE_Safran"
replace commune = 81500   if lieu =="LAVAU" 
replace commune = 41170   if lieu =="MARCE"   /* <<<<--- a verifier "Saint-Marc-du-Cor"  ?????*/		
replace commune = 12100   if lieu =="MILLA"   
replace commune = 64000   if lieu =="PAU"   
replace commune = 12800   if lieu =="QUINS"   
replace commune = 12800   if lieu =="QUINS_Safran"   
*replace commune = XXXX   if lieu =="SALLL"   		 /* <<<<--- a trouver */	
replace commune = 09200   if lieu =="STGIR"   
replace commune = 31000   if lieu =="TOULO"   
replace commune = 31290   if lieu =="VILLE"   

/* On a 365-366 valeurs pour chaque commune ann�e !!*/ 

*/

		
		
