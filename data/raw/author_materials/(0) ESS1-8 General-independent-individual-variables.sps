

* Encoding: UTF-8.

* GENERAL  & construction of individual-level independent variables.

****year and country 

**maak een variabele aan die de verschillende rondes van de ess weergeeft.
recode essround (1=2002) (2=2004) (3=2006) (4=2008) (5=2010) (6=2012) (7=2014) (8=2016) into essyear.
fre essyear.

recode cntry 
('AT' =40)
('BE' =56)
('BG'=88)
('CZ' =203)
('DK' =208)
('FI' =246)
('FR' =250)
('DE' =276)
('EE' =288)
('GR' =300)
('HR' = 311)
('HU' =348)
('IE' =372)
('IT' =380)
('LT'=488)
('NL' =528)
('NO' =578)
('PL' =616)
('PT' =620)
('SK' =703)
('SI' =705)
('ES' =724)
('SE' =752)
('CH' =756)
('GB' =826) into cnr.

*Welke landen zitten in welke ESS waves?.
cross cntry by essyear.

***COUNTRY ID van ParlGov toekennen:

freq cntry.
recode cntry 
('AT' =59)
('BE' =64)
('BG'=10)
('CH' =40)
('CY'=51)
('CZ' =68)
('DE' =54)
('DK' =21)
('ES' =27)
('EE' =75)
('FI' =67)
('FR' =43)
('GB' =44)
('GR' =41)
('HR' = 62)
('HU' =39)
('IE' =37)
('IS' =56)
('IT' =26)
('LT'=15)
('LU'=7)
('NL' =8)
('NO' =9)
('PL' =74)
('PT' =63)
('SK' =1)
('SI' =60)
('SE' =35)  into country_id.

*Latvia = 55.
*Malta =72.
*Romania = 23.

freq country_id.

**Selecting countries 22 countries.
use all.
Select IF (cntry = "AT") or (cntry = "BE") or (cntry = "BG") or (cntry = "CH") or (cntry = "CZ") or (cntry = "DE")  or (cntry = "DK") 
or (cntry="EE")  or (cntry = "FI") or (cntry = "FR") or (cntry="GB") or (cntry="GR")
or (cntry= "HU") or (cntry= "HR") or (cntry="LT") or (cntry="IT") or (cntry = "NL") or (cntry = "NO") or (cntry = "PL") or (cntry = "SK") or (cntry= "SI") or (cntry= "SE").
freq cntry.


***INDIVIDUAL-LEVEL INDEPENDENT VARIABLES.

***ATTITUDINAL VARIABLES***

*** X1: Political trust

FREQUENCIES trstprl trstplt trstprt.

SORT CASES  BY cntry.
SPLIT FILE LAYERED BY cntry.
RELIABILITY /VARIABLES=  trstprl trstplt trstprt /SUMMARY=TOTAL. 
split file off.

COMPUTE poltrust=mean (trstprl, trstplt, trstprt).

**Anti-immigration**.
cross essround by imbgeco imueclt imwbcnt /missing=include.
fre imbgeco imueclt imwbcnt.

SORT CASES  BY cntry.
SPLIT FILE LAYERED BY cntry.
RELIABILITY vars imbgeco imueclt imwbcnt/summary=all.
split file off.

recode imbgeco imueclt imwbcnt (0=10) (1=9) (2=8) (3=7) (4=6) (5=5) (6=4) (7=3) (8=2) (9=1) (10=0) (else=sysmis) into imbgecoR imuecltR imwbcntR.
fre imbgecoR imuecltR imwbcntR.
compute antimmi= mean (imbgecor, imuecltr, imwbcntr).
fre antimmi.

**authoritarian sentiments

FREQUENCIES ipbhprp impsafe ipfrule ipstrgv  imptrad.

recode ipfrule (1=5) (2=4) (3=3) (4=2) (5=1) (6=0) (else=SYSMIS) into autsen1.
VARIABLE LABELS autsen1 important follow rules.

recode ipstrgv (1=5) (2=4) (3=3) (4=2) (5=1) (6=0) (else=SYSMIS) into autsen2.
VARIABLE LABELS autsen2 important strong goverment.

recode ipbhprp (1=5) (2=4) (3=3) (4=2) (5=1) (6=0) (else=SYSMIS) (else=SYSMIS) into autsen3.
VARIABLE LABELS autsen3 important behave properly.

recode imptrad (1=5) (2=4) (3=3) (4=2) (5=1) (6=0) (else=SYSMIS) (else=SYSMIS) into autsen4.
VARIABLE LABELS autsen4 important to follow traditions.

recode impsafe (1=5) (2=4) (3=3) (4=2) (5=1) (6=0) (else=SYSMIS) (else=SYSMIS) into autsen5.
VARIABLE LABELS autsen4 important to live in secure surroundings.

SORT CASES  BY cntry.
SPLIT FILE LAYERED BY cntry.
RELIABILITY vars ipbhprp impsafe ipfrule ipstrgv  imptrad/summary=all.
split file off.

COMPUTE autsen=mean (autsen1, autsen2, autsen3, autsen4, autsen5).
FREQUENCIES autsen.

**Support for income distribution**

FREQUENCIES gincdif.
RECODE gincdif (1=4) (2=3) (3=2) (4=1) (5=0) (ELSE=SYSMIS) into redistribute.
FREQUENCIES redistribute.

**dissatisfaction with national economy**

fre stfeco.
recode stfeco (0=10) (1=9) (2=8) (3=7) (4=6) (5=5) (6=4) (7=3) (8=2) (9=1) (10=0) (else=sysmis) into badeconomy.
fre badeconomy.

***SOCIODEMOGRAPHIC VARIABLES***

**Economic well-being**

fre hincfel.
recode hincfel (1=3) (2=2) (3=1) (4=0) into subincome.
fre subincome.

** Education. 
*VIA EISCED. > 13.4% missing.
FREQUENCIES eisced.
RECODE eisced (1=0)(2=1)(3=2)(4=3)(5=4)(6=5)(7=6) (ELSE=SYSMIS) into education.
DESCRIPTIVES education.
fre education.

*Educ** 
*VIA een andere manier (zelf categorieen maken, idem als Rooduijn 2017)

fre edulvla edulvlb eisced eduyrs.
cross year by eisced.
cross year by edulvla.
cross cnr by eisced.
cross eisced by edulvla edulvlb.
fre eisced.
fre edulvlb.
recode edulvlb (0=1) (113=1) (129=2) (212=2) (213=2) (221=2) (222=2) (223=2) (229=3) (311=3) (321=3) (322=3) 
(312=3) (313=3) (323=3) (412=4) (413=4) (421=4) (422=4) (423=4) (510=5) (520=5) (610=5) (620=5) (710=5) (720=5) (800=5) (else=sysmis) into edulvlbR.
fre edulvlbr edulvla.

compute eduSC5=edulvla.
if (edulvlbr=1) edusc5=1.
if (edulvlbr=2) edusc5=2.
if (edulvlbr=3) edusc5=3.
if (edulvlbr=4) edusc5=4.
if (edulvlbr=5) edusc5=5.
recode edusc5 (55=sysmis) (else=copy).
fre edusc5.
add value labels edusc5 
1 "Less than lower secondary education"
2 "Lower secondary education completed"
3 "Upper secondary education completed"
4 "Post-secondary non-tertiary education completed"
5 "Tertiary education completed".
fre edusc5.
   
recode edusc5 (2=1) (else=0) into lowersecondary.
recode edusc5 (3=1) (else=0) into uppersecondary.
recode edusc5 (4=1) (else=0) into postsecondary.
recode edusc5 (5=1) (else=0) into tertiary.

RECODE edusc5 (1=0)(2=1)(3=2)(4=3)(5=4) into educ.

** Renaming age variable.
FREQUENCIES agea.
recode agea (14 thru 17=sysmis) (18 thru 102=copy) (999=SYSMIS) into age.
FREQUENCIES age.

**gender.
recode gndr (1=0) (2=1) into female.
des female.
fre female.

** Creating binomial unemployed variable.
FREQUENCIES mnactic.
RECODE mnactic (1 THRU 2=0) (3 thru 4=1) (5 thru 9= 0) (ELSE=SYSMIS) into unemployed.
FORMATS unemployed (f1.0).
FREQUENCIES unemployed.

**Religiosity**
fre rlgdgr.
recode rlgdgr (0=0) (1=1) (2=2) (3=3) (4=4) (5=5) (6=6) (7=7) (8=8) (9=9) (10=10) (else=sysmis) into religiosity.
fre religiosity.

**Political interest.
fre polintr.
recode polintr (1=3) (2=2) (3=1) (4=0) (else=sysmis) into polinterest.
fre polinterest.

**Rural/Urban**
fre domicil.
recode domicil (1=1) (2=1) (3=1) (4 =0)(5=0) (else=sysmis) into urban.
fre urban.

*** dummies of countries & ess rounds.
recode essround (1=1) (else=0) into Round1.
recode essround (2=1) (else=0) into Round2.
recode essround (3=1) (else=0) into Round3.
recode essround (4=1) (else=0) into Round4.
recode essround (5=1) (else=0) into Round5.
recode essround (6=1) (else=0) into Round6.
recode essround (7=1) (else=0) into Round7.
recode essround (8=1) (else=0) into Round8.

fre cnr.

recode cnr (40=1) (else=0) into Austria.
recode cnr (56=1) (else=0) into Belgium.
recode cnr (88=1) (else=0) into Bulgaria.
recode cnr (756=1) (else=0) into Switzerland.
recode cnr (203=1) (else=0) into CzechRepublic.
recode cnr (276=1) (else=0) into Germany.
recode cnr (208=1) (else=0) into Denmark.
recode cnr (288=1) (else=0) into Estonia.
recode cnr (246=1) (else=0) into Finland.
recode cnr (250=1) (else=0) into France.
recode cnr (300=1) (else=0) into Greece.
recode cnr (311=1) (else=0) into Croatia.
recode cnr (348=1) (else=0) into Hungary.
recode cnr (380=1) (else=0) into Italy.
recode cnr (488=1) (else=0) into Lithuania.
recode cnr (528=1) (else=0) into Netherlands.
recode cnr (578=1) (else=0) into Norway.
recode cnr (616=1) (else=0) into Poland.
recode cnr (703=1) (else=0) into Slovakia.
recode cnr (705=1) (else=0) into Slovenia.
recode cnr (752=1) (else=0) into Sweden.
recode cnr (826=1) (else=0) into UnitedKingdom.


***************************
***INTERVIEW DATE.
****Maak variabele "interview date".

**Maak eerst een nieuwe variabele aan die de losse informatie (jaar, maand, dag van interview) combineert tot 1 variabele.
compute interviewdate1 = (inwyr *10000) + (inwmm *  100) + inwdd.
compute interviewdate2 = (inwyys *10000) + (inwmms *  100) + inwdds.
freq interviewdate1.
freq interviewdate2.

compute interviewdate_temp = MEAN (interviewdate1, interviewdate2).
freq interviewdate_temp.
** There are 230 respondents missing (no interview date).

*sorteer de resultaten vd analyseop basis van land en ess-ronde.
SORT CASES  BY cntry essyear.
SPLIT FILE LAYERED BY cntry essyear.
*Vraag op wat het max en min zijn: eerste interviewdatum en laatste interview datum van fieldwork. 
DESCRIPTIVES VARIABLES=interviewdate_temp
  /STATISTICS=MIN MAX.
SPLIT FILE OFF.

**Handling the MISSINGS:.
IF (interviewdate_temp > 0) datemissing =0.
recode datemissing (sysmis=1)(0=0).
fre datemissing.
 
DO IF datemissing = 1.
compute interviewdate3 = (inwyye *10000) + (inwmme *  100) + inwdde.
end if.

compute interviewdate_temp2 = MEAN (interviewdate_temp, interviewdate3).
fre interviewdate_temp2.

***Geef missing cases (N=63) de eerste interviewdatum van ESS fieldwork, 
*De interviewdate-missings zijn geconcentreerd in BE, GB, SI.
*In deze landen geldt: de government veranderde NIET van far-right GOV naar OPP, of andersom, gedurende de ESS round.

IF (interviewdate_temp2 > 0) datestillmissing =0.
recode datestillmissing (sysmis=1)(0=0).
fre datestillmissing.

SORT CASES  BY cntry essyear.
SPLIT FILE LAYERED BY cntry essyear.
freq datestillmissing.
SPLIT FILE OFF.

delete variables interviewdate_min.
AGGREGATE
  /OUTFILE=* MODE=ADDVARIABLES
  /BREAK=essroundcnr
  /interviewdate_min=MIN(interviewdate_temp2).

FREQUENCIES interviewdate_min.

DO IF datestillmissing = 1.
compute interviewdate4 = interviewdate_min.
end if.
compute interviewdate = MEAN (interviewdate4, interviewdate).

use all.
fre interviewdate.

***************************
***************************


