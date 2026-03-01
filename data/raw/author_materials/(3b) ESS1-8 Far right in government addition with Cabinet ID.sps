* Encoding: UTF-8.

**"Far right in government" toevoegen op basis van Government ID (parlgov).

IF ( cabinetid=	889	)	farrightpower = 1.
IF ( cabinetid=	257	)	farrightpower = 1.
IF ( cabinetid=	257	)	farrightpower = 1.
IF ( cabinetid=	888	)	farrightpower = 1.
IF ( cabinetid=	805	)	farrightpower = 1.
IF ( cabinetid=	805	)	farrightpower = 1.
IF ( cabinetid=	198	)	farrightpower = 1.
IF ( cabinetid=	855	)	farrightpower = 1.
IF ( cabinetid=	855	)	farrightpower = 1.
IF ( cabinetid=	903	)	farrightpower = 1.
IF ( cabinetid=	903	)	farrightpower = 1.
IF ( cabinetid=	989	)	farrightpower = 1.
IF ( cabinetid=	989	)	farrightpower = 1.
IF ( cabinetid=	1212	)	farrightpower = 1.
IF ( cabinetid=	482	)	farrightpower = 1.
IF ( cabinetid=	482	)	farrightpower = 1.
IF ( cabinetid=	316	)	farrightpower = 1.
IF ( cabinetid=	788	)	farrightpower = 1.
IF ( cabinetid=	71	)	farrightpower = 1.
IF ( cabinetid=	1163	)	farrightpower = 1.
IF ( cabinetid=	1104	)	farrightpower = 1.
IF ( cabinetid=	1104	)	farrightpower = 1.
IF ( cabinetid=	256	)	farrightpower = 1.
IF ( cabinetid=	1042	)	farrightpower = 1.
IF ( cabinetid=	1042	)	farrightpower = 1.
IF ( cabinetid=	763	)	farrightpower = 1.
IF ( cabinetid=	804	)	farrightpower = 1.
IF ( cabinetid=	1065	)	farrightpower = 1.
IF ( cabinetid=	1065	)	farrightpower = 1.
IF ( cabinetid=	1196	)	farrightpower = 1.
IF ( cabinetid=	990	)	farrightpower = 1.
IF ( cabinetid=	269	)	farrightpower = 1.
IF ( cabinetid=	269	)	farrightpower = 1.
IF ( cabinetid=	269	)	farrightpower = 1.

recode farrightpower (1=1)(sysmis=0).
freq farrightpower.

*check all Cabinets per country, per ess round.

SORT CASES  BY cntry essround.
SPLIT FILE LAYERED BY cntry essround.
freq cabinetid.
split file off.

**compute "PERIOD"
**= "units of analysis" based on ess rounds &  governmental term.

freq cabinetid cnr essround.

compute period=(essround*10000000)+(cnr*10000)+ cabinetid.  
fre period.

**How many periods contain a radical right party?

AGGREGATE
  /OUTFILE=* MODE=ADDVARIABLES
  /BREAK=period
  /radicalright_mean=MEAN(radicalright).

** What is Percentage of radical right support per period?

SORT CASES  BY period.
SPLIT FILE LAYERED BY period.
freq radicalright.
split file off.
    
MEANS TABLES=radicalright BY period
 /CELLS=MEAN COUNT STDDEV.



**FILTER only "periods" that have one or more far-right voters.
* four periods are not not selected for this reason.


*** hier bezig ***
*** nu bezig met "EELECTIONS".






crosstab seats by radicalright.

SORT CASES  BY radicalright.
SPLIT FILE LAYERED BY radicalright.
freq seats.
split file off.

SORT CASES  BY radicalright seats.





****

IF	(	party=	104003.00	)	partyid=	50	.
IF	(	party=	204003.00	)	partyid=	50	.
IF	(	party=	304003.00	)	partyid=	50	.
IF	(	party=	404003.00	)	partyid=	50	.
IF	(	party=	704003.00	)	partyid=	50	.
IF	(	party=	804003.00	)	partyid=	50	.
fre partyid.

*delete variables radicalright_mean.
AGGREGATE
  /OUTFILE=* MODE=ADDVARIABLES
  /BREAK=period
  /radicalright_mean=MEAN(radicalright).

fre radicalright_mean.


IF (radicalright_mean > 0) selectperiod = 1.
IF (radicalright_mean = 0) selectperiod = 0.


freq selectperiod.


**far right cabinets, per period. *Please note: no of respondents includes non-voters.

crosstabs period by farrightpower.

**FILTER only periods that have one or more far-right voters.
* four periods are not not selected for this reason.

*delete variables filter.
USE ALL.
COMPUTE filter_$ =(selectperiod = 1).
FILTER BY filter_$.
EXECUTE.

MEANS TABLES=radicalright BY period
 /CELLS=MEAN COUNT STDDEV.

