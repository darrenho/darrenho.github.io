DATA soil;
INPUT soil $  rep   texture $   method $ prop;
Y = prop/100;
CARDS;
Beaumont       1     Clay         ISNT      95.77
Beaumont       2     Clay         ISNT      97.09
Beaumont       3     Clay         ISNT      97.04
LA-RP          1     Clay         ISNT      96.94
LA-RP          2     Clay         ISNT      96.89
LA-RP          3     Clay         ISNT      96.80
ARShark        1     Clay         ISNT      96.97
ARShark        2     Clay         ISNT      94.56
ARShark        3     Clay         ISNT      95.37
Ganado         1     SiltLoam     ISNT      98.33
Ganado         2     SiltLoam     ISNT      95.76
Ganado         3     SiltLoam     ISNT      94.53
LA-VP          1     SiltLoam     ISNT      96.34
LA-VP          2     SiltLoam     ISNT        .
LA-VP          3     SiltLoam     ISNT      95.43
Forestdale     1     SiltLoam     ISNT      94.93
Forestdale     2     SiltLoam     ISNT      96.27
Forestdale     3     SiltLoam     ISNT      95.59
Beaumont       1     Clay         DSD       94.93
Beaumont       2     Clay         DSD       92.36
Beaumont       3     Clay         DSD       93.65
LA-RP          1     Clay         DSD       91.75
LA-RP          2     Clay         DSD       93.48
LA-RP          3     Clay         DSD       93.10
ARShark        1     Clay         DSD       91.45
ARShark        2     Clay         DSD       90.32
ARShark        3     Clay         DSD       91.34
Ganado         1     SiltLoam     DSD       89.02
Ganado         2     SiltLoam     DSD       87.32
Ganado         3     SiltLoam     DSD       89.15
LA-VP          1     SiltLoam     DSD       89.78
LA-VP          2     SiltLoam     DSD       94.95
LA-VP          3     SiltLoam     DSD       94.95
Forestdale     1     SiltLoam     DSD       90.79
Forestdale     2     SiltLoam     DSD       86.20
Forestdale     3     SiltLoam     DSD       90.65
;
TITLE 'random effects model: pseudo likelihood';
PROC GLIMMIX DATA = soil PLOTS = (studentpanel) NOBOUND;
	CLASS texture soil method;
 	MODEL Y = texture method texture*method / dist = beta link = logit ddfm=kr;
	RANDOM soil(texture) method*soil(texture);
	COVTEST 'H_0: independent model' INDEP;
	LSMEANS texture method / ILINK PLOT = meanplot(join cl) ADJUST = tukey;
	LSMEANS texture*method / ILINK PLOT = meanplot(sliceby = texture join cl) ADJUST = tukey;
RUN;

/* run the fixed effects without the random effects or soil*/
/* Note: as we used pseudo likelihood before, we can't do any formal model selection */
TITLE 'fixed effects model';
PROC GLIMMIX DATA = soil PLOTS = (residualpanel pearsonpanel studentpanel) METHOD=quadrature;
	CLASS texture method;
 	MODEL Y = texture method texture*method / dist = beta link = logit;
	LSMEANS texture method / ILINK PLOT = meanplot(join cl) ADJUST = tukey;
RUN;

/* These following two GLIMMIX statements give the same output (up to numerical approximation) */
/* As they use a genuine likelihood (as opposed to the default pseudo), we can get goodness of fit statistics */

TITLE 'random effects model: laplace';
PROC GLIMMIX DATA = soil PLOTS = (residualpanel pearsonpanel studentpanel) METHOD=laplace;
	CLASS texture soil method;
 	MODEL Y = texture method texture*method / dist = beta link = logit;
	RANDOM soil(texture) method*soil(texture);
	LSMEANS texture method / ILINK PLOT = meanplot(join cl) ADJUST = tukey;
RUN;

TITLE 'random effects model: quadrature';
PROC GLIMMIX DATA = soil PLOTS = (residualpanel pearsonpanel studentpanel) METHOD=quadrature;
	CLASS texture soil method;
 	MODEL Y = texture method texture*method / dist = beta link = logit;
	RANDOM intercept / subject = soil(texture);
	RANDOM intercept / subject = method*soil(texture);
	LSMEANS texture method / ILINK PLOT = meanplot(join cl) ADJUST = tukey;
RUN;