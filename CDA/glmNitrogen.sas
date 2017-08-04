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

TITLE 'Fixed effects model';
PROC GLIMMIX DATA = soil PLOTS = (residualpanel pearsonpanel studentpanel);
	CLASS texture soil method;
 	MODEL Y = texture soil(texture) method texture*method method*soil(texture) / dist = beta link = logit;
	LSMEANS texture method / ILINK ADJUST = tukey;
	LSMEANS soil(texture) / ILINK SLICE = texture SLICEDIFF = texture PLOT = meanplot(sliceby = texture join cl) ADJUST = tukey;
RUN;
