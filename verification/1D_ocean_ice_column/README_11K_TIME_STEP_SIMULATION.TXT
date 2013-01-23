HOW TO RUN AND ANALYZE A 11,000 TIME STEP EXPERIMENT
JAN 23, 2013

MITgcm checkpoint 64c.  
seaice_growth.F version: 1.185 (2012/12/27)

1) Make the following changes to the input_ad/data* files 
for the 11,000 time step simulation.

a) data 

>  nTimeSteps= 11000,
>  monitorFreq=0.
>  adjMonitorFreq = 1.,

b) data.autodiff 

>   useKPPinAdMode = .FALSE.,

c) data.diagnostics

>   frequency(1)  = 86400,
>   fields(1,1)   = 'EXFuwind','EXFvwind','EXFlwdn', 'EXFswdn','EXFatemp','EXFaqh','EXFevap','EXFpreci','EXFempmr','EXFhl   ','EXFhs   ',
>   filename(1)   = 'EXF_avg_daily',

>   frequency(2)  = 86400,
>   fields(1:3,2)   = 'SIarea  ','SIheff  ','SIhsnow ',
>   filename(2)   = 'ICE_avg_daily',

d) data.pkg

>   useGrdchk = .FALSE.,
>   useMNC    = .TRUE,
>   useDiagnostics = .TRUE.,


2) Make the following changes to packages.conf in code_ad:

a) packages.conf 

> diagnostics
> mnc


3) After running, use the two matlab scripts (analyze_forward_run_general.m, analyze_adjoint_run_general.m) in ./matlab_scripts to plot the results.  Be sure that the helper script, 'tenPercentAboveBelowLeftRight.m' is in your matlab path. 
