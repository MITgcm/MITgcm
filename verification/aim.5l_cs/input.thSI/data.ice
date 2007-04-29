 &THSICE_CONST
 Tf0kel  = 273.15,
 rhosw   = 1030.,
#- with LANL albedo:
#albWarmSnow= 0.75,
 iceMaskMin = 0.01,
 hThinIce   = 0.1,
 hiMax      = 10.,
 hsMax      = 10.,
#albIceMax  = 0.7,
#albIceMin  = 0.7,
 &

 &THSICE_PARM01
#startIceModel=1,
 stepFwd_oceMxL=.TRUE.,
#ocean_deltaT=3600.,
 tauRelax_MxL=5184000.,
#thSIceAdvScheme=1,
#thSIce_diffK   =800.,
 stressReduction=0.,
#thSIce_taveFreq=2592000.,
 thSIce_diagFreq=5184000.,
#thSIce_monFreq=864000.,
#thSIce_diagFreq=0.,
 &

