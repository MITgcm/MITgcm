 &THSICE_CONST
 Tf0kel  = 273.15,
#- with LANL albedo:
#albWarmSnow= 0.75,
#- for full ice-fraction :
#iceMaskMin = 1.,
#hThinIce   = 0.01,
#fracEnMelt = 0.,
#hThickIce  = 100.,
#- with fractional ice:
 iceMaskMin = 0.05,
 hiMax      = 10.,
 hsMax      = 10.,
 dhSnowLin  = 0.1, 
 fracEnFreez= 0.4, 
 hNewIceMax = 1.,
#albIceMax  = 0.7,
#albIceMin  = 0.7,
#albColdSnow= 0.85,
 albWarmSnow= 0.60,
 tempSnowAlb= -5.,
 albOldSnow = 0.60,
#hNewSnowAge= 2.e-3,
#snowAgTime = 4320000.,
 albIceMax  = 0.60,
#albIceMin  = 0.20,
 hAlbIce    = 0.44,
 hAlbSnow   = 0.15,
 &

 &THSICE_PARM01
#StartIceModel=1,
 thSIceAdvScheme=77,
#thSIce_diffK   =800.,
 stressReduction=0.,
#thSIce_taveFreq=2592000.,
#thSIce_diagFreq=2592000.,
#thSIce_monFreq =2592000.,
 &

