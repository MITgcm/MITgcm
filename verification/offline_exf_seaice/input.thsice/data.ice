 &THSICE_CONST
#- with fractional ice:
 iceMaskMin = 0.001,
 hiMax      = 10.,
 hsMax      = 10.,
 dhSnowLin  = 0.1,
 fracEnFreez= 0.4,
 hNewIceMax = 1.,
 albIceMax  = 0.6,
 albIceMin  = 0.6,
#albColdSnow= 0.85,
#albWarmSnow= 0.60,
#tempSnowAlb= -5.,
#albOldSnow = 0.60,
#hNewSnowAge= 2.e-3,
#snowAgTime = 4320000.,
#hAlbIce    = 0.44,
#hAlbSnow   = 0.15,
 &

 &THSICE_PARM01
#StartIceModel=1,
#thSIce_skipThermo=.TRUE.,
#thSIceAdvScheme=77,
#thSIce_diffK   =800.,
 stressReduction=0.,
 thSIceFract_InitFile='ice0_area.bin',
 thSIceThick_InitFile='const+20.bin',
#thSIce_diagFreq=2592000.,
#thSIce_monFreq =43200.,
 thSIce_monFreq =36000.,
 &

