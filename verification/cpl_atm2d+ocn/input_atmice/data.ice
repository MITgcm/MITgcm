 &THSICE_CONST
# Tf0kel  = 273.15,
# rhosw   = 1030.,
#- with LANL albedo:
#albWarmSnow=0.75,
#- for full ice-fraction :
#icemaskmin = 1.,
#hThinIce   = 0.01,
#fracMelting= 0.,
#hThickIce  =100.,
#- with fractional ice:
 iceMaskmin = 0.02,
 hThinIce   = 0.2,
 hiMax      = 5.,
 hsMax      = 5.,
#albIceMax  =0.7,
#albIceMin  =0.7,
 &

 &THSICE_PARM01
 startIceModel=1,
 thSIce_calc_albNIR=.TRUE.,
# stepFwd_oceMxL=.TRUE.,
# ocean_deltaT=3600.,
#tauRelax_MxL=5184000.,
# stressReduction=0.,
 thSIce_taveFreq=315360000.,
#thSIce_monFreq=864000.,
 thSIce_diagFreq=0.,
 &
