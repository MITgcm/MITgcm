#--------------------+-----------------
# CFC pkg parameter setting
# lines starting with "#" are comments
#--------------------+-----------------
#  atmCFC_inpFile    : file name of Atmospheric CFC time series (ASCII file)
#                        default = 'cfc1112.atm'
#  atmCFC_recSepTime : time spacing between 2 records of atmos CFC [s]
#                        default = 31104000. (360 days)
#  atmCFC_timeOffset : time offset for atmos CFC (cfcTime = myTime + offSet)
#                        default = recSepTime - deltaT * PTRACERS_Iter0
#  atmCFC_yNorthBnd  : Northern Lat boundary for interpolation [y-unit]
#                        default =  +10.
#  atmCFC_ySouthBnd  : Southern Lat boundary for interpolation [y-unit]
#                        default =  -10.
#  CFC_windFile      : file name of wind speeds          ; default =  ' '
#  CFC_atmospFile    : file name of atmospheric pressure ; default =  ' '
#  CFC_iceFile       : file name of seaice fraction      ; default =  ' '
#  CFC_forcingPeriod : record spacing time for CFC forcing (seconds)
#                        default = externForcingPeriod
#  CFC_forcingCycle  : periodic-cycle freq for CFC forcing (seconds)
#                        default = externForcingCycle
#--------------------+-----------------
 &CFC_FORCING
#- default atmCFC_timeOffset is set according to PTRACERS_Iter0
#   and corresponds to -5899*360*86400 as set just below:
# atmCFC_timeOffset = -183482496000.,
  CFC_iceFile='fice.bin',
  CFC_windFile='tren_speed.bin',
 &
