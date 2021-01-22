CBOP
C     !ROUTINE: THSICE_TAVE.h
C     !INTERFACE:
C     include "THSICE_TAVE.h"
C     !DESCRIPTION:
C     \bv
C     *==========================================================*
C     | THSICE_TAVE.h
C     | o Header for Therm-SeaICE time-average diagnostic
C     *==========================================================*
C     \ev
CEOP

#ifdef ALLOW_THSICE
#ifdef ALLOW_TIMEAVE

C--   COMMON /THSICE_TAVE_VARS/ Time average THermodynamic-SeaICE variables
C     ice_timeAve    :: cumulated time [s]
C     ice_fract_Ave  :: cumulated Ice fraction  [0-1]
C     ice_iceH_Ave   :: cumulated Ice thickness [m]
C     ice_snowH_Ave  :: cumulated Snow thickness [m]
C     ice_Tsrf_Ave   :: cumulated surface Temperature [oC]
C     ice_Tice1_Ave  :: cumulated 1srt level Temp. [oC]
C     ice_Tice2_Ave  :: cumulated 2nd  level Temp. [oC]
cC    ice_snowPr_Ave :: cumulated snow precipitation (+=down) [kg/m2/s]
C     ice_flx2oc_Ave :: cumulated heat flux out of the ocean (+=up) [W/m2]
C     ice_frw2oc_Ave :: cumulated fresh-water flux out off the ocean (E-P) [m/s]
C     ice_salFx_Ave  :: cumulated salt flux out of the ocean (+=up) [g/m2/s]
C     ice_flxAtm_Ave :: cumulated net heat flux from Atmosphere (+=down) [W/m2]
C     ice_frwAtm_Ave :: cumulated fresh-water flux from Atmos. (+=up) [kg/m2/s]
C     ice_albedo_Ave :: cumulated sea-ice albedo [0-1]
C     ice_tMxL_Ave   :: cumulated ocean mixed-layer Temp. [oC]
C     ice_sMxL_Ave   :: cumulated ocean mixed-layer salinity [g/kg]

      _RL ice_timeAve(nSx,nSy)
      _RL ice_fract_Ave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL ice_iceH_Ave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL ice_snowH_Ave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL ice_Tsrf_Ave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL ice_Tice1_Ave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL ice_Tice2_Ave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
c     _RL ice_snowPr_Ave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL ice_flx2oc_Ave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL ice_frw2oc_Ave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL ice_salFx_Ave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL ice_flxAtm_Ave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL ice_frwAtm_Ave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL ice_albedo_Ave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL ice_tMxL_Ave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL ice_sMxL_Ave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      COMMON /THSICE_TAVE_ARRAYS/ ice_timeAve,
     &                 ice_fract_Ave, ice_iceH_Ave,
     &                 ice_snowH_Ave, ice_Tsrf_Ave,
     &                 ice_Tice1_Ave, ice_Tice2_Ave,
     &                 ice_flx2oc_Ave,ice_frw2oc_Ave,ice_salFx_Ave,
     &                 ice_flxAtm_Ave,ice_frwAtm_Ave,ice_albedo_Ave,
     &                 ice_tMxL_Ave, ice_sMxL_Ave

#endif /* ALLOW_TIMEAVE */
#endif /* ALLOW_THSICE */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
