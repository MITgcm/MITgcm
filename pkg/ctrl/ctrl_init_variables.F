#include "CTRL_OPTIONS.h"

CBOP
C     !ROUTINE: CTRL_INIT_VARIABLES
C     !INTERFACE:
      SUBROUTINE CTRL_INIT_VARIABLES( myThid )
C     !DESCRIPTION: \bv
c     ==================================================================
c     SUBROUTINE CTRL_INIT_VARIABLES
c     ==================================================================
c
c     o Set parts of the vector of control variables and initialize the
c       rest to zero.
c
c     started: heimbach@mit.edu 25-Mar-2002
c
c     ==================================================================
c     SUBROUTINE CTRL_INIT_VARIABLES
c     ==================================================================
C     \ev
C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#ifdef ALLOW_CTRL
# include "CTRL_SIZE.h"
# include "ctrl.h"
# include "CTRL_FIELDS.h"
# include "CTRL_GENARR.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid :: Thread number for this instance of the routine.
      INTEGER myThid

#ifdef ALLOW_CTRL

C     !LOCAL VARIABLES:
C     == Local variables
      INTEGER bi,bj
      INTEGER k
#if (defined ALLOW_BOTTOMDRAG_CONTROL) || (defined ECCO_CTRL_DEPRECATED)
      INTEGER i,j
#endif
#ifdef ECCO_CTRL_DEPRECATED
      INTEGER itlo,ithi
      INTEGER jtlo,jthi
#endif
c     == end of interface ==
CEOP

#ifdef ECCO_CTRL_DEPRECATED
      jtlo = myByLo(myThid)
      jthi = myByHi(myThid)
      itlo = myBxLo(myThid)
      ithi = myBxHi(myThid)

      DO bj = jtlo,jthi
        DO bi = itlo,ithi
          num_zero_mean(bi,bj) = 0. _d 0
          objf_zero_mean(bi,bj) = 0. _d 0
          objf_zero_smoo(bi,bj) = 0. _d 0
        ENDDO
      ENDDO
#endif

#ifdef ECCO_CTRL_DEPRECATED
#ifdef ALLOW_HFLUX_CONTROL
      DO bj = jtlo,jthi
        DO bi = itlo,ithi
          DO j = 1-OLy,sNy+OLy
            DO i = 1-OLx,sNx+OLx
              xx_hflux0(i,j,bi,bj) = 0. _d 0
              xx_hflux1(i,j,bi,bj) = 0. _d 0
            ENDDO
          ENDDO
        ENDDO
      ENDDO
#endif

#ifdef ALLOW_SFLUX_CONTROL
      DO bj = jtlo,jthi
        DO bi = itlo,ithi
          DO j = 1-OLy,sNy+OLy
            DO i = 1-OLx,sNx+OLx
              xx_sflux0(i,j,bi,bj) = 0. _d 0
              xx_sflux1(i,j,bi,bj) = 0. _d 0
            ENDDO
          ENDDO
        ENDDO
      ENDDO
#endif

#ifdef ALLOW_USTRESS_CONTROL
      DO bj = jtlo,jthi
        DO bi = itlo,ithi
          DO j = 1-OLy,sNy+OLy
            DO i = 1-OLx,sNx+OLx
              xx_tauu0(i,j,bi,bj) = 0. _d 0
              xx_tauu1(i,j,bi,bj) = 0. _d 0
            ENDDO
          ENDDO
        ENDDO
      ENDDO
#endif

#ifdef ALLOW_VSTRESS_CONTROL
      DO bj = jtlo,jthi
        DO bi = itlo,ithi
          DO j = 1-OLy,sNy+OLy
            DO i = 1-OLx,sNx+OLx
              xx_tauv0(i,j,bi,bj) = 0. _d 0
              xx_tauv1(i,j,bi,bj) = 0. _d 0
            ENDDO
          ENDDO
        ENDDO
      ENDDO
#endif

#ifdef ALLOW_UWIND_CONTROL
      DO bj = jtlo,jthi
        DO bi = itlo,ithi
          DO j = 1-OLy,sNy+OLy
            DO i = 1-OLx,sNx+OLx
              xx_uwind0(i,j,bi,bj) = 0. _d 0
              xx_uwind1(i,j,bi,bj) = 0. _d 0
            ENDDO
          ENDDO
        ENDDO
      ENDDO
#endif

#ifdef ALLOW_VWIND_CONTROL
      DO bj = jtlo,jthi
        DO bi = itlo,ithi
          DO j = 1-OLy,sNy+OLy
            DO i = 1-OLx,sNx+OLx
              xx_vwind0(i,j,bi,bj) = 0. _d 0
              xx_vwind1(i,j,bi,bj) = 0. _d 0
            ENDDO
          ENDDO
        ENDDO
      ENDDO
#endif

#ifdef ALLOW_ATEMP_CONTROL
      DO bj = jtlo,jthi
        DO bi = itlo,ithi
          DO j = 1-OLy,sNy+OLy
            DO i = 1-OLx,sNx+OLx
              xx_atemp0(i,j,bi,bj) = 0. _d 0
              xx_atemp1(i,j,bi,bj) = 0. _d 0
            ENDDO
          ENDDO
        ENDDO
      ENDDO
#endif

#ifdef ALLOW_AQH_CONTROL
      DO bj = jtlo,jthi
        DO bi = itlo,ithi
          DO j = 1-OLy,sNy+OLy
            DO i = 1-OLx,sNx+OLx
              xx_aqh0(i,j,bi,bj) = 0. _d 0
              xx_aqh1(i,j,bi,bj) = 0. _d 0
            ENDDO
          ENDDO
        ENDDO
      ENDDO
#endif

#ifdef ALLOW_PRECIP_CONTROL
      DO bj = jtlo,jthi
        DO bi = itlo,ithi
          DO j = 1-OLy,sNy+OLy
            DO i = 1-OLx,sNx+OLx
              xx_precip0(i,j,bi,bj) = 0. _d 0
              xx_precip1(i,j,bi,bj) = 0. _d 0
            ENDDO
          ENDDO
        ENDDO
      ENDDO
#endif

#ifdef ALLOW_SWFLUX_CONTROL
      DO bj = jtlo,jthi
        DO bi = itlo,ithi
          DO j = 1-OLy,sNy+OLy
            DO i = 1-OLx,sNx+OLx
              xx_swflux0(i,j,bi,bj) = 0. _d 0
              xx_swflux1(i,j,bi,bj) = 0. _d 0
            ENDDO
          ENDDO
        ENDDO
      ENDDO
#endif

#ifdef ALLOW_SWDOWN_CONTROL
      DO bj = jtlo,jthi
        DO bi = itlo,ithi
          DO j = 1-OLy,sNy+OLy
            DO i = 1-OLx,sNx+OLx
              xx_swdown0(i,j,bi,bj) = 0. _d 0
              xx_swdown1(i,j,bi,bj) = 0. _d 0
            ENDDO
          ENDDO
        ENDDO
      ENDDO
#endif

#ifdef ALLOW_LWFLUX_CONTROL
      DO bj = jtlo,jthi
        DO bi = itlo,ithi
          DO j = 1-OLy,sNy+OLy
            DO i = 1-OLx,sNx+OLx
              xx_lwflux0(i,j,bi,bj) = 0. _d 0
              xx_lwflux1(i,j,bi,bj) = 0. _d 0
            ENDDO
          ENDDO
        ENDDO
      ENDDO
#endif

#ifdef ALLOW_LWDOWN_CONTROL
      DO bj = jtlo,jthi
        DO bi = itlo,ithi
          DO j = 1-OLy,sNy+OLy
            DO i = 1-OLx,sNx+OLx
              xx_lwdown0(i,j,bi,bj) = 0. _d 0
              xx_lwdown1(i,j,bi,bj) = 0. _d 0
            ENDDO
          ENDDO
        ENDDO
      ENDDO
#endif

#ifdef ALLOW_EVAP_CONTROL
      DO bj = jtlo,jthi
        DO bi = itlo,ithi
          DO j = 1-OLy,sNy+OLy
            DO i = 1-OLx,sNx+OLx
              xx_evap0(i,j,bi,bj) = 0. _d 0
              xx_evap1(i,j,bi,bj) = 0. _d 0
            ENDDO
          ENDDO
        ENDDO
      ENDDO
#endif

#ifdef ALLOW_SNOWPRECIP_CONTROL
      DO bj = jtlo,jthi
        DO bi = itlo,ithi
          DO j = 1-OLy,sNy+OLy
            DO i = 1-OLx,sNx+OLx
              xx_snowprecip0(i,j,bi,bj) = 0. _d 0
              xx_snowprecip1(i,j,bi,bj) = 0. _d 0
            ENDDO
          ENDDO
        ENDDO
      ENDDO
#endif

#ifdef ALLOW_APRESSURE_CONTROL
      DO bj = jtlo,jthi
        DO bi = itlo,ithi
          DO j = 1-OLy,sNy+OLy
            DO i = 1-OLx,sNx+OLx
              xx_apressure0(i,j,bi,bj) = 0. _d 0
              xx_apressure1(i,j,bi,bj) = 0. _d 0
            ENDDO
          ENDDO
        ENDDO
      ENDDO
#endif

#ifdef ALLOW_RUNOFF_CONTROL
      DO bj = jtlo,jthi
        DO bi = itlo,ithi
          DO j = 1-OLy,sNy+OLy
            DO i = 1-OLx,sNx+OLx
              xx_runoff0(i,j,bi,bj) = 0. _d 0
              xx_runoff1(i,j,bi,bj) = 0. _d 0
            ENDDO
          ENDDO
        ENDDO
      ENDDO
#endif

#ifdef ALLOW_SST_CONTROL
      DO bj = jtlo,jthi
        DO bi = itlo,ithi
          DO j = 1-OLy,sNy+OLy
            DO i = 1-OLx,sNx+OLx
              xx_sst0(i,j,bi,bj) = 0. _d 0
              xx_sst1(i,j,bi,bj) = 0. _d 0
            ENDDO
          ENDDO
        ENDDO
      ENDDO
#endif

#ifdef ALLOW_SSS_CONTROL
      DO bj = jtlo,jthi
        DO bi = itlo,ithi
          DO j = 1-OLy,sNy+OLy
            DO i = 1-OLx,sNx+OLx
              xx_sss0(i,j,bi,bj) = 0. _d 0
              xx_sss1(i,j,bi,bj) = 0. _d 0
            ENDDO
          ENDDO
        ENDDO
      ENDDO
#endif

#ifdef ALLOW_ATM_MEAN_CONTROL
      DO bj = jtlo,jthi
        DO bi = itlo,ithi
          DO j = 1-OLy,sNy+OLy
            DO i = 1-OLx,sNx+OLx
# ifdef ALLOW_ATEMP_CONTROL
              xx_atemp_mean (i,j,bi,bj) = 0. _d 0
# endif
# ifdef ALLOW_AQH_CONTROL
              xx_aqh_mean   (i,j,bi,bj) = 0. _d 0
# endif
# ifdef ALLOW_UWIND_CONTROL
              xx_uwind_mean (i,j,bi,bj) = 0. _d 0
# endif
# ifdef ALLOW_VWIND_CONTROL
              xx_vwind_mean (i,j,bi,bj) = 0. _d 0
# endif
# ifdef ALLOW_PRECIP_CONTROL
              xx_precip_mean(i,j,bi,bj) = 0. _d 0
# endif
# ifdef ALLOW_SWDOWN_CONTROL
              xx_swdown_mean(i,j,bi,bj) = 0. _d 0
# endif
            ENDDO
          ENDDO
        ENDDO
      ENDDO
#endif /* ALLOW_ATM_MEAN_CONTROL */
#endif /* ECCO_CTRL_DEPRECATED */

#ifdef ALLOW_BOTTOMDRAG_CONTROL
      DO bj = myByLo(myThid),myByHi(myThid)
        DO bi = myBxLo(myThid),myBxHi(myThid)
          DO j = 1-OLy,sNy+OLy
            DO i = 1-OLx,sNx+OLx
              bottomDragFld(i,j,bi,bj) = 0. _d 0
            ENDDO
          ENDDO
        ENDDO
      ENDDO
#endif

#ifdef ALLOW_OBCS_CONTROL
      IF (useOBCS) CALL CTRL_INIT_OBCS_VARIABLES( myThid )
#endif

C--   Map the control variables onto the model state.
C-    Needs to be called after few {PKG}_init_varia (e.g., PTRACERS_INIT_VARIA)
      IF ( .NOT. ctrlUseGen ) THEN
# ifdef ALLOW_ECCO
#  ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('CTRL_MAP_INI_ECCO',myThid)
#  endif
      CALL CTRL_MAP_INI_ECCO( myThid )
# else
#  ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('CTRL_MAP_INI',myThid)
#  endif
      CALL CTRL_MAP_INI( myThid )
# endif
      ENDIF !IF ( .NOT. ctrlUseGen ) THEN

      IF ( ctrlUseGen ) THEN
#if (defined (ALLOW_GENARR2D_CONTROL) || defined (ALLOW_GENARR3D_CONTROL))
      CALL CTRL_MAP_INI_GENARR( myThid )
#endif
#ifdef ALLOW_GENTIM2D_CONTROL
      CALL CTRL_MAP_INI_GENTIM2D( myThid )
#endif

#ifdef ALLOW_GENTIM2D_CONTROL
      DO bj = myByLo(myThid),myByHi(myThid)
        DO bi = myBxLo(myThid),myBxHi(myThid)
          DO k=1,maxCtrlTim2D
            objf_gentim2d(bi,bj,k)     = 0. _d 0
            num_gentim2d(bi,bj,k)      = 0. _d 0
#ifdef ECCO_CTRL_DEPRECATED
            num_gentim2dm(bi,bj,k)     = 0. _d 0
            objf_gentim2dm(bi,bj,k)    = 0. _d 0
            objf_gentim2dsmoo(bi,bj,k) = 0. _d 0
#endif /* ECCO_CTRL_DEPRECATED */
          ENDDO
        ENDDO
      ENDDO
#endif
#ifdef ALLOW_GENARR2D_CONTROL
      DO bj = myByLo(myThid),myByHi(myThid)
        DO bi = myBxLo(myThid),myBxHi(myThid)
          DO k=1,maxCtrlArr2D
            objf_genarr2d(bi,bj,k)    = 0. _d 0
            num_genarr2d(bi,bj,k)    = 0. _d 0
          ENDDO
        ENDDO
      ENDDO
#endif
#ifdef ALLOW_GENARR3D_CONTROL
      DO bj = myByLo(myThid),myByHi(myThid)
        DO bi = myBxLo(myThid),myBxHi(myThid)
          DO k=1,maxCtrlArr3D
            objf_genarr3d(bi,bj,k)    = 0. _d 0
            num_genarr3d(bi,bj,k)    = 0. _d 0
          ENDDO
        ENDDO
      ENDDO
#endif
      ENDIF !IF ( .NOT. ctrlUseGen ) THEN

#ifdef ALLOW_SEAICE
      CALL SEAICE_CTRL_MAP_INI( myThid )
#endif

#endif /* ALLOW_CTRL */

      RETURN
      END
