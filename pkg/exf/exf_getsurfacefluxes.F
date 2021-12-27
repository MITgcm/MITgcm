#include "EXF_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif
#ifdef ALLOW_ECCO
# include "ECCO_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: EXF_GETSURFACEFLUXES
C     !INTERFACE:
      SUBROUTINE EXF_GETSURFACEFLUXES(
     I                                 mytime,
     I                                 myiter,
     I                                 mythid
     &                               )
C     !DESCRIPTION: \bv
c     ==================================================================
c     SUBROUTINE EXF_GETSURFACEFLUXES
c     ==================================================================
c
c     o Mid-level routine for enabling the use of flux fields as control
c       variables.
c
c     started: Christian Eckert eckert@mit.edu  30-Jun-1999
c
c     changed: Christian Eckert eckert@mit.edu  14-Jan-2000
c              - Restructured the code in order to create a package
c                for the MITgcmUV.
c
c              Christian Eckert eckert@mit.edu  12-Feb-2000
c              - Changed Routine names (package prefix: exf_)
c
c     mods for pkg/seaice: menemenlis@jpl.nasa.gov 20-Dec-2002
c
c     ==================================================================
c     SUBROUTINE EXF_GETSURFACEFLUXES
c     ==================================================================
C     \ev
C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"

#include "EXF_FIELDS.h"
#include "EXF_PARAM.h"
#ifdef ALLOW_CTRL
# include "CTRL_SIZE.h"
# include "ctrl.h"
# include "ctrl_dummy.h"
# include "CTRL_GENARR.h"
#endif
#if (defined (ALLOW_ECCO) && defined (ECCO_CTRL_DEPRECATED))
#  include "ecco_cost.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime :: Current time in simulation
C     myIter :: Current iteration number in simulation
C     myThid :: Thread number for this instance of the routine.
      _RL mytime
      INTEGER myiter
      INTEGER mythid

C     !LOCAL VARIABLES:
C     == Local variables
#ifdef ALLOW_CTRL
# ifdef ECCO_CTRL_DEPRECATED
      _RS mask2D(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
# endif
# ifdef ALLOW_ROTATE_UV_CONTROLS
      _RL     tmpUE(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL     tmpVN(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL     tmpUX(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL     tmpVY(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
# endif

#ifdef ALLOW_GENTIM2D_CONTROL
      INTEGER iarr
#endif

#if (defined (ALLOW_ROTATE_UV_CONTROLS) || defined (ALLOW_GENTIM2D_CONTROL))
      INTEGER i,j,bi,bj
#endif
CEOP

#ifdef ALLOW_ROTATE_UV_CONTROLS
      if ( useCTRL ) then
        DO bj = mybylo(mythid),mybyhi(mythid)
         DO bi = mybxlo(mythid),mybxhi(mythid)
          DO j = 1-oly,sny+oly
           DO i = 1-olx,snx+olx
             tmpUE(i,j,bi,bj) = 0. _d 0
             tmpVN(i,j,bi,bj) = 0. _d 0
             tmpUX(i,j,bi,bj) = 0. _d 0
             tmpVY(i,j,bi,bj) = 0. _d 0
           ENDDO
          ENDDO
         ENDDO
        ENDDO
      endif
#endif

#if (defined (ALLOW_CTRL) && \
     defined (ALLOW_GENTIM2D_CONTROL))
      IF ( useCTRL.AND.ctrlUseGen) THEN
       DO bj = myByLo(myThid),myByHi(myThid)
        DO bi = myBxLo(myThid),mybxhi(myThid)
         DO j = 1,sNy
          DO i = 1,sNx
           DO iarr = 1, maxCtrlTim2D
#ifndef ALLOW_ROTATE_UV_CONTROLS
           if (xx_gentim2d_file(iarr)(1:7).EQ.'xx_tauu')
     &       ustress(i,j,bi,bj)=ustress(i,j,bi,bj)+
     &                         xx_gentim2d(i,j,bi,bj,iarr)
           if (xx_gentim2d_file(iarr)(1:7).EQ.'xx_tauv')
     &       vstress(i,j,bi,bj)=vstress(i,j,bi,bj)+
     &                         xx_gentim2d(i,j,bi,bj,iarr)
#else
           if (xx_gentim2d_file(iarr)(1:7).EQ.'xx_tauu')
     &       tmpUE(i,j,bi,bj)=tmpUE(i,j,bi,bj)
     &          +xx_gentim2d(i,j,bi,bj,iarr)
           if (xx_gentim2d_file(iarr)(1:7).EQ.'xx_tauv')
     &       tmpVN(i,j,bi,bj)=tmpVN(i,j,bi,bj)
     &          +xx_gentim2d(i,j,bi,bj,iarr)
#endif
           if (xx_gentim2d_file(iarr)(1:8).EQ.'xx_hflux')
     &       hflux(i,j,bi,bj)=hflux(i,j,bi,bj)+
     &                         xx_gentim2d(i,j,bi,bj,iarr)
           if (xx_gentim2d_file(iarr)(1:8).EQ.'xx_sflux')
     &       sflux(i,j,bi,bj)=sflux(i,j,bi,bj)+
     &                         xx_gentim2d(i,j,bi,bj,iarr)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF !if (ctrlUseGen) then
#endif

CML#if (!defined (ALLOW_ECCO) || defined (ECCO_CTRL_DEPRECATED))
#ifdef ECCO_CTRL_DEPRECATED

#ifdef ALLOW_HFLUX_CONTROL
      CALL CTRL_GET_MASK2D( xx_hflux_file, mask2D, myThid )
      CALL CTRL_GET_GEN (
     &     xx_hflux_file, xx_hfluxstartdate, xx_hfluxperiod,
     &     mask2D, hflux, xx_hflux0, xx_hflux1, xx_hflux_dummy,
     &     xx_hflux_remo_intercept, xx_hflux_remo_slope,
     &     whflux, mytime, myiter, mythid )
#endif

#ifdef ALLOW_SFLUX_CONTROL
      CALL CTRL_GET_MASK2D( xx_sflux_file, mask2D, myThid )
      CALL CTRL_GET_GEN (
     &     xx_sflux_file, xx_sfluxstartdate, xx_sfluxperiod,
     &     mask2D, sflux, xx_sflux0, xx_sflux1, xx_sflux_dummy,
     &     xx_sflux_remo_intercept, xx_sflux_remo_slope,
     &     wsflux, mytime, myiter, mythid )
#endif

      IF ( .NOT.useAtmWind ) THEN

      IF ( .NOT.ctrlUseGen ) THEN
#ifndef ALLOW_ROTATE_UV_CONTROLS

#ifdef ALLOW_USTRESS_CONTROL
      CALL CTRL_GET_MASK2D( xx_tauu_file, mask2D, myThid )
      CALL CTRL_GET_GEN (
     &     xx_tauu_file, xx_tauustartdate, xx_tauuperiod,
     &     mask2D, ustress, xx_tauu0, xx_tauu1, xx_tauu_dummy,
     &     xx_tauu_remo_intercept, xx_tauu_remo_slope,
     &     wtauu, mytime, myiter, mythid )
#endif

#ifdef ALLOW_VSTRESS_CONTROL
      CALL CTRL_GET_MASK2D( xx_tauv_file, mask2D, myThid )
      CALL CTRL_GET_GEN (
     &     xx_tauv_file, xx_tauvstartdate, xx_tauvperiod,
     &     mask2D, vstress, xx_tauv0, xx_tauv1, xx_tauv_dummy,
     &     xx_tauv_remo_intercept, xx_tauv_remo_slope,
     &     wtauv, mytime, myiter, mythid )
#endif

#else

#if (defined (ALLOW_USTRESS_CONTROL) && defined (ALLOW_VSTRESS_CONTROL))
      CALL CTRL_GET_MASK2D( xx_tauu_file, mask2D, myThid )
      CALL CTRL_GET_GEN (
     &     xx_tauu_file, xx_tauustartdate, xx_tauuperiod,
     &     mask2D, tmpUE, xx_tauu0, xx_tauu1, xx_tauu_dummy,
     &     xx_tauu_remo_intercept, xx_tauu_remo_slope,
     &     wtauu, mytime, myiter, mythid )

      CALL CTRL_GET_MASK2D( xx_tauv_file, mask2D, myThid )
      CALL CTRL_GET_GEN (
     &     xx_tauv_file, xx_tauvstartdate, xx_tauvperiod,
     &     mask2D, tmpVN, xx_tauv0, xx_tauv1, xx_tauv_dummy,
     &     xx_tauv_remo_intercept, xx_tauv_remo_slope,
     &     wtauv, mytime, myiter, mythid )

#endif /* ALLOW_USTRESS_CONTROL and ALLOW_VSTRESS_CONTROL */

#endif /* ALLOW_ROTATE_UV_CONTROLS */
      ENDIF ! (.NOT.ctrlUseGen)

#else
      IF ( (useCTRL).AND.(.NOT.useAtmWind) ) THEN
#endif /* undef ALLOW_ECCO || def ECCO_CTRL_DEPRECATED */

#ifdef ALLOW_ROTATE_UV_CONTROLS
      _EXCH_XY_RL(tmpUE,myThid)
      _EXCH_XY_RL(tmpVN,myThid)

      CALL ROTATE_UV2EN_RL(tmpUX,tmpVY,tmpUE,tmpVN,
     &     .FALSE.,stressIsOnCgrid,.TRUE.,1,mythid)

      IF ( stressIsOnCgrid ) THEN
        CALL EXCH_UV_XY_RL( tmpUX, tmpVY, .TRUE., myThid )
      ELSE
        CALL EXCH_UV_AGRID_3D_RL( tmpUX, tmpVY, .TRUE., 1, myThid)
      ENDIF

        DO bj = mybylo(mythid),mybyhi(mythid)
         DO bi = mybxlo(mythid),mybxhi(mythid)
          DO j = 1-oly,sny+oly
           DO i = 1-olx,snx+olx
             ustress(i,j,bi,bj)=ustress(i,j,bi,bj)+tmpUX(i,j,bi,bj)
             vstress(i,j,bi,bj)=vstress(i,j,bi,bj)+tmpVY(i,j,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
#endif /* ALLOW_ROTATE_UV_CONTROLS */

      ENDIF !( .NOT.useAtmWind )

#endif /* ALLOW_CTRL */

      END
