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
# include "CTRL.h"
# include "CTRL_DUMMY.h"
# include "CTRL_GENARR.h"
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
      IF ( useCTRL) THEN
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
      ENDIF !if (useCTRL) then
#endif

      IF ( (useCTRL).AND.(.NOT.useAtmWind) ) THEN

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
