C $Header: /u/gcmpack/MITgcm/pkg/seaice/ostres.F,v 1.10 2004/03/03 05:17:45 dimitri Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

CStartOfInterface
      SUBROUTINE ostres( DWATN, COR_ICE, myThid )
C     /==========================================================\
C     | SUBROUTINE ostres                                        |
C     | o Calculate ocean surface stress                         |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "SEAICE.h"
#include "SEAICE_PARAMS.h"

C     === Routine arguments ===
C     myThid - Thread no. that called this routine.
      _RL DWATN      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL COR_ICE        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      INTEGER myThid
CEndOfInterface

#ifdef ALLOW_SEAICE

C     === Local variables ===
C     i,j,bi,bj - Loop counters

      INTEGER i, j, bi, bj
      _RL  SINWIN, COSWIN, SINWAT, COSWAT

C   25 DEG GIVES SIN EQUAL TO 0.4226
      SINWIN=0.4226 _d 0
      COSWIN=0.9063 _d 0
      SINWAT=0.4226 _d 0
      COSWAT=0.9063 _d 0
c do not introduce turning angle
      SINWIN=ZERO
      COSWIN=ONE
      SINWAT=ZERO
      COSWAT=ONE

C--   Update overlap regions
      CALL EXCH_UV_XY_RL(WINDX, WINDY, .TRUE., myThid)

#ifndef SEAICE_EXTERNAL_FLUXES
cdm !!! NOTE that wind stress under sea-ice is set to the      !!!
cdm !!! same value as it would be if there was no sea-ice.     !!!
cdm !!! This is a placeholder until a more physically-sensible !!!
cdm !!! stable solution is developed.  Apparently ocean stress !!!
cdm !!! that is based on sea-ice velocity can cause trouble in !!!
cdm !!! Arctic Ocean configurations.  This temporary solution  !!!
cdm !!! also corresponds to that adopted by CCSM-2.            !!!
C--   Interpolate wind stress (N/m^2) from South-West B-grid
C     to South-West C-grid for forcing ocean model.
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
            fu(I,J,bi,bj)=HALF
     &           *(WINDX(I,J+1,bi,bj)+WINDX(I,J,bi,bj))
            fv(I,J,bi,bj)=HALF
     &           *(WINDY(I+1,J,bi,bj)+WINDY(I,J,bi,bj))
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      CALL EXCH_UV_XY_RS(fu, fv, .TRUE., myThid)
#endif /* ifndef SEAICE_EXTERNAL_FLUXES */

#endif /* ALLOW_SEAICE */

      RETURN
      END
