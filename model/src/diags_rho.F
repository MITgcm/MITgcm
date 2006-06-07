C $Header: /u/gcmpack/MITgcm/model/src/diags_rho.F,v 1.3 2006/06/07 01:55:12 heimbach Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: DIAGS_RHO
C     !INTERFACE:
      SUBROUTINE DIAGS_RHO( 
     I                       k, bi, bj,
     I                       rhoK, rhoKm1,
     I                       myTime, myIter, myThid)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R DIAGS_RHO                                    
C     | o Buoyancy Flux diagnostics 
C     *==========================================================*
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
c #include "SURFACE.h"
#include "DYNVARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     k, bi,bj      :: level & tile indices
C     phiHydC    :: hydrostatic potential anomaly at cell center
C                  (atmos: =Geopotential ; ocean-z: =Pressure/rho)
C     myTime :: Current time
C     myIter :: Current iteration number
C     myThid :: Instance number for this call of the routine.
      INTEGER k, bi,bj
      _RL rhoK  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rhoKm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL myTime
      INTEGER myIter, myThid
CEOP

#ifdef ALLOW_DIAGNOSTICS

C     !LOCAL VARIABLES:
C     == Local variables ==
C     i,j :: Loop counters
      INTEGER i,j
      _RL tmpFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON

      IF ( DIAGNOSTICS_IS_ON('RHOANOSQ',myThid) ) THEN
        DO j=1,sNy
         DO i=1,sNx
           tmpFld(i,j) = rhoK(i,j)*rhoK(i,j)
         ENDDO
        ENDDO
        CALL DIAGNOSTICS_FILL(tmpFld,'RHOANOSQ',k,1,2,bi,bj,myThid)
      ENDIF

      IF ( DIAGNOSTICS_IS_ON('URHOMASS',myThid) ) THEN
        DO j=1,sNy
         DO i=1,sNx+1
           tmpFld(i,j) = uVel(i,j,k,bi,bj)*_hFacW(i,j,k,bi,bj)
     &                 *(rhoK(i-1,j)+rhoK(i,j))*0.5 _d 0
         ENDDO
        ENDDO
        CALL DIAGNOSTICS_FILL(tmpFld,'URHOMASS',k,1,2,bi,bj,myThid)
      ENDIF

      IF ( DIAGNOSTICS_IS_ON('VRHOMASS',myThid) ) THEN
        DO j=1,sNy+1
         DO i=1,sNx
           tmpFld(i,j) = vVel(i,j,k,bi,bj)*_hFacS(i,j,k,bi,bj)
     &                 *(rhoK(i,j-1)+rhoK(i,j))*0.5 _d 0
         ENDDO
        ENDDO
        CALL DIAGNOSTICS_FILL(tmpFld,'VRHOMASS',k,1,2,bi,bj,myThid)
      ENDIF

      IF ( DIAGNOSTICS_IS_ON('WRHOMASS',myThid) ) THEN
       IF ( k.EQ.1 ) THEN
        DO j=1,sNy
         DO i=1,sNx
           tmpFld(i,j) = wVel(i,j,k,bi,bj)*rhoK(i,j)
         ENDDO
        ENDDO
       ELSE
        DO j=1,sNy
         DO i=1,sNx
           tmpFld(i,j) = wVel(i,j,k,bi,bj)
     &                 *(rhoKm1(i,j)+rhoK(i,j))*0.5 _d 0
         ENDDO
        ENDDO
       ENDIF
       CALL DIAGNOSTICS_FILL(tmpFld,'WRHOMASS',k,1,2,bi,bj,myThid)
      ENDIF

#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
