C $Header: /u/gcmpack/MITgcm/model/src/pressure_for_eos.F,v 1.4 2008/08/16 02:44:20 jmc Exp $
C $Name:  $
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: PRESSURE_FOR_EOS
C     !INTERFACE:
      SUBROUTINE PRESSURE_FOR_EOS(
     I        bi, bj, iMin, iMax, jMin, jMax,  k,
     O        locPres,
     I        myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE PRESSURE_FOR_EOS
C     | o Provide a local copy of the total pressure
C     |   at cell center (level k) for use in EOS funct. of P
C     *==========================================================*
C     \ev

C     !USES:

      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     bi,bj, k  :: tile and level indices
C     iMin,iMax,jMin,jMax :: computational domain
C     myThid - Thread number for this instance of the routine.
      INTEGER bi, bj, k
      INTEGER iMin,iMax,jMin,jMax
      _RL locPres(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     i,j :: loop index
      INTEGER  i,j
CEOP

C
C     provide the pressure for use in the equation of state
C
      IF ( usingZCoords ) THEN
C     in Z coordinates the pressure is rho0 * (hydrostatic) Potential
        IF ( useDynP_inEos_Zc ) THEN
C----------
C     NOTE: For now, totPhiHyd only contains the Potential anomaly
C           since PhiRef is not available for Atmos and has not (yet)
C           been added in S/R DIAGS_PHI_HYD
C----------
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
            locPres(i,j) = rhoConst*(
     &                   totPhiHyd(i,j,k,bi,bj)
     &                  -rC(k)*gravity )
c    &                             *maskC(i,j,k,bi,bj)
          ENDDO
         ENDDO
       ELSE
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
            locPres(i,j) = -rhoConst*rC(k)*gravity
c    &                              *maskC(i,j,k,bi,bj)
          ENDDO
         ENDDO
       ENDIF
      ELSEIF ( usingPCoords ) THEN
C     in P coordinates the pressure is just the coordinate of
C     the tracer point
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
            locPres(i,j) = rC(k)
c    &                   * maskC(i,j,k,bi,bj)
          ENDDO
         ENDDO
      ENDIF

      RETURN
      END
