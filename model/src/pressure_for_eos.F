C $Header: /u/gcmpack/MITgcm/model/src/pressure_for_eos.F,v 1.7 2016/04/27 22:09:13 jmc Exp $
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
#ifdef ALLOW_NONHYDROSTATIC
# include "NH_VARS.h"
#endif /* ALLOW_NONHYDROSTATIC */

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
#ifdef ALLOW_NONHYDROSTATIC
       IF ( selectP_inEOS_Zc.EQ.3 ) THEN
C-     use full (hydrostatic+non-hydrostatic) dynamical pressure:
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            locPres(i,j) = rhoConst*(
     &                   totPhiHyd(i,j,k,bi,bj)
     &                 +( phi_nh(i,j,k,bi,bj) - dPhiNH(i,j,bi,bj) )
     &                 + phiRef(2*k) )
c    &                             *maskC(i,j,k,bi,bj)
          ENDDO
         ENDDO
       ELSEIF ( selectP_inEOS_Zc.EQ.2 ) THEN
#else /* ALLOW_NONHYDROSTATIC */
       IF     ( selectP_inEOS_Zc.EQ.2 ) THEN
#endif /* ALLOW_NONHYDROSTATIC */
C-     use hydrostatic dynamical pressure:
C----------
C     NOTE: For now, totPhiHyd only contains the Potential anomaly
C           since PhiRef has not (yet) been added in S/R DIAGS_PHI_HYD
C----------
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            locPres(i,j) = rhoConst*(
     &                   totPhiHyd(i,j,k,bi,bj)
     &                 + phiRef(2*k) )
c    &                             *maskC(i,j,k,bi,bj)
          ENDDO
         ENDDO
c      ELSEIF ( selectP_inEOS_Zc.EQ.1 ) THEN
C note: for the case selectP_inEOS_Zc=0, also use pRef4EOS (set to
C       rhoConst*phiRef(2*k) ) to reproduce same previous machine truncation
       ELSEIF ( selectP_inEOS_Zc.LE.1 ) THEN
C-     use horizontally uniform reference pressure pRef
C      (solution of: pRef = integral{-g*rho(Tref,Sref,pRef)*dz} )
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            locPres(i,j) = pRef4EOS(k)
c    &                              *maskC(i,j,k,bi,bj)
          ENDDO
         ENDDO
       ELSE
C-     simplest case: -g*rhoConst*z
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            locPres(i,j) = rhoConst*phiRef(2*k)
c    &                              *maskC(i,j,k,bi,bj)
          ENDDO
         ENDDO
       ENDIF
      ELSEIF ( usingPCoords ) THEN
C     in P coordinates the pressure is just the coordinate of
C     the tracer point
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            locPres(i,j) = rC(k)
c    &                   * maskC(i,j,k,bi,bj)
          ENDDO
         ENDDO
      ENDIF

      RETURN
      END
