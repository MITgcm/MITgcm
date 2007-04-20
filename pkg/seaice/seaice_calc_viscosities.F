C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_calc_viscosities.F,v 1.4 2007/04/20 18:29:58 mlosch Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

CStartOfInterface
      SUBROUTINE SEAICE_CALC_VISCOSITIES( 
     I     e11, e22, e12, zMin, zMax, hEffM, press0,
     O     eta, zeta, press, 
     I     myThid )
C     /==========================================================\
C     | SUBROUTINE  SEAICE_CALC_VISCOSITIES                      |
C     | o compute shear and bulk viscositites eta, zeta and the  |
C     |   corrected ice strength P                               |
C     |   (see Zhang and Hibler,   JGR, 102, 8691-8702, 1997)    |
C     |==========================================================|
C     | written by Martin Losch, Mar 2006                        |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SEAICE_PARAMS.h"

#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C     === Routine arguments ===
C     myThid - Thread no. that called this routine.
      INTEGER myThid
C     strain rate tensor
      _RL e11   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL e22   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL e12   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     
      _RL zMin  (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL zMax  (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL hEffM (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
C     
      _RL press0(1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RL press (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
C     bulk viscosity
      _RL  eta  (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
C     shear viscosity
      _RL zeta  (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
CEndOfInterface

#if ( defined (SEAICE_CGRID) && defined (SEAICE_ALLOW_DYNAMICS) )
C     === Local variables ===
C     i,j,bi,bj - Loop counters
C     e11, e12, e22 - components of strain rate tensor
C     ecm2          - inverse of square of eccentricity of yield curve
      INTEGER i, j, bi, bj
      _RL ECM2, e12C, deltaC

C--   FIRST SET UP BASIC CONSTANTS
      ecm2=0. _d 0
      IF ( SEAICE_eccen .NE. 0. _d 0 ) ecm2=ONE/(SEAICE_eccen**2)
C
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
          e12C = 0.25 * ( e12(I,J  ,bi,bj) + e12(I+1,J  ,bi,bj) 
     &         +          e12(I,J+1,bi,bj) + e12(I+1,J+1,bi,bj) )
          deltaC = (e11(i,j,bi,bj)**2+e22(i,j,bi,bj)**2)*(ONE+ecm2)
     &         + 4. _d 0*ecm2*e12C**2 
     &         + 2. _d 0*e11(i,j,bi,bj)*e22(i,j,bi,bj)*(ONE-ecm2)
          deltaC = SQRT(MAX(deltaC,SEAICE_EPS_SQ))
          zeta(I,J,bi,bj)=HALF*PRESS0(I,J,bi,bj)/deltaC
C NOW PUT MIN AND MAX VISCOSITIES IN
          zeta(I,J,bi,bj)=MIN(ZMAX(I,J,bi,bj),zeta(I,J,bi,bj))
          zeta(I,J,bi,bj)=MAX(ZMIN(I,J,bi,bj),zeta(I,J,bi,bj))
C NOW SET VISCOSITIES TO ZERO AT HEFFMFLOW PTS 
          zeta (I,J,bi,bj) = zeta(I,J,bi,bj)*HEFFM(I,J,bi,bj) 
          eta  (I,J,bi,bj) = ECM2*zeta(I,J,bi,bj)
          press(I,J,bi,bj) = TWO*zeta(I,J,bi,bj)*deltaC
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C--   Update overlap regions
      _EXCH_XY_R8(  eta, myThid)
      _EXCH_XY_R8( zeta, myThid)
      _EXCH_XY_R8(press, myThid)
#endif /* SEAICE_ALLOW_DYNAMICS and SEAICE_CGRID */
      RETURN
      END
