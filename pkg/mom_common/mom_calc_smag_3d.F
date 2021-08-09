#include "MOM_COMMON_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: MOM_CALC_SMAG_3D

C     !INTERFACE:
      SUBROUTINE MOM_CALC_SMAG_3D(
     I        str11, str22, str33, str12, str13, str23,
     O        viscAh3d_00, viscAh3d_12,
     O        viscAh3d_13, viscAh3d_23,
     I        smag3D_hLsC, smag3D_hLsW, smag3D_hLsS, smag3D_hLsZ,
     I        k, bi, bj, myThid )

C     !DESCRIPTION:
C     Calculate Smagorinsky 3-D (harmonic) viscosities
C      at current level k (for viscAh3d_00 & viscAh3d_12)
C      and at level k+1   (for viscAh3d_13 & viscAh3d_23)

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#ifdef ALLOW_SMAG_3D_DIFFUSIVITY
# include "DYNVARS.h"
#endif
c#include "MOM_VISC.h"

C     !INPUT PARAMETERS:
C     str11       :: strain component Vxx @ grid-cell center
C     str22       :: strain component Vyy @ grid-cell center
C     str33       :: strain component Vzz @ grid-cell center
C     str12       :: strain component Vxy @ grid-cell corner
C     str13       :: strain component Vxz @ above uVel
C     str23       :: strain component Vyz @ above vVel
C     smag3D_hLsC :: horiz. grid length scale (power 2/3) at grid cell center
C     smag3D_hLsW :: horiz. grid length scale (power 2/3) at western  edge
C     smag3D_hLsS :: horiz. grid length scale (power 2/3) at southern egde
C     smag3D_hLsZ :: horiz. grid length scale (power 2/3) at grid cell corner
C     k           :: current level index
C     bi, bj      :: tile indices
C     myThid      :: my Thread Id number
      _RL str11(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL str22(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL str33(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL str12(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL str13(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+1)
      _RL str23(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+1)
      _RS smag3D_hLsC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS smag3D_hLsW(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS smag3D_hLsS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS smag3D_hLsZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER k, bi, bj
      INTEGER myThid

C     !OUTPUT PARAMETERS:
C     viscAh3d_00 :: Smagorinsky viscosity @ grid-cell center
C     viscAh3d_12 :: Smagorinsky viscosity @ grid-cell corner
C     viscAh3d_13 :: Smagorinsky viscosity @ above uVel
C     viscAh3d_23 :: Smagorinsky viscosity @ above vVel
      _RL viscAh3d_00(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL viscAh3d_12(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL viscAh3d_13(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+1)
      _RL viscAh3d_23(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+1)
CEOP

#ifdef ALLOW_SMAG_3D
C     !LOCAL VARIABLES:
C     S66   :: Sum of squared of the 3 strain component @ grid-cell center
C     S12   :: Squared of strain component Vxy @ grid-cell corner
C     S13   :: Squared of strain component Vxz @ above uVel
C     S23   :: Squared of strain component Vyz @ above vVel
      INTEGER i, j
      INTEGER kl, n
      _RL twoThird, tmpFac, locVisc
      _RL S66(1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL S12(1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL S13(1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL S23(1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      twoThird = 2. _d 0 / 3. _d 0

      DO n=1,2
        kl = k + n - 1
        IF ( kl.LE.Nr ) THEN
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
             S66(i,j,n) = str11(i,j,kl)**2
     &                  + str22(i,j,kl)**2
     &                  + str33(i,j,kl)**2
             S12(i,j,n) = str12(i,j,kl)**2
           ENDDO
          ENDDO
        ELSE
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
             S66(i,j,n) = 0. _d 0
             S12(i,j,n) = 0. _d 0
           ENDDO
          ENDDO
        ENDIF
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
             S13(i,j,n) = str13(i,j,kl)**2
             S23(i,j,n) = str23(i,j,kl)**2
           ENDDO
          ENDDO
      ENDDO

C--  ------------------------------------------------------------------
C--  calculate current level Smag viscosity coeff
C--  ------------------------------------------------------------------

C     Current level k --> n=1
      kl = k
      n = 1
      tmpFac = twoRL*SQRT(twoRL)*drF(kl)**twoThird

C     viscAh3d_00 = sqrt( S11+S22+S33+2*(S12+S13+S23) ) @ grid-cell center

      DO j=1-OLy,sNy+OLy-1
       DO i=1-OLx,sNx+OLx-1
         locVisc =
     &    smag3D_hLsC(i,j,bi,bj)*tmpFac*SQRT(
     &            S66( i , j , n )
     &   +  0.5*( S12( i ,j+1, n )+S12(i+1,j+1, n )
     &           +S12( i , j , n )+S12(i+1, j , n ) )
     &   +  0.5*( S13( i , j , n )+S13(i+1, j , n )
     &           +S13( i , j ,n+1)+S13(i+1, j ,n+1) )
     &   +  0.5*( S23( i , j , n )+S23( i ,j+1, n )
     &           +S23( i , j ,n+1)+S23( i ,j+1,n+1) )
     &                                      )
         viscAh3d_00(i,j,kl) = locVisc*smag3D_coeff
#ifdef ALLOW_SMAG_3D_DIFFUSIVITY
         smag3D_diffK(i,j,kl,bi,bj) = locVisc*smag3D_diffCoeff
#endif
       ENDDO
      ENDDO

C     viscAh3d_12 = sqrt( S11+S22+S33+2*(S12+S13+S23) ) @ grid-cell corner

      tmpFac = smag3D_coeff*tmpFac
      DO j=2-OLy,sNy+OLy
       DO i=2-OLx,sNx+OLx
         viscAh3d_12(i,j,kl) =
     &    smag3D_hLsZ(i,j,bi,bj)*tmpFac*SQRT(
     &     0.25*( S66(i-1, j , n )+S66( i , j , n )
     &           +S66(i-1,j-1, n )+S66( i ,j-1, n ) )
     &   + 2.0 *  S12( i , j , n )
     &   + 0.5 *( S13( i ,j-1, n )+S13( i , j , n )
     &           +S13( i ,j-1,n+1)+S13( i , j ,n+1) )
     &   + 0.5 *( S23(i-1, j , n )+S23( i , j , n )
     &           +S23(i-1, j ,n+1)+S23( i , j ,n+1) )
     &                                      )
       ENDDO
      ENDDO

C--  ------------------------------------------------------------------
C--  calculate  next level (k+1) viscosity coeff (uz,vz)
C--  ------------------------------------------------------------------

      IF ( k.EQ.1 ) THEN
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           viscAh3d_13(i,j,k) = 0. _d 0
           viscAh3d_23(i,j,k) = 0. _d 0
         ENDDO
        ENDDO
      ENDIF

C     Next level k+1 --> n=2
      kl = k+1
      n = 2
      tmpFac = smag3D_coeff*twoRL*SQRT(twoRL)*drC(kl)**twoThird

C     viscAh3d_13 = sqrt( S11+S22+S33+2*(S12+S13+S23) ) @ above uVel

      DO j=1-OLy,sNy+OLy-1
       DO i=2-OLx,sNx+OLx
         viscAh3d_13(i,j,kl) =
     &    smag3D_hLsW(i,j,bi,bj)*tmpFac*SQRT(
     &     0.25*( S66(i-1, j ,n-1)+S66( i , j ,n-1)
     &           +S66(i-1, j , n )+S66( i , j , n ) )
     &   + 0.5 *( S12( i , j ,n-1)+S12( i ,j+1,n-1)
     &           +S12( i , j , n )+S12( i ,j+1, n ) )
     &   + 2.0 *  S13( i , j , n )
     &   + 0.5 *( S23(i-1,j+1, n )+S23( i ,j+1, n )
     &           +S23(i-1, j , n )+S23( i , j , n ) )
     &                                      )
       ENDDO
      ENDDO

C     viscAh3d_23 = sqrt( S11+S22+S33+2*(S12+S13+S23) ) @ above vVel

      DO j=2-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx-1
         viscAh3d_23(i,j,kl) =
     &    smag3D_hLsS(i,j,bi,bj)*tmpFac*SQRT(
     &     0.25*( S66( i ,j-1,n-1)+S66( i , j ,n-1)
     &           +S66( i ,j-1, n )+S66( i , j , n ) )
     &   + 0.5 *( S12( i , j ,n-1)+S12(i+1, j ,n-1)
     &           +S12( i , j , n )+S12(i+1, j , n ) )
     &   + 0.5 *( S13( i , j , n )+S13(i+1, j , n )
     &           +S13( i ,j-1, n )+S13(i+1,j-1, n ) )
     &   + 2.0 *  S23( i , j , n )
     &                                      )
       ENDDO
      ENDDO

#ifdef ALLOW_DIAGNOSTICS
c     IF ( useDiagnostics.AND. k.EQ.Nr ) THEN
c       CALL DIAGNOSTICS_FILL( viscAh3d_00, 'Smag3D_C',
c    &                         0, Nr, 2, bi, bj, myThid )
c     ENDIF
#endif

#endif /* ALLOW_SMAG_3D */
      RETURN
      END
