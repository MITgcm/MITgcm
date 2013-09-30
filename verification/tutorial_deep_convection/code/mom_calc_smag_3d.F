C $Header: /u/gcmpack/MITgcm/verification/tutorial_deep_convection/code/Attic/mom_calc_smag_3d.F,v 1.1 2013/09/30 18:19:41 jmc Exp $
C $Name:  $

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

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
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
C     viscAh3d_00 :: strain magnitude @ grid-cell center
C     viscAh3d_12 :: strain magnitude @ grid-cell corner
C     viscAh3d_13 :: strain magnitude @ above uVel
C     viscAh3d_23 :: strain magnitude @ above vVel
      _RL viscAh3d_00(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL viscAh3d_12(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL viscAh3d_13(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+1)
      _RL viscAh3d_23(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+1)
CEOP

C     !LOCAL VARIABLES:
      INTEGER i, j
      INTEGER kp, n
      _RL twoThird, tmpFac
      _RL S11(1-OLx:sNx+OLx,1-OLy:sNy+OLy,3)
      _RL S22(1-OLx:sNx+OLx,1-OLy:sNy+OLy,3)
      _RL S33(1-OLx:sNx+OLx,1-OLy:sNy+OLy,3)
      _RL S12(1-OLx:sNx+OLx,1-OLy:sNy+OLy,3)
      _RL S13(1-OLx:sNx+OLx,1-OLy:sNy+OLy,3)
      _RL S23(1-OLx:sNx+OLx,1-OLy:sNy+OLy,3)

      n = 1
      DO kp=k-1,k+1
        IF ( kp.GE.1 .AND. kp.LE.Nr ) THEN
          DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
              S11(i,j,n) = str11(i,j,kp)**2
              S22(i,j,n) = str22(i,j,kp)**2
              S33(i,j,n) = str33(i,j,kp)**2
              S12(i,j,n) = str12(i,j,kp)**2
              S13(i,j,n) = str13(i,j,kp)**2
              S23(i,j,n) = str23(i,j,kp)**2
            ENDDO
          ENDDO
        ELSEIF ( kp.GE.1 ) THEN
          DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
              S11(i,j,n) = 0. _d 0
              S22(i,j,n) = 0. _d 0
              S33(i,j,n) = 0. _d 0
              S12(i,j,n) = 0. _d 0
              S13(i,j,n) = str13(i,j,kp)**2
              S23(i,j,n) = str23(i,j,kp)**2
            ENDDO
          ENDDO
        ELSE
          DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
              S11(i,j,n) = 0. _d 0
              S22(i,j,n) = 0. _d 0
              S33(i,j,n) = 0. _d 0
              S12(i,j,n) = 0. _d 0
              S13(i,j,n) = 0. _d 0
              S23(i,j,n) = 0. _d 0
            ENDDO
          ENDDO
        ENDIF
        n=n+1
      ENDDO

C--  ------------------------------------------------------------------
C--  calculate current level Smag viscosity coeff
C--  ------------------------------------------------------------------

C     Current level k --> n=2
      n = 2

C     viscAh3d_00 = sqrt( S11+S22+S33+2*(S12+S13+S23) ) @ grid-cell center

      twoThird = 2. _d 0 / 3. _d 0
      tmpFac = smag3D_coeff * drF(k)**twoThird * SQRT(twoRL)
      DO j=1-OLy,sNy+OLy-1
       DO i=1-OLx,sNx+OLx-1
c         viscAh3d_00( i , j , k ) = tmpFac*smag3D_hLsC(i,j,bi,bj)*SQRT(
c          *sqrt(
          viscAh3d_00( i , j , k ) = sqrt(
     &            S11( i , j , n )
     &   +        S22( i , j , n )
     &   +        S33( i , j , n )
     &   +  0.5*( S12( i ,j+1, n )+S12(i+1,j+1, n )
     &           +S12( i , j , n )+S12(i+1, j , n ) )
     &   +  0.5*( S13( i , j , n )+S13(i+1, j , n )
     &           +S13( i , j ,n+1)+S13(i+1, j ,n+1) )
     &   +  0.5*( S23( i , j , n )+S23( i ,j+1, n )
     &           +S23( i , j ,n+1)+S23( i ,j+1,n+1) )
     &                                   )
       ENDDO
      ENDDO

C     viscAh3d_12 = sqrt( S11+S22+S33+2*(S12+S13+S23) ) @ grid-cell corner

      DO j=2-OLy,sNy+OLy
       DO i=2-OLx,sNx+OLx
          viscAh3d_12( i , j , k ) = sqrt(
     &     0.25*( S11(i-1, j , n )+S11( i , j , n )
     &           +S11(i-1,j-1, n )+S11( i ,j-1, n ) )
     &   + 0.25*( S22(i-1, j , n )+S22( i , j , n )
     &           +S22(i-1,j-1, n )+S22( i ,j-1, n ) )
     &   + 0.25*( S33(i-1, j , n )+S33( i , j , n )
     &           +S33(i-1,j-1, n )+S33( i ,j-1, n ) )
     &   + 2.0 *  S12( i , j , n )
     &   + 0.5 *( S13( i ,j-1, n )+S13( i , j , n )
     &           +S13( i ,j-1,n+1)+S13( i , j ,n+1) )
     &   + 0.5 *( S23(i-1, j , n )+S23( i , j , n )
     &           +S23(i-1, j ,n+1)+S23( i , j ,n+1) )
     &                                   )
       ENDDO
      ENDDO

C--  ------------------------------------------------------------------
C--  calculate  next level (k+1) viscosity coeff (uz,vz)
C--  ------------------------------------------------------------------

C     Next level k+1 --> n=3
      n = 3

C     viscAh3d_13 = sqrt( S11+S22+S33+2*(S12+S13+S23) ) @ above uVel

      DO j=1-OLy,sNy+OLy-1
       DO i=2-OLx,sNx+OLx
          viscAh3d_13( i , j ,k+1) = sqrt(
     &     0.25*( S11(i-1, j ,n-1)+S11( i , j ,n-1)
     &           +S11(i-1, j , n )+S11( i , j , n ) )
     &   + 0.25*( S22(i-1, j ,n-1)+S22( i , j ,n-1)
     &           +S22(i-1, j , n )+S22( i , j , n ) )
     &   + 0.25*( S33(i-1, j ,n-1)+S33( i , j ,n-1)
     &           +S33(i-1, j , n )+S33( i , j , n ) )
     &   + 0.5 *( S12( i , j ,n-1)+S12( i ,j+1,n-1)
     &           +S12( i , j , n )+S12( i ,j+1, n ) )
     &   + 2.0 *  S13( i , j , n )
     &   + 0.5 *( S23(i-1,j+1, n )+S23( i ,j+1, n )
     &           +S23(i-1, j , n )+S23( i , j , n ) )
     &                                   )
       ENDDO
      ENDDO

C     viscAh3d_23 = sqrt( S11+S22+S33+2*(S12+S13+S23) ) @ above vVel

      DO j=2-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx-1
         viscAh3d_23( i , j ,k+1) = sqrt(
     &     0.25*( S11( i ,j-1,n-1)+S11( i , j ,n-1)
     &           +S11( i ,j-1, n )+S11( i , j , n ) )
     &   + 0.25*( S22( i ,j-1,n-1)+S22( i , j ,n-1)
     &           +S22( i ,j-1, n )+S22( i , j , n ) )
     &   + 0.25*( S33( i ,j-1,n-1)+S33( i , j ,n-1)
     &           +S33( i ,j-1, n )+S33( i , j , n ) )
     &   + 0.5 *( S12( i , j ,n-1)+S12(i+1, j ,n-1)
     &           +S12( i , j , n )+S12(i+1, j , n ) )
     &   + 0.5 *( S13( i , j , n )+S13(i+1, j , n )
     &           +S13( i ,j-1, n )+S13(i+1,j-1, n ) )
     &   + 2.0 *  S23( i , j , n )
     &                                  )
       ENDDO
      ENDDO

#ifdef ALLOW_DIAGNOSTICS
c     IF (useDiagnostics) THEN
c      CALL DIAGNOSTICS_FILL(viscAh_D,'VISCAHD ',k,1,2,bi,bj,myThid)
c     ENDIF
#endif

      RETURN
      END
