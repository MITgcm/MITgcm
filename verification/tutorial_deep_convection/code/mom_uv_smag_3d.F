C $Header: /u/gcmpack/MITgcm/verification/tutorial_deep_convection/code/Attic/mom_uv_smag_3d.F,v 1.2 2013/10/01 14:38:56 jmc Exp $
C $Name:  $

#include "MOM_COMMON_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: MOM_UV_SMAG_3D

C     !INTERFACE:
      SUBROUTINE MOM_UV_SMAG_3D(
     I        str11, str22, str12, str13, str23,
     I        viscAh3d_00, viscAh3d_12,
     I        viscAh3d_13, viscAh3d_23,
c    I        hFacZ,
     O        uDissip, vDissip,
     I        k, bi, bj, myThid )

C     !DESCRIPTION:

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"
#include "PARAMS.h"

C     !INPUT PARAMETERS:
      _RL str11(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL str22(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL str12(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL str13(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+1)
      _RL str23(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+1)
c     _RS hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL viscAh3d_00(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL viscAh3d_12(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL viscAh3d_13(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+1)
      _RL viscAh3d_23(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+1)
      INTEGER k, bi, bj
      INTEGER myThid

C     !OUTPUT PARAMETERS:
      _RL uDissip(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vDissip(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
CEOP

C     !LOCAL VARIABLES:
      INTEGER i,j
      INTEGER km1, kp1
      _RL maskM1, maskP1

      km1 = MAX( 1,k-1)
      kp1 = MIN(Nr,k+1)
      maskM1 = 1.
      maskP1 = 1.
      IF ( k.LE.1 )  maskM1 = 0.
      IF ( k.GE.Nr ) maskP1 = 0.

C-    Initialise output arrays:
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
         uDissip(i,j) = 0. _d 0
         vDissip(i,j) = 0. _d 0
       ENDDO
      ENDDO

C     - Laplacian and bi-harmonic terms
c     IF (harmonic) THEN

C- note: free-slip @ bottom is commented out

C-    use simple (momentum conserving) scaling
C      (but not conserving angular momentum)

       DO j=2-OLy,sNy+OLy-1
        DO i=2-OLx,sNx+OLx-1
         uDissip(i,j) = recip_rAw(i,j,bi,bj)*(
     &     ( viscAh3d_00( i , j , k )*str11( i , j , k )
     &         *dyF( i , j ,bi,bj)
     &      -viscAh3d_00(i-1, j , k )*str11(i-1, j , k )
     &         *dyF(i-1, j ,bi,bj)
     &     )
     &    +( viscAh3d_12( i ,j+1, k )*str12( i ,j+1, k )
     &         *dxV( i ,j+1,bi,bj)
     &      -viscAh3d_12( i , j , k )*str12( i , j , k )
     &         *dxV( i , j ,bi,bj)
     &     )                                 )
     &                + recip_drF( k )
     &    *( viscAh3d_13( i , j ,k+1)*str13( i , j ,k+1)
c    &         *maskW(i,j,kp1,bi,bj)*maskP1
     &      -viscAh3d_13( i , j , k )*str13( i , j , k )
c    &         *maskW(i,j,km1,bi,bj)*maskM1
     &     )*rkSign*recip_hFacW(i,j,k,bi,bj)
        ENDDO
       ENDDO

       DO j=2-OLy,sNy+OLy-1
        DO i=2-OLx,sNx+OLx-1
         vDissip(i,j) = recip_rAs(i,j,bi,bj)*(
     &     ( viscAh3d_12(i+1, j , k )*str12(i+1, j ,k)
     &         *dyU(i+1, j ,bi,bj)
     &      -viscAh3d_12( i , j , k )*str12( i , j ,k)
     &         *dyU( i , j ,bi,bj)
     &     )
     &    +( viscAh3d_00( i , j , k )*str22( i , j ,k)
     &         *dxF( i , j ,bi,bj)
     &      -viscAh3d_00( i ,j-1, k )*str22( i ,j-1,k)
     &         *dxF( i ,j-1,bi,bj)
     &     )                                )
     &                + recip_drF( k )
     &    *( viscAh3d_23( i , j ,k+1)*str23( i , j ,k+1)
c    &         *maskS(i,j,kp1,bi,bj)*maskP1
     &      -viscAh3d_23( i , j , k )*str23( i , j , k )
c    &         *maskS(i,j,km1,bi,bj)*maskM1
     &     )*rkSign*recip_hFacS(i,j,k,bi,bj)

        ENDDO
       ENDDO

c     ENDIF

c     IF (biharmonic) THEN
c      STOP 'MOM_UV_SMAG_3D: BIHARMONIC NOT ALLOWED WITH SMAG_3D'
c     ENDIF

      RETURN
      END
