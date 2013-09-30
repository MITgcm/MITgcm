C $Header: /u/gcmpack/MITgcm/verification/tutorial_deep_convection/code/Attic/mom_w_smag_3d.F,v 1.1 2013/09/30 18:19:42 jmc Exp $
C $Name:  $

#include "MOM_COMMON_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: MOM_W_SMAG_3D

C     !INTERFACE:
      SUBROUTINE MOM_W_SMAG_3D(
     I        str13, str23, str33,
     I        viscAh3d_00, viscAh3d_13, viscAh3d_23,
     I        rThickC_W, rThickC_S, rThickC_C, recip_rThickC,
     O        wDissip,
     I        k, bi, bj, myThid )

C     !DESCRIPTION:

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     !INPUT PARAMETERS:
C     rThickC_W     :: thickness (in r-units) of W-Cell at Western Edge
C     rThickC_S     :: thickness (in r-units) of W-Cell at Southern Edge
C     rThickC_C     :: thickness (in r-units) of W-Cell (centered on W pt)
C     recip_rThickC :: reciprol thickness of W-Cell (centered on W-point)
      _RL str13(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+1)
      _RL str23(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+1)
      _RL str33(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL viscAh3d_00(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL viscAh3d_13(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+1)
      _RL viscAh3d_23(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+1)
      _RL rThickC_W    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rThickC_S    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rThickC_C    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL recip_rThickC(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER k, bi, bj
      INTEGER myThid

C     !OUTPUT PARAMETERS:
      _RL wDissip(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
CEOP

C     !LOCAL VARIABLES:
      INTEGER i,j
      INTEGER km1
      _RL maskM1

      km1 = MAX(1,k-1)
      maskM1 = 1.
      IF ( k.LE.1 ) maskM1 = 0.

C     - Laplacian and bi-harmonic terms
c     IF (harmonic) THEN

C- note: free-slip @ bottom is commented out

C-    use simple (momentum conserving) scaling
C      (but not conserving angular momentum)

       DO j=1-OLy,sNy+OLy-1
        DO i=1-OLx,sNx+OLx-1
c        wDissip(i,j) = recip_rThickC(i,j)*
         wDissip(i,j) =
     &                  recip_rA(i,j,bi,bj)*(
     &     ( viscAh3d_13(i+1, j , k )*str13(i+1, j ,k)
c    &         *dyG(i+1, j ,bi,bj)*rThickC_W(i+1, j )
     &         *dyG(i+1, j ,bi,bj)
     &      -viscAh3d_13( i , j , k )*str13( i , j ,k)
c    &         *dyG( i , j ,bi,bj)*rThickC_W( i , j )
     &         *dyG( i , j ,bi,bj)
     &     )
     &    +( viscAh3d_23( i ,j+1, k )*str23( i ,j+1,k)
c    &         *dxG( i ,j+1,bi,bj)*rThickC_S( i ,j+1)
     &         *dxG( i ,j+1,bi,bj)
     &      -viscAh3d_23( i , j , k )*str23( i , j ,k)
c    &         *dxG( i , j ,bi,bj)*rThickC_S( i , j )
     &         *dxG( i , j ,bi,bj)
     &     )                                 )
     &                + recip_rThickC(i,j)
     &    *( viscAh3d_00( i , j , k )*str33( i , j , k )
     &      -viscAh3d_00( i , j ,km1)*str33( i , j ,km1)*maskM1
     &     )*rkSign
        ENDDO
       ENDDO

c     ENDIF

c     IF (biharmonic) THEN
c      STOP 'MOM_UV_SMAG_3D: BIHARMONIC NOT ALLOWED WITH SMAG_3D'
c     ENDIF

      RETURN
      END
