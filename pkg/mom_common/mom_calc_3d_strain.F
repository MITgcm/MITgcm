C $Header: /u/gcmpack/MITgcm/pkg/mom_common/mom_calc_3d_strain.F,v 1.2 2013/11/06 00:37:11 jmc Exp $
C $Name:  $

#include "MOM_COMMON_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: MOM_CALC_3D_STRAIN

C     !INTERFACE:
      SUBROUTINE MOM_CALC_3D_STRAIN(
     O        str11, str22, str33, str12, str13, str23,
     I        bi, bj, myThid )

C     !DESCRIPTION:
C     Calculates the strain tensor of the 3-D flow field

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"

C     !INPUT PARAMETERS:
C     bi, bj      :: tile indices
C     myThid      :: my Thread Id number
      INTEGER bi, bj
      INTEGER myThid

C     !OUTPUT PARAMETERS:
C     str11       :: strain component Vxx @ grid-cell center
C     str22       :: strain component Vyy @ grid-cell center
C     str33       :: strain component Vzz @ grid-cell center
C     str12       :: strain component Vxy @ grid-cell corner
C     str13       :: strain component Vxz @ above uVel
C     str23       :: strain component Vyz @ above vVel
      _RL str11(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL str22(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL str33(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL str12(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL str13(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+1)
      _RL str23(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+1)

#ifdef ALLOW_SMAG_3D
C     !LOCAL VARIABLES:
C      i, j, k    :: loop indices
      INTEGER i, j, k
      INTEGER kp1
      _RL maskp1
      LOGICAL freeSlip3d
CEOP
      freeSlip3d = .NOT.( no_slip_sides .AND. no_slip_bottom )

      DO k=1,Nr
       kp1 = MIN(k+1,Nr)
       maskp1 = oneRL
       IF ( k.EQ.Nr ) maskp1 = zeroRL

C-    Fills up array edges:
       i = sNx+OLx
       DO j=1-OLy,sNy+OLy
         str11(i,j,k) = 0. _d 0
       ENDDO
       j = sNy+OLy
       DO i=1-OLx,sNx+OLx
         str22(i,j,k) = 0. _d 0
       ENDDO
       i = 1-OLx
       DO j=1-OLy,sNy+OLy
         str12(i,j,k) = 0. _d 0
         str13(i,j,k) = 0. _d 0
       ENDDO
       j = 1-OLy
       DO i=1-OLx,sNx+OLx
         str12(i,j,k) = 0. _d 0
         str23(i,j,k) = 0. _d 0
       ENDDO

C     str11 = u_x
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx-1
          str11(i,j,k) = recip_dxF(i,j,bi,bj)
     &           *( uVel(i+1, j , k ,bi,bj)-uVel( i , j , k ,bi,bj) )
        ENDDO
       ENDDO

C     str22 = v_y
       DO j=1-OLy,sNy+OLy-1
        DO i=1-OLx,sNx+OLx
          str22(i,j,k) = recip_dyF(i,j,bi,bj)
     &           *( vVel( i ,j+1, k ,bi,bj)-vVel( i , j , k ,bi,bj) )
        ENDDO
       ENDDO

C     str33 = w_z
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
          str33(i,j,k) = recip_drF(k)*rkSign
     &    *( maskp1*wVel( i , j ,kp1,bi,bj)-wVel( i , j , k ,bi,bj) )
        ENDDO
       ENDDO

C     str12 = ( u_y + v_x )/2
       DO j=2-OLy,sNy+OLy
        DO i=2-OLx,sNx+OLx
          str12(i,j,k) = halfRL*(
     &      recip_dyU(i,j,bi,bj)
     &           *( uVel( i , j , k ,bi,bj)-uVel( i ,j-1, k ,bi,bj) )
     &     +recip_dxV(i,j,bi,bj)
     &           *( vVel( i , j , k ,bi,bj)-vVel(i-1, j , k ,bi,bj) )
     &                          )
        ENDDO
       ENDDO

C     str13 & str23 special case: k=1
       IF ( k.EQ.1 .AND. freeSlip3d ) THEN
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          str13(i,j,k) = 0. _d 0
          str23(i,j,k) = 0. _d 0
         ENDDO
        ENDDO
       ELSEIF ( k.EQ.1 ) THEN
C--    should put surface wind-stress if z-coords; but right in p-coords:
        DO j=1-OLy,sNy+OLy
         DO i=2-OLx,sNx+OLx
          str13(i,j,k) = halfRL*(
     &      recip_drC(k)*rkSign
     &           *( uVel( i , j , k ,bi,bj)*twoRL )
     &     +recip_dxC(i,j,bi,bj)
     &           *( wVel( i , j , k ,bi,bj)-wVel(i-1, j , k ,bi,bj) )
     &                          )
         ENDDO
        ENDDO
        DO j=2-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          str23(i,j,k) = halfRL*(
     &      recip_drC(k)*rkSign
     &           *( vVel( i , j , k ,bi,bj)*twoRL )
     &     +recip_dyC(i,j,bi,bj)
     &           *( wVel( i , j , k ,bi,bj)-wVel( i ,j-1, k ,bi,bj) )
     &                          )
         ENDDO
        ENDDO
       ELSE
C     str13 = ( u_z + w_x )/2
        DO j=1-OLy,sNy+OLy
         DO i=2-OLx,sNx+OLx
          str13(i,j,k) = halfRL*(
     &      recip_drC(k)*rkSign
     &           *( uVel( i , j , k ,bi,bj)-uVel( i , j ,k-1 ,bi,bj) )
     &     +recip_dxC(i,j,bi,bj)
     &           *( wVel( i , j , k ,bi,bj)-wVel(i-1, j , k ,bi,bj) )
     &                          )
         ENDDO
        ENDDO
C     str23 = ( v_z + w_y )/2
        DO j=2-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          str23(i,j,k) = halfRL*(
     &      recip_drC(k)*rkSign
     &           *( vVel( i , j , k ,bi,bj)-vVel( i , j ,k-1,bi,bj) )
     &     +recip_dyC(i,j,bi,bj)
     &           *( wVel( i , j , k ,bi,bj)-wVel( i ,j-1, k ,bi,bj) )
     &                          )
         ENDDO
        ENDDO
       ENDIF

       IF ( freeSlip3d ) THEN
        DO j=2-OLy,sNy+OLy
         DO i=2-OLx,sNx+OLx
           str12(i,j,k) = str12(i,j,k)
     &                  *maskW(i,j-1,k,bi,bj)*maskW(i,j,k,bi,bj)
         ENDDO
        ENDDO
        IF ( k.GE.2 ) THEN
         DO j=1-OLy,sNy+OLy
          DO i=2-OLx,sNx+OLx
           str13(i,j,k) = str13(i,j,k)
     &                  *maskW(i,j,k-1,bi,bj)*maskW(i,j,k,bi,bj)
          ENDDO
         ENDDO
         DO j=2-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           str23(i,j,k) = str23(i,j,k)
     &                  *maskS(i,j,k-1,bi,bj)*maskS(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDIF
       ENDIF

C--   end k loop
      ENDDO

C--   fill-up strain tensor component at the very bottom (k=Nr+1)
      k = Nr+1

      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
         str13(i,j,k) = 0. _d 0
         str23(i,j,k) = 0. _d 0
       ENDDO
      ENDDO

      IF ( .NOT.freeSlip3d ) THEN

C     str13 = ( u_z + w_x )/2
       DO j=1-OLy,sNy+OLy
        DO i=2-OLx,sNx+OLx
          str13(i,j,k) =
     &         recip_drF(Nr)*rkSign
c    &      recip_drC(k)*rkSign
     &            *( 0. _d 0 - uVel( i , j ,k-1 ,bi,bj) )
        ENDDO
       ENDDO

C     str23 = ( v_z + w_y )/2
       DO j=2-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
        str23(i,j,k) =
     &         recip_drF(Nr)*rkSign
c    &      recip_drC(k)*rkSign
     &            *( 0. _d 0 - vVel( i , j ,k-1,bi,bj) )
        ENDDO
       ENDDO

      ENDIF

C     Special stuff for Cubed Sphere
c     IF (useCubedSphereExchange) THEN
c      STOP 'S/R MOM_CALC_3D_STRAIN: should not be used on the cube!'
c     ENDIF

#endif /* ALLOW_SMAG_3D */
      RETURN
      END
