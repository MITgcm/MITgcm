C $Header: /u/gcmpack/MITgcm/pkg/mom_vecinv/Attic/mom_vi_calc_ke.F,v 1.2 2001/05/29 14:01:39 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE MOM_VI_CALC_KE(
     I        bi,bj,k,
     I        uFld, vFld,
     O        KE,
     I        myThid)
      IMPLICIT NONE
C
C     Calculate Kinetic Energy, KE, with units (m/s)^2
C       - assumes hydrostatic formulation only
C

C     == Global variables ==
#include "SIZE.h"
#include "GRID.h"

C     == Routine arguments ==
      INTEGER bi,bj,k
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL KE(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER I,J

      DO J=1-OLy,sNy+OLy-1
       DO I=1-OLx,sNx+OLx-1

C       This defn of KE should not ever be used. Just to let you know.
C                            2       2
C                 1  /  ___I    ___J   \
C           KE = --- |   U    +  V     |
C                 2  \                 /
C
c       KE(i,j) = 0.125*(
c    &      ( uFld(i,j)+uFld(i+1, j ) )**2
c    &     +( vFld(i,j)+vFld( i ,j+1) )**2 )

C       This defn of KE should be used for the vector invariant equations.
C                       _____I  _____J
C                 1  /     2       2   \
C           KE = --- |   U    +  V     |
C                 2  \                 /
C
        KE(i,j) = 0.25*(
     &       uFld( i , j )*uFld( i , j )
     &      +uFld(i+1, j )*uFld(i+1, j )
     &      +vFld( i , j )*vFld( i , j )
     &      +vFld( i ,j+1)*vFld( i ,j+1) )

C       As above but including the lopping factors and should be used
C       for the conservative form of the momentum equations.
c       KE(i,j) = 0.25*(
c    &       uFld( i , j )*uFld( i , j )*hFacW(i,j,k,bi,bj)
c    &      +uFld(i+1, j )*uFld(i+1, j )*hFacW(i+1,j,k,bi,bj)
c    &      +vFld( i , j )*vFld( i , j )*hFacS(i,j,k,bi,bj)
c    &      +vFld( i ,j+1)*vFld( i ,j+1)*hFacS(i,j+1,k,bi,bj)
c    &        )
c    &         *recip_hFacC(i,j,k,bi,bj)

       ENDDO
      ENDDO

      RETURN
      END
