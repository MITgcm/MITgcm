C $Header: /u/gcmpack/MITgcm/pkg/mom_fluxform/Attic/mom_calc_ke.F,v 1.3 2001/09/26 19:05:21 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C !ROUTINE: MOM_CALC_KE

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_CALC_KE(
     I        bi,bj,k,
     I        uFld, vFld,
     O        KE,
     I        myThid)

C !DESCRIPTION:
C Calculates the Kinetic energy of horizontal flow
C \begin{equation*}
C KE = \frac{1}{2} \left( h_w \overline{u^2}^i + h_s \overline{v^2}^j \right)
C \end{equation*}

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"

C !INPUT PARAMETERS: ===================================================
C  bi,bj                :: tile indices
C  k                    :: vertical level
C  uFld                 :: zonal flow
C  vFld                 :: meridional flow
C  myThid               :: thread number
      INTEGER bi,bj,k
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  KE                   :: Kinetic energy
      _RL KE(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
      INTEGER I,J
CEOP

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
c       KE(i,j) = 0.25*(
c    &       uFld( i , j )*uFld( i , j )
c    &      +uFld(i+1, j )*uFld(i+1, j )
c    &      +vFld( i , j )*vFld( i , j )
c    &      +vFld( i ,j+1)*vFld( i ,j+1) )

C       As above but including the lopping factors and should be used
C       for the conservative form of the momentum equations.
        KE(i,j) = 0.25*(
     &       uFld( i , j )*uFld( i , j )*hFacW(i,j,k,bi,bj)
     &      +uFld(i+1, j )*uFld(i+1, j )*hFacW(i+1,j,k,bi,bj)
     &      +vFld( i , j )*vFld( i , j )*hFacS(i,j,k,bi,bj)
     &      +vFld( i ,j+1)*vFld( i ,j+1)*hFacS(i,j+1,k,bi,bj)
     &        )
     &         *recip_hFacC(i,j,k,bi,bj)

       ENDDO
      ENDDO

      RETURN
      END
