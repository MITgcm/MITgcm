C $Header: /u/gcmpack/MITgcm/pkg/mom_common/mom_calc_ke.F,v 1.6 2014/04/04 20:08:11 jmc Exp $
C $Name:  $

#include "MOM_COMMON_OPTIONS.h"

CBOP
C !ROUTINE: MOM_CALC_KE

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_CALC_KE(
     I        bi,bj,k,KEscheme,
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
C  KEscheme             :: spacial discretisation scheme for KE
C  uFld                 :: zonal flow
C  vFld                 :: meridional flow
C  KE                   :: Kinetic Energy
C  myThid               :: thread number
      INTEGER bi,bj,k
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER KEscheme
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  KE                   :: Kinetic energy
      _RL KE(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
      INTEGER i,j
CEOP

C       This defn of KE should not ever be used. Just to let you know.
C                            2       2
C                 1  /  ___I    ___J   \
C           KE = --- |   U    +  V     |
C                 2  \                 /
C
#ifdef ALLOW_AUTODIFF
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
         KE(i,j) = 0.
       ENDDO
      ENDDO
#endif

      IF (KEscheme.EQ.-1) THEN
       DO j=1-OLy,sNy+OLy-1
        DO i=1-OLx,sNx+OLx-1
         KE(i,j) = 0.125*(
     &             ( uFld(i,j)+uFld(i+1, j ) )**2
     &            +( vFld(i,j)+vFld( i ,j+1) )**2 )
        ENDDO
       ENDDO

      ELSEIF (KEscheme.EQ.0) THEN
C       This defn of KE should be used for the vector invariant equations.
C                       _____I  _____J
C                 1  /     2       2   \
C           KE = --- |   U    +  V     |
C                 2  \                 /
C
       DO j=1-OLy,sNy+OLy-1
        DO i=1-OLx,sNx+OLx-1
         KE(i,j) = 0.25*(
     &           ( uFld( i , j )*uFld( i , j )
     &            +uFld(i+1, j )*uFld(i+1, j ) )
     &         + ( vFld( i , j )*vFld( i , j )
     &            +vFld( i ,j+1)*vFld( i ,j+1) )
     &                  )
        ENDDO
       ENDDO

      ELSEIF (KEscheme.EQ.1) THEN
C       As above but including the area
       DO j=1-OLy,sNy+OLy-1
        DO i=1-OLx,sNx+OLx-1
         KE(i,j) = 0.25*(
     &           ( uFld(i, j )*uFld(i, j )*rAw(i ,j, bi,bj)
     &            +uFld(i+1,j)*uFld(i+1,j)*rAw(i+1,j,bi,bj) )
     &         + ( vFld(i, j )*vFld(i, j )*rAs(i ,j, bi,bj)
     &            +vFld(i,j+1)*vFld(i,j+1)*rAs(i,j+1,bi,bj) )
     &                  )*recip_rA(i,j,bi,bj)
        ENDDO
       ENDDO

      ELSEIF (KEscheme.EQ.2) THEN
C       As KEscheme=0 but including the lopping factors and should be used
C       for the conservative form of the momentum equations.
       DO j=1-OLy,sNy+OLy-1
        DO i=1-OLx,sNx+OLx-1
         KE(i,j) = 0.25*(
     &           ( uFld( i , j )*uFld( i , j )*_hFacW(i,j,k,bi,bj)
     &            +uFld(i+1, j )*uFld(i+1, j )*_hFacW(i+1,j,k,bi,bj) )
     &         + ( vFld( i , j )*vFld( i , j )*_hFacS(i,j,k,bi,bj)
     &            +vFld( i ,j+1)*vFld( i ,j+1)*_hFacS(i,j+1,k,bi,bj) )
     &            )*_recip_hFacC(i,j,k,bi,bj)
        ENDDO
       ENDDO

      ELSEIF (KEscheme.EQ.3) THEN
C       As above but including the area
       DO j=1-OLy,sNy+OLy-1
        DO i=1-OLx,sNx+OLx-1
         KE(i,j) = 0.25*(
     &           (
     &    uFld(i, j )*uFld(i, j )
     &        *_hFacW(i ,j, k,bi,bj)*rAw(i ,j, bi,bj)
     &   +uFld(i+1,j)*uFld(i+1,j)
     &        *_hFacW(i+1,j,k,bi,bj)*rAw(i+1,j,bi,bj)
     &           )
     &         + (
     &    vFld(i, j )*vFld(i, j )
     &        *_hFacS(i, j, k,bi,bj)*rAs(i ,j, bi,bj)
     &   +vFld(i,j+1)*vFld(i,j+1)
     &        *_hFacS(i,j+1,k,bi,bj)*rAs(i,j+1,bi,bj)
     &           )      )*_recip_hFacC(i,j,k,bi,bj)
     &                   * recip_rA(i,j,bi,bj)
        ENDDO
       ENDDO

      ELSE
       STOP 'S/R MOM_CALC_KE: We should never reach this point!'
      ENDIF

      RETURN
      END
