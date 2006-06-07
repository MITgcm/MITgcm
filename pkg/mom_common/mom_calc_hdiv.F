C $Header: /u/gcmpack/MITgcm/pkg/mom_common/mom_calc_hdiv.F,v 1.3 2006/06/07 01:55:14 heimbach Exp $
C $Name:  $

#include "MOM_COMMON_OPTIONS.h"

      SUBROUTINE MOM_CALC_HDIV( 
     I        bi,bj,k, hDivScheme,
     I        uFld, vFld,
     O        hDiv,
     I        myThid)
      IMPLICIT NONE
C     /==========================================================\
C     | S/R MOM_CALC_HDIV                                        |
C     |==========================================================|
C     \==========================================================/

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
C     == Routine arguments ==
C     myThid - Instance number for this innvocation of CALC_MOM_RHS
      INTEGER bi,bj,k,hDivScheme
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL hDiv(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER i,j

      IF (hDivScheme.EQ.1) THEN
       DO J=1-Oly,sNy+Oly-1
        DO I=1-Olx,sNx+Olx-1
C       This discretization is the straight forward horizontal divergence
C       that only considers the horizontal grid variations.
         hDiv(I,J)=(
     &       uFld(i+1, j )*dyg(i+1, j ,bi,bj)
     &      -uFld( i , j )*dyg( i , j ,bi,bj)
     &      +vFld( i ,j+1)*dxg( i ,j+1,bi,bj)
     &      -vFld( i , j )*dxg( i , j ,bi,bj)
     &             )*recip_rA(I,J,bi,bj)
        ENDDO
       ENDDO

      ELSEIF (hDivScheme.EQ.2) THEN
       DO J=1-Oly,sNy+Oly-1
        DO I=1-Olx,sNx+Olx-1
C       This discretization takes into account the fractional areas
C       due to the lopping. Whether we should do this is not clear!
         hDiv(I,J)= 
     &    ( ( uFld(i+1, j )*dyg(i+1, j ,bi,bj)*hFacW(i+1, j ,K,bi,bj)
     &       -uFld( i , j )*dyg( i , j ,bi,bj)*hFacW( i , j ,K,bi,bj) )
     &     +( vFld( i ,j+1)*dxg( i ,j+1,bi,bj)*hFacS( i ,j+1,K,bi,bj)
     &       -vFld( i , j )*dxg( i , j ,bi,bj)*hFacS( i , j ,K,bi,bj) )
     &    )*recip_rA(I,J,bi,bj)
     &     *_recip_hFacC(i,j,k,bi,bj)
        ENDDO
       ENDDO

      ELSE
       STOP 'S/R MOM_CALC_HDIV: We should never reach this point!'
      ENDIF

      RETURN
      END
