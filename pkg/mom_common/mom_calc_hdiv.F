#include "MOM_COMMON_OPTIONS.h"

      SUBROUTINE MOM_CALC_HDIV(
     I        bi,bj,k, hDivScheme,
     I        uFld, vFld,
     O        hDiv,
     I        myThid )
      IMPLICIT NONE
C     *==========================================================*
C     | S/R MOM_CALC_HDIV                                        |
C     *==========================================================*
C     *==========================================================*

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
C     == Routine arguments ==
C     myThid  :: my Thread Id number
      INTEGER bi,bj,k,hDivScheme
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL hDiv(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER i,j

#ifdef ALLOW_AUTODIFF
C     Initialise (again) hDiv (in case it got dropped in calling S/R):
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        hDiv(i,j)=0. _d 0
       ENDDO
      ENDDO
#endif
      IF (hDivScheme.EQ.1) THEN
       DO j=1-OLy,sNy+OLy-1
        DO i=1-OLx,sNx+OLx-1
C       This discretization is the straight forward horizontal divergence
C       that only considers the horizontal grid variations.
         hDiv(i,j)=(
     &       uFld(i+1, j )*dyG(i+1, j ,bi,bj)
     &      -uFld( i , j )*dyG( i , j ,bi,bj)
     &      +vFld( i ,j+1)*dxG( i ,j+1,bi,bj)
     &      -vFld( i , j )*dxG( i , j ,bi,bj)
     &             )*recip_rA(i,j,bi,bj)*recip_deepFacC(k)
#ifdef ALLOW_OBCS
     &              *maskInC(i,j,bi,bj)
#endif
        ENDDO
       ENDDO

      ELSEIF (hDivScheme.EQ.2) THEN
       DO j=1-OLy,sNy+OLy-1
        DO i=1-OLx,sNx+OLx-1
C       This discretization takes into account the fractional areas
C       due to the lopping. Whether we should do this is not clear!
         hDiv(i,j)=
     &    ( ( uFld(i+1, j )*dyG(i+1, j ,bi,bj)*hFacW(i+1, j ,k,bi,bj)
     &       -uFld( i , j )*dyG( i , j ,bi,bj)*hFacW( i , j ,k,bi,bj) )
     &     +( vFld( i ,j+1)*dxG( i ,j+1,bi,bj)*hFacS( i ,j+1,k,bi,bj)
     &       -vFld( i , j )*dxG( i , j ,bi,bj)*hFacS( i , j ,k,bi,bj) )
     &    )*recip_rA(i,j,bi,bj)*recip_deepFacC(k)
     &     *_recip_hFacC(i,j,k,bi,bj)
#ifdef ALLOW_OBCS
     &     *maskInC(i,j,bi,bj)
#endif
        ENDDO
       ENDDO

      ELSE
       STOP 'S/R MOM_CALC_HDIV: We should never reach this point!'
      ENDIF

      RETURN
      END
