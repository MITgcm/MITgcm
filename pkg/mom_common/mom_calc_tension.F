C $Header: /u/gcmpack/MITgcm/pkg/mom_common/mom_calc_tension.F,v 1.3 2004/07/07 14:10:42 adcroft Exp $
C $Name:  $

#include "MOM_COMMON_OPTIONS.h"

CBOP
C !ROUTINE: MOM_CALC_TENSION

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_CALC_TENSION( 
     I        bi,bj,k,
     I        uFld, vFld,
     O        tension,
     I        myThid)
C !DESCRIPTION:
C Calculates the tension of the horizontal flow field (at tracer points):
C \begin{equation*}
C D_T = \frac{\Delta y_f}{\Delta x_f} \delta_i \frac{u}{\Delta y_g}
C     - \frac{\Delta x_f}{\Delta y_f} \delta_j \frac{v}{\Delta x_g}
C \end{equation*}

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
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
C  tension              :: tension of horizontal flow
      _RL tension(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
      INTEGER i,j
CEOP

      DO j=1-Oly,sNy+Oly-1
       DO i=1-Olx,sNx+Olx-1

C Calculate tension of horizontal flow (ignoring lopping factors)
C *NOTE* that masking is implicit in the contents of the (u,v) fields.
        tension(i,j)= 
     &    ( dyg(i+1, j ,bi,bj)*uFld(i+1, j )
     &     -dyg( i , j ,bi,bj)*uFld( i , j )
     &     -dxg( i ,j+1,bi,bj)*vFld( i ,j+1)
     &     +dxg( i , j ,bi,bj)*vFld( i , j ) )*recip_rA(i,j,bi,bj)
c    &    (dyf(i,j,bi,bj)*recip_dxf(i,j,bi,bj))
c    &   *( uFld(i+1, j )*recip_dyg(i+1, j ,bi,bj)
c    &     -uFld( i , j )*recip_dyg( i , j ,bi,bj) )
c    &   -(dxf(i,j,bi,bj)*recip_dyf(i,j,bi,bj))
c    &   *( vFld( i ,j+1)*recip_dxg( i ,j+1,bi,bj)
c    &     -vFld( i , j )*recip_dxg( i , j ,bi,bj) )

       ENDDO
      ENDDO
        
      RETURN
      END
