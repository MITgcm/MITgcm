C $Header: /u/gcmpack/MITgcm/pkg/mom_common/mom_calc_strain.F,v 1.4 2005/10/12 00:58:29 jmc Exp $
C $Name:  $

#include "MOM_COMMON_OPTIONS.h"

CBOP
C !ROUTINE: MOM_CALC_STRAIN

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_CALC_STRAIN( 
     I        bi,bj,k,
     I        uFld, vFld, hFacZ,
     O        strain,
     I        myThid)

C !DESCRIPTION:
C Calculates the strain of the horizontal flow field (at vorticity points):
C \begin{equation*}
C D_S = \frac{\Delta y_u}{\Delta x_v} \delta_i \frac{v}{\Delta y_c}
C     + \frac{\Delta x_v}{\Delta y_u} \delta_j \frac{u}{\Delta x_c}
C \end{equation*}
C assuming free-slip boundaries.

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
C  hFacZ                :: open-water thickness at vorticity points
C  myThid               :: thread number
      INTEGER bi,bj,k
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  strain               :: strain of horizontal flow
      _RL strain(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
      INTEGER i,j
CEOP

      DO J=2-Oly,sNy+Oly
       DO I=2-Olx,sNx+Olx

C       Strain of horizontal flow field (ignoring lopping factors)
        strain(I,J)=
     &    ( dyc( i , j ,bi,bj)*vFld( i , j )
     &     -dyc(i-1, j ,bi,bj)*vFld(i-1, j )
     &     +dxc( i , j ,bi,bj)*uFld( i , j )
     &     -dxc( i ,j-1,bi,bj)*uFld( i ,j-1) )*recip_rAz(i,j,bi,bj)
c    &    dyu(I,J,bi,bj)*recip_dxv(I,J,bi,bj)*(
c    &              vFld( I , J )*recip_dyc( I , J ,bi,bj)
c    &             -vFld(I-1, J )*recip_dyc(I-1, J ,bi,bj) )
c    &   +dxv(I,J,bi,bj)*recip_dyu(I,J,bi,bj)*(
c    &             +uFld( I , J )*recip_dxc( I , J ,bi,bj)
c    &             -uFld( I ,J-1)*recip_dxc( I ,J-1,bi,bj) )

C       Set strain to zero on boundaries (free-slip)
C       mask is now applied afterwards, outside this S/R.
c       IF (hFacZ(i,j).EQ.0.) THEN
c        strain(I,J)=0.
c       ENDIF

       ENDDO
      ENDDO
        
C     Special stuff for Cubed Sphere
      IF (useCubedSphereExchange) THEN
c      STOP 'S/R MOM_CALC_STRAIN: We should not use strain on the cube!'
      ENDIF

      RETURN
      END
