C $Header: /u/gcmpack/MITgcm/pkg/mom_vecinv/Attic/mom_calc_strain.F,v 1.3 2001/11/27 15:45:06 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

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
     &    dyu(I,J,bi,bj)*recip_dxv(I,J,bi,bj)*(
     &              vFld( I , J )*recip_dyc( I , J ,bi,bj)
     &             -vFld(I-1, J )*recip_dyc(I-1, J ,bi,bj) )
     &   +dxv(I,J,bi,bj)*recip_dyu(I,J,bi,bj)*(
     &             +uFld( I , J )*recip_dxc( I , J ,bi,bj)
     &             -uFld( I ,J-1)*recip_dxc( I ,J-1,bi,bj) )

C       Set strain to zero on boundaries (free-slip)
        IF (hFacZ(i,j).EQ.0.) THEN
         strain(I,J)=0.
        ENDIF

       ENDDO
      ENDDO
        
C     Special stuff for Cubed Sphere
      IF (useCubedSphereExchange) THEN
        I=1
        J=1
        strain(I,J)=0.
        I=sNx+1
        J=1
        strain(I,J)=0.
        I=1
        J=sNy+1
        strain(I,J)=0.
        I=sNx+1
        J=sNy+1
        strain(I,J)=0.
      ENDIF

      RETURN
      END
