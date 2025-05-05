#include "MOM_FLUXFORM_OPTIONS.h"

CBOP
C !ROUTINE: MOM_U_ADV_VU

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_U_ADV_VU(
     I        bi, bj, k,
     I        vTrans, uFld,
     O        AdvectFluxVU,
     I        myThid )

C !DESCRIPTION:
C Calculates the meridional advective flux of zonal momentum:
C \begin{equation*}
C F^y = \overline{V}^i \overline{u}^{j}
C \end{equation*}

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C !INPUT PARAMETERS: ===================================================
C  bi, bj               :: tile indices
C  k                    :: vertical level
C  vTrans               :: meridional transport
C  uFld                 :: zonal velocity
C  myThid               :: my Thread Id number
      INTEGER bi, bj, k
      _RL vTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  AdvectFluxVU         :: advective flux
      _RL AdvectFluxVU(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
      INTEGER i,j
CEOP

      IF ( selectMetricTerms.NE.3 ) THEN
        DO j=1-OLy+1,sNy+OLy
         DO i=1-OLx+1,sNx+OLx
          AdvectFluxVU(i,j) = 0.25 _d 0
     &        *( vTrans(i,j) + vTrans(i-1,j) )
#ifdef MOM_BOUNDARY_CONSERVE
     &        *(   uFld(i,j)*_maskW(i,j-1,k,bi,bj)
     &         + uFld(i,j-1)*_maskW(i,j,k,bi,bj) )
#else
     &        *( uFld(i,j) + uFld(i,j-1) )
#endif
#ifdef OLD_ADV_BCS
     &        *_maskW(i,j,k,bi,bj)
     &        *_maskW(i,j-1,k,bi,bj)
#endif /* OLD_ADV_BCS */
         ENDDO
        ENDDO
      ELSE
C-    Advect u*dxC (--> account for metric term u*v*tanPhi/R)
        DO j=1-OLy+1,sNy+OLy
         DO i=1-OLx+1,sNx+OLx
          AdvectFluxVU(i,j) = 0.25 _d 0
     &        *( vTrans(i,j) + vTrans(i-1,j) )
#ifdef MOM_BOUNDARY_CONSERVE
     &        *( uFld(i, j )*dxC(i, j, bi,bj)*_maskW(i,j-1,k,bi,bj)
     &         + uFld(i,j-1)*dxC(i,j-1,bi,bj)*_maskW(i, j, k,bi,bj) )
#else
     &        *( uFld(i, j )*dxC(i, j, bi,bj)
     &         + uFld(i,j-1)*dxC(i,j-1,bi,bj) )
#endif
         ENDDO
        ENDDO
      ENDIF

      RETURN
      END
