C $Header: /u/gcmpack/MITgcm/pkg/mom_fluxform/mom_u_adv_vu.F,v 1.5 2007/10/28 21:38:21 jmc Exp $
C $Name:  $

#include "MOM_FLUXFORM_OPTIONS.h"

CBOP
C !ROUTINE: MOM_U_ADV_VU

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_U_ADV_VU(
     I        bi,bj,k,
     I        vTrans, uFld,
     O        AdvectFluxVU,
     I        myThid)

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
C  bi,bj                :: tile indices
C  k                    :: vertical level
C  vTrans               :: meridional transport
C  uFld                 :: zonal flow
C  myThid               :: thread number
      INTEGER bi,bj,k
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

      DO j=1-Oly+1,sNy+Oly
       DO i=1-Olx+1,sNx+Olx
        AdvectFluxVU(i,j) =
     &  0.25*( vTrans(i,j) + vTrans(i-1,j) )
#ifdef MOM_BOUNDARY_CONSERVE
     &      *(   uFld(i,j)*_maskW(i,j-1,k,bi,bj)
     &         + uFld(i,j-1)*_maskW(i,j,k,bi,bj) )
#else
     &      *( uFld(i,j) + uFld(i,j-1) )
#endif
#ifdef OLD_ADV_BCS
     &      *_maskW(i,j,k,bi,bj)
     &      *_maskW(i,j-1,k,bi,bj)
#endif /* OLD_ADV_BCS */
       ENDDO
      ENDDO

      RETURN
      END
