C $Header: /u/gcmpack/MITgcm/pkg/mom_fluxform/mom_v_adv_uv.F,v 1.5 2007/10/28 21:38:21 jmc Exp $
C $Name:  $

#include "MOM_FLUXFORM_OPTIONS.h"

CBOP
C !ROUTINE: MOM_V_ADV_UV

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_V_ADV_UV(
     I        bi,bj,k,
     I        uTrans, vFld,
     O        AdvectFluxUV,
     I        myThid)

C !DESCRIPTION:
C Calculates the zonal advective flux of meridional momentum:
C \begin{equation*}
C F^x = \overline{U}^j \overline{v}^{i}
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
C  uTrans               :: zonal transport
C  vFld                 :: meridional flow
C  myThid               :: thread number
      INTEGER bi,bj,k
      _RL uTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid
C !OUTPUT PARAMETERS: ==================================================
C  AdvectFluxUV         :: advective flux
      _RL AdvectFluxUV(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
      INTEGER i,j
CEOP

      DO j=1-Oly+1,sNy+Oly
       DO i=1-Olx+1,sNx+Olx
        AdvectFluxUV(i,j) =
     &  0.25*( uTrans(i,j) + uTrans(i,j-1) )
#ifdef MOM_BOUNDARY_CONSERVE
     &      *(   vFld(i,j)*_maskS(i-1,j,k,bi,bj)
     &         + vFld(i-1,j)*_maskS(i,j,k,bi,bj) )
#else
     &      *(   vFld(i,j) +   vFld(i-1,j) )
#endif
#ifdef OLD_ADV_BCS
     &      *_maskS(i,j,k,bi,bj)
     &      *_maskS(i-1,j,k,bi,bj)
#endif /* OLD_ADV_BCS */
       ENDDO
      ENDDO

      RETURN
      END
