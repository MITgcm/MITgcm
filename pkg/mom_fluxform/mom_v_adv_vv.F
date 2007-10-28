C $Header: /u/gcmpack/MITgcm/pkg/mom_fluxform/mom_v_adv_vv.F,v 1.5 2007/10/28 21:38:21 jmc Exp $
C $Name:  $

#include "MOM_FLUXFORM_OPTIONS.h"

CBOP
C !ROUTINE: MOM_V_ADV_VV

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_V_ADV_VV(
     I        bi,bj,k,
     I        vTrans, vFld,
     O        AdvectFluxVV,
     I        myThid)

C !DESCRIPTION:
C Calculates the meridional advective flux of meridional momentum:
C \begin{equation*}
C F^y = \overline{V}^j \overline{v}^{j}
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
C  vFld                 :: meridional flow
C  myThid               :: thread number
      INTEGER bi,bj,k
      _RL vTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  AdvectFluxVV         :: advective flux
      _RL AdvectFluxVV(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
      INTEGER i,j
CEOP

      DO j=1-Oly,sNy+Oly-1
       DO i=1-Olx,sNx+Olx-1
        AdvectFluxVV(i,j) =
     &  0.25*( vTrans(i,j) + vTrans(i,j+1) )
#ifdef MOM_BOUNDARY_CONSERVE
     &      *(   vFld(i,j)*_maskS(i,j+1,k,bi,bj)
     &         + vFld(i,j+1)*_maskS(i,j,k,bi,bj) )
#else
     &      *(   vFld(i,j) +   vFld(i,j+1) )
#endif
       ENDDO
      ENDDO

      RETURN
      END
