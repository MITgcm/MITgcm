C $Header: /u/gcmpack/MITgcm/pkg/mom_fluxform/mom_u_adv_uu.F,v 1.5 2007/10/28 21:38:21 jmc Exp $
C $Name:  $

#include "MOM_FLUXFORM_OPTIONS.h"

CBOP
C !ROUTINE: MOM_U_ADV_UU

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_U_ADV_UU(
     I        bi,bj,k,
     I        uTrans, uFld,
     O        AdvectFluxUU,
     I        myThid)

C !DESCRIPTION:
C Calculates the zonal advective flux of zonal momentum:
C \begin{equation*}
C F^x = \overline{U}^i \overline{u}^{i}
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
C  uFld                 :: zonal flow
C  myThid               :: thread number
      INTEGER bi,bj,k
      _RL uTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  AdvectFluxUU         :: advective flux
      _RL AdvectFluxUU(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
      INTEGER i,j
CEOP

      DO j=1-Oly,sNy+Oly-1
       DO i=1-Olx,sNx+Olx-1
        AdvectFluxUU(i,j) =
     &  0.25*( uTrans(i,j) + uTrans(i+1,j) )
#ifdef MOM_BOUNDARY_CONSERVE
     &      *(   uFld(i,j)*_maskW(i+1,j,k,bi,bj)
     &         + uFld(i+1,j)*_maskW(i,j,k,bi,bj) )
#else
     &      *(   uFld(i,j) +   uFld(i+1,j) )
#endif
       ENDDO
      ENDDO

      RETURN
      END
