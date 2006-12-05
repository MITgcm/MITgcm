C $Header: /u/gcmpack/MITgcm/pkg/mom_fluxform/mom_u_del2u.F,v 1.7 2006/12/05 05:30:38 jmc Exp $
C $Name:  $

#include "MOM_FLUXFORM_OPTIONS.h"

CBOP
C !ROUTINE: MOM_U_DEL2U

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_U_DEL2U(
     I        bi,bj,k,
     I        uFld, hFacZ,
     O        del2u,
     I        myThid)

C !DESCRIPTION:
C Calculates the Laplacian of zonal flow

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
C  hFacZ                :: fractional thickness at vorticity points
C  myThid               :: thread number
      INTEGER bi,bj,k
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  del2u                :: Laplacian
      _RL del2u(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
      INTEGER I,J
      _RL fZon(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fMer(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS hFacZClosedS,hFacZClosedN
CEOP

C     Zonal flux d/dx U
      DO j=1-Oly+1,sNy+Oly-1
       DO i=1-Olx,sNx+Olx-1
        fZon(i,j) = drF(k)*_hFacC(i,j,k,bi,bj)
     &   *_dyF(i,j,bi,bj)
     &   *_recip_dxF(i,j,bi,bj)
     &   *(uFld(i+1,j)-uFld(i,j))
#ifdef COSINEMETH_III
     &   *sqCosFacU(J,bi,bj)
#endif
c    &   *deepFacC(k)        ! dyF scaling factor
c    &   *recip_deepFacC(k)  ! recip_dxF scaling factor
       ENDDO
      ENDDO

C     Meridional flux d/dy U
      DO j=1-Oly+1,sNy+Oly
       DO i=1-Olx+1,sNx+Olx-1
        fMer(i,j) = drF(k)*hFacZ(i,j)
     &   *_dxV(i,j,bi,bj)
     &   *_recip_dyU(i,j,bi,bj)
     &   *(uFld(i,j)-uFld(i,j-1))
#if (defined (ISOTROPIC_COS_SCALING) && defined (COSINEMETH_III))
     &   *sqCosFacV(J,bi,bj)
#endif
c    &   *deepFacC(k)        ! dxV scaling factor
c    &   *recip_deepFacC(k)  ! recip_dyU scaling factor
       ENDDO
      ENDDO

C     del^2 U
      DO j=1-Oly+1,sNy+Oly-1
       DO i=1-Olx+1,sNx+Olx-1
        del2u(i,j) =
     &   recip_drF(k)*_recip_hFacW(i,j,k,bi,bj)
     &  *recip_rAw(i,j,bi,bj)*recip_deepFac2C(k)
     &  *( fZon(i,j  )    - fZon(i-1,j)
     &    +fMer(i,j+1)    - fMer(i  ,j)
     &   )*_maskW(i,j,k,bi,bj)
       ENDDO
      ENDDO

      IF (no_slip_sides) THEN
C-- No-slip BCs impose a drag at walls...
      DO j=1-Oly+1,sNy+Oly-1
       DO i=1-Olx+1,sNx+Olx-1
        hFacZClosedS = _hFacW(i,j,k,bi,bj) - hFacZ(i,j)
        hFacZClosedN = _hFacW(i,j,k,bi,bj) - hFacZ(i,j+1)
        del2u(i,j) = del2u(i,j)
     &  -_recip_hFacW(i,j,k,bi,bj)*recip_drF(k)
     &   *recip_rAw(i,j,bi,bj)*recip_deepFac2C(k)
     &   *( hFacZClosedS*dxV(i, j ,bi,bj)
     &     *_recip_dyU(i, j ,bi,bj)
     &     +hFacZClosedN*dxV(i,j+1,bi,bj)
     &     *_recip_dyU(i,j+1,bi,bj)
     &    )*drF(k)*2.*uFld(i,j)
     &     *_maskW(i,j,k,bi,bj)
       ENDDO
      ENDDO
      ENDIF

      RETURN
      END
