C $Header: /u/gcmpack/MITgcm/pkg/mom_fluxform/mom_u_del2u.F,v 1.3 2001/09/26 19:05:21 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

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
#include "DYNVARS.h"

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
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx-1
        fZon(i,j) = drF(k)*_hFacC(i,j,k,bi,bj)
     &   *_dyF(i,j,bi,bj)
     &   *_recip_dxF(i,j,bi,bj)
     &   *(uFld(i+1,j)-uFld(i,j))
#ifdef COSINEMETH_III
     &   *sqcosFacU(J,bi,bj)
#endif
       ENDDO
      ENDDO

C     Meridional flux d/dy U
      DO j=1-Oly+1,sNy+Oly
       DO i=1-Olx,sNx+Olx
        fMer(i,j) = drF(k)*hFacZ(i,j)
     &   *_dxV(i,j,bi,bj)
     &   *_recip_dyU(i,j,bi,bj)
     &   *(uFld(i,j)-uFld(i,j-1))
       ENDDO
      ENDDO

C     del^2 U
      DO j=0,sNy+1
       DO i=0,sNx+2
        del2u(i,j) =
     &  recip_drF(k)*_recip_hFacW(i,j,k,bi,bj)
     &  *recip_rAw(i,j,bi,bj)
     &  *( fZon(i,j  )    - fZon(i-1,j)
     &    +fMer(i,j+1)    - fMer(i  ,j)
     &   )*_maskW(i,j,k,bi,bj)
       ENDDO
      ENDDO

      IF (no_slip_sides) THEN
C-- No-slip BCs impose a drag at walls...
      DO j=0,sNy+1
       DO i=0,sNx+2
        hFacZClosedS = _hFacW(i,j,k,bi,bj) - hFacZ(i,j)
        hFacZClosedN = _hFacW(i,j,k,bi,bj) - hFacZ(i,j+1)
        del2u(i,j) = del2u(i,j)
     &  - _recip_hFacW(i,j,k,bi,bj)*recip_drF(k)
     &   *recip_rAw(i,j,bi,bj)
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
