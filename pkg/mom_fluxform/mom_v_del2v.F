C $Header: /u/gcmpack/MITgcm/pkg/mom_fluxform/mom_v_del2v.F,v 1.3 2001/09/26 19:05:21 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C !ROUTINE: MOM_V_DEL2V

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_V_DEL2V(
     I        bi,bj,k,
     I        vFld, hFacZ,
     O        del2v,
     I        myThid)

C !DESCRIPTION:
C Calculates the Laplacian of meridional flow

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C !INPUT PARAMETERS: ===================================================
C  bi,bj                :: tile indices
C  k                    :: vertical level
C  vFld                 :: meridional flow
C  hFacZ                :: fractional thickness at vorticity points
C  myThid               :: thread number`
      INTEGER bi,bj,k
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid
C !OUTPUT PARAMETERS: ==================================================
C  del2v                :: Laplacian
      _RL del2v(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
      INTEGER I,J
      _RL fZon(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fMer(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS hFacZClosedE,hFacZClosedW
CEOP

C     Zonal flux d/dx V
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx+1,sNx+Olx
        fZon(i,j) = drF(k)*hFacZ(i,j)
     &   *_dyU(i,j,bi,bj)
     &   *_recip_dxV(i,j,bi,bj)
     &   *(vFld(i,j)-vFld(i-1,j))
#ifdef COSINEMETH_III
     &   *sqcosFacV(J,bi,bj)
#endif
       ENDDO
      ENDDO

C     Meridional flux d/dy V
      DO j=1-Oly,sNy+Oly-1
       DO i=1-Olx,sNx+Olx
        fMer(i,j) = drF(k)*_hFacC(i,j,k,bi,bj)
     &   *_dxF(i,j,bi,bj)
     &   *_recip_dyF(i,j,bi,bj)
     &   *(vFld(i,j+1)-vFld(i,j))
       ENDDO
      ENDDO

C     del^2 U
      DO j=0,sNy+2
       DO i=0,sNx+1
        del2v(i,j) = recip_drF(k)*_recip_hFacS(i,j,k,bi,bj)
     &  *recip_rAs(i,j,bi,bj)
     &  *( fZon(i+1,j)    - fZon(i, j )
     &    +fMer( i ,j)    - fMer(i,j-1)
     &   )*_maskS(i,j,k,bi,bj)
       ENDDO
      ENDDO

      IF (no_slip_sides) THEN
C-- No-slip BCs impose a drag at walls...
      DO j=0,sNy+2
       DO i=0,sNx+1
        hFacZClosedW = _hFacS(i,j,k,bi,bj) - hFacZ(i,j)
        hFacZClosedE = _hFacS(i,j,k,bi,bj) - hFacZ(i+1,j)
        del2v(i,j) = del2v(i,j)
     &  -_recip_hFacS(i,j,k,bi,bj)*recip_drF(k)
     &  *recip_rAs(i,j,bi,bj)
     &   *( hFacZClosedW*dyU( i ,j,bi,bj)
     &      *_recip_dxV( i ,j,bi,bj)
     &     +hFacZClosedE*dyU(i+1,j,bi,bj)
     &      *_recip_dxV(i+1,j,bi,bj)
     &    )*drF(k)*2.*vFld(i,j)
     &      *_maskS(i,j,k,bi,bj)
       ENDDO
      ENDDO
      ENDIF

      RETURN
      END
