C $Header: /u/gcmpack/MITgcm/pkg/mom_fluxform/Attic/mom_calc_hfacz.F,v 1.3 2001/09/26 19:05:21 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C !ROUTINE: MOM_CALC_HFACZ

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_CALC_HFACZ(
     I        bi,bj,k,
     O        hFacZ,r_hFacZ,
     I        myThid)

C !DESCRIPTION:
C Calculates the fractional thickness at vorticity points

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"

C !INPUT PARAMETERS: ===================================================
C  bi,bj                :: tile indices
C  k                    :: vertical level
C  myThid               :: thread number
      INTEGER bi,bj,k
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  hFacZ                :: fractional thickness at vorticity points
C  r_hFacZ              :: reciprocal
      _RS hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS r_hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
      INTEGER I,J
      _RL hFacZOpen
CEOP

C--   Calculate open water fraction at vorticity points

      DO i=1-Olx,sNx+Olx
       hFacZ(i,1-Oly)=0.
       r_hFacZ(i,1-Oly)=0.
      ENDDO

      DO j=2-Oly,sNy+Oly
       hFacZ(1-Olx,j)=0.
       r_hFacZ(1-Olx,j)=0.
       DO i=2-Olx,sNx+Olx
        hFacZOpen=min(_hFacW(i,j,k,bi,bj),
     &                _hFacW(i,j-1,k,bi,bj))
        hFacZOpen=min(_hFacS(i,j,k,bi,bj),hFacZOpen)
        hFacZOpen=min(_hFacS(i-1,j,k,bi,bj),hFacZOpen)
        hFacZ(i,j)=hFacZOpen
        IF (hFacZ(i,j).EQ.0.) THEN
         r_hFacZ(i,j)=0.
        ELSE
         r_hFacZ(i,j)=1./hFacZ(i,j)
        ENDIF
       ENDDO
      ENDDO

      RETURN
      END
