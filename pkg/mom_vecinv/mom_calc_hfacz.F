C $Header: /u/gcmpack/MITgcm/pkg/mom_vecinv/Attic/mom_calc_hfacz.F,v 1.2 2001/05/29 14:01:39 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE MOM_CALC_HFACZ(
     I        bi,bj,k,
     O        hFacZ,r_hFacZ,
     I        myThid)
      IMPLICIT NONE
C
C     Calculate Kinetic Energy, KE, with units (m/s)^2
C       - assumes hydrostatic formulation only
C

C     == Global variables ==
#include "SIZE.h"
#include "GRID.h"

C     == Routine arguments ==
      INTEGER bi,bj,k
      _RS hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS r_hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL hFacZOpen
      INTEGER myThid

C     == Local variables ==
      INTEGER I,J

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
