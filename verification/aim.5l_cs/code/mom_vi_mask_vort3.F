C $Header: /u/gcmpack/MITgcm/verification/aim.5l_cs/code/mom_vi_mask_vort3.F,v 1.1 2002/01/09 00:28:56 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE MOM_VI_MASK_VORT3(
     I        bi,bj,k,
     O        hFacZ,r_hFacZ,
     U        vort3,
     I        myThid)
      IMPLICIT NONE
C
C     Compute hFactor and recip_hFactor at the corner (Z-point)
C     and set vort3 to zero where hFac = 0 (apply a mask)
C

C     == Global variables ==
#include "SIZE.h"
#include "GRID.h"

C     == Routine arguments ==
      INTEGER bi,bj,k
      _RS hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS r_hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vort3(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS hFacZOpen
      INTEGER myThid

C     == Local variables ==
      INTEGER I,J

C--   Calculate open water fraction at vorticity points

c     DO i=1-Olx,sNx+Olx
c      hFacZ(i,1-Oly)=0.
c      r_hFacZ(i,1-Oly)=0.
c     ENDDO

      DO j=2-Oly,sNy+Oly
c      hFacZ(1-Olx,j)=0.
c      r_hFacZ(1-Olx,j)=0.
       DO i=2-Olx,sNx+Olx
        hFacZOpen=min(_hFacW(i,j,k,bi,bj),
     &                _hFacW(i,j-1,k,bi,bj))
        hFacZOpen=min(_hFacS(i,j,k,bi,bj),hFacZOpen)
        hFacZOpen=min(_hFacS(i-1,j,k,bi,bj),hFacZOpen)
        hFacZ(i,j)=hFacZOpen
        IF (hFacZ(i,j).EQ.0.) THEN
         r_hFacZ(i,j)=0.
         vort3(i,j)=0.
        ELSE
         r_hFacZ(i,j)=1./hFacZ(i,j)
        ENDIF
       ENDDO
      ENDDO

      RETURN
      END
