C $Header: /u/gcmpack/MITgcm/verification/aim.5l_cs/code/mom_vi_hfacz_diss.F,v 1.1 2002/01/09 00:28:56 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE MOM_VI_HFACZ_DISS(
     I        bi,bj,k,
     O        hFacZ,r_hFacZ,
     I        myThid)
      IMPLICIT NONE
C
C     Compute hFactor (and reciprol) at the corner (Z-point)
C       used for vorticity,divergence form of viscous term 
C        (also in Shap_S2 formulation)
C

C     == Global variables ==
#include "SIZE.h"
#include "GRID.h"

C     == Routine arguments ==
      INTEGER bi,bj,k
      _RS hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS r_hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS hFacZOpen
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
        hFacZOpen=MIN(_hFacW(i,j,k,bi,bj)
     &              + _hFacW(i,j-1,k,bi,bj)
     &              , _hFacS(i,j,k,bi,bj)
     &              + _hFacS(i-1,j,k,bi,bj)
     &               )
        hFacZ(i,j)=0.5*hFacZOpen         
        IF (hFacZ(i,j).EQ.0.) THEN
         r_hFacZ(i,j)=0.
        ELSE
         r_hFacZ(i,j)=1./hFacZ(i,j)
        ENDIF
       ENDDO
      ENDDO

      RETURN
      END
