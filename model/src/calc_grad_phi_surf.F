C $Header: /u/gcmpack/MITgcm/model/src/calc_grad_phi_surf.F,v 1.2 2001/09/26 18:09:13 cnh Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CALC_GRAD_PHI_SURF
C     !INTERFACE:
      SUBROUTINE CALC_GRAD_PHI_SURF( bi, bj, iMin, iMax, jMin, jMax,
     I                       etaFld,
     O                       phiSurfX, phiSurfY,
     I                       myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R CALC_GRAD_PHI_SURF                                    
C     | o Calculate the gradient of the surface Potential anomaly 
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     etaFld             :: free-surface r-anomaly (r unit).
C     phiSurfX, phiSurfY :: Gradient in the X and Y directions of surface
C       Potentiel anomaly (atmos: =Geopotential ; ocean: =Pressure/rho) 
C     bi,bj,iMin,iMax,jMin,jMax :: Loop counters
C     myThid :: Instance number for this call of the routine.
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      _RL etaFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL phiSurfX(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL phiSurfY(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     i,j :: Loop counters
      INTEGER i,j
CEOP

C     Zonal term
      DO j=jMin,jMax
       DO i=iMin,iMax
        phiSurfX(i,j)=_recip_dxC(i,j,bi,bj)*
     &   ( Bo_surf(i,j,bi,bj)*etaFld(i,j,bi,bj)
     &   - Bo_surf(i-1,j,bi,bj)*etaFld(i-1,j,bi,bj) )      
       ENDDO
      ENDDO

C     Meridional term
      DO j=jMin,jMax
       DO i=iMin,iMax
        phiSurfY(i,j)=_recip_dyC(i,j,bi,bj)*
     &   ( Bo_surf(i,j,bi,bj)*etaFld(i,j,bi,bj)
     &   - Bo_surf(i,j-1,bi,bj)*etaFld(i,j-1,bi,bj) ) 
       ENDDO
      ENDDO

      RETURN
      END
