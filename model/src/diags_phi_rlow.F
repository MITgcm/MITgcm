C $Header: /u/gcmpack/MITgcm/model/src/diags_phi_rlow.F,v 1.1 2003/02/09 02:58:39 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: DIAGS_PHI_RLOW
C     !INTERFACE:
      SUBROUTINE DIAGS_PHI_RLOW( 
     I                       k, bi, bj, iMin,iMax, jMin,jMax,
     I                       phiHyd, alphRho, tFld, sFld,
     I                       myTime, myIter, myThid)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R DIAGS_PHI_RLOW                                    
C     | o Diagnose Phi-Hydrostatic at r-lower boundary
C     |   = bottom pressure (ocean in z-coord) ; 
C     |   = sea surface elevation (ocean in p-coord) ;
C     |   = height at the top of atmosphere (in p-coord) ;
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
#include "DYNVARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     bi,bj      :: tile index            
C     iMin,iMax,jMin,jMax :: Loop counters
C     phiHyd     :: Hydrostatic Potential anomaly 
C                  (atmos: =Geopotential ; ocean-z: =Pressure/rho)
C     alphRho    :: Density (z-coord) or specific volume (p-coord)
C     tFld       :: Potential temp.
C     sFld       :: Salinity 
C     myTime :: Current time
C     myIter :: Current iteration number
C     myThid :: Instance number for this call of the routine.
      INTEGER k, bi,bj, iMin,iMax, jMin,jMax
      _RL phiHyd(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL alphRho(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL sFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL myTime
      INTEGER myIter, myThid

#ifdef INCLUDE_PHIHYD_CALCULATION_CODE

C     !LOCAL VARIABLES:
C     == Local variables ==
C     i,j :: Loop counters
      INTEGER i,j
      _RL zero, one, half
      _RL dRloc
      PARAMETER ( zero= 0. _d 0 , one= 1. _d 0 , half= .5 _d 0 )
CEOP

        dRloc=drC(k)
        IF (k.EQ.1) dRloc=drF(1)

      IF ( buoyancyRelation .eq. 'OCEANIC' ) THEN

       IF (integr_GeoPot.EQ.1) THEN
C  --  Finite Volume Form

         DO j=jMin,jMax
          DO i=iMin,iMax
           IF ( k .EQ. kLowC(i,j,bi,bj) ) THEN
            phiHydLow(i,j,bi,bj) = phiHyd(i,j,k)
     &          + hFacC(i,j,k,bi,bj)
     &             *drF(K)*gravity*alphRho(i,j)*recip_rhoConst
     &          + Bo_surf(i,j,bi,bj)*etaN(i,j,bi,bj)
     &          + phi0surf(i,j,bi,bj)
           ENDIF
          ENDDO
         ENDDO

       ELSE
C  --  Finite Difference Form

C---------- Compute bottom pressure deviation from gravity*rho0*H
C           This has to be done starting from phiHyd at the current
C           tracer point and .5 of the cell's thickness has to be
C           substracted from hFacC

         DO j=jMin,jMax
          DO i=iMin,iMax
           IF ( K .EQ. kLowC(i,j,bi,bj) ) THEN
            phiHydLow(i,j,bi,bj) = phiHyd(i,j,k)
     &          + (half*dRloc+(hFacC(i,j,k,bi,bj)-half)*drF(k))
     &                    *gravity*alphRho(i,j)*recip_rhoConst
     &          + Bo_surf(i,j,bi,bj)*etaN(i,j,bi,bj)
     &          + phi0surf(i,j,bi,bj)
           ENDIF
          ENDDO
         ENDDO

C  --  end if integr_GeoPot = ...
       ENDIF
        
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      ELSEIF ( buoyancyRelation .eq. 'OCEANICP' ) THEN

       IF (integr_GeoPot.EQ.1) THEN
C  --  Finite Volume Form

         DO j=jMin,jMax
          DO i=iMin,iMax
           IF ( K .EQ. kLowC(i,j,bi,bj) ) THEN
            phiHydLow(i,j,bi,bj) = phiHyd(i,j,k)
     &          + hFacC(i,j,k,bi,bj)*drF(K)*alphRho(i,j)
     &          + Bo_surf(i,j,bi,bj)*etaN(i,j,bi,bj)
     &          + phi0surf(i,j,bi,bj)
           ENDIF
          ENDDO
         ENDDO

       ELSE
C  --  Finite Difference Form

C---------- Compute gravity*(sea surface elevation) first
C           This has to be done starting from phiHyd at the current
C           tracer point and .5 of the cell's thickness has to be
C           substracted from hFacC

         DO j=jMin,jMax
          DO i=iMin,iMax
           IF ( K .EQ. kLowC(i,j,bi,bj) ) THEN
            phiHydLow(i,j,bi,bj) = phiHyd(i,j,k)
     &          + ( half*dRloc+(hFacC(i,j,k,bi,bj)-half)*drF(k)
     &            )*alphRho(i,j)
     &          + Bo_surf(i,j,bi,bj)*etaN(i,j,bi,bj)
     &          + phi0surf(i,j,bi,bj)
           ENDIF
          ENDDO
         ENDDO

C  --  end if integr_GeoPot = ...
       ENDIF

c     ELSEIF ( buoyancyRelation .eq. 'ATMOSPHERIC' ) THEN
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      ENDIF

#endif /* INCLUDE_PHIHYD_CALCULATION_CODE */

      RETURN
      END
