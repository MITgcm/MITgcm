C $Header: /u/gcmpack/MITgcm/pkg/salt_plume/salt_plume_tendency_apply_t.F,v 1.1 2014/05/21 10:52:21 heimbach Exp $
C $Name:  $

#include "SALT_PLUME_OPTIONS.h"

CBOP 0
C     !ROUTINE: SALT_PLUME_TENDENCY_APPLY_T
C     !INTERFACE:
      SUBROUTINE SALT_PLUME_TENDENCY_APPLY_T(
     &     iMin, iMax, jMin, jMax,
     &     bi,bj,kLev,myTime,myThid)

C     !DESCRIPTION:
C     Add salt_plume tendency terms to T tendency.
C     Routine works for one level at a time.
C     SaltPlume is the amount of salt rejected by ice while freezing;
C     it is here redistributed to multiple vertical levels as per
C     Duffy et al. (GRL 1999).

C     !INPUT PARAMETERS:
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "SALT_PLUME.h"

C     !INPUT PARAMETERS:
      integer iMin, iMax, jMin, jMax, kLev, bi, bj, myThid
      _RL myTime
CEOP

#ifdef ALLOW_SALT_PLUME
#ifdef SALT_PLUME_VOLUME

C     !LOCAL VARIABLES:
      integer i, j
Catn unit plumeEtend [m/s * kg/m3 * J/kg/degC * degC] = [W/m2]
      _RL plumeEtend(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

Catn define local Tfrz -1.9degC when sea-ice forms
      DO j=jMin,jMax
       DO i=iMin,iMax
        IF ( SaltPlumeDepth(i,j,bi,bj) .GT. abs(rF(kLev)) ) THEN
         plumeEtend(i,j) = SPforcingT(i,j,kLev,bi,bj)
         gT(i,j,kLev,bi,bj)=gT(i,j,kLev,bi,bj)+plumeEtend(I,J)
     &        *recip_drF(kLev)*mass2rUnit*_recip_hFacC(i,j,kLev,bi,bj)
     &        /HeatCapacity_Cp
        ELSE
         plumeEtend(i,j) = 0. _d 0
        ENDIF
       ENDDO
      ENDDO

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
       CALL DIAGNOSTICS_FILL (
     &      plumeEtend,'oceEPtnd',kLev,1,2,bi,bj,myThid )
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */
      
#endif /* SALT_PLUME_VOLUME */
#endif /* ALLOW_SALT_PLUME */
      
      RETURN
      END
