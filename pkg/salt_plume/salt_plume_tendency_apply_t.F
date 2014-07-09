C $Header: /u/gcmpack/MITgcm/pkg/salt_plume/salt_plume_tendency_apply_t.F,v 1.2 2014/07/09 17:00:50 jmc Exp $
C $Name:  $

#include "SALT_PLUME_OPTIONS.h"

CBOP 0
C     !ROUTINE: SALT_PLUME_TENDENCY_APPLY_T
C     !INTERFACE:
      SUBROUTINE SALT_PLUME_TENDENCY_APPLY_T(
     U                      gT_arr,
     I                      iMin,iMax,jMin,jMax, k, bi, bj,
     I                      myTime, myIter, myThid )

C     !DESCRIPTION:
C     Add salt_plume tendency terms to T tendency.
C     Routine works for one level at a time.
C     SaltPlume is the amount of salt rejected by ice while freezing;
C     it is here redistributed to multiple vertical levels as per
C     Duffy et al. (GRL 1999).

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
c#include "DYNVARS.h"
#include "SALT_PLUME.h"

C     !INPUT/OUTPUT PARAMETERS:
C     gT_arr    :: the tendency array
C     iMin,iMax :: Working range of x-index for applying forcing.
C     jMin,jMax :: Working range of y-index for applying forcing.
C     k         :: Current vertical level index
C     bi,bj     :: Current tile indices
C     myTime    :: Current time in simulation
C     myIter    :: Current iteration number
C     myThid    :: my Thread Id number
      _RL     gT_arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin, iMax, jMin, jMax
      INTEGER k, bi, bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_SALT_PLUME
#ifdef SALT_PLUME_VOLUME

C     !LOCAL VARIABLES:
      INTEGER i, j
Catn unit plumeEtend [m/s * kg/m3 * J/kg/degC * degC] = [W/m2]
      _RL plumeEtend(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

Catn define local Tfrz -1.9degC when sea-ice forms
      DO j=jMin,jMax
       DO i=iMin,iMax
        IF ( SaltPlumeDepth(i,j,bi,bj) .GT. ABS(rF(k)) ) THEN
         plumeEtend(i,j) = SPforcingT(i,j,k,bi,bj)
         gT_arr(i,j) = gT_arr(i,j) + plumeEtend(I,J)
     &        *recip_drF(k)*mass2rUnit*_recip_hFacC(i,j,k,bi,bj)
     &        /HeatCapacity_Cp
        ELSE
         plumeEtend(i,j) = 0. _d 0
        ENDIF
       ENDDO
      ENDDO

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
       CALL DIAGNOSTICS_FILL (
     &      plumeEtend,'oceEPtnd',k,1,2,bi,bj,myThid )
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif /* SALT_PLUME_VOLUME */
#endif /* ALLOW_SALT_PLUME */

      RETURN
      END
