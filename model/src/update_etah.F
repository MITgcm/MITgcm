C $Header: /u/gcmpack/MITgcm/model/src/update_etah.F,v 1.7 2005/11/04 01:19:24 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: UPDATE_ETAH
C     !INTERFACE:
      SUBROUTINE UPDATE_ETAH( myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE UPDATE_ETAH                                 
C     | o Update etaH at the begining of the time step. 
C     |  (required with NLFS to derive surface layer thickness)
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "SURFACE.h"
#include "FFIELDS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime  :: Current time in simulation
C     myIter  :: Current iteration number in simulation
C     myThid  :: Thread number for this instance of the routine.
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
#ifdef EXACT_CONSERV
C     Local variables in common block

C     Local variables
C     i,j,bi,bj  :: Loop counters
      INTEGER i,j,bi,bj
CEOP


      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid) 

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   before updating etaH, save current etaH field in etaHnm1
         DO j=1-Oly,sNy+Oly
           DO i=1-Olx,sNx+Olx
             etaHnm1(i,j,bi,bj) = etaH(i,j,bi,bj) 
           ENDDO
         ENDDO

C--   Update etaH at the end of the time step : 
C     Incorporate the Explicit part of -Divergence(Barotropic_Flow)

        IF (implicDiv2Dflow.EQ. 1. _d 0) THEN
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
            etaH(i,j,bi,bj) = etaN(i,j,bi,bj) 
          ENDDO
         ENDDO

        ELSE
         DO j=1,sNy
          DO i=1,sNx
            etaH(i,j,bi,bj) = etaN(i,j,bi,bj) 
     &       + (1. - implicDiv2Dflow)*dEtaHdt(i,j,bi,bj)
     &                               *deltaTfreesurf
          ENDDO
         ENDDO
        ENDIF

#ifdef ALLOW_OBCS
#ifdef NONLIN_FRSURF
C- note: 1) needs to apply OBC to etaH since viscous terms depend on hFacZ.
C           that is not only function of boundaries hFac values.
C        2) has to be done before calc_surf_dr; but since obcs_calc is 
C           called later, hFacZ will lag 1 time step behind OBC update. 
C        3) avoid also unrealistic value of etaH in OB regions that
C           might produce many "WARNING" message from calc_surf_dr.  
C-------
       IF ( useOBCS .AND. nonlinFreeSurf.GT.0 )
     &    CALL OBCS_APPLY_ETA( bi, bj, etaH, myThid )   
#endif /* NONLIN_FRSURF */
#endif /* ALLOW_OBCS */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C- end bi,bj loop.
       ENDDO
      ENDDO 

      IF (implicDiv2Dflow .NE. 1. _d 0 .OR. useOBCS )
     &    _EXCH_XY_RL( etaH, myThid ) 

c     IF (useRealFreshWaterFlux .AND. myTime.EQ.startTime)
c    &    _EXCH_XY_R4( PmEpR, myThid )

#endif /* EXACT_CONSERV */

      RETURN
      END
