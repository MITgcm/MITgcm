C $Header: /u/gcmpack/MITgcm/model/src/update_etah.F,v 1.12 2011/05/20 16:29:49 jmc Exp $
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
C     | o Update etaH after mom-correction-step/integr_continuity
C     |  (required with NLFS to derive surface layer thickness)
C     | o Also derive SSH tendency at grid-cell Western and
C     |   Southern edges (for hybrid sigma-coordinate)
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
C--    Apply OBC to etaH (NonLin-FreeSurf): needed since viscous terms
C       depend on hFacZ which is not only function of boundary hFac values.
       IF ( useOBCS.AND.nonlinFreeSurf.GT.0 )
     &    CALL OBCS_APPLY_ETA( bi, bj, etaH, myThid )
#endif /* ALLOW_OBCS */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C- end bi,bj loop.
       ENDDO
      ENDDO

      IF ( implicDiv2Dflow .NE. 1. _d 0 .OR.
     &    ( useOBCS.AND.nonlinFreeSurf.GT.0 ) )
     &    CALL EXCH_XY_RL( etaH, myThid )

c     IF (useRealFreshWaterFlux .AND. myTime.EQ.startTime)
c    &    _EXCH_XY_RL( PmEpR, myThid )

#ifdef NONLIN_FRSURF
# ifndef DISABLE_SIGMA_CODE
      IF ( nonlinFreeSurf.GT.0 .AND. selectSigmaCoord.NE.0 ) THEN

       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
C-     2nd bi,bj loop :

C-- copy etaHX -> dEtaXdt
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
            dEtaWdt(i,j,bi,bj) = etaHw(i,j,bi,bj)
            dEtaSdt(i,j,bi,bj) = etaHs(i,j,bi,bj)
          ENDDO
         ENDDO

         DO j=1,sNy+1
          DO i=1,sNx+1
            etaHw(i,j,bi,bj)   = ( etaH (i-1,j,bi,bj)
     &                           + etaH ( i ,j,bi,bj) )*0.5 _d 0
            etaHs(i,j,bi,bj)   = ( etaH (i,j-1,bi,bj)
     &                           + etaH (i, j ,bi,bj) )*0.5 _d 0
c           etaHw(i,j,bi,bj)   = 0.5 _d 0
c    &                         *(   etaH (i-1,j,bi,bj)*rA(i-1,j,bi,bj)
c    &                            + etaH ( i ,j,bi,bj)*rA( i ,j,bi,bj)
c    &                          )*recip_rAw(i,j,bi,bj)
c           etaHs(i,j,bi,bj)   = 0.5 _d 0
c    &                         *(   etaH (i,j-1,bi,bj)*rA(i,j-1,bi,bj)
c    &                            + etaH (i, j ,bi,bj)*rA(i, j ,bi,bj)
c    &                          )*recip_rAs(i,j,bi,bj)
          ENDDO
         ENDDO

C- end 2nd bi,bj loop.
        ENDDO
       ENDDO

       CALL EXCH_UV_XY_RL( etaHw, etaHs, .FALSE., myThid )
       CALL EXCH_XY_RL( dEtaHdt, myThid )

       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
C-     3rd bi,bj loop :

         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
            dEtaWdt(i,j,bi,bj) = ( etaHw(i,j,bi,bj)
     &                           - dEtaWdt(i,j,bi,bj) )/deltaTfreesurf
            dEtaSdt(i,j,bi,bj) = ( etaHs(i,j,bi,bj)
     &                           - dEtaSdt(i,j,bi,bj) )/deltaTfreesurf
          ENDDO
         ENDDO

C- end 3rd bi,bj loop.
        ENDDO
       ENDDO

      ENDIF
# endif /* DISABLE_SIGMA_CODE */
#endif /* NONLIN_FRSURF */

#endif /* EXACT_CONSERV */

      RETURN
      END
