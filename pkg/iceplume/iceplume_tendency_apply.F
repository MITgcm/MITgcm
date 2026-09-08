
#include "ICEPLUME_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: ICEPLUME_TENDENCY_APPLY_T

C !INTERFACE:
      SUBROUTINE ICEPLUME_TENDENCY_APPLY_T(
     U                    gT_arr,
     I                    iMin,iMax,jMin,jMax,
     I                    k, bi, bj, myTime, myIter, myThid )

C     !DESCRIPTION:
C     As for S/R ICEPLUME_TENDENCY_APPLY_T
C     Add iceplume tendency terms to T (theta) tendency.
C     Routine works for one level at a time.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "ICEPLUME_PARAMS.h"
#include "ICEPLUME_FIELDS.h"


C     !INPUT/OUTPUT PARAMETERS:
C     gT_arr    :: the tendency array
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

C     !LOCAL VARIABLES:
      INTEGER i, j
      _RL tmpVar(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tmpVar1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

#ifdef ALLOW_ICEPLUME

C This is the background melt tendency
C 26Jun2026
c Because the bg submarine melt is also contributing massflux to the
c grid cell, should we treat it the same way as the plume? i.e. need
c to account for the massflux*theta or massflux*Tref
      IF ( applyIcePlumeBGTendT ) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
          tmpVar(i,j) =
     &         (HeatFlux3Dbg(i,j,k,bi,bj)/HeatCapacity_Cp
     &         )*mass2rUnit
     &          *recip_rA(i,j,bi,bj)
     &          *recip_drF(k)
C The orig calc of tendency inside iceplume_calc did not account for hfac
c     &          *_recip_hFacC(i,j,k,bi,bj)
          gT_arr(i,j) = gT_arr(i,j) + tmpVar(i,j)
C Now mult recip_hFacC before output, for comparison with IP_gTbg
          tmpVar(i,j) = tmpVar(i,j)
     &          *_recip_hFacC(i,j,k,bi,bj)
C The orig calc also did not account for the correction to tendency due
C to addmass, which we applied for subglacial plume in apply_forcing.F
          tmpVar1(i,j)= -addMass3Dbg(i,j,k,bi,bj)*theta(i,j,k,bi,bj)
     &          *mass2rUnit
     &          *recip_rA(i,j,bi,bj)
     &          *recip_drF(k)
c     &          *_recip_hFacC(i,j,k,bi,bj)
        ENDDO
       ENDDO
      ENDIF

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
       CALL DIAGNOSTICS_FILL (
     &      tmpVar,'IP_gTbga',k,1,2,bi,bj,myThid )
       CALL DIAGNOSTICS_FILL (
     &      tmpVar1,'IP_gTbgb',k,1,2,bi,bj,myThid )
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif /* ALLOW_ICEPLUME */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: ICEPLUME_TENDENCY_APPLY_S

C !INTERFACE:
      SUBROUTINE ICEPLUME_TENDENCY_APPLY_S(
     U                    gS_arr,
     I                    iMin,iMax,jMin,jMax,
     I                    k, bi, bj, myTime, myIter, myThid )

C     !DESCRIPTION:
C     As for S/R ICEPLUME_TENDENCY_APPLY_S
C     Add iceplume tendency terms to S tendency.
C     Routine works for one level at a time.

C     !INPUT PARAMETERS:
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "ICEPLUME_PARAMS.h"
#include "ICEPLUME_FIELDS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     gS_arr    :: the tendency array
C     k         :: Current vertical level index
C     bi,bj     :: Current tile indices
C     myTime    :: Current time in simulation
C     myIter    :: Current iteration number
C     myThid    :: my Thread Id number
      _RL     gS_arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin, iMax, jMin, jMax
      INTEGER k, bi, bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
      INTEGER i, j
      _RL tmpVar(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tmpVar1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

#ifdef ALLOW_ICEPLUME

C This is the background melt tendency
      IF ( applyIcePlumeBGTendS ) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
         tmpVar(i,j) =
     &         (SaltFlux3Dbg(i,j,k,bi,bj)
     &         )*mass2rUnit
     &         *recip_rA(i,j,bi,bj)
     &         *recip_drF(k)
C The orig calc of tendency inside iceplume_calc did not account for hfac
c     &         *_recip_hFacC(i,j,k,bi,bj)
          gS_arr(i,j) = gS_arr(i,j) + tmpVar(i,j)
          tmpVar(i,j) = tmpVar(i,j)
     &         *_recip_hFacC(i,j,k,bi,bj)
C Now mult recip_hFacC before output, for comparison with IP_gSbg
         tmpVar1(i,j) = -addMass3Dbg(i,j,k,bi,bj)*salt(i,j,k,bi,bj)
     &         *mass2rUnit
     &         *recip_rA(i,j,bi,bj)
     &         *recip_drF(k)
     &         *_recip_hFacC(i,j,k,bi,bj)
        ENDDO
       ENDDO
      ENDIF

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
       CALL DIAGNOSTICS_FILL (
     &      tmpVar,'IP_gSbga',k,1,2,bi,bj,myThid )
       CALL DIAGNOSTICS_FILL (
     &      tmpVar1,'IP_gSbgb',k,1,2,bi,bj,myThid )
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif /* ALLOW_ICEPLUME */

      RETURN
      END
