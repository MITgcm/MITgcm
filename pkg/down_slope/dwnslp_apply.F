#include "DWNSLP_OPTIONS.h"

CBOP
C     !ROUTINE: DWNSLP_APPLY
C     !INTERFACE:
      SUBROUTINE DWNSLP_APPLY(
     I            trIdentity, bi, bj, kBottom,
     I            tracer,
     U            gTracer,
     I            recip_hFac, recip_rA_arg, recip_drF,
     I            deltaTLev, myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE DWNSLP_APPLY
C     | o Apply the dowsloping-transport to tracer field
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DWNSLP_SIZE.h"
#include "DWNSLP_PARAMS.h"
#include "DWNSLP_VARS.h"
#ifdef ALLOW_GENERIC_ADVDIFF
# include "GAD.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     trIdentity :: tracer identification number
C     bi,bj      :: Tile indices
C     kBottom    :: bottom level
C     tracer     :: tracer field at current time (input)
C     gTracer    :: tracer tendency to update
C     recip_hFac :: Reciprocal of cell open-depth factor
C     recip_rA_arg :: Reciprocal of cell Area
C     recip_drF  :: Reciprocal of cell thickness
C     deltaTLev  :: tracer time step
C     myTime     :: Current time in simulation
C     myIter     :: Current time-step number
C     myThid     :: my Thread Id. number
      INTEGER trIdentity
      INTEGER bi, bj
      INTEGER kBottom ( xySize, nSx, nSy )
      _RL tracer      ( xySize, Nr )
      _RL gTracer     ( xySize, Nr )
      _RS recip_hFac  ( xySize, Nr )
      _RS recip_rA_arg( xySize, nSx, nSy )
      _RS recip_drF(Nr)
      _RL deltaTLev(Nr)
      _RL     myTime
      INTEGER myIter, myThid

#ifdef ALLOW_DOWN_SLOPE
#ifdef ALLOW_DIAGNOSTICS
C-    !FUNCTIONS:
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON
#endif /* ALLOW_DIAGNOSTICS */

C     !LOCAL VARIABLES:
C     === Local variables ===
C     msgBuf     :: Informational/error message buffer
      INTEGER k
      INTEGER n,ijd,ijs,kshelf,kDeep
      _RL     gTrLoc(0:Nr)
      _RL     dTrac(1:Nr)
      INTEGER upward
      LOGICAL onOffFlag

#ifdef ALLOW_DIAGNOSTICS
      CHARACTER*8 diagName
      CHARACTER*4 diagSufx
      LOGICAL     doDiagDwnSlpTend
      _RL         tmpFac
#ifdef ALLOW_GENERIC_ADVDIFF
      CHARACTER*4 GAD_DIAG_SUFX
      EXTERNAL    GAD_DIAG_SUFX
#endif
#endif /* ALLOW_DIAGNOSTICS */

CEOP

      onOffFlag = .TRUE.
#ifdef ALLOW_GENERIC_ADVDIFF
      IF ( trIdentity.EQ.GAD_TEMPERATURE ) onOffFlag = temp_useDWNSLP
      IF ( trIdentity.EQ.GAD_SALINITY    ) onOffFlag = salt_useDWNSLP
#endif
      IF ( onOffFlag ) THEN
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

c      upward = rkSign*NINT(-gravitySign)
       upward = 1
       IF (usingZCoords) upward = -1

#ifdef ALLOW_DIAGNOSTICS
       IF ( useDiagnostics ) THEN
        IF ( trIdentity.GE.1 ) THEN
C--   Set diagnostic suffix for the current tracer
#ifdef ALLOW_GENERIC_ADVDIFF
          diagSufx = GAD_DIAG_SUFX( trIdentity, myThid )
#else
          diagSufx = 'aaaa'
#endif
          diagName = 'DSLP'//diagSufx
        ELSE
          STOP 'S/R DWNSLP_APPLY: should never reach this point !'
        ENDIF
        doDiagDwnSlpTend = DIAGNOSTICS_IS_ON(diagName,myThid)
       ELSE
        doDiagDwnSlpTend = .FALSE.
       ENDIF
       IF ( doDiagDwnSlpTend ) THEN
         tmpFac = -1. _d 0
         CALL DIAGNOSTICS_SCALE_FILL( gTracer, tmpFac, 1, diagName,
     &                                0, Nr, -2, bi, bj, myThid )
       ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#ifndef TARGET_NEC_SX
       IF ( DWNSLP_ioUnit.GT.0 ) THEN
        _BEGIN_MASTER(myThid)
        WRITE(DWNSLP_ioUnit,'(A,I8,3I4)')
     &   ' DWNSLP_APPLY: iter, iTr, bi,bj=', myIter,trIdentity, bi,bj
        WRITE(DWNSLP_ioUnit,'(2A)') '  bi  bj     n    ijDp    ijSh',
     &   ' kDp   Tr_Dp         Gt_Dp         Tr_Sh         Gt_Sh'
        _END_MASTER(myThid)
       ENDIF
#endif

       DO n=1,DWNSLP_NbSite(bi,bj)
        IF (DWNSLP_deepK(n,bi,bj).NE.0) THEN

C- detect density gradient along the slope => Downsloping flow
         ijd = DWNSLP_ijDeep(n,bi,bj)
         ijs = ijd + DWNSLP_shVsD(n,bi,bj)

         kshelf = kBottom(ijs,bi,bj)
         kDeep = DWNSLP_deepK(n,bi,bj)
C- precomute vertical tracer differences to help TAF
         DO k=kshelf,kDeep+upward,-upward
          dTrac(k) = tracer(ijd,k-upward)-tracer(ijd,k)
         ENDDO
         dTrac(kDeep) = tracer(ijs,kshelf)-tracer(ijd,kDeep)
C- downsloping flow (in) & upward return flow :
         DO k=kshelf,kDeep,-upward
          gTrLoc(k) = DWNSLP_Transp(n,bi,bj)
     &       * dTrac(k)
     &       *recip_drF(k)*recip_hFac(ijd,k)
     &       *recip_rA_arg(ijd,bi,bj)
          gTracer(ijd,k) = gTracer(ijd,k) + gTrLoc(k)
         ENDDO
C- downsloping flow (out) & return flow to the shelf
         k = kshelf
         gTrLoc(0) = DWNSLP_Transp(n,bi,bj)
     &       * ( tracer(ijd,k)-tracer(ijs,k) )
     &       *recip_drF(k)*recip_hFac(ijs,k)
     &       *recip_rA_arg(ijs,bi,bj)
         gTracer(ijs,k) = gTracer(ijs,k) + gTrLoc(0)

#ifndef TARGET_NEC_SX
C-    skip within do-loop write statement when compiling on a vector computer
         IF ( DWNSLP_ioUnit.GT.0 ) THEN
          _BEGIN_MASTER(myThid)
          k = kDeep
          WRITE(DWNSLP_ioUnit,'(2I4,I6,2I8,I4,1P4E14.6)')
     &      bi,bj,n,ijd,ijs,k,
     &      tracer(ijd,k), deltaTLev(k)*gTrLoc(k),
     &      tracer(ijs,kshelf), deltaTLev(k)*gTrLoc(0)
          _END_MASTER(myThid)
         ENDIF
#endif
        ENDIF
       ENDDO
#ifndef TARGET_NEC_SX
       IF ( DWNSLP_ioUnit.GT.0 ) THEN
         _BEGIN_MASTER(myThid)
         WRITE(DWNSLP_ioUnit,*)
         _END_MASTER(myThid)
       ENDIF
#endif

#ifdef ALLOW_DIAGNOSTICS
       IF ( doDiagDwnSlpTend )
     &  CALL DIAGNOSTICS_FILL( gTracer, diagName, 0,Nr,2,bi,bj,myThid )
#endif /* ALLOW_DIAGNOSTICS */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   end if on-off-flag
      ENDIF

#endif /* ALLOW_DOWN_SLOPE */

      RETURN
      END
