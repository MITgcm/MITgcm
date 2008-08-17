C $Header: /u/gcmpack/MITgcm/pkg/down_slope/dwnslp_apply.F,v 1.1 2008/08/17 02:01:27 jmc Exp $
C $Name:  $

#include "DWNSLP_OPTIONS.h"

CBOP
C     !ROUTINE: DWNSLP_APPLY
C     !INTERFACE:
      SUBROUTINE DWNSLP_APPLY(
     I            trIdentity, bi, bj, kBottom,
     I            recip_drF, recip_hFac_arg, recip_rA_arg,
     I            tracer,
     U            trStar,
     I            myTime, myIter, myThid )
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
C     recip_drF  :: Reciprol of cell thickness
C     recip_hFac_arg :: Reciprol of cell open-depth factor
C     recip_rA_arg   :: Reciprol of cell Area
C     tracer     :: tracer field at current time (input)
C     trStar     :: future tracer field (modified)
C     myTime     :: Current time in simulation
C     myIter     :: Current time-step number
C     myThid     :: my Thread Id. number
      INTEGER trIdentity
      INTEGER bi, bj
      INTEGER kBottom( xySize, nSx, nSy )
      _RS recip_drF(Nr)
      _RS recip_rA_arg( xySize, nSx, nSy )
      _RS recip_hFac_arg( xySize, Nr, nSx, nSy )
      _RL tracer        ( xySize, Nr, nSx, nSy )
      _RL trStar        ( xySize, Nr, nSx, nSy )
      _RL     myTime
      INTEGER myIter, myThid

#ifdef ALLOW_DOWN_SLOPE

C     !LOCAL VARIABLES:
C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
      INTEGER ij, k
      INTEGER n,ijd,ijs,kshelf
      _RL     gTrLoc(0:Nr)
      _RL     tmpFld
      INTEGER upward
      LOGICAL onOffFlag

#ifdef ALLOW_DIAGNOSTICS
      CHARACTER*8 diagName
      CHARACTER*4 diagSufx
      LOGICAL     doDiagDwnSlpTend
      _RL         gTracer( xySize, Nr )
C-    Functions:
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON
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
        IF ( doDiagDwnSlpTend ) THEN
         DO k=1,Nr
          DO ij=1,xySize
           gTracer(ij,k) = 0. _d 0
          ENDDO
         ENDDO
        ENDIF
       ELSE
        doDiagDwnSlpTend = .FALSE.
       ENDIF
#endif /* ALLOW_DIAGNOSTICS */

       IF (debugMode) THEN
        WRITE(DWNSLP_ioUnit,'(A,I8,3I4)')
     &   ' DWNSLP_APPLY: iter, iTr, bi,bj=', myIter,trIdentity, bi,bj
        WRITE(DWNSLP_ioUnit,'(2A)') '  bi  bj     n    ijDp    ijSh',
     &   ' kDp   Tr_Dp         Gt_Dp         Tr_Sh         Gt_Sh'
       ENDIF

       DO n=1,DWNSLP_NbSite(bi,bj)
        IF (DWNSLP_deepK(n,bi,bj).NE.0) THEN

C- detect density dradient along the slope => Downsloping flow

         ijd = DWNSLP_ijDeep(n,bi,bj)
         ijs = ijd + DWNSLP_shVsD(n,bi,bj)

         kshelf = kBottom(ijs,bi,bj)
         tmpFld = tracer(ijs,kshelf,bi,bj)
c- downsloping flow (in) & upward return flow :
         DO k=DWNSLP_deepK(n,bi,bj),kshelf,upward
          gTrLoc(k) = DWNSLP_Transp(n,bi,bj)
     &       *( tmpFld - tracer(ijd,k,bi,bj) )
     &       *recip_drF(k)*recip_hFac_arg(ijd,k,bi,bj)
     &       *recip_rA_arg(ijd,bi,bj)
          trStar(ijd,k,bi,bj) = trStar(ijd,k,bi,bj)
     &                        + dTtracerLev(k)*gTrLoc(k)
          tmpFld = tracer(ijd,k,bi,bj)
         ENDDO
c- downsloping flow (out) & return flow to the shelf
          k = kshelf
          gTrLoc(0) = DWNSLP_Transp(n,bi,bj)
     &       *( tmpFld - tracer(ijs,k,bi,bj) )
     &       *recip_drF(k)*recip_hFac_arg(ijs,k,bi,bj)
     &       *recip_rA_arg(ijs,bi,bj)
          trStar(ijs,k,bi,bj) = trStar(ijs,k,bi,bj)
     &                        + dTtracerLev(k)*gTrLoc(0)

#ifdef ALLOW_DIAGNOSTICS
         IF ( doDiagDwnSlpTend ) THEN
           gTracer(ijs,k) = gTracer(ijs,k) + gTrLoc(0)
          DO k=DWNSLP_deepK(n,bi,bj),kshelf,upward
           gTracer(ijd,k) = gTracer(ijd,k) + gTrLoc(k)
          ENDDO
         ENDIF
#endif /* ALLOW_DIAGNOSTICS */

         IF (debugMode) THEN
          k=DWNSLP_deepK(n,bi,bj)
          WRITE(DWNSLP_ioUnit,'(2I4,I6,2I8,I4,1P4E14.6)')
     &      bi,bj,n,ijd,ijs,k,
     &      tracer(ijd,k,bi,bj),
     &      dTtracerLev(k)*DWNSLP_Transp(n,bi,bj)
     &       *recip_drF(k)*recip_hFac_arg(ijd,k,bi,bj)
     &       *recip_rA_arg(ijd,bi,bj)*
     &        (tracer(ijs,kshelf,bi,bj)-tracer(ijd,k,bi,bj)),
     &      tracer(ijs,kshelf,bi,bj),
     &      dTtracerLev(k)*DWNSLP_Transp(n,bi,bj)
     &       *recip_drF(kshelf)*recip_hFac_arg(ijs,kshelf,bi,bj)
     &       *recip_rA_arg(ijs,bi,bj)*
     &        (tmpFld-tracer(ijs,kshelf,bi,bj))
         ENDIF
        ENDIF
       ENDDO
       IF (debugMode) WRITE(DWNSLP_ioUnit,*)

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
