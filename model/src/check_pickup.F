c#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: CHECK_PICKUP
C     !INTERFACE:
      SUBROUTINE CHECK_PICKUP(
     I                 missFldList,
     I                 nMissing, nbFields,
     I                 myIter, myThid )

C     !DESCRIPTION:
C     Check that fields that are needed to restart have been read.
C     In case some fields are missing, stop if pickupStrictlyMatch=T
C     or try, if possible, to restart without the missing field.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "RESTART.h"
c#ifdef ALLOW_GENERIC_ADVDIFF
c# include "GAD.h"
c#endif

C     !INPUT/OUTPUT PARAMETERS:
C     missFldList :: List of missing fields   (attempted to read but not found)
C     nMissing    :: Number of missing fields (attempted to read but not found)
C     nbFields    :: number of fields in pickup file (read from meta file)
C     myIter      :: Iteration number
C     myThid      :: my Thread Id. number
      CHARACTER*(8) missFldList(*)
      INTEGER nMissing
      INTEGER nbFields
      INTEGER myIter
      INTEGER myThid
CEOP

C     !FUNCTIONS
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      INTEGER j
      INTEGER ioUnit
      INTEGER warnCnts
      LOGICAL stopFlag
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      ioUnit = errorMessageUnit

c     IF (pickup_read_mdsio) THEN
      _BEGIN_MASTER( myThid )

       IF ( nbFields.GE.1 ) THEN
C-     flag startFromPickupAB2 is becoming obsolete with new way to read
C      pickup file: cancel its effect (from initialisation) by resetting
C      start-AB parameters:
         tempStartAB = nIter0
         saltStartAB = nIter0
         mom_StartAB = nIter0
         nHydStartAB = nIter0
       ENDIF
       IF ( selectNHfreeSurf.GE.1 ) THEN
         IF ( nbFields.EQ.0 ) THEN
           WRITE(msgBuf,'(4A)') '** WARNING ** CHECK_PICKUP: ',
     &      'restart like hydrostatic free-surf (dPhiNH missing)'
           CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
         ELSE
C-     assume reading dPhiNH was OK (otherwise expected in missing field list)
           dPhiNHstatus = 1
         ENDIF
       ENDIF

       IF ( nMissing.GE.1 ) THEN
        stopFlag = .FALSE.
        warnCnts = nMissing
        DO j=1,nMissing
C-    Case where missing field is not essential or can be recomputed
         IF     ( missFldList(j).EQ.'dEtaHdt '
     &        .AND. .NOT.useRealFreshWaterFlux ) THEN
          warnCnts = warnCnts - 1
          IF ( .NOT.pickupStrictlyMatch ) THEN
           WRITE(msgBuf,'(4A)') ' CHECK_PICKUP: ',
     &      'no RealFreshWaterFlux => can restart without "dEtaHdt "'
           CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
          ENDIF
         ELSEIF ( missFldList(j).EQ.'dPhiNH  '
     &        .AND. implicitNHPress.EQ.1. _d 0 ) THEN
          warnCnts = warnCnts - 1
          dPhiNHstatus = 0
          IF ( .NOT.pickupStrictlyMatch ) THEN
           WRITE(msgBuf,'(4A)') ' CHECK_PICKUP: ',
     &      'fully Implic.NH-Press => can restart without "dPhiNH  "'
           CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
          ENDIF
C-    Old pickup for which special code takes care of missing fields
         ELSEIF ( missFldList(j).EQ.'dEtaHdt '
     &                 .AND.usePickupBeforeC54 ) THEN
C-    with RealFreshWaterFlux, needs dEtaHdt to restart when:
C     * synchronousTimeStep & usingPCoords => needs PmEpR for surf-forcing
C         <- present code might be wrong if usePickupBeforeC54 and LinFS
C     * synchronousTimeStep & nonlinFreeSurf > 0 => needs PmEpR for surf-forcing
C     * select_rStar <> 0 => needs dEtaHdt for 1rst Integr_continuity
          IF ( .NOT.pickupStrictlyMatch ) THEN
           WRITE(msgBuf,'(4A)') '** WARNING ** CHECK_PICKUP: ',
     &      'restart as before C54 without "dEtaHdt "'
           CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
          ENDIF

C-    fields used only to speed-up solver(s) convergence:
C     (no serious problems expected if missing, but get a non-perfect restart)
         ELSEIF ( missFldList(j).EQ.'EtaN    '
     &                          .AND. rigidLid ) THEN
          IF ( .NOT.pickupStrictlyMatch ) THEN
           WRITE(msgBuf,'(4A)') '** WARNING ** CHECK_PICKUP: ',
     &      'restart with 1rst guess == 0 for CG2D solver'
           CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
          ENDIF
         ELSEIF ( missFldList(j).EQ.'Phi_NHyd' ) THEN
          IF ( .NOT.pickupStrictlyMatch ) THEN
           WRITE(msgBuf,'(4A)') '** WARNING ** CHECK_PICKUP: ',
     &      'restart with 1rst guess == 0 for CG3D solver'
           CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
          ENDIF
         ELSEIF ( missFldList(j).EQ.'dPhiNH  ' ) THEN
          dPhiNHstatus = 0
          IF ( .NOT.pickupStrictlyMatch ) THEN
           WRITE(msgBuf,'(4A)') '** WARNING ** CHECK_PICKUP: ',
     &      'restart like hydrostatic free-surf (dPhiNH missing)'
           CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
          ENDIF
         ELSEIF ( missFldList(j).EQ.'AddMass '
     &               .AND. selectAddFluid.EQ.2 ) THEN
          IF ( .NOT.pickupStrictlyMatch ) THEN
           WRITE(msgBuf,'(4A)') '** WARNING ** CHECK_PICKUP: ',
     &      'restart with AddMass == 0'
           CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
          ENDIF
         ELSEIF ( missFldList(j).EQ.'SmagDiff' ) THEN
          IF ( .NOT.pickupStrictlyMatch ) THEN
           WRITE(msgBuf,'(4A)') '** WARNING ** CHECK_PICKUP: ',
     &      'restart with zero Smag-3D Diffusivity for first time-step'
           CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
          ENDIF
         ELSEIF ( missFldList(j).EQ.'FricHeat' ) THEN
          IF ( .NOT.pickupStrictlyMatch ) THEN
           WRITE(msgBuf,'(4A)') '** WARNING ** CHECK_PICKUP: ',
     &      'restart with Frictional Dissipation Heating == 0'
           CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
          ENDIF

C-    Absolutely needed fields:
         ELSEIF ( missFldList(j).EQ.'Uvel    ' .OR.
     &            missFldList(j).EQ.'Vvel    ' .OR.
     &            missFldList(j).EQ.'Theta   ' .OR.
     &            missFldList(j).EQ.'Salt    ' .OR.
     &            missFldList(j).EQ.'EtaN    ' ) THEN
           stopFlag = .TRUE.
           WRITE(msgBuf,'(4A)') 'CHECK_PICKUP: ',
     &       'cannot restart without field "',missFldList(j),'"'
           CALL PRINT_ERROR( msgBuf, myThid )

C-    fields needed for restart (alternative not presently implemented)
         ELSEIF ( missFldList(j).EQ.'PhiHyd  ' .OR.
     &            missFldList(j).EQ.'Phi_rLow' .OR.
     &            missFldList(j).EQ.'AddMass ' .OR.
     &            missFldList(j).EQ.'dEtaHdt ' .OR.
     &            missFldList(j).EQ.'EtaH    ' ) THEN
           stopFlag = .TRUE.
           WRITE(msgBuf,'(4A)') 'CHECK_PICKUP: ',
     &     'cannot currently restart without field "',missFldList(j),'"'
           CALL PRINT_ERROR( msgBuf, myThid )

C-    fields with alternative in place to restart without:
C-    (but get a non-perfect restart)
         ELSEIF ( missFldList(j).EQ.'GuNm1   ' .OR.
     &            missFldList(j).EQ.'GvNm1   ' ) THEN
           mom_StartAB = 0
         ELSEIF ( missFldList(j).EQ.'GuNm2   ' .OR.
     &            missFldList(j).EQ.'GvNm2   ' ) THEN
           mom_StartAB = MIN( mom_startAB, 1 )
         ELSEIF ( missFldList(j).EQ.'GtNm1   ' .OR.
     &            missFldList(j).EQ.'TempNm1 ' ) THEN
           tempStartAB = 0
         ELSEIF ( missFldList(j).EQ.'GtNm2   ' .OR.
     &            missFldList(j).EQ.'TempNm2 ' ) THEN
           tempStartAB = MIN( tempStartAB, 1 )
         ELSEIF ( missFldList(j).EQ.'GsNm1   ' .OR.
     &            missFldList(j).EQ.'SaltNm1 ' ) THEN
           saltStartAB = 0
         ELSEIF ( missFldList(j).EQ.'GsNm2   ' .OR.
     &            missFldList(j).EQ.'SaltNm2 ' ) THEN
           saltStartAB = MIN( saltStartAB, 1 )
         ELSEIF ( missFldList(j).EQ.'GwNm1   ' ) THEN
           nHydStartAB = 0
         ELSEIF ( missFldList(j).EQ.'GwNm2   ' ) THEN
           nHydStartAB = MIN( nHydStartAB, 1 )
         ELSEIF ( missFldList(j).EQ.'QH_GwNm1' ) THEN
           qHydStartAB = 0
         ELSEIF ( missFldList(j).EQ.'QH_GwNm2' ) THEN
           qHydStartAB = MIN( qHydStartAB, 1 )

         ELSE
C-    not recognized fields:
           stopFlag = .TRUE.
           WRITE(msgBuf,'(4A)') 'CHECK_PICKUP: ',
     &       'missing field "',missFldList(j),'" not recognized'
           CALL PRINT_ERROR( msgBuf, myThid )
         ENDIF
        ENDDO

        IF ( stopFlag ) THEN
         STOP 'ABNORMAL END: S/R CHECK_PICKUP'
        ELSEIF ( pickupStrictlyMatch ) THEN
         WRITE(msgBuf,'(4A)') 'CHECK_PICKUP: ',
     &      'try with " pickupStrictlyMatch=.FALSE.,"',
     &      ' in file: "data", NameList: "PARM03"'
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R CHECK_PICKUP'
        ELSEIF ( warnCnts .GT. 0 ) THEN
         WRITE(msgBuf,'(4A)') '** WARNING ** CHECK_PICKUP: ',
     &     'Will get only an approximated Restart'
         CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
         IF ( mom_StartAB.LT.nIter0 .OR.
     &        nHydStartAB.LT.nIter0 .OR.
     &        tempStartAB.LT.nIter0 .OR.
     &        saltStartAB.LT.nIter0 ) THEN
          WRITE(msgBuf,'(2(A,I10))')
     &     ' Continue with mom_StartAB =', mom_StartAB,
     &                 ' ; nHydStartAB =', nHydStartAB
          CALL PRINT_MESSAGE(msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
          WRITE(msgBuf,'(2(A,I10))')
     &     '          with tempStartAB =', tempStartAB,
     &                 ' ; saltStartAB =', saltStartAB
          CALL PRINT_MESSAGE(msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
         ENDIF
         IF ( qHydStartAB.LT.nIter0 ) THEN
          WRITE(msgBuf,'(2(A,I10))')
     &     ' Continue with qHydStartAB =', qHydStartAB
          CALL PRINT_MESSAGE(msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
         ENDIF
        ENDIF

       ENDIF

      _END_MASTER( myThid )
c     ENDIF

      RETURN
      END
