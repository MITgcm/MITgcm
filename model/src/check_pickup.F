C $Header: /u/gcmpack/MITgcm/model/src/check_pickup.F,v 1.1 2007/10/23 15:22:04 jmc Exp $
C $Name:  $

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
c#include "DYNVARS.h"
c#include "SURFACE.h"
c#ifdef ALLOW_GENERIC_ADVDIFF
c# include "GAD.h"
c#endif
c#ifdef ALLOW_NONHYDROSTATIC
c#include "NH_VARS.h"
c#endif
c#ifdef ALLOW_MNC
c#include "MNC_PARAMS.h"
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
      LOGICAL stopFlag
      CHARACTER*(MAX_LEN_MBUF) msgBuf

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

       IF ( nMissing.GE.1 ) THEN
        stopFlag = .FALSE.
        DO j=1,nMissing
C-    Old pickup for which special code takes care of missing fields
         IF     ( missFldList(j).EQ.'dEtaHdt '
     &                 .AND.usePickupBeforeC54 ) THEN

C-    Absolutely needed fields:
         ELSEIF ( missFldList(j).EQ.'Uvel    ' .OR.
     &            missFldList(j).EQ.'Vvel    ' .OR.
     &            missFldList(j).EQ.'Theta   ' .OR.
     &            missFldList(j).EQ.'Salt    ' .OR.
     &            missFldList(j).EQ.'EtaN    ' .OR.
     &            missFldList(j).EQ.'PhiHyd  ' .OR.
c    &            missFldList(j).EQ.'dEtaHdt ' .OR.
     &            missFldList(j).EQ.'EtaH    ' .OR.
     &            missFldList(j).EQ.'Phi_NHyd' ) THEN
           stopFlag = .TRUE.
           WRITE(msgBuf,'(4A)') 'CHECK_PICKUP: ',
     &       'cannot restart without field "',missFldList(j),'"'
           CALL PRINT_ERROR( msgBuf, myThid )

C-    missing fields which allow a non-perfect restart :
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
        ELSE
         WRITE(msgBuf,'(4A)') '** WARNINGS ** CHECK_PICKUP: ',
     &     'Will get only an approximated Restart'
         CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
         WRITE(msgBuf,'(2(A,I10))')
     &     ' Continue with mom_StartAB =', mom_StartAB,
     &                 ' ; nHydStartAB =', nHydStartAB
         CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
         WRITE(msgBuf,'(2(A,I10))')
     &     '          with tempStartAB =', tempStartAB,
     &                 ' ; saltStartAB =', saltStartAB
         CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
        ENDIF

       ENDIF

      _END_MASTER( myThid )
c     ENDIF

      RETURN
      END
