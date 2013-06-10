C $Header: /u/gcmpack/MITgcm/pkg/cfc/cfc_atmos.F,v 1.5 2013/06/10 02:51:47 jmc Exp $
C $Name:  $

#include "GCHEM_OPTIONS.h"

CBOP
C     !ROUTINE: CFC_ATMOS
C     !INTERFACE:
      SUBROUTINE CFC_ATMOS( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE CFC_ATMOS
C     | o read in time-series of atmoshperic CFC
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CFC.h"
#include "CFC_ATMOS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid    :: My Thread Id. number
      INTEGER myThid
CEOP

#ifdef ALLOW_CFC
C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
C     msgBuf     :: message buffer
      INTEGER iUnit, i, it, iL
      LOGICAL exst
      _RL tmpVar(5)
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      _BEGIN_MASTER( myThid )

C read in CFC atmospheric timeseries data
      iL = ILNBLNK(atmCFC_inpFile)
      IF ( iL.EQ.0 ) THEN
        WRITE(msgBuf,'(A)')
     &       'CFC_ATMOS: File-name missing for atmos CFC time-series'
        CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R CFC_ATMOS'
      ENDIF
      INQUIRE( FILE=atmCFC_inpFile(1:iL), EXIST=exst )
      IF (exst) THEN
       WRITE(msgBuf,'(3A)')
     &   'CFC_ATMOS: opening file "', atmCFC_inpFile(1:iL), '"'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
      ELSE
       WRITE(msgBuf,'(3A)')
     &   'CFC_ATMOS: File "', atmCFC_inpFile(1:iL),'" does not exist!'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R CFC_ATMOS'
      ENDIF

C assign a free unit number as the I/O channel for this subroutine
      CALL MDSFINDUNIT( iUnit, myThid )
      OPEN(iUnit,FILE=atmCFC_inpFile(1:iL),STATUS='old')
C skip 6 descriptor lines
      DO i =1,6
        READ(iUnit,*)
      ENDDO
C Read in CFC11 and CFC12, N and S Hemisphere time histories
      it = 0
      DO WHILE ( it.LE.ACFCrecSize )
        READ(iUnit,*,END=1001) (tmpVar(i),i=1,5)
        it = it + 1
        IF ( it .LE. ACFCrecSize ) THEN
          ACFCyear(it) = tmpVar(1)
          ACFC11(it,1) = tmpVar(2)
          ACFC12(it,1) = tmpVar(3)
          ACFC11(it,2) = tmpVar(4)
          ACFC12(it,2) = tmpVar(5)
        ENDIF
      ENDDO
      IF ( it.GT.ACFCrecSize ) THEN
       CLOSE(iUnit)
       WRITE(msgBuf,'(3A)')
     &   'CFC_ATMOS: length of file "',atmCFC_inpFile(1:iL),'"'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(2A,I9)') 'CFC_ATMOS: exceeds max num',
     &             ' of records: ACFCrecSize=', ACFCrecSize
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R CFC_ATMOS'
      ENDIF
 1001 CONTINUE
      CLOSE(iUnit)
      ACFCnRec = it

C--  Print values to check:
      WRITE(msgBuf,'(A,I8,A)')
     &  'CFC_ATMOS: read', ACFCnRec, ' (=ACFCnRec) time records :'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &  '  year , cfc11_N, cfc12_N, cfc11_S, cfc12_S'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      DO it = 1,ACFCnRec
        WRITE(msgBuf,'(F7.1,4F9.2)')
     &        ACFCyear(it), ACFC11(it,1),ACFC12(it,1),
     &                      ACFC11(it,2),ACFC12(it,2)
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      ENDDO

      WRITE(msgBuf,'(A)')
     &  'CFC_ATMOS: Setting atmos CFC time series: done'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_CFC */

      RETURN
      END
