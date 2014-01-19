C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_list_check.F,v 1.6 2014/01/19 14:42:58 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: DIAGNOSTICS_LIST_CHECK

C     !INTERFACE:
      SUBROUTINE DIAGNOSTICS_LIST_CHECK(
     O                       ndCount,
     I                       ld, md,
     I                       nbLists, nbFlds, fldsList, myThid )

C     !DESCRIPTION:
C     Check a list of fields for disable diagnostics,
C     starting with the current one:
C     print relevant error message and return the number
C     of disable diagnostics that this list contains.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"

C     !INPUT PARAMETERS:
C     ld       :: current ouput stream number
C     md       :: current field in ouput stream ld
C     nbLists  :: number of output streams
C     nbFlds(n):: number of diagnostics for output stream # n
C     fldsList :: list of field names to check
C     myThid   :: my Thread Id. number
      INTEGER ld, md, nbLists
      INTEGER nbFlds(numLists)
      CHARACTER*8 fldsList(numperList,numLists)
      INTEGER myThid
C     !OUTPUT PARAMETERS:
C     ndCount  :: number of disable diagnostics in list "fldsList"
      INTEGER ndCount
CEOP

C     !LOCAL VARIABLES:
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL found
      CHARACTER*8 diagName
      INTEGER n,m,m1,i

      ndCount = 0
      _BEGIN_MASTER(myThid)

C-    File names:
      m1 = md
      DO n = ld,nbLists
       DO m = m1,nbFlds(n)

        diagName = fldsList(m,n)
        found = .FALSE.
        IF ( ndCount.GT.0 ) THEN
C        Search all possible model diagnostics
         DO i=1,ndiagt
          IF ( diagName.EQ.cdiag(i) ) found = .TRUE.
         ENDDO
        ENDIF
        IF ( .NOT.found ) THEN
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
         IF ( diagName.EQ.'PRESSURE' ) THEN
           WRITE(msgBuf,'(3A)') 'DIAGNOSTICS_LIST_CHECK: diagnostic "',
     &             diagName, '" was replaced by "RCENTER "'
           CALL PRINT_ERROR( msgBuf , myThid )
           WRITE(msgBuf,'(3A)') 'DIAGNOSTICS_LIST_CHECK: ',
     &              '=> need to change "data.diagnostics" accordingly'
           CALL PRINT_ERROR( msgBuf , myThid )
           ndCount = ndCount + 1
         ENDIF
         IF ( diagName.EQ.'TICE    ' ) THEN
           WRITE(msgBuf,'(3A)') 'DIAGNOSTICS_LIST_CHECK: diagnostic "',
     &             diagName, '" was replaced by "oceFreez"'
           CALL PRINT_ERROR( msgBuf , myThid )
           WRITE(msgBuf,'(3A)') 'DIAGNOSTICS_LIST_CHECK: ',
     &              '=> need to change "data.diagnostics" accordingly'
           CALL PRINT_ERROR( msgBuf , myThid )
           ndCount = ndCount + 1
         ENDIF
         IF ( diagName.EQ.'TAUX    ' ) THEN
           WRITE(msgBuf,'(3A)') 'DIAGNOSTICS_LIST_CHECK: diagnostic "',
     &             diagName, '" was replaced by "oceTAUX "'
           CALL PRINT_ERROR( msgBuf , myThid )
           WRITE(msgBuf,'(3A)') 'DIAGNOSTICS_LIST_CHECK: ',
     &              '=> need to change "data.diagnostics" accordingly'
           CALL PRINT_ERROR( msgBuf , myThid )
           ndCount = ndCount + 1
         ENDIF
         IF ( diagName.EQ.'TAUY    ' ) THEN
           WRITE(msgBuf,'(3A)') 'DIAGNOSTICS_LIST_CHECK: diagnostic "',
     &             diagName, '" was replaced by "oceTAUY "'
           CALL PRINT_ERROR( msgBuf , myThid )
           WRITE(msgBuf,'(3A)') 'DIAGNOSTICS_LIST_CHECK: ',
     &              '=> need to change "data.diagnostics" accordingly'
           CALL PRINT_ERROR( msgBuf , myThid )
           ndCount = ndCount + 1
         ENDIF
         IF ( diagName.EQ.'SWFLUX  ' ) THEN
           WRITE(msgBuf,'(3A)') 'DIAGNOSTICS_LIST_CHECK: diagnostic "',
     &             diagName, '" was replaced by "oceQsw  "'
           CALL PRINT_ERROR( msgBuf , myThid )
           WRITE(msgBuf,'(3A)') 'DIAGNOSTICS_LIST_CHECK: ',
     &              '=> need to change "data.diagnostics" accordingly'
           CALL PRINT_ERROR( msgBuf , myThid )
           ndCount = ndCount + 1
         ENDIF
         IF ( diagName(1:5).EQ.'DIFx_' .OR.
     &        diagName(1:5).EQ.'DIFxT' ) THEN
           WRITE(msgBuf,'(5A)') 'DIAGNOSTICS_LIST_CHECK: diagnostic "',
     &             diagName, '" was replaced by "DFxE',
     &             diagName(5:8), '"'
           CALL PRINT_ERROR( msgBuf , myThid )
           WRITE(msgBuf,'(3A)') 'DIAGNOSTICS_LIST_CHECK: ',
     &              '=> need to change "data.diagnostics" accordingly'
           CALL PRINT_ERROR( msgBuf , myThid )
           ndCount = ndCount + 1
         ENDIF
         IF ( diagName(1:5).EQ.'DIFy_' .OR.
     &        diagName(1:5).EQ.'DIFyT' ) THEN
           WRITE(msgBuf,'(5A)') 'DIAGNOSTICS_LIST_CHECK: diagnostic "',
     &             diagName, '" was replaced by "DFyE',
     &             diagName(5:8), '"'
           CALL PRINT_ERROR( msgBuf , myThid )
           WRITE(msgBuf,'(3A)') 'DIAGNOSTICS_LIST_CHECK: ',
     &              '=> need to change "data.diagnostics" accordingly'
           CALL PRINT_ERROR( msgBuf , myThid )
           ndCount = ndCount + 1
         ENDIF
         IF ( diagName.EQ.'KPPmld  ' ) THEN
           WRITE(msgBuf,'(3A)') 'DIAGNOSTICS_LIST_CHECK: diagnostic "',
     &             diagName, '" was retired in favor of "MXLDEPTH"'
           CALL PRINT_ERROR( msgBuf , myThid )
           WRITE(msgBuf,'(3A)') 'DIAGNOSTICS_LIST_CHECK: ',
     &              '=> need to change "data.diagnostics" accordingly'
           CALL PRINT_ERROR( msgBuf , myThid )
           ndCount = ndCount + 1
         ENDIF
         IF ( diagName.EQ.'KPPghat ' ) THEN
           WRITE(msgBuf,'(3A)') 'DIAGNOSTICS_LIST_CHECK: diagnostic "',
     &             diagName, '" was retired in favor of "KPPghatK"'
           CALL PRINT_ERROR( msgBuf , myThid )
           WRITE(msgBuf,'(3A)') 'DIAGNOSTICS_LIST_CHECK: ',
     &              '=> need to change "data.diagnostics" accordingly'
           CALL PRINT_ERROR( msgBuf , myThid )
           ndCount = ndCount + 1
         ENDIF
         IF ( diagName.EQ.'SIyneg  ' ) THEN
           WRITE(msgBuf,'(3A)') 'DIAGNOSTICS_LIST_CHECK: diagnostic "',
     &             diagName, '" was retired in favor of "SIdHbOCN"'
           CALL PRINT_ERROR( msgBuf , myThid )
           WRITE(msgBuf,'(3A)') 'DIAGNOSTICS_LIST_CHECK: ',
     &              '=> need to change "data.diagnostics" accordingly'
           CALL PRINT_ERROR( msgBuf , myThid )
           ndCount = ndCount + 1
         ENDIF
         IF ( diagName.EQ.'SIfice  ' ) THEN
           WRITE(msgBuf,'(3A)') 'DIAGNOSTICS_LIST_CHECK: diagnostic "',
     &             diagName, '" was retired in favor of "SIdHbATC"'
           CALL PRINT_ERROR( msgBuf , myThid )
           WRITE(msgBuf,'(3A)') 'DIAGNOSTICS_LIST_CHECK: ',
     &              '=> need to change "data.diagnostics" accordingly'
           CALL PRINT_ERROR( msgBuf , myThid )
           ndCount = ndCount + 1
         ENDIF
         IF ( diagName.EQ.'SIsnwice' ) THEN
           WRITE(msgBuf,'(3A)') 'DIAGNOSTICS_LIST_CHECK: diagnostic "',
     &             diagName, '" was retired in favor of "SIdHbFLO"'
           CALL PRINT_ERROR( msgBuf , myThid )
           WRITE(msgBuf,'(3A)') 'DIAGNOSTICS_LIST_CHECK: ',
     &              '=> need to change "data.diagnostics" accordingly'
           CALL PRINT_ERROR( msgBuf , myThid )
           ndCount = ndCount + 1
         ENDIF
         IF ( diagName.EQ.'SIthdgrh' ) THEN
           WRITE(msgBuf,'(3A)') 'DIAGNOSTICS_LIST_CHECK: diagnostic "',
     &             diagName, '" was retired in favor of "SIdHb*"'
           CALL PRINT_ERROR( msgBuf , myThid )
           WRITE(msgBuf,'(3A)') 'DIAGNOSTICS_LIST_CHECK: ',
     &              '=> need to change "data.diagnostics" accordingly'
           CALL PRINT_ERROR( msgBuf , myThid )
           ndCount = ndCount + 1
         ENDIF
         IF ( diagName.EQ.'SI_FrcFx' ) THEN
           WRITE(msgBuf,'(3A)') 'DIAGNOSTICS_LIST_CHECK: diagnostic "',
     &             diagName, '" was retired (identical to "SI_Fract")'
           CALL PRINT_ERROR( msgBuf , myThid )
           WRITE(msgBuf,'(3A)') 'DIAGNOSTICS_LIST_CHECK: ',
     &              '=> need to change "data.diagnostics" accordingly'
           CALL PRINT_ERROR( msgBuf , myThid )
           ndCount = ndCount + 1
         ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
        ENDIF
        IF ( ndCount.EQ.0 ) RETURN

       ENDDO
       m1 = 1
      ENDDO

      _END_MASTER(myThid)

      RETURN
      END
