C $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_readparms.F,v 1.3 2004/03/22 05:10:10 edhill Exp $
C $Name:  $

#include "MNC_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MNC_READPARMS( myThid )

      implicit none

#include "SIZE.h"
#include "mnc_common.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     Arguments
      integer myThid

C     Functions
      integer ILNBLNK

C     Local Variables
      integer i, iUnit, nl, isu1,isu2,mdu, errio, IL
      character*(MAX_LEN_MBUF) data_file
      character*(MAX_LEN_MBUF) msgBuf
      CHARACTER*(MAX_LEN_PREC) record
      NAMELIST /MNC_01/
     &     useMNC,
     &     mnc_echo_gtypes,
     &     mnc_pickup_create,
     &     mnc_pickup_read
      LOGICAL  exst

C     Default values MNC
      useMNC             =  .FALSE.
      mnc_echo_gtypes    =  .FALSE.
      mnc_pickup_create  =  .FALSE.
      mnc_pickup_read    =  .FALSE.
      
C     Set the file name
      DO i=1,MAX_LEN_MBUF
        data_file(i:i) = ' '
      ENDDO
      WRITE(data_file,'(a)') 'data.mnc'
      nl = ILNBLNK(data_file)

C     Verify that the file exists and, if not, disable MNC
      INQUIRE( FILE=data_file, EXIST=exst )
      IF (exst) THEN
        WRITE(msgbuf,'(3a)')
     &       ' MNC_READPARMS: opening file ''',
     &       data_file(1:nl), ''''
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &       SQUEEZE_RIGHT , mythid)
      ELSE
        WRITE(msgBuf,'(3a)')
     &       'Data file: ''',data_file(1:nl),
     &       ''' does not exist so MNC will be disabled'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &       SQUEEZE_RIGHT , mythid)
        RETURN
      ENDIF

C     Open files
      isu1 = 60
      isu2 = 61
      mdu = 62
      OPEN(UNIT=isu1, STATUS='SCRATCH')
      OPEN(UNIT=isu2, STATUS='SCRATCH')
      OPEN(UNIT=mdu, FILE=data_file, STATUS='OLD', IOSTAT=errio)
      IF ( errio .LT. 0 ) THEN
        WRITE(msgBuf,'(3a)')
     &       'Unable to open data file: ''',data_file(1:nl),
     &       ''' so MNC will be disabled'
        CALL PRINT_ERROR( msgBuf , 1)
        STOP 'ABNORMAL END: S/R MNC_READPARMS'
        RETURN
      ENDIF     
      
      DO WHILE ( .TRUE. )
        READ(mdu,FMT='(A)',END=1001) RECORD
        IL = MAX(ILNBLNK(RECORD),1)
        IF ( RECORD(1:1) .NE. commentCharacter ) THEN
          CALL NML_SET_TERMINATOR( RECORD )
          WRITE(UNIT=isu1,FMT='(A)') RECORD(:IL)
        ENDIF
        WRITE(UNIT=isu2,FMT='(A)') RECORD(:IL)
      ENDDO
 1001 CONTINUE
      CLOSE(mdu)
      
C--   Report contents of model parameter file
      WRITE(msgBuf,'(A)') 
     &     '// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &     SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(3a)') '// Parameter file "',data_file(1:nl),'"'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &     SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)') 
     &     '// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT , 1)
      iUnit = isu2
      REWIND(iUnit)
      DO WHILE ( .TRUE. )
        READ(UNIT=iUnit,FMT='(A)',END=2001) RECORD
        IL = MAX(ILNBLNK(RECORD),1)
        WRITE(msgBuf,'(2a)') '>',RECORD(:IL)
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &       SQUEEZE_RIGHT , 1)
      ENDDO
 2001 CONTINUE
      CLOSE(iUnit)
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT , 1)

      iUnit = isu1
      REWIND(iUnit)
      READ(UNIT=iUnit,NML=MNC_01,IOSTAT=errio,err=3)
      GOTO 4
 3    CONTINUE
      WRITE(msgBuf,'(A,A,A)')
     &     'ERROR: while reading file ''',data_file(1:nl), 
     &     ''' -- please check file contents'
      CALL PRINT_ERROR( msgBuf , 1)
      STOP 'ABNORMAL END: S/R MNC_READPARMS'
 4    CONTINUE

      WRITE(msgBuf,'(a)') ' MNC_READPARMS: finished reading data.mnc'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT , 1)

      CALL MNC_SET_OUTDIR(myThid)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MNC_SET_OUTDIR( myThid )

      implicit none

#include "SIZE.h"
#include "mnc_common.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     Arguments
      integer myThid

C     Functions
      integer ILNBLNK

C     Local Variables
      integer i, nstr, iodate, ioseq, num

#define HAVE_MNCCDIR
#ifdef HAVE_MNCCDIR
      CALL mnccdir(iodate, ioseq)
#else
      iodate = 1
      ioseq = 1
#endif

      DO i = 1,MNC_MAX_CHAR
        mnc_out_path(i:i) = ' '
      ENDDO
      WRITE(mnc_out_path,'(a4,i8,a1,i4,a1)') 
     &     'mnc_', iodate, '_', ioseq, '/'
      nstr = ILNBLNK(mnc_out_path)
      DO i = 1,nstr
        IF (mnc_out_path(i:i) .EQ. ' ')  mnc_out_path(i:i) = '0'
      ENDDO
      num = ILNBLNK(mnc_out_path)
      write(*,*) 'mnc_out_path = ', mnc_out_path(1:num)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
