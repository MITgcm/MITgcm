C $Header: /u/gcmpack/MITgcm/pkg/cal/cal_checkdate.F,v 1.5 2012/04/07 16:20:07 jmc Exp $
C $Name:  $

#include "CAL_OPTIONS.h"

      SUBROUTINE CAL_CHECKDATE(
     I                          date,
     O                          valid,
     O                          calerr,
     I                          myThid )

C     ==================================================================
C     SUBROUTINE cal_CheckDate
C     ==================================================================
C
C     o Check whether the array date conforms with the required format.
C
C     started: Christian Eckert eckert@mit.edu  30-Jun-1999
C     changed: Christian Eckert eckert@mit.edu  29-Dec-1999
C              - restructured the original version in order to have a
C                better interface to the MITgcmUV.
C              Christian Eckert eckert@mit.edu  03-Feb-2000
C              - Introduced new routine and function names, cal_<NAME>,
C                for verion 0.1.3.
C              21-Sep-2003: fixed check_sign logic to work with
C              negative intervals (menemenlis@jpl.nasa.gov)
C
C     ==================================================================
C     SUBROUTINE cal_CheckDate
C     ==================================================================

      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "cal.h"

C     == routine arguments ==
      INTEGER date(4)
      LOGICAL valid
      INTEGER calerr
      INTEGER myThid

C     == local variables ==
C     msgBuf     :: Informational/error message buffer
      INTEGER yy,mm,dd
      INTEGER nsecs
      INTEGER lp,wd
      INTEGER hhmmss
      INTEGER yymmdd
      INTEGER fac
      LOGICAL check_sign
      CHARACTER*(MAX_LEN_MBUF) msgBuf
C     == end of interface ==

      valid  = .true.
      hhmmss = 0
      calerr = 0
      fac    = 1

      check_sign = ( (date(1).lt.0) .and. date(2).gt.0 )
     &        .OR. ( (date(1).gt.0) .and. date(2).lt.0 )

      if (date(4) .le. 0) then
        if (date(4) .ne. -1) then
          calerr = 1801
        else
          if (date(3) .ne. 0) then
            calerr = 1802
          else
            if ( check_sign ) then
              calerr = 1803
            ELSEIF ( cal_setStatus .LT. 1 ) THEN
              WRITE( msgBuf,'(2A,4I9)')  'CAL_CHECKDATE: ',
     &          'date=',date(1),date(2),date(3),date(4)
              CALL PRINT_ERROR( msgBuf, myThid )
              WRITE( msgBuf,'(2A,I2,A)') 'CAL_CHECKDATE: ',
     &          'called too early (cal_setStatus=',cal_setStatus,' )'
              CALL PRINT_ERROR( msgBuf, myThid )
            else
              call cal_ConvDate(date,yy,mm,dd,nsecs,lp,wd,myThid)
              if (nsecs .lt. 0) fac = -1
              hhmmss  = fac*nsecs/secondsperminute
              hhmmss  = hhmmss/minutesperhour*10000 +
     &                  mod(hhmmss,minutesperhour)*100 +
     &                  mod(fac*nsecs,secondsperminute)
              hhmmss  = fac*hhmmss
              if (date(2) .ne. hhmmss) then
                calerr = 1804
              endif
            endif
          endif
        endif
      else
        if (date(4) .gt. 8) then
          calerr = 1805
        else
          if ((date(3) .ne. 1) .and.
     &        (date(3) .ne. 2)) then
            calerr = 1806
          else
            if ( check_sign ) then
              calerr = 1803
            else
              call cal_ConvDate( date,yy,mm,dd,nsecs,lp,wd,myThid )
              if (date(1) .lt. refdate(1)) then
                calerr = 1807
              ELSEIF ( cal_setStatus .LT. 1 ) THEN
                WRITE( msgBuf,'(2A,4I9)')  'CAL_CHECKDATE: ',
     &           'date=',date(1),date(2),date(3),date(4)
                CALL PRINT_ERROR( msgBuf, myThid )
                WRITE( msgBuf,'(2A,I2,A)') 'CAL_CHECKDATE: ',
     &           'called too early (cal_setStatus=',cal_setStatus,' )'
                CALL PRINT_ERROR( msgBuf, myThid )
              else
                hhmmss  = nsecs/secondsperminute
                hhmmss  = hhmmss/minutesperhour*10000 +
     &                    mod(hhmmss,minutesperhour)*100 +
     &                    mod(nsecs,secondsperminute)
                if (date(2) .ne. hhmmss) then
                  calerr = 1804
                endif
              endif
              yymmdd  = yy*10000 + mm*100 + dd
              if (date(1) .ne. yymmdd) then
                calerr = 1808
              endif
            endif
          endif
        endif
      endif

      if (calerr .ne. 0) valid = .FALSE.

      RETURN
      END
