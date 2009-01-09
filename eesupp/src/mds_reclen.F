C $Header: /u/gcmpack/MITgcm/eesupp/src/mds_reclen.F,v 1.3 2009/01/09 22:51:13 jmc Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"

      INTEGER FUNCTION MDS_RECLEN( filePrec, nnn, myThid )
C IN:
C   filePrec    integer :: precision of file in bits
C   nnn         integer :: number of elements in record
C   myThid      integer :: my thread id number
C OUT:
C   MDS_RECLEN  integer :: appropriate length of record in bytes or words
C
C Created: 03/29/99 eckert@mit.edu + adcroft@mit.edu

      IMPLICIT NONE

C Global variables
#include "SIZE.h"
#include "EEPARAMS.h"
C
C Arguments
      INTEGER filePrec
      INTEGER nnn
      INTEGER myThid
C
C Local
      CHARACTER*(MAX_LEN_MBUF) msgBuf
C     ------------------------------------------------------------------

      IF (filePrec .EQ. precFloat32) THEN
       MDS_RECLEN = nnn*WORDLENGTH
      ELSEIF (filePrec .EQ. precFloat64) THEN
       MDS_RECLEN = nnn*WORDLENGTH*2
      ELSE
       WRITE(msgBuf,'(A,I2.2)')
     &   ' MDS_RECLEN: filePrec = ',filePrec
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &   ' MDS_RECLEN: illegal value for filePrec'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R MDS_RECLEN'
      ENDIF

C     ------------------------------------------------------------------
      RETURN
      END
