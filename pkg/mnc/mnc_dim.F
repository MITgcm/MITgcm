C $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_dim.F,v 1.2 2004/01/17 13:55:49 edhill Exp $
C $Name:  $
      
#include "MNC_OPTIONS.h"
      
C==================================================================

      SUBROUTINE MNC_DIM_INIT( 
     I     myThid, 
     I     fname, 
     I     dname, 
     I     dunits, 
     I     dlen )

      implicit none
#include "mnc_common.h"
#include "EEPARAMS.h"

C     Arguments
      integer myThid
      character*(*) fname, dname, dunits
      integer dlen

C     Functions
      integer ILNBLNK

C     Local Variables
      integer i,j, indf,indd, n,nf,ndn,ndu
      character*(MAX_LEN_MBUF) msgbuf

      nf = ILNBLNK(fname)
      ndn = ILNBLNK(dname)
      ndu = ILNBLNK(dunits)

C     Verify that the file exists
      CALL MNC_GET_IND(myThid, MNC_MAX_ID, fname, mnc_f_names, indf)
      IF ( indf .LT. 1 ) THEN
        write(msgbuf,'(3a)') 'MNC ERROR: file ''', fname(1:nf),
     &       ''' does not exist'
        CALL print_error( msgbuf, mythid )
        stop 'ABNORMAL END: S/R MNC_DIM_INIT'
      ENDIF

C     Verify that the dim is not currently defined within the file
      n = mnc_f_alld(indf,1)
      DO i = 1,n
        j = mnc_f_alld(indf,i+1)
        IF (dname(1:ndn) .EQ. mnc_d_names(j)(1:ndn)) THEN
          write(msgbuf,'(5a)') 'MNC ERROR: dimension ''', dname(1:ndn), 
     &         ''' already exists within file ''', fname(1:nf), 
     &         ''' and cannot be re-initalized'
          CALL print_error( msgbuf, mythid )
          stop 'ABNORMAL END: S/R MNC_DIM_INIT'
        ENDIF
      ENDDO

      CALL MNC_GET_NEXT_EMPTY_IND(myThid, MNC_MAX_ID, mnc_d_names, indd)

C     Check that the dimension name fits in the table
      IF ( ndn .GT. MNC_MAX_CHAR) THEN
        ndn = MNC_MAX_CHAR
        write(msgbuf,'(a)') 'MNC WARNING: dimension name too long'
        CALL print_error(msgbuf, mythid)
      ENDIF
      IF ( ndu .GT. MNC_MAX_CHAR) THEN
        ndu = MNC_MAX_CHAR
        write(msgbuf,'(a)') 'MNC WARNING: dimension units too long'
        CALL print_error(msgbuf, mythid)
      ENDIF

C     Add to tables
      mnc_d_names(indd)(1:ndn) = dname(1:ndn)
      mnc_d_units(indd)(1:ndu) = dunits(1:ndu)
      mnc_d_size(indd) = dlen
      mnc_f_alld(indf,1) = n + 1
      mnc_f_alld(indf,n+2) = indd

      RETURN
      END

