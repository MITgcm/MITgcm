C $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_dim.F,v 1.3 2004/01/25 00:22:57 edhill Exp $
C $Name:  $
      
#include "MNC_OPTIONS.h"
      
C==================================================================

      SUBROUTINE MNC_DIM_INIT( 
     I     myThid, 
     I     fname, 
     I     dname, 
     I     dlen )

C     Arguments
      integer myThid, dlen
      character*(*) fname, dname

      CALL MNC_DIM_INIT_ALL(myThid, fname, dname, dlen, 'Y')

      RETURN
      END


C==================================================================

      SUBROUTINE MNC_DIM_INIT_ALL( 
     I     myThid, 
     I     fname, 
     I     dname, 
     I     dlen, 
     I     doWrite ) 

      implicit none
#include "netcdf.inc"
#include "mnc_common.h"
#include "EEPARAMS.h"

C     Arguments
      integer myThid, dlen
      character*(*) fname, dname
      character*(1) doWrite

C     Functions
      integer ILNBLNK, IFNBLNK

C     Local Variables
      integer i,j, indf,indd, n,nf, dnf,dnl, ntmp, idd, err, tlen
      character*(MAX_LEN_MBUF) msgbuf

      nf = ILNBLNK(fname)

      dnf = IFNBLNK(dname)
      dnl = ILNBLNK(dname)

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
        ntmp = ILNBLNK(mnc_d_names(j))
        j = mnc_f_alld(indf,i+1)
        IF ((ntmp .EQ. (dnl-dnf+1)) 
     &       .AND. (dname(dnf:dnl) .EQ. mnc_d_names(j)(1:n))) THEN
          write(msgbuf,'(5a)') 'MNC ERROR: dimension ''', 
     &         dname(dnf:dnl), 
     &         ''' already exists within file ''', fname(1:nf), 
     &         ''' and cannot be re-initalized'
          CALL print_error( msgbuf, mythid )
          stop 'ABNORMAL END: S/R MNC_DIM_INIT'
        ENDIF
      ENDDO

      CALL MNC_GET_NEXT_EMPTY_IND(myThid, MNC_MAX_ID, mnc_d_names, indd)

C     Create the dim within the file
      IF (doWrite(1:1) .EQ. 'Y') THEN

        tlen = dlen
        IF (dlen .LT. 1)  tlen = NF_UNLIMITED

        CALL MNC_FILE_REDEF(myThid, fname)
        err = NF_DEF_DIM(mnc_f_info(indf,2), dname(dnf:dnl), tlen, idd)
        write(msgbuf,'(5a)') 'MNC_DIM_INIT ERROR: cannot create ',
     &       'dim ''', dname(dnf:dnl), ''' in file ''', fname(1:nf)
        CALL MNC_HANDLE_ERR(myThid, err, msgbuf)

      ENDIF

C     Add to tables
      mnc_d_names(indd)(1:(dnl-dnf+1)) = dname(dnf:dnl)
      mnc_d_size(indd) = dlen
      mnc_d_ids(indd) = idd
      mnc_f_alld(indf,1) = n + 1
      mnc_f_alld(indf,n+2) = indd

      RETURN
      END

