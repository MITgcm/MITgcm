C $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_dim.F,v 1.17 2008/06/20 20:36:58 utke Exp $
C $Name:  $
      
#include "MNC_OPTIONS.h"
      
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: MNC_DIM_INIT

C     !INTERFACE:
      SUBROUTINE MNC_DIM_INIT( 
     I     fname, 
     I     dname, 
     I     dlen, 
     I     myThid )

C     !DESCRIPTION:
C     Create a dimension within the MNC look-up tables.

C     !INPUT PARAMETERS:
      integer myThid, dlen
      character*(*) fname, dname
CEOP

      CALL MNC_DIM_INIT_ALL(fname, dname, dlen, 'Y', myThid)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 1
C     !ROUTINE: MNC_DIM_INIT_ALL

C     !INTERFACE:
      SUBROUTINE MNC_DIM_INIT_ALL( 
     I     fname, 
     I     dname, 
     I     dlen, 
     I     doWrite, 
     I     myThid ) 

C     !DESCRIPTION:
C     Create a dimension within the MNC look-up tables.

C     !USES:
      implicit none

C     !INPUT PARAMETERS:
      integer myThid, dlen
      character*(*) fname, dname
      character*(1) doWrite
CEOP

      CALL MNC_DIM_INIT_ALL_CV(fname,dname,dlen,doWrite,-1,-1,myThid)
      
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 1
C     !ROUTINE: MNC_DIM_INIT_ALL_CV

C     !INTERFACE:
      SUBROUTINE MNC_DIM_INIT_ALL_CV( 
     I     fname, 
     I     dname, 
     I     dlen, 
     I     doWrite, 
     I     bi,bj,
     I     myThid ) 

C     !DESCRIPTION:
C     Create a dimension within the MNC look-up tables.

C     !USES:
      implicit none
#include "MNC_COMMON.h"
#include "EEPARAMS.h"
#include "netcdf.inc"

C     !INPUT PARAMETERS:
      integer myThid, dlen, bi,bj
      character*(*) fname, dname
      character*(1) doWrite
CEOP

C     !LOCAL VARIABLES:
      integer i,j, indf,indd, n,nf, dnf,dnl
      integer ntmp, idd, err, tlen
      character*(MAX_LEN_MBUF) msgbuf

C     Functions
      integer ILNBLNK, IFNBLNK

      nf = ILNBLNK(fname)

      dnf = IFNBLNK(dname)
      dnl = ILNBLNK(dname)

C     Verify that the file exists
      CALL MNC_GET_IND(MNC_MAX_FID, fname, mnc_f_names, indf, myThid)
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
        ntmp = ILNBLNK(mnc_d_names(j))
        IF ((ntmp .EQ. (dnl-dnf+1)) 
     &       .AND. (dname(dnf:dnl) .EQ. mnc_d_names(j)(1:ntmp))) THEN
          IF (mnc_d_size(j) .NE. dlen) THEN
            IF ((mnc_d_size(j) .GT. 0) .OR. (dlen .GT. 0)) THEN
              write(msgbuf,'(5a)') 'MNC ERROR: dimension ''', 
     &             dname(dnf:dnl), ''' already exists within file ''', 
     &             fname(1:nf), ''' and its size cannot be changed'
              CALL print_error(msgbuf, mythid)
              stop 'ABNORMAL END: S/R MNC_DIM_INIT'
            ELSE
C             Its OK, the names are the same and both are specifying the
C             unlimited dimension
              RETURN
            ENDIF
          ELSE
C           Its OK, the names and sizes are identical
            RETURN
          ENDIF
        ENDIF
      ENDDO

      CALL MNC_GET_NEXT_EMPTY_IND(MNC_MAX_ID, mnc_d_names, 
     &     'mnc_d_names', indd, myThid)

C     Create the dim within the file
      IF (doWrite(1:1) .EQ. 'Y') THEN

        tlen = dlen
        IF (dlen .LT. 1)  tlen = NF_UNLIMITED

        CALL MNC_FILE_REDEF(fname, myThid)
        err = NF_DEF_DIM(mnc_f_info(indf,2), dname(dnf:dnl), tlen, idd)
        write(msgbuf,'(5a)') 'MNC_DIM_INIT ERROR: cannot create ',
     &       'dim ''', dname(dnf:dnl), ''' in file ''', fname(1:nf)
        CALL MNC_HANDLE_ERR(err, msgbuf, myThid)

C       Create and write the associated CF-convention
C       coordinate variable
        IF (bi .GT. -1) THEN
          CALL MNC_CW_WRITE_CVAR(fname, dname(dnf:dnl), 
     &         mnc_f_info(indf,2), idd, bi, bj, myThid)
        ENDIF

      ENDIF

C     Add to tables
      mnc_d_names(indd)(1:(dnl-dnf+1)) = dname(dnf:dnl)
      mnc_d_size(indd) = dlen
      mnc_d_ids(indd) = idd
      mnc_f_alld(indf,1) = n + 1
      mnc_f_alld(indf,n+2) = indd

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 1
C     !ROUTINE: MNC_DIM_UNLIM_SIZE

C     !INTERFACE:
      SUBROUTINE MNC_DIM_UNLIM_SIZE( 
     I     fname, 
     I     unlim_sz, 
     I     myThid )

C     !DESCRIPTION:
C     Get the size of the unlimited dimension.
      
C     !USES:
      implicit none
#include "MNC_COMMON.h"
#include "EEPARAMS.h"
#include "netcdf.inc"

C     !INPUT PARAMETERS:
      integer myThid, unlim_sz
      character*(*) fname
CEOP

C     !LOCAL VARIABLES:
      integer i,j, nf, indf, fid, unlimid, err
      character*(MAX_LEN_MBUF) msgbuf

C     Functions
      integer ILNBLNK

      nf = ILNBLNK(fname)

C     Verify that the file exists
      CALL MNC_GET_IND(MNC_MAX_FID, fname, mnc_f_names, indf, myThid)
      IF (indf .LT. 1) THEN
        write(msgbuf,'(3a)') 'MNC ERROR: file ''', fname(1:nf),
     &       ''' does not exist'
        CALL print_error(msgbuf, mythid)
        stop 'ABNORMAL END: S/R MNC_DIM_UNLIM_SIZE'
      ENDIF
      fid = mnc_f_info(indf,2)

C     Find the unlimited dim and its current size
      unlim_sz = -1
      DO i = 1,mnc_f_alld(indf,1)
        j = mnc_f_alld(indf,i+1)
        IF (mnc_d_size(j) .EQ. -1) THEN
          unlimid = mnc_d_ids(j)
          err = NF_INQ_DIMLEN(fid, unlimid, unlim_sz)
          write(msgbuf,'(3a)') 'MNC_DIM_UNLIM_SIZE ERROR: cannot ',
     &         'determine unlimited dim size in file ''', fname(1:nf)
          CALL MNC_HANDLE_ERR(err, msgbuf, myThid)
          RETURN
        ENDIF
      ENDDO

      RETURN
      END


C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

