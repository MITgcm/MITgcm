C $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_var.F,v 1.22 2006/03/10 22:01:54 edhill Exp $
C $Name:  $
      
#include "MNC_OPTIONS.h"
      
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 1
C     !ROUTINE: MNC_VAR_INIT_DBL

C     !INTERFACE:
      SUBROUTINE MNC_VAR_INIT_DBL( 
     I     fname, 
     I     gname, 
     I     vname, 
     I     irv,
     I     myThid )

C     !DESCRIPTION:
C     Create a double-precision real variable within a NetCDF file
C     context.
      
C     !USES:
      implicit none
#include "netcdf.inc"

C     !INPUT PARAMETERS:
      integer irv,myThid
      character*(*) fname,gname,vname
CEOP

      CALL MNC_VAR_INIT_ANY(fname,gname,vname, NF_DOUBLE, irv,myThid)
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 1
C     !ROUTINE: MNC_VAR_INIT_REAL

C     !INTERFACE:
      SUBROUTINE MNC_VAR_INIT_REAL( 
     I     fname, 
     I     gname, 
     I     vname, 
     I     irv,
     I     myThid )

C     !DESCRIPTION:
C     Create a single-precision real variable within a NetCDF file
C     context.
      
C     !USES:
      implicit none
#include "netcdf.inc"

C     !INPUT PARAMETERS:
      integer irv,myThid
      character*(*) fname,gname,vname
CEOP

      CALL MNC_VAR_INIT_ANY(fname,gname,vname, NF_FLOAT, irv,myThid)
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 1
C     !ROUTINE: MNC_VAR_INIT_INT

C     !INTERFACE:
      SUBROUTINE MNC_VAR_INIT_INT( 
     I     fname, 
     I     gname, 
     I     vname, 
     I     irv,
     I     myThid )

C     !DESCRIPTION:
C     Create an integer variable within a NetCDF file context.
  
C     !USES:
      implicit none
#include "netcdf.inc"

C     !INPUT PARAMETERS:
      integer irv,myThid
      character*(*) fname,gname,vname
CEOP

      CALL MNC_VAR_INIT_ANY(fname,gname,vname, NF_INT, irv,myThid)
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 1
C     !ROUTINE: MNC_VAR_INIT_ANY

C     !INTERFACE:
      SUBROUTINE MNC_VAR_INIT_ANY( 
     I     fname, 
     I     gname, 
     I     vname, 
     I     vtype, 
     I     irv,
     I     myThid )

C     !DESCRIPTION:
C     General function for creating variables within a NetCDF file
C     context.
      
C     !USES:
      implicit none
#include "netcdf.inc"
#include "mnc_common.h"
#include "EEPARAMS.h"

C     !INPUT PARAMETERS:
      integer irv,myThid
      character*(*) fname,gname,vname
      integer vtype
CEOP

C     !LOCAL VARIABLES:
      integer i,j,k, n, nf, indf,indv, fid, nd, ngrid, is,ie, err
      integer vid, nv, ind_g_finfo, needed, nvar
      character*(MAX_LEN_MBUF) msgbuf
      integer ids(20)
      integer lenf,leng,lenv

C     Functions
      integer ILNBLNK

C     Strip trailing spaces
      lenf = ILNBLNK(fname)
      leng = ILNBLNK(gname)
      lenv = ILNBLNK(vname)

C     Check that the file is open
      CALL MNC_GET_IND(MNC_MAX_FID, fname, mnc_f_names, indf, myThid)
      IF (indf .LT. 1) THEN
        nf = ILNBLNK( fname )
        write(msgbuf,'(3a)') 'MNC ERROR: file ''', fname(1:nf), 
     &       ''' must be opened first'
        CALL print_error(msgbuf, mythid)
        stop 'ABNORMAL END: S/R MNC_VAR_INIT_ANY'
      ENDIF
      fid = mnc_f_info(indf,2)

C     Check for sufficient storage space in mnc_fv_ids
      needed = 1 + 3*(mnc_fv_ids(indf,1) + 1)
      IF (needed .GE. MNC_MAX_INFO) THEN
        write(msgbuf,'(2a,i7,a)') 'MNC ERROR: MNC_MAX_INFO exceeded', 
     &       ': please increase it to ', 2*MNC_MAX_INFO, 
     &       ' in the file ''pkg/mnc/mnc_common.h'''
        CALL print_error(msgbuf, mythid)
        stop 'ABNORMAL END: S/R MNC_VAR_INIT_ANY'
      ENDIF

C     Get the grid information
      ngrid = mnc_f_info(indf,3)
      IF (ngrid .LT. 1) THEN
        write(msgbuf,'(3a)') 'MNC ERROR: file ''', fname(1:lenf), 
     &       ''' contains NO grids'
        CALL print_error(msgbuf, mythid)
        stop 'ABNORMAL END: S/R MNC_VAR_INIT_ANY'
      ENDIF
      DO i = 1,ngrid
        j = 4 + (i-1)*3
        k = mnc_f_info(indf,j)
        n = ILNBLNK(mnc_g_names(k))
        IF ((leng .EQ. n) 
     &       .AND. (mnc_g_names(k)(1:n) .EQ. gname(1:n))) THEN
          ind_g_finfo = j
          is = mnc_f_info(indf,(j+1))
          ie = mnc_f_info(indf,(j+2))
          nd = 0
          DO k = is,ie
            nd = nd + 1
            ids(nd) = mnc_d_ids(mnc_fd_ind(indf,k))
          ENDDO
          GOTO 10
        ENDIF
      ENDDO
      write(msgbuf,'(5a)') 'MNC ERROR: file ''', fname(1:lenf), 
     &     ''' does not contain grid ''', gname(1:leng), ''''
      CALL print_error(msgbuf, mythid)
      stop 'ABNORMAL END: S/R MNC_VAR_INIT_ANY'
 10   CONTINUE

C     Check if the variable is already defined
      nvar = mnc_fv_ids(indf,1)
      DO i = 1,nvar
        j = 2 + 3*(i-1)
        IF (mnc_v_names(mnc_fv_ids(indf,j)) .EQ. vname) THEN
          k = mnc_f_info(indf,mnc_fv_ids(indf,j+2))
          IF (mnc_g_names(k) .NE. gname) THEN
            write(msgbuf,'(5a)') 'MNC ERROR: variable ''', 
     &           vname(1:lenv), ''' is already defined in file ''', 
     &           fname(1:lenf), ''' but using a different grid shape'
            CALL print_error(msgbuf, mythid)
            stop 'ABNORMAL END: S/R MNC_VAR_INIT_ANY'
          ELSE
C           Its OK, the variable and grid names are the same
            irv = 0
            RETURN
          ENDIF
        ENDIF
      ENDDO

      irv = 1

C     Add the variable definition
      CALL MNC_FILE_REDEF(fname, myThid)
      err = NF_DEF_VAR(fid, vname, vtype, nd, ids, vid)
      IF ( err .NE. NF_NOERR ) THEN
        write(msgbuf,'(2a)') 'ERROR:  MNC will not ',
     &       'overwrite variables in existing NetCDF'
        CALL PRINT_ERROR( msgBuf, myThid )
        write(msgbuf,'(2a)') '        files.  Please',
     &       ' make sure that you are not trying to'
        CALL PRINT_ERROR( msgBuf, myThid )
        write(msgbuf,'(2a)') '        overwrite output',
     &       'files from a previous model run!'
        CALL PRINT_ERROR( msgBuf, myThid )
        write(msgbuf,'(5a)') 'defining variable ''', vname(1:lenv),
     &     ''' in file ''', fname(1:lenf), ''''
        CALL MNC_HANDLE_ERR(err, msgbuf, myThid)
      ENDIF

C     Success, so save the variable info
      CALL MNC_GET_NEXT_EMPTY_IND(MNC_MAX_ID,mnc_v_names,'mnc_v_names',
     &     indv, myThid)
      mnc_v_names(indv)(1:lenv) = vname(1:lenv)
      nv = mnc_fv_ids(indf,1)
      i = 2 + nv*3
      mnc_fv_ids(indf,i)   = indv
      mnc_fv_ids(indf,i+1) = vid
      mnc_fv_ids(indf,i+2) = ind_g_finfo
      mnc_fv_ids(indf,1) = nv + 1

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 1
C     !ROUTINE: MNC_VAR_ADD_ATTR_STR

C     !INTERFACE:
      SUBROUTINE MNC_VAR_ADD_ATTR_STR( 
     I     fname, 
     I     vname, 
     I     atname, 
     I     sval, 
     I     myThid )

C     !DESCRIPTION:
C     Subroutine for adding a character string attribute to a NetCDF
C     file.
      
C     !USES:
      implicit none

C     !INPUT PARAMETERS:
      integer myThid
      character*(*) fname,vname,atname,sval
CEOP
      real*8 dZero(1)
      real*4 sZero(1)
      integer iZero(1)
      dZero(1) = 0.0D0
      sZero(1) = 0.0
      iZero(1) = 0

      CALL MNC_VAR_ADD_ATTR_ANY(fname,vname,atname, 
     &     1, sval, 0, dZero, sZero, iZero, myThid)
      RETURN
      END
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 1
C     !ROUTINE: MNC_VAR_ADD_ATTR_DBL

C     !INTERFACE:
      SUBROUTINE MNC_VAR_ADD_ATTR_DBL( 
     I     fname, 
     I     vname, 
     I     atname, 
     I     nv, 
     I     dval, 
     I     myThid )

C     !DESCRIPTION:
C     Subroutine for adding a double-precision real attribute to a
C     NetCDF file.
   
C     !USES:
      implicit none

C     !INPUT PARAMETERS:
      integer myThid,nv
      character*(*) fname,vname,atname
      REAL*8 dval(*)
CEOP
      real*4 sZero(1)
      integer iZero(1)
      sZero(1) = 0.0
      iZero(1) = 0

      CALL MNC_VAR_ADD_ATTR_ANY(fname,vname,atname, 
     &     2, ' ', nv, dval, sZero, iZero, myThid)
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 1
C     !ROUTINE: MNC_VAR_ADD_ATTR_REAL

C     !INTERFACE:
      SUBROUTINE MNC_VAR_ADD_ATTR_REAL( 
     I     fname, 
     I     vname, 
     I     atname, 
     I     nv, 
     I     rval, 
     I     myThid )

C     !DESCRIPTION:
C     Subroutine for adding a single-precision real attribute to a
C     NetCDF file.
   
C     !USES:
      implicit none

C     !INPUT PARAMETERS:
      integer myThid,nv
      character*(*) fname,vname,atname
      REAL*4 rval(*)
CEOP
      real*8 dZero(1)
      integer iZero(1)
      dZero(1) = 0.0D0
      iZero(1) = 0

      CALL MNC_VAR_ADD_ATTR_ANY(fname,vname,atname, 
     &     3, ' ', nv, dZero, rval, iZero, myThid)
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 1
C     !ROUTINE: MNC_VAR_ADD_ATTR_INT

C     !INTERFACE:
      SUBROUTINE MNC_VAR_ADD_ATTR_INT( 
     I     fname, 
     I     vname, 
     I     atname, 
     I     nv, 
     I     ival, 
     I     myThid )

C     !DESCRIPTION:
C     Subroutine for adding an integer attribute to a
C     NetCDF file.
      
C     !USES:
      implicit none

C     !INPUT PARAMETERS:
      integer myThid,nv
      character*(*) fname,vname,atname
      integer ival(*)
CEOP
      real*8 dZero(1)
      real*4 sZero(1)
      dZero(1) = 0.0D0
      sZero(1) = 0.0

      CALL MNC_VAR_ADD_ATTR_ANY(fname,vname,atname, 
     &     4, ' ', nv, dZero, sZero, ival, myThid)
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 1
C     !ROUTINE: MNC_VAR_ADD_ATTR_ANY

C     !INTERFACE:
      SUBROUTINE MNC_VAR_ADD_ATTR_ANY( 
     I     fname, 
     I     vname, 
     I     atname, 
     I     atype, cs,len,dv,rv,iv, 
     I     myThid )

C     !DESCRIPTION:
C     General subroutine for adding attributes to a NetCDF file.
      
C     !USES:
      implicit none
#include "netcdf.inc"
#include "mnc_common.h"
#include "EEPARAMS.h"

C     !INPUT PARAMETERS:
      integer myThid,atype,len
      character*(*) fname,vname,atname
      character*(*) cs
      REAL*8 dv(*)
      REAL*4 rv(*)
      integer iv(*)
CEOP

C     !LOCAL VARIABLES:
      integer n, indf,ind_fv_ids, fid,vid, err
      character*(MAX_LEN_MBUF) msgbuf
      integer lenf,lenv,lenat,lens

C     Functions
      integer ILNBLNK

C     Strip trailing spaces
      lenf = ILNBLNK(fname)
      lenv = ILNBLNK(vname)
      lenat = ILNBLNK(atname)
      lens = ILNBLNK(cs)

      CALL MNC_GET_FVINDS(fname, vname, indf, ind_fv_ids, myThid)
      IF ((indf .LT. 1).OR.(ind_fv_ids .LT. 1)) THEN
        write(msgbuf,'(5a)') 'MNC ERROR: file ''', fname(1:lenf), 
     &       ''' is not open or does not contain variable ''', 
     &       vname(1:lenv), ''''
        CALL print_error(msgbuf, mythid)
        stop 'ABNORMAL END: S/R MNC_VAR_ADD_ATTR_STR'
      ENDIF
      fid = mnc_f_info(indf,2)
      vid = mnc_fv_ids(indf,(ind_fv_ids+1))

C     Set the attribute
      CALL MNC_FILE_REDEF(fname, myThid)
      IF (atype .EQ. 1) THEN
        err = NF_PUT_ATT_TEXT(fid, vid, atname, lens, cs)
      ELSEIF (atype .EQ. 2) THEN
        err = NF_PUT_ATT_DOUBLE(fid, vid, atname, NF_DOUBLE, len, dv)
      ELSEIF (atype .EQ. 3) THEN
        err = NF_PUT_ATT_REAL(fid, vid, atname, NF_FLOAT, len, rv)
      ELSEIF (atype .EQ. 4) THEN
        err = NF_PUT_ATT_INT(fid, vid, atname, NF_INT, len, iv)
      ELSE
        write(msgbuf,'(a,i10,a)') 'MNC ERROR: atype = ''', atype, 
     &       ''' is invalid--must be: [1-4]'
        n = ILNBLNK(msgbuf)
        CALL print_error(msgbuf(1:n), mythid)
        stop 'ABNORMAL END: S/R MNC_VAR_ADD_ATTR_ANY'
      ENDIF
      write(msgbuf,'(5a)') 'adding attribute ''', atname(1:lenat),
     &     ''' to file ''', fname(1:lenf), ''''
      CALL MNC_HANDLE_ERR(err, msgbuf, myThid)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MNC_VAR_WRITE_DBL( 
     I     fname, 
     I     vname, 
     I     var, 
     I     myThid )

      implicit none
C     Arguments
      integer myThid
      character*(*) fname,vname
      REAL*8 var(*)

      CALL MNC_VAR_WRITE_ANY(fname,vname,1,0,var,0.0,0, myThid)
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MNC_VAR_WRITE_REAL( 
     I     fname, 
     I     vname, 
     I     var, 
     I     myThid )

      implicit none
C     Arguments
      integer myThid
      character*(*) fname,vname
      REAL*4 var(*)

      CALL MNC_VAR_WRITE_ANY(fname,vname,2,0,0.0D0,var,0, myThid)
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MNC_VAR_WRITE_INT( 
     I     fname, 
     I     vname, 
     I     var, 
     I     myThid )

      implicit none
C     Arguments
      integer myThid
      character*(*) fname,vname
      integer var(*)

      CALL MNC_VAR_WRITE_ANY(fname,vname,3,0,0.0D0,0.0,var, myThid)
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MNC_VAR_APPEND_DBL( 
     I     fname, 
     I     vname, 
     I     var, 
     I     append, 
     I     myThid )

      implicit none
C     Arguments
      integer myThid, append
      character*(*) fname,vname
      REAL*8 var(*)

      CALL MNC_VAR_WRITE_ANY(fname,vname,1,append,var,0.0,0, myThid)
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MNC_VAR_APPEND_REAL( 
     I     fname, 
     I     vname, 
     I     var, 
     I     append, 
     I     myThid )

      implicit none
C     Arguments
      integer myThid, append
      character*(*) fname,vname
      REAL*4 var(*)

      CALL MNC_VAR_WRITE_ANY(fname,vname,2,append,0.0D0,var,0,myThid)
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MNC_VAR_APPEND_INT( 
     I     fname, 
     I     vname, 
     I     var, 
     I     append, 
     I     myThid )

      implicit none
C     Arguments
      integer myThid, append
      character*(*) fname,vname
      integer var(*)

      CALL MNC_VAR_WRITE_ANY(fname,vname,3,append,0.0D0,0.0,var,myThid)
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MNC_VAR_WRITE_ANY( 
     I     fname, 
     I     vname, 
     I     vtype,
     I     append,
     I     dv,
     I     rv,
     I     iv, 
     I     myThid )

      implicit none
#include "netcdf.inc"
#include "mnc_common.h"
#include "EEPARAMS.h"

C     Arguments
      integer myThid, vtype
      character*(*) fname,vname
      REAL*8 dv(*)
      REAL*4 rv(*)
      integer iv(*)
      integer append

C     Functions
      integer ILNBLNK

C     Local Variables
      integer i,j,k, n, indf,ind_fv_ids, fid,vid,did, ig, err, ds,de
      character*(MAX_LEN_MBUF) msgbuf
      integer lenf,lenv, lend
      integer vstart(100), vcount(100)

C     Strip trailing spaces
      lenf = ILNBLNK(fname)
      lenv = ILNBLNK(vname)

      CALL MNC_GET_FVINDS(fname, vname, indf, ind_fv_ids, myThid)
      IF ((indf .LT. 1).OR.(ind_fv_ids .LT. 1)) THEN
        write(msgbuf,'(5a)') 'MNC ERROR: file ''', fname(1:lenf), 
     &       ''' is not open or does not contain variable ''', 
     &       vname(1:lenv), ''''
        CALL print_error(msgbuf, mythid)
        stop 'ABNORMAL END: S/R MNC_VAR_ADD_ATTR_STR'
      ENDIF
      fid = mnc_f_info(indf,2)
      vid = mnc_fv_ids(indf,(ind_fv_ids+1))

C     Get the lengths from the dim IDs
      ig = mnc_fv_ids(indf,(ind_fv_ids+2))
      ds = mnc_f_info(indf,ig+1)
      de = mnc_f_info(indf,ig+2)
      k = 0
      DO i = ds,de
        k = k + 1
        vstart(k) = 1
        vcount(k) = mnc_d_size( mnc_fd_ind(indf,i) )
      ENDDO

C     Check for the unlimited dimension
      j = mnc_d_size( mnc_fd_ind(indf,de) )
      IF (j .LT. 1) THEN
        did = mnc_d_ids( mnc_fd_ind(indf,de) )
        err = NF_INQ_DIMLEN(fid, did, lend)
        write(msgbuf,'(a)') 'reading current length of unlimited dim'
        CALL MNC_HANDLE_ERR(err, msgbuf, myThid)
        IF (append .GT. 0) THEN
          lend = lend + append
        ENDIF
        IF (lend .LT. 1) lend = 1
        vstart(k) = lend
        vcount(k) = 1
      ENDIF

      CALL MNC_FILE_ENDDEF(fname, myThid)
      IF (vtype .EQ. 1) THEN
        err = NF_PUT_VARA_DOUBLE(fid, vid, vstart, vcount, dv)
      ELSEIF (vtype .EQ. 2) THEN
        err = NF_PUT_VARA_REAL(fid, vid, vstart, vcount, rv)
      ELSEIF (vtype .EQ. 3) THEN
        err = NF_PUT_VARA_INT(fid, vid, vstart, vcount, iv)
      ELSE
        write(msgbuf,'(a,i10,a)') 'MNC ERROR: vtype = ''', vtype, 
     &       ''' is invalid--must be: [1|2|3]'
        n = ILNBLNK(msgbuf)
        CALL print_error(msgbuf(1:n), mythid)
        stop 'ABNORMAL END: S/R MNC_VAR_WRITE_ALL'
      ENDIF  
      write(msgbuf,'(5a)') 'writing variable ''', vname(1:lenv),
     &     ''' to file ''', fname(1:lenf), ''''
      CALL MNC_HANDLE_ERR(err, msgbuf, myThid)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

