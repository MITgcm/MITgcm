C $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_var.F,v 1.5 2004/01/08 07:24:47 edhill Exp $
C $Name:  $
      
#include "MNC_OPTIONS.h"
      
C==================================================================

      SUBROUTINE MNC_VAR_INIT_DBL( 
     I     myThid, 
     I     fname, 
     I     gname, 
     I     vname,
     I     units )

      implicit none
#include "netcdf.inc"

C     Arguments
      integer myThid
      character*(*) fname,gname,vname,units

      CALL MNC_VAR_INIT_ANY(myThid,fname,gname,vname,units, NF_DOUBLE)
      RETURN
      END

C==================================================================

      SUBROUTINE MNC_VAR_INIT_REAL( 
     I     myThid, 
     I     fname, 
     I     gname, 
     I     vname,
     I     units )

      implicit none
#include "netcdf.inc"

C     Arguments
      integer myThid
      character*(*) fname,gname,vname,units

      CALL MNC_VAR_INIT_ANY(myThid,fname,gname,vname,units, NF_FLOAT)
      RETURN
      END

C==================================================================

      SUBROUTINE MNC_VAR_INIT_INT( 
     I     myThid, 
     I     fname, 
     I     gname, 
     I     vname,
     I     units )

      implicit none
#include "netcdf.inc"

C     Arguments
      integer myThid
      character*(*) fname,gname,vname,units

      CALL MNC_VAR_INIT_ANY(myThid,fname,gname,vname,units, NF_INT)
      RETURN
      END

C==================================================================

      SUBROUTINE MNC_VAR_INIT_ANY( 
     I     myThid, 
     I     fname, 
     I     gname, 
     I     vname, 
     I     units, 
     I     vtype )

      implicit none
#include "netcdf.inc"
#include "mnc_common.h"
#include "EEPARAMS.h"

C     Arguments
      integer myThid
      character*(*) fname,gname,vname,units
      integer vtype

C     Functions
      integer ILNBLNK

C     Local Variables
      integer i,j,k, n, indf,indv, fid, nd, ngrid, is,ie, err
      integer vid, nv, ind_g_finfo
      character*(MAX_LEN_MBUF) msgbuf
      integer rids(10), ids(10)
      integer lenf,leng,lenv,lenu

C     Strip trailing spaces
      lenf = ILNBLNK(fname)
      leng = ILNBLNK(gname)
      lenv = ILNBLNK(vname)
      lenu = ILNBLNK(units)

C     Check that the file is open
      CALL MNC_GET_IND(myThid, MNC_MAX_ID, fname, mnc_f_names, indf)
      IF (indf .LT. 1) THEN
        write(msgbuf,'(3a)') 'MNC ERROR: file ''', fname, 
     &       ''' must be opened first'
        CALL print_error(msgbuf, mythid)
        stop 'ABNORMAL END: S/R MNC_VAR_INIT_DBL'
      ENDIF
      fid = mnc_f_info(indf,2)

C     Get the grid information
      ngrid = mnc_f_info(indf,3)
      IF (ngrid .LT. 1) THEN
        write(msgbuf,'(3a)') 'MNC ERROR: file ''', fname(1:lenf), 
     &       ''' contains NO grids'
        CALL print_error(msgbuf, mythid)
        stop 'ABNORMAL END: S/R MNC_VAR_INIT_DBL'
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
            ids(nd) = mnc_fg_ids(indf,k)
          ENDDO
          GOTO 10
        ENDIF
      ENDDO
      write(msgbuf,'(5a)') 'MNC ERROR: file ''', fname(1:lenf), 
     &     ''' does not contain grid ''', gname(1:leng), ''''
      CALL print_error(msgbuf, mythid)
      stop 'ABNORMAL END: S/R MNC_VAR_INIT_DBL'
 10   CONTINUE

C     Add the variable definition
      CALL MNC_FILE_REDEF(myThid, fname)
      err = NF_DEF_VAR(fid, vname, vtype, nd, ids, vid)
      write(msgbuf,'(5a)') 'defining variable ''', vname(1:lenv),
     &     ''' in file ''', fname(1:lenf), ''''
      CALL MNC_HANDLE_ERR(myThid, err, msgbuf)

C     Success, so save the variable info
      CALL MNC_GET_NEXT_EMPTY_IND(myThid, MNC_MAX_ID, mnc_v_names, indv)
      mnc_v_names(indv)(1:lenv) = vname(1:lenv)
      nv = mnc_fv_ids(indf,1)
      i = 2 + nv*3
      j = i + 1
      k = i + 2
      mnc_fv_ids(indf,i) = indv
      mnc_fv_ids(indf,j) = vid
      mnc_fv_ids(indf,k) = ind_g_finfo
      mnc_fv_ids(indf,1) = nv + 1

C     Add the units
      CALL MNC_VAR_ADD_ATTR_STR(myThid, fname, vname, 'units', units)

      RETURN
      END

C==================================================================

      SUBROUTINE MNC_VAR_ADD_ATTR_STR( 
     I     myThid, 
     I     fname, 
     I     vname, 
     I     atname, 
     I     sval )

      implicit none
C     Arguments
      integer myThid
      character*(*) fname,vname,atname,sval

      CALL MNC_VAR_ADD_ATTR_ANY(myThid,fname,vname,atname, 
     &     1, sval, 0, 0.0D0, 0.0, 0)
      RETURN
      END
C==================================================================

      SUBROUTINE MNC_VAR_ADD_ATTR_DBL( 
     I     myThid, 
     I     fname, 
     I     vname, 
     I     atname, 
     I     nv, 
     I     dval )

      implicit none
C     Arguments
      integer myThid,nv
      character*(*) fname,vname,atname
      _RL dval(*)

      CALL MNC_VAR_ADD_ATTR_ANY(myThid,fname,vname,atname, 
     &     2, ' ', nv, dval, 0.0, 0)
      RETURN
      END

C==================================================================

      SUBROUTINE MNC_VAR_ADD_ATTR_REAL( 
     I     myThid, 
     I     fname, 
     I     vname, 
     I     atname, 
     I     nv, 
     I     rval )

      implicit none
C     Arguments
      integer myThid,nv
      character*(*) fname,vname,atname
      _RS rval(*)

      CALL MNC_VAR_ADD_ATTR_ANY(myThid,fname,vname,atname, 
     &     3, ' ', nv, 0.0D0, rval, 0)
      RETURN
      END

C==================================================================

      SUBROUTINE MNC_VAR_ADD_ATTR_INT( 
     I     myThid, 
     I     fname, 
     I     vname, 
     I     atname, 
     I     nv, 
     I     ival )

      implicit none
C     Arguments
      integer myThid,nv
      character*(*) fname,vname,atname
      integer ival(*)

      CALL MNC_VAR_ADD_ATTR_ANY(myThid,fname,vname,atname, 
     &     4, ' ', nv, 0.0D0, 0.0, ival)
      RETURN
      END

C==================================================================

      SUBROUTINE MNC_VAR_ADD_ATTR_ANY( 
     I     myThid, 
     I     fname, 
     I     vname, 
     I     atname, 
     I     atype, cs,len,dv,rv,iv )

      implicit none
#include "netcdf.inc"
#include "mnc_common.h"
#include "EEPARAMS.h"

C     Arguments
      integer myThid,atype,len
      character*(*) fname,vname,atname
      character*(*) cs
      _RL dv(*)
      _RS rv(*)
      integer iv(*)

C     Functions
      integer ILNBLNK

C     Local Variables
      integer i,j,k, n, nv, indf,ind_fv_ids, fid,vid, err
      character*(MAX_LEN_MBUF) msgbuf
      integer lenf,lenv,lenat,lens

C     Strip trailing spaces
      lenf = ILNBLNK(fname)
      lenv = ILNBLNK(vname)
      lenat = ILNBLNK(atname)
      lens = ILNBLNK(cs)

      CALL MNC_GET_FVINDS(myThid, fname, vname, indf, ind_fv_ids)
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
      CALL MNC_FILE_REDEF(myThid, fname)
      print *, 'atype = ', atype
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
      CALL MNC_HANDLE_ERR(myThid, err, msgbuf)

      RETURN
      END

C==================================================================

      SUBROUTINE MNC_VAR_WRITE_DBL( 
     I     myThid, 
     I     fname, 
     I     vname, 
     I     var )

      implicit none
C     Arguments
      integer myThid
      character*(*) fname,vname
      _RL var(*)

      CALL MNC_VAR_WRITE_ANY(myThid,fname,vname, 1, var, 0.0, 0 )
      RETURN
      END

C==================================================================

      SUBROUTINE MNC_VAR_WRITE_REAL( 
     I     myThid, 
     I     fname, 
     I     vname, 
     I     var )

      implicit none
C     Arguments
      integer myThid
      character*(*) fname,vname
      _RS var(*)

      CALL MNC_VAR_WRITE_ANY(myThid,fname,vname, 2, 0.0D0, var, 0 )
      RETURN
      END

C==================================================================

      SUBROUTINE MNC_VAR_WRITE_INT( 
     I     myThid, 
     I     fname, 
     I     vname, 
     I     var )

      implicit none
C     Arguments
      integer myThid
      character*(*) fname,vname
      integer var(*)

      CALL MNC_VAR_WRITE_ANY(myThid,fname,vname, 3, 0.0D0, 0.0, var )
      RETURN
      END

C==================================================================

      SUBROUTINE MNC_VAR_WRITE_ANY( 
     I     myThid, 
     I     fname, 
     I     vname, 
     I     vtype,
     I     dv,
     I     rv,
     I     iv )

      implicit none
#include "netcdf.inc"
#include "mnc_common.h"
#include "EEPARAMS.h"

C     Arguments
      integer myThid, vtype
      character*(*) fname,vname
      _RL dv(*)
      _RS rv(*)
      integer iv(*)

C     Functions
      integer ILNBLNK

C     Local Variables
      integer i,j,k, n, indf,ind_fv_ids, fid,vid,did, ig, err, ds,de
      character*(MAX_LEN_MBUF) msgbuf
      integer lenf,lenv, lend
      integer vstart(100), vcount(100)
      integer rvstart(100), rvcount(100)

C     Strip trailing spaces
      lenf = ILNBLNK(fname)
      lenv = ILNBLNK(vname)

      CALL MNC_GET_FVINDS(myThid, fname, vname, indf, ind_fv_ids)
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
        did = mnc_fg_ids(indf,de)
        err = NF_INQ_DIMLEN(fid, did, lend)
        write(msgbuf,'(a)') 'reading current length of unlimited dim'
        CALL MNC_HANDLE_ERR(myThid, err, msgbuf)
        IF (lend .LT. 1)  lend = lend + 1
        vstart(k) = lend
        vcount(k) = 1
      ENDIF

      CALL MNC_FILE_ENDDEF(myThid, fname)
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
      CALL MNC_HANDLE_ERR(myThid, err, msgbuf)

      RETURN
      END

