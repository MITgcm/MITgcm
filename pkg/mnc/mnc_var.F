C $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_var.F,v 1.3 2004/01/07 07:29:13 edhill Exp $
C $Name:  $
      
#include "MNC_OPTIONS.h"
      
C==================================================================

      SUBROUTINE MNC_VAR_INIT_DBL( 
     I     myThid, 
     I     fname, 
     I     gname, 
     I     vname, 
     I     fillval )

      implicit none
#include "netcdf.inc"
#include "mnc_common.h"
#include "EEPARAMS.h"

C     Arguments
      integer myThid
      character*(*) fname
      character*(*) gname
      character*(*) vname
      _RL fillval

C     Functions
      integer ILNBLNK

C     Local Variables
      integer i,j,k, n, indf,indv, fid, nd, ngrid, is,ie, err
      integer vid, nv
      character*(MAX_LEN_MBUF) msgbuf
      integer rids(10), ids(10)
      integer lenf,leng,lenv

C     Strip trailing spaces
      lenf = ILNBLNK(fname)
      leng = ILNBLNK(gname)
      lenv = ILNBLNK(vname)

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
      err = NF_DEF_VAR(fid, vname, NF_DOUBLE, nd, ids, vid)
      write(msgbuf,'(5a)') 'defining variable ''', vname(1:lenv),
     &     ''' in file ''', fname(1:lenf), ''''
      CALL MNC_HANDLE_ERR(myThid, err, msgbuf)

C     Success, so save the variable info
      CALL MNC_GET_NEXT_EMPTY_IND(myThid, MNC_MAX_ID, mnc_v_names, indv)
      mnc_v_names(indv)(1:lenv) = vname(1:lenv)
      nv = mnc_fv_ids(indf,1)
      i = 2 + nv*2
      j = i + 1
      mnc_fv_ids(indf,i) = indv
      mnc_fv_ids(indf,j) = vid
      mnc_fv_ids(indf,1) = nv + 1

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
#include "netcdf.inc"
#include "mnc_common.h"
#include "EEPARAMS.h"

C     Arguments
      integer myThid
      character*(*) fname
      character*(*) vname
      character*(*) atname
      character*(*) sval

C     Functions
      integer ILNBLNK

C     Local Variables
      integer i,j,k, n, nv, indf, fid,vid, err
      character*(MAX_LEN_MBUF) msgbuf
      integer lenf,lenv,lenat,lens

C     Strip trailing spaces
      lenf = ILNBLNK(fname)
      lenv = ILNBLNK(vname)
      lenat = ILNBLNK(atname)
      lens = ILNBLNK(sval)

C     Check that the file is open
      CALL MNC_GET_IND(myThid, MNC_MAX_ID, fname, mnc_f_names, indf)
      IF (indf .LT. 1) THEN
        write(msgbuf,'(3a)') 'MNC ERROR: file ''', fname(1:lenf), 
     &       ''' must be opened first'
        CALL print_error(msgbuf, mythid)
        stop 'ABNORMAL END: S/R MNC_VAR_ADD_ATTR_STR'
      ENDIF
      fid = mnc_f_info(indf,2)

C     Find the vID
      nv = mnc_fv_ids(indf,1)
      DO i = 1,nv
        k = 2*i
        j = mnc_fv_ids(indf,k)
        n = ILNBLNK(mnc_v_names(j))
        IF ((n .EQ. lenv) 
     &       .AND. (mnc_v_names(j)(1:n) .EQ. vname(1:n))) THEN
          k = k + 1
          vid = mnc_fv_ids(indf,k)
          GOTO 10
        ENDIF
      ENDDO
      write(msgbuf,'(5a)') 'MNC ERROR: file ''', fname(1:lenf), 
     &     ''' does not contain variable ''', vname(1:lenv), ''''
      CALL print_error(msgbuf, mythid)
      stop 'ABNORMAL END: S/R MNC_VAR_ADD_ATTR_STR'
 10   CONTINUE

C     Set the attribute
      CALL MNC_FILE_REDEF(myThid, fname)
      err = NF_PUT_ATT_TEXT(fid, vid, atname, lens, sval)
      write(msgbuf,'(5a)') 'adding attribute ''', atname(1:lenat),
     &     ''' to file ''', fname(1:lenf), ''''
      CALL MNC_HANDLE_ERR(myThid, err, msgbuf)

      RETURN
      END

