C $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_var.F,v 1.26 2011/08/01 16:00:17 jahn Exp $
C $Name:  $

#include "MNC_OPTIONS.h"

C--  File mnc_var.F: Handle NetCDF variables (definition,description & writing)
C--   Contents
C--   o MNC_VAR_INIT_DBL
C--   o MNC_VAR_INIT_REAL
C--   o MNC_VAR_INIT_INT
C--   o MNC_VAR_INIT_ANY
C--   o MNC_VAR_ADD_ATTR_STR
C--   o MNC_VAR_ADD_ATTR_DBL
C--   o MNC_VAR_ADD_ATTR_REAL
C--   o MNC_VAR_ADD_ATTR_INT
C--   o MNC_VAR_ADD_ATTR_ANY
C--   o MNC_VAR_WRITE_DBL
C--   o MNC_VAR_WRITE_REAL
C--   o MNC_VAR_WRITE_INT
C--   o MNC_VAR_APPEND_DBL
C--   o MNC_VAR_APPEND_REAL
C--   o MNC_VAR_APPEND_INT
C--   o MNC_VAR_WRITE_ANY

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
C     Create a double-precision real variable within a NetCDF file context.

C     !USES:
      IMPLICIT NONE
#include "netcdf.inc"

C     !INPUT PARAMETERS:
      CHARACTER*(*) fname,gname,vname
      INTEGER irv,myThid
CEOP

      CALL MNC_VAR_INIT_ANY( fname,gname,vname, NF_DOUBLE, irv,myThid )

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
C     Create a single-precision real variable within a NetCDF file context.

C     !USES:
      IMPLICIT NONE
#include "netcdf.inc"

C     !INPUT PARAMETERS:
      CHARACTER*(*) fname,gname,vname
      INTEGER irv,myThid
CEOP

      CALL MNC_VAR_INIT_ANY( fname,gname,vname, NF_FLOAT, irv,myThid )

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
      IMPLICIT NONE
#include "netcdf.inc"

C     !INPUT PARAMETERS:
      CHARACTER*(*) fname,gname,vname
      INTEGER irv,myThid
CEOP

      CALL MNC_VAR_INIT_ANY( fname,gname,vname, NF_INT, irv,myThid )

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
C     General function for creating variables within a NetCDF file context.

C     !USES:
      IMPLICIT NONE
#include "MNC_COMMON.h"
#include "EEPARAMS.h"
#include "netcdf.inc"

C     !INPUT PARAMETERS:
      CHARACTER*(*) fname,gname,vname
      INTEGER vtype
      INTEGER irv,myThid
CEOP

C     Functions
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      INTEGER i,j,k, n, nf, indf,indv, fid, nd, ngrid, is,ie, err
      INTEGER vid, nv, ind_g_finfo, needed, nvar
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER ids(20)
      INTEGER lenf,leng,lenv

C     Strip trailing spaces
      lenf = ILNBLNK(fname)
      leng = ILNBLNK(gname)
      lenv = ILNBLNK(vname)

C     Check that the file is open
      CALL MNC_GET_IND(MNC_MAX_FID, fname, mnc_f_names, indf, myThid)
      IF (indf .LT. 1) THEN
        nf = ILNBLNK( fname )
        WRITE(msgBuf,'(3A)') 'MNC ERROR: file ''', fname(1:nf),
     &       ''' must be opened first'
        CALL print_error(msgBuf, myThid)
        STOP 'ABNORMAL END: S/R MNC_VAR_INIT_ANY'
      ENDIF
      fid = mnc_f_info(indf,2)

C     Check for sufficient storage space in mnc_fv_ids
      needed = 1 + 3*(mnc_fv_ids(indf,1) + 1)
      IF (needed .GE. MNC_MAX_INFO) THEN
        WRITE(msgBuf,'(2A,I7,A)') 'MNC ERROR: MNC_MAX_INFO exceeded',
     &       ': please increase it to ', 2*MNC_MAX_INFO,
     &       ' in the file ''pkg/mnc/MNC_SIZE.h'''
        CALL print_error(msgBuf, myThid)
        STOP 'ABNORMAL END: S/R MNC_VAR_INIT_ANY'
      ENDIF

C     Get the grid information
      ngrid = mnc_f_info(indf,3)
      IF (ngrid .LT. 1) THEN
        WRITE(msgBuf,'(3A)') 'MNC ERROR: file ''', fname(1:lenf),
     &       ''' contains NO grids'
        CALL print_error(msgBuf, myThid)
        STOP 'ABNORMAL END: S/R MNC_VAR_INIT_ANY'
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
      WRITE(msgBuf,'(5A)') 'MNC ERROR: file ''', fname(1:lenf),
     &     ''' does not contain grid ''', gname(1:leng), ''''
      CALL print_error(msgBuf, myThid)
      STOP 'ABNORMAL END: S/R MNC_VAR_INIT_ANY'
 10   CONTINUE

C     Check if the variable is already defined
      nvar = mnc_fv_ids(indf,1)
      DO i = 1,nvar
        j = 2 + 3*(i-1)
        IF (mnc_v_names(mnc_fv_ids(indf,j)) .EQ. vname) THEN
          k = mnc_f_info(indf,mnc_fv_ids(indf,j+2))
          IF (mnc_g_names(k) .NE. gname) THEN
            WRITE(msgBuf,'(5A)') 'MNC ERROR: variable ''',
     &           vname(1:lenv), ''' is already defined in file ''',
     &           fname(1:lenf), ''' but using a different grid shape'
            CALL print_error(msgBuf, myThid)
            STOP 'ABNORMAL END: S/R MNC_VAR_INIT_ANY'
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
        WRITE(msgBuf,'(2A)') 'ERROR:  MNC will not ',
     &       'overwrite variables in existing NetCDF'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') '        files.  Please',
     &       ' make sure that you are not trying to'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') '        overwrite output',
     &       'files from a previous model run!'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(5A)') 'defining variable ''', vname(1:lenv),
     &     ''' in file ''', fname(1:lenf), ''''
        CALL MNC_HANDLE_ERR(err, msgBuf, myThid)
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
C     Subroutine for adding a character string attribute to a NetCDF file.

C     !USES:
      IMPLICIT NONE

C     !INPUT PARAMETERS:
      CHARACTER*(*) fname,vname,atname,sval
      INTEGER myThid
CEOP
      real*8 dZero(1)
      real*4 sZero(1)
      INTEGER iZero(1)
      dZero(1) = 0.0D0
      sZero(1) = 0.0
      iZero(1) = 0

      CALL MNC_VAR_ADD_ATTR_ANY( fname,vname,atname,
     &     1, sval, 0, dZero, sZero, iZero, myThid )

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
C     Subroutine for adding a double-precision real attribute to a NetCDF file.

C     !USES:
      IMPLICIT NONE

C     !INPUT PARAMETERS:
      CHARACTER*(*) fname,vname,atname
      INTEGER nv
      Real*8 dval(*)
      INTEGER myThid
CEOP
      real*4 sZero(1)
      INTEGER iZero(1)
      sZero(1) = 0.0
      iZero(1) = 0

      CALL MNC_VAR_ADD_ATTR_ANY( fname,vname,atname,
     &     2, ' ', nv, dval, sZero, iZero, myThid )

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
C     Subroutine for adding a single-precision real attribute to a NetCDF file.

C     !USES:
      IMPLICIT NONE

C     !INPUT PARAMETERS:
      CHARACTER*(*) fname,vname,atname
      INTEGER nv
      Real*4 rval(*)
      INTEGER myThid
CEOP
      real*8 dZero(1)
      INTEGER iZero(1)
      dZero(1) = 0.0D0
      iZero(1) = 0

      CALL MNC_VAR_ADD_ATTR_ANY( fname,vname,atname,
     &     3, ' ', nv, dZero, rval, iZero, myThid )
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
C     Subroutine for adding an integer attribute to a NetCDF file.

C     !USES:
      IMPLICIT NONE

C     !INPUT PARAMETERS:
      CHARACTER*(*) fname,vname,atname
      INTEGER nv
      INTEGER ival(*)
      INTEGER myThid
CEOP
      real*8 dZero(1)
      real*4 sZero(1)
      dZero(1) = 0.0D0
      sZero(1) = 0.0

      CALL MNC_VAR_ADD_ATTR_ANY( fname,vname,atname,
     &     4, ' ', nv, dZero, sZero, ival, myThid )

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
C     General SUBROUTINE for adding attributes to a NetCDF file.

C     !USES:
      IMPLICIT NONE
#include "MNC_COMMON.h"
#include "EEPARAMS.h"
#include "netcdf.inc"

C     !INPUT PARAMETERS:
      CHARACTER*(*) fname,vname,atname
      INTEGER atype
      CHARACTER*(*) cs
      INTEGER len
      Real*8 dv(*)
      Real*4 rv(*)
      INTEGER iv(*)
      INTEGER myThid
CEOP

C     Functions
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      INTEGER n, indf,ind_fv_ids, fid,vid, err
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER lenf,lenv,lenat,lens

C     Strip trailing spaces
      lenf = ILNBLNK(fname)
      lenv = ILNBLNK(vname)
      lenat = ILNBLNK(atname)
      lens = ILNBLNK(cs)

      CALL MNC_GET_FVINDS(fname, vname, indf, ind_fv_ids, myThid)
      IF ((indf .LT. 1).OR.(ind_fv_ids .LT. 1)) THEN
        WRITE(msgBuf,'(5A)') 'MNC ERROR: file ''', fname(1:lenf),
     &       ''' is not open or does not contain variable ''',
     &       vname(1:lenv), ''''
        CALL print_error(msgBuf, myThid)
        STOP 'ABNORMAL END: S/R MNC_VAR_ADD_ATTR_STR'
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
        WRITE(msgBuf,'(A,I10,A)') 'MNC ERROR: atype = ''', atype,
     &       ''' is invalid--must be: [1-4]'
        n = ILNBLNK(msgBuf)
        CALL print_error(msgBuf(1:n), myThid)
        STOP 'ABNORMAL END: S/R MNC_VAR_ADD_ATTR_ANY'
      ENDIF
      WRITE(msgBuf,'(5A)') 'adding attribute ''', atname(1:lenat),
     &     ''' to file ''', fname(1:lenf), ''''
      CALL MNC_HANDLE_ERR(err, msgBuf, myThid)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MNC_VAR_WRITE_DBL(
     I     fname,
     I     vname,
     I     var,
     I     myThid )

      IMPLICIT NONE
C     Arguments
      CHARACTER*(*) fname, vname
      Real*8 var(*)
      INTEGER myThid

C     Local Variables
      Real*4  dummyR4(1)
      INTEGER dummyI (1)

      DATA dummyR4 / 0. /
      DATA dummyI  / 0 /

      CALL MNC_VAR_WRITE_ANY( fname, vname, 1, 0,
     &                        var, dummyR4, dummyI, myThid )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MNC_VAR_WRITE_REAL(
     I     fname,
     I     vname,
     I     var,
     I     myThid )

      IMPLICIT NONE
C     Arguments
      CHARACTER*(*) fname, vname
      Real*4 var(*)
      INTEGER myThid

C     Local Variables
      Real*8  dummyR8(1)
      INTEGER dummyI (1)

      DATA dummyR8 / 0. _d 0 /
      DATA dummyI  / 0 /

      CALL MNC_VAR_WRITE_ANY( fname, vname, 2, 0,
     &                        dummyR8, var, dummyI, myThid )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MNC_VAR_WRITE_INT(
     I     fname,
     I     vname,
     I     var,
     I     myThid )

      IMPLICIT NONE
C     Arguments
      CHARACTER*(*) fname, vname
      INTEGER var(*)
      INTEGER myThid

C     Local Variables
      Real*8  dummyR8(1)
      Real*4  dummyR4(1)

      DATA dummyR8 / 0. _d 0 /
      DATA dummyR4 / 0. /

      CALL MNC_VAR_WRITE_ANY( fname, vname, 3, 0,
     &                        dummyR8, dummyR4, var, myThid )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MNC_VAR_APPEND_DBL(
     I     fname,
     I     vname,
     I     var,
     I     append,
     I     myThid )

      IMPLICIT NONE
C     Arguments
      CHARACTER*(*) fname, vname
      Real*8 var(*)
      INTEGER append, myThid

C     Local Variables
      Real*4  dummyR4(1)
      INTEGER dummyI (1)

      DATA dummyR4 / 0. /
      DATA dummyI  / 0 /

      CALL MNC_VAR_WRITE_ANY( fname, vname, 1, append,
     &                        var, dummyR4, dummyI, myThid )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MNC_VAR_APPEND_REAL(
     I     fname,
     I     vname,
     I     var,
     I     append,
     I     myThid )

      IMPLICIT NONE
C     Arguments
      CHARACTER*(*) fname, vname
      Real*4 var(*)
      INTEGER append, myThid

C     Local Variables
      Real*8  dummyR8(1)
      INTEGER dummyI (1)

      DATA dummyR8 / 0. _d 0 /
      DATA dummyI  / 0 /

      CALL MNC_VAR_WRITE_ANY( fname, vname, 2, append,
     &                        dummyR8, var, dummyI, myThid )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MNC_VAR_APPEND_INT(
     I     fname,
     I     vname,
     I     var,
     I     append,
     I     myThid )

      IMPLICIT NONE
C     Arguments
      CHARACTER*(*) fname, vname
      INTEGER var(*)
      INTEGER append, myThid

C     Local Variables
      Real*8  dummyR8(1)
      Real*4  dummyR4(1)

      DATA dummyR8 / 0. _d 0 /
      DATA dummyR4 / 0. /

      CALL MNC_VAR_WRITE_ANY( fname, vname, 3, append,
     &                        dummyR8, dummyR4, var, myThid )

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

      IMPLICIT NONE
#include "MNC_COMMON.h"
#include "EEPARAMS.h"
#include "netcdf.inc"

C     Arguments
      CHARACTER*(*) fname, vname
      INTEGER vtype
      INTEGER append
      Real*8 dv(*)
      Real*4 rv(*)
      INTEGER iv(*)
      INTEGER myThid

C     Functions
      INTEGER ILNBLNK

C     Local Variables
      INTEGER i,j,k, n, indf,ind_fv_ids, fid,vid,did, ig, err, ds,de
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER lenf,lenv, lend
      INTEGER vstart(100), vcount(100)

C     Strip trailing spaces
      lenf = ILNBLNK(fname)
      lenv = ILNBLNK(vname)

      CALL MNC_GET_FVINDS(fname, vname, indf, ind_fv_ids, myThid)
      IF ((indf .LT. 1).OR.(ind_fv_ids .LT. 1)) THEN
        WRITE(msgBuf,'(5A)') 'MNC ERROR: file ''', fname(1:lenf),
     &       ''' is not open or does not contain variable ''',
     &       vname(1:lenv), ''''
        CALL print_error(msgBuf, myThid)
        STOP 'ABNORMAL END: S/R MNC_VAR_ADD_ATTR_STR'
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
        WRITE(msgBuf,'(A)') 'reading current length of unlimited dim'
        CALL MNC_HANDLE_ERR(err, msgBuf, myThid)
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
        WRITE(msgBuf,'(A,I10,A)') 'MNC ERROR: vtype = ''', vtype,
     &       ''' is invalid--must be: [1|2|3]'
        n = ILNBLNK(msgBuf)
        CALL print_error(msgBuf(1:n), myThid)
        STOP 'ABNORMAL END: S/R MNC_VAR_WRITE_ALL'
      ENDIF
      WRITE(msgBuf,'(5A)') 'writing variable ''', vname(1:lenv),
     &     ''' to file ''', fname(1:lenf), ''''
      CALL MNC_HANDLE_ERR(err, msgBuf, myThid)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

