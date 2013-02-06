C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_mnc_out.F,v 1.5 2013/02/06 16:24:29 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"
#undef DIAG_MNC_COORD_NEEDSWORK

C--   File diagnostics_mnc_out.F: Routines to write MNC diagnostics output
C--    Contents:
C--    o DIAGNOSTICS_MNC_SET
C--    o DIAGNOSTICS_MNC_OUT

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: DIAGNOSTICS_MNC_SET

C     !INTERFACE:
      SUBROUTINE DIAGNOSTICS_MNC_SET(
     I     nLevOutp, listId, lm,
     O     diag_mnc_bn,
     I     misValLoc, myTime, myIter, myThid )

C     !DESCRIPTION:
C     Set MNC file for writing diagnostics fields.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"


C     !INPUT/OUTPUT PARAMETERS:
C     nLevOutp        :: number of levels to write in output file
C     listId          :: Diagnostics list number being written
C     lm              :: loop index (averageCycle)
C     diag_mnc_bn     :: NetCDF output file name
C     misValLoc       :: local Missing Value
C     myTime          :: current time of simulation (s)
C     myIter          :: current iteration number
C     myThid          :: my Thread Id number
      INTEGER nLevOutp
      INTEGER listId, lm
      CHARACTER*(MAX_LEN_FNAM) diag_mnc_bn
      _RL     misValLoc
      _RL     myTime
      INTEGER myIter, myThid
CEOP

#ifdef ALLOW_MNC
C     !FUNCTIONS:
      INTEGER ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      _RL tmpLev
      INTEGER iLen

c     CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER ii, klev
      INTEGER CW_DIMS, NLEN
      PARAMETER ( CW_DIMS = 10 )
      PARAMETER ( NLEN    = 80 )
      INTEGER dim(CW_DIMS), ib(CW_DIMS), ie(CW_DIMS)
      CHARACTER*(NLEN) dn(CW_DIMS)
c     CHARACTER*(NLEN) d_cw_name
c     CHARACTER*(NLEN) dn_blnk
#ifdef DIAG_MNC_COORD_NEEDSWORK
      INTEGER NrMax
      PARAMETER( NrMax = numLevels )
      INTEGER i, j
      CHARACTER*(5) ctmp
      _RS ztmp(NrMax)
#endif
      REAL*8  misval_r8(2)
      REAL*4  misval_r4(2)
      INTEGER misval_int(2)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
c     IF (useMNC .AND. diag_mnc) THEN

C     Handle missing value attribute (land points)
C     Defaults to UNSET_I
        DO ii=1,2
          misval_r4(ii)  = misValLoc
          misval_r8(ii)  = misValLoc
          misval_int(ii) = UNSET_I
        ENDDO
c       DO i = 1,MAX_LEN_FNAM
c         diag_mnc_bn(i:i) = ' '
c       ENDDO
c       DO i = 1,NLEN
c         dn_blnk(i:i) = ' '
c       ENDDO
        iLen = ILNBLNK(fnames(listId))
        WRITE( diag_mnc_bn, '(A)' ) fnames(listId)(1:iLen)

C       Update the record dimension by writing the iteration number
        klev = myIter + lm - averageCycle(listId)
        tmpLev = myTime + deltaTClock*( lm - averageCycle(listId) )
        CALL MNC_CW_SET_UDIM(diag_mnc_bn, -1, myThid)
        CALL MNC_CW_RL_W_S('D',diag_mnc_bn,0,0,'T',tmpLev,myThid)
        CALL MNC_CW_SET_UDIM(diag_mnc_bn, 0, myThid)
        CALL MNC_CW_I_W_S('I',diag_mnc_bn,0,0,'iter',klev,myThid)

C       NOTE: at some point it would be a good idea to add a time_bounds
C       variable that has dimension (2,T) and clearly denotes the
C       beginning and ending times for each diagnostics period

c       dn(1)(1:NLEN) = dn_blnk(1:NLEN)
        WRITE(dn(1),'(a,i6.6)') 'Zmd', nLevOutp
        dim(1) = nLevOutp
        ib(1)  = 1
        ie(1)  = nLevOutp

        CALL MNC_CW_ADD_GNAME('diag_levels', 1,
     &       dim, dn, ib, ie, myThid)
        CALL MNC_CW_ADD_VNAME('diag_levels', 'diag_levels',
     &       0,0, myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('diag_levels','description',
     &       'Idicies of vertical levels within the source arrays',
     &       myThid)
C     suppress the missing value attribute (iflag = 0)
        CALL MNC_CW_VATTR_MISSING('diag_levels', 0,
     I       misval_r8, misval_r4, misval_int, myThid )

        CALL MNC_CW_RL_W('D',diag_mnc_bn,0,0,
     &       'diag_levels', levs(1,listId), myThid)

        CALL MNC_CW_DEL_VNAME('diag_levels', myThid)
        CALL MNC_CW_DEL_GNAME('diag_levels', myThid)

#ifdef DIAG_MNC_COORD_NEEDSWORK
C       This part has been placed in an #ifdef because, as its currently
C       written, it will only work with variables defined on a dynamics
C       grid.  As we start using diagnostics for physics grids, ice
C       levels, land levels, etc. the different vertical coordinate
C       dimensions will have to be taken into account.

C       20051021 JMC & EH3 : We need to extend this so that a few
C       variables each defined on different grids do not have the same
C       vertical dimension names so we should be using a pattern such
C       as: Z[uml]td000000 where the 't' is the type as specified by
C       gdiag(10)

C       Now define:  Zmdxxxxxx, Zudxxxxxx, Zldxxxxxx
        ctmp(1:5) = 'mul  '
        DO i = 1,3
c         dn(1)(1:NLEN) = dn_blnk(1:NLEN)
          WRITE(dn(1),'(3a,i6.6)') 'Z',ctmp(i:i),'d',nlevels(listId)
          CALL MNC_CW_ADD_GNAME(dn(1), 1, dim, dn, ib, ie, myThid)
          CALL MNC_CW_ADD_VNAME(dn(1), dn(1), 0,0, myThid)

C         The following three ztmp() loops should eventually be modified
C         to reflect the fractional nature of levs(j,l) -- they should
C         do something like:
C            ztmp(j) = rC(INT(FLOOR(levs(j,l))))
C                      + ( rC(INT(FLOOR(levs(j,l))))
C                          + rC(INT(CEIL(levs(j,l)))) )
C                        / ( levs(j,l) - FLOOR(levs(j,l)) )
C         for averaged levels.
          IF (i .EQ. 1) THEN
            DO j = 1,nlevels(listId)
              ztmp(j) = rC(NINT(levs(j,listId)))
            ENDDO
            CALL MNC_CW_ADD_VATTR_TEXT(dn(1),'description',
     &           'Dimensional coordinate value at the mid point',
     &           myThid)
          ELSEIF (i .EQ. 2) THEN
            DO j = 1,nlevels(listId)
              ztmp(j) = rF(NINT(levs(j,listId)) + 1)
            ENDDO
            CALL MNC_CW_ADD_VATTR_TEXT(dn(1),'description',
     &           'Dimensional coordinate value at the upper point',
     &           myThid)
          ELSEIF (i .EQ. 3) THEN
            DO j = 1,nlevels(listId)
              ztmp(j) = rF(NINT(levs(j,listId)))
            ENDDO
            CALL MNC_CW_ADD_VATTR_TEXT(dn(1),'description',
     &           'Dimensional coordinate value at the lower point',
     &           myThid)
          ENDIF
C     suppress the missing value attribute (iflag = 0)
          IF (useMissingValue)
     &          CALL MNC_CW_VATTR_MISSING(dn(1), 0,
     I          misval_r8, misval_r4, misval_int, myThid )
          CALL MNC_CW_RS_W('D',diag_mnc_bn,0,0, dn(1), ztmp, myThid)
          CALL MNC_CW_DEL_VNAME(dn(1), myThid)
          CALL MNC_CW_DEL_GNAME(dn(1), myThid)
        ENDDO
#endif /*  DIAG_MNC_COORD_NEEDSWORK  */

c     ENDIF
#endif /*  ALLOW_MNC  */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: DIAGNOSTICS_MNC_OUT

C     !INTERFACE:
      SUBROUTINE DIAGNOSTICS_MNC_OUT(
     I     NrMax, nLevOutp, listId, ndId, mate,
     I     diag_mnc_bn, qtmp,
     I     misValLoc, myTime, myIter, myThid )

C     !DESCRIPTION:
C     write diagnostics fields to MNC file.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"

C     !INPUT PARAMETERS:
C     NrMax           :: 3rd dimension of output-field array to write
C     nLevOutp        :: number of levels to write in output file
C     listId          :: Diagnostics list number being written
C     ndId            :: diagnostics Id number (in available diagnostics list)
C     mate            :: counter diagnostic number if any ; 0 otherwise
C     diag_mnc_bn     :: NetCDF output file name
C     qtmp            :: output-field array to write
C     misValLoc       :: local Missing Value
C     myTime          :: current time of simulation (s)
C     myIter          :: current iteration number
C     myThid          :: my Thread Id number
      INTEGER NrMax
      INTEGER nLevOutp
      INTEGER listId, ndId, mate
      CHARACTER*(MAX_LEN_FNAM) diag_mnc_bn
      _RL     qtmp(1-OLx:sNx+OLx,1-OLy:sNy+OLy,NrMax,nSx,nSy)
      _RL     misValLoc
      _RL     myTime
      INTEGER myIter, myThid
CEOP

#ifdef ALLOW_MNC
C     !FUNCTIONS:
c     INTEGER ILNBLNK
c     EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
C     i,j,k :: loop indices
C     bi,bj :: tile indices
      INTEGER i, j, k
      INTEGER bi, bj

c     CHARACTER*(MAX_LEN_MBUF) msgBuf
c     INTEGER ll, llMx, jj, jjMx
      INTEGER ii, klev
      INTEGER CW_DIMS, NLEN
      PARAMETER ( CW_DIMS = 10 )
      PARAMETER ( NLEN    = 80 )
      INTEGER dim(CW_DIMS), ib(CW_DIMS), ie(CW_DIMS)
      CHARACTER*(NLEN) dn(CW_DIMS)
      CHARACTER*(NLEN) d_cw_name
c     CHARACTER*(NLEN) dn_blnk
      LOGICAL useMisValForThisDiag
      REAL*8  misval_r8(2)
      REAL*4  misval_r4(2)
      INTEGER misval_int(2)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
c     IF (useMNC .AND. diag_mnc) THEN

        _BEGIN_MASTER( myThid )

         DO ii = 1,CW_DIMS
c          d_cw_name(1:NLEN) = dn_blnk(1:NLEN)
c          dn(ii)(1:NLEN) = dn_blnk(1:NLEN)
           dn(ii) =  ' '
         ENDDO
         DO ii=1,2
           misval_r4(ii)  = misValLoc
           misval_r8(ii)  = misValLoc
           misval_int(ii) = UNSET_I
         ENDDO

C        Note that the "d_cw_name" variable is a hack that hides a
C        subtlety within MNC.  Basically, each MNC-wrapped file is
C        caching its own concept of what each "grid name" (that is, a
C        dimension group name) means.  So one cannot re-use the same
C        "grid" name for different collections of dimensions within a
C        given file.  By appending the "ndId" values to each name, we
C        guarantee uniqueness within each MNC-produced file.
         WRITE(d_cw_name,'(a,i6.6)') 'd_cw_',ndId

C        XY dimensions
         dim(1)       = sNx + 2*OLx
         dim(2)       = sNy + 2*OLy
         ib(1)        = OLx + 1
         ib(2)        = OLy + 1
         IF (gdiag(ndId)(2:2) .EQ. 'M') THEN
           dn(1)(1:2) = 'X'
           ie(1)      = OLx + sNx
           dn(2)(1:2) = 'Y'
           ie(2)      = OLy + sNy
         ELSEIF (gdiag(ndId)(2:2) .EQ. 'U') THEN
           dn(1)(1:3) = 'Xp1'
           ie(1)      = OLx + sNx + 1
           dn(2)(1:2) = 'Y'
           ie(2)      = OLy + sNy
         ELSEIF (gdiag(ndId)(2:2) .EQ. 'V') THEN
           dn(1)(1:2) = 'X'
           ie(1)      = OLx + sNx
           dn(2)(1:3) = 'Yp1'
           ie(2)      = OLy + sNy + 1
         ELSEIF (gdiag(ndId)(2:2) .EQ. 'Z') THEN
           dn(1)(1:3) = 'Xp1'
           ie(1)      = OLx + sNx + 1
           dn(2)(1:3) = 'Yp1'
           ie(2)      = OLy + sNy + 1
         ENDIF

C        Z is special since it varies
         WRITE(dn(3),'(a,i6.6)') 'Zd', nLevOutp
         IF ( (gdiag(ndId)(10:10) .EQ. 'R')
     &        .AND. (gdiag(ndId)(9:9) .EQ. 'M') ) THEN
           WRITE(dn(3),'(a,i6.6)') 'Zmd', nLevOutp
         ENDIF
         IF ( (gdiag(ndId)(10:10) .EQ. 'R')
     &        .AND. (gdiag(ndId)(9:9) .EQ. 'L') ) THEN
           WRITE(dn(3),'(a,i6.6)') 'Zld', nLevOutp
         ENDIF
         IF ( (gdiag(ndId)(10:10) .EQ. 'R')
     &        .AND. (gdiag(ndId)(9:9) .EQ. 'U') ) THEN
           WRITE(dn(3),'(a,i6.6)') 'Zud', nLevOutp
         ENDIF
         dim(3) = NrMax
         ib(3)  = 1
         ie(3)  = nLevOutp

C        Time dimension
         dn(4)(1:1) = 'T'
         dim(4) = -1
         ib(4)  = 1
         ie(4)  = 1

         CALL MNC_CW_ADD_GNAME( d_cw_name, 4,
     &                          dim, dn, ib, ie, myThid )
         CALL MNC_CW_ADD_VNAME( cdiag(ndId), d_cw_name,
     &                          4, 5, myThid )
         CALL MNC_CW_ADD_VATTR_TEXT( cdiag(ndId),'description',
     &                               tdiag(ndId), myThid )
         CALL MNC_CW_ADD_VATTR_TEXT( cdiag(ndId),'units',
     &                               udiag(ndId), myThid )

         useMisValForThisDiag = mate.GT.0
C     Use the missing values for masking out the land points:
C     only for scalar diagnostics at mass points (so far)
         IF ( useMissingValue.AND.gdiag(ndId)(1:2).EQ.'SM' ) THEN
           useMisValForThisDiag = .TRUE.
C     note: better to use 2-D mask if kdiag <> Nr or vert.integral
           DO bj = myByLo(myThid), myByHi(myThid)
            DO bi = myBxLo(myThid), myBxHi(myThid)
             DO k = 1,nLevOutp
              klev = NINT(levs(k,listId))
              IF ( fflags(listId)(2:2).EQ.'I' ) kLev = 1
              DO j = 1-OLy,sNy+OLy
               DO i = 1-OLx,sNx+OLx
                IF ( maskC(i,j,klev,bi,bj) .EQ. 0. )
     &                qtmp(i,j,k,bi,bj) = misValLoc
               ENDDO
              ENDDO
             ENDDO
            ENDDO
           ENDDO
         ENDIF
         IF ( useMisValForThisDiag ) THEN
C     assign missing values and set flag for adding the netCDF atttibute
           CALL MNC_CW_VATTR_MISSING(cdiag(ndId), 2,
     I                misval_r8, misval_r4, misval_int, myThid )
         ELSE
C     suppress the missing value attribute (iflag = 0)
C     Note: We have to call the following subroutine for each mnc that has
C     been created "on the fly" by mnc_cw_add_vname and will be deleted
C     by mnc_cw_del_vname, because all of these variables use the same
C     identifier so that mnc_cw_vfmv(indv) needs to be overwritten for
C     each of these variables
           CALL MNC_CW_VATTR_MISSING( cdiag(ndId), 0,
     I                 misval_r8, misval_r4, misval_int, myThid )
         ENDIF

         IF (  ((writeBinaryPrec .EQ. precFloat32).AND.
     &          (fflags(listId)(1:1) .NE. 'D'))
     &         .OR. (fflags(listId)(1:1) .EQ. 'R') ) THEN
           CALL MNC_CW_RL_W( 'R',diag_mnc_bn,0,0,
     &                       cdiag(ndId), qtmp, myThid)
         ELSEIF ( (writeBinaryPrec .EQ. precFloat64)
     &         .OR.  (fflags(listId)(1:1) .EQ. 'D') ) THEN
           CALL MNC_CW_RL_W( 'D',diag_mnc_bn,0,0,
     &                       cdiag(ndId), qtmp, myThid)
         ENDIF

         CALL MNC_CW_DEL_VNAME(cdiag(ndId), myThid)
         CALL MNC_CW_DEL_GNAME(d_cw_name, myThid)

        _END_MASTER( myThid )

c     ENDIF
#endif /*  ALLOW_MNC  */

      RETURN
      END
