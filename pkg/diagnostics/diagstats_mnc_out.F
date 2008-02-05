C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagstats_mnc_out.F,v 1.8 2008/02/05 15:31:19 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: DIAGSTATS_MNC_OUT

C     !INTERFACE:
      SUBROUTINE DIAGSTATS_MNC_OUT(
     I     statGlob, nLev, ndId,
     I     mId, listId, myTime, myIter, myThid )

C     !DESCRIPTION:
C     Write Global statistics to a netCDF file

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"

#ifdef ALLOW_FIZHI
#include "fizhi_SIZE.h"
#else
      INTEGER Nrphys
      PARAMETER (Nrphys=0)
#endif

C     !INPUT PARAMETERS:
C     statGlob :: AVERAGED DIAGNOSTIC QUANTITY
C     nLev     :: 2nd Dimension (max Nb of levels) of statGlob array
C     ndId     :: diagnostic Id number (in diagnostics long list)
C     mId      :: field rank in list "listId"
C     listId   :: current output Stream list
C     myIter   :: current Iteration Number
C     myTime   :: current time of simulation (s)
C     myThid   :: my thread Id number
      INTEGER nLev
      _RL     statGlob(0:nStats,0:nLev,0:nRegions)
      _RL     myTime
      INTEGER ndId, mId, listId
      INTEGER myIter, myThid
CEOP

C     !LOCAL VARIABLES:
#ifdef ALLOW_MNC
      INTEGER im, ix, iv, ist
      PARAMETER ( iv = nStats - 2 , im = nStats - 1 , ix = nStats )
      INTEGER i, j, k
      CHARACTER*(MAX_LEN_MBUF) tnam
      CHARACTER*(3) stat_typ(5)
      INTEGER ILNBLNK
      EXTERNAL ILNBLNK
      INTEGER ii, ilen
      CHARACTER*(MAX_LEN_FNAM) diag_mnc_bn
      INTEGER CW_DIMS, NLEN
      PARAMETER ( CW_DIMS = 10 )
      PARAMETER ( NLEN    = 80 )
      INTEGER dim(CW_DIMS), ib(CW_DIMS), ie(CW_DIMS)
      CHARACTER*(NLEN) dn(CW_DIMS)
      CHARACTER*(NLEN) d_cw_gname
      CHARACTER*(NLEN) d_cw_gname0
      CHARACTER*(NLEN) dn_blnk
#ifdef DIAGST_MNC_NEEDSWORK
      CHARACTER*(5) ctmp
      _RS ztmp(Nr+Nrphys)
#endif
      _RL stmp(Nr+Nrphys+1,nRegions+1)
#endif /*  ALLOW_MNC  */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_MNC

      _BEGIN_MASTER( myThid)

      stat_typ(1) = 'vol'
      stat_typ(2) = 'ave'
      stat_typ(3) = 'std'
      stat_typ(4) = 'min'
      stat_typ(5) = 'max'

#ifdef ALLOW_USE_MPI
      IF ( diagSt_MNC .AND. mpiMyId.EQ.0 ) THEN
#else
      IF ( diagSt_MNC ) THEN
#endif

        DO i = 1,MAX_LEN_FNAM
          diag_mnc_bn(i:i) = ' '
        ENDDO
        DO i = 1,NLEN
          dn_blnk(i:i) = ' '
        ENDDO
        ilen = ILNBLNK(diagSt_Fname(listId))
        WRITE(diag_mnc_bn, '(a)') diagSt_Fname(listId)(1:ilen)

        IF (mId .EQ. 1) THEN
C         Update the record dimension by writing the iteration number
          CALL MNC_CW_SET_UDIM(diag_mnc_bn, -1, myThid)
          CALL MNC_CW_RL_W_S('D',diag_mnc_bn,1,1,'T',myTime,myThid)
          CALL MNC_CW_SET_UDIM(diag_mnc_bn, 0, myThid)
          CALL MNC_CW_I_W_S('I',diag_mnc_bn,1,1,'iter',myIter,myThid)
        ENDIF

#ifdef DIAGST_MNC_NEEDSWORK
C       This is turned off for the time being but it should eventually
C       be re-worked and turned on so that coordinate dimensions are
C       supplied along with the data.  Unfortunately, the current
C       diagnostics system has **NO** way of telling us whether a
C       quantity is defined on a typical vertical grid (eg. the dynamics
C       grid), a gridalt--style grid, or a single-level field that has
C       no specified vertical location.

        dn(1)(1:NLEN) = dn_blnk(1:NLEN)
        WRITE(dn(1),'(a,i6.6)') 'Zmd', kdiag(ndId)
        dim(1) = kdiag(ndId)
        ib(1)  = 1
        ie(1)  = kdiag(ndId)

        CALL MNC_CW_ADD_GNAME('diag_levels', 1,
     &       dim, dn, ib, ie, myThid)
        CALL MNC_CW_ADD_VNAME('diag_levels', 'diag_levels',
     &       0,0, myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('diag_levels','description',
     &       'Idicies of vertical levels within the source arrays',
     &       myThid)

        CALL MNC_CW_RL_W('D',diag_mnc_bn,1,1,
     &       'diag_levels', levs(1,listId), myThid)

        CALL MNC_CW_DEL_VNAME('diag_levels', myThid)
        CALL MNC_CW_DEL_GNAME('diag_levels', myThid)

C       Now define:  Zmdxxxxxx, Zudxxxxxx, Zldxxxxxx
        ctmp(1:5) = 'mul  '
        DO i = 1,3
          dn(1)(1:NLEN) = dn_blnk(1:NLEN)
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
          CALL MNC_CW_RS_W('D',diag_mnc_bn,1,1, dn(1), ztmp, myThid)
          CALL MNC_CW_DEL_VNAME(dn(1), myThid)
          CALL MNC_CW_DEL_GNAME(dn(1), myThid)
        ENDDO
#endif  /* DIAGST_MNC_NEEDSWORK */

        DO ii = 1,CW_DIMS
          d_cw_gname(1:NLEN) = dn_blnk(1:NLEN)
          dn(ii)(1:NLEN) = dn_blnk(1:NLEN)
        ENDDO

C       Z is special since it varies
        WRITE(dn(1),'(a,i6.6)') 'Zd', kdiag(ndId)
        IF ( (gdiag(ndId)(10:10) .EQ. 'R')
     &       .AND. (gdiag(ndId)(9:9) .EQ. 'M') ) THEN
          WRITE(dn(1),'(a,i6.6)') 'Zmd', kdiag(ndId)
        ENDIF
        IF ( (gdiag(ndId)(10:10) .EQ. 'R')
     &       .AND. (gdiag(ndId)(9:9) .EQ. 'L') ) THEN
          WRITE(dn(1),'(a,i6.6)') 'Zld', kdiag(ndId)
        ENDIF
        IF ( (gdiag(ndId)(10:10) .EQ. 'R')
     &       .AND. (gdiag(ndId)(9:9) .EQ. 'U') ) THEN
          WRITE(dn(1),'(a,i6.6)') 'Zud', kdiag(ndId)
        ENDIF
        dim(1) = Nr+Nrphys+1
        ib(1)  = 1
        ie(1)  = kdiag(ndId)

C       "region" dimension
        dim(2)     = nRegions + 1
        ib(2)      = 1
        dn(2)(1:6) = 'region'
        ie(2)      = nRegions + 1

C       Time dimension
        dn(3)(1:1) = 'T'
        dim(3)     = -1
        ib(3)      = 1
        ie(3)      = 1

C       Note that the "d_cw_gname" variable is a hack that hides a
C       subtlety within MNC.  Basically, each MNC-wrapped file is
C       caching its own concept of what each "grid name" (that is, a
C       dimension group name) means.  So one cannot re-use the same
C       "grid" name for different collections of dimensions within a
C       given file.  By appending the "ndId" values to each name, we
C       guarantee uniqueness within each MNC-produced file.

        WRITE(d_cw_gname,'(a7,i6.6)') 'dst_cw_', ndId
        CALL MNC_CW_ADD_GNAME(d_cw_gname, 3,
     &       dim, dn, ib, ie, myThid)

        WRITE(dn(1),'(a3)') 'Zd0'
        ie(1)  = 1
        WRITE(d_cw_gname0,'(a9,i6.6)') 'dst_cw_0_', ndId
        CALL MNC_CW_ADD_GNAME(d_cw_gname0, 3,
     &       dim, dn, ib, ie, myThid)

        DO ist = 0,nStats

          DO i = 1,MAX_LEN_FNAM
            tnam(i:i) = ' '
          ENDDO

c         IF ( kdiag(ndId) .GT. 1 ) THEN

            ilen = ILNBLNK(cdiag(ndId))
            WRITE(tnam,'(a,a1,a3)')
     &           cdiag(ndId)(1:ilen),'_',stat_typ(ist+1)

            CALL MNC_CW_ADD_VNAME(tnam, d_cw_gname0,
     &           0,0, myThid)
            CALL MNC_CW_ADD_VATTR_TEXT(tnam,'description',
     &           tdiag(ndId),myThid)
            CALL MNC_CW_ADD_VATTR_TEXT(tnam,'units',
     &           udiag(ndId),myThid)

C           Copy the data into a temporary with the necessary shape
            DO j = 0,nRegions
              stmp(1,j+1) = statGlob(ist,0,j)
            ENDDO

C-jmc: fflags is not for Statistics-Diagnostics, can be unset, and since
C-     size of the output file will not be an issue here: Always write real*8
c           IF ((fflags(listId)(1:1) .EQ. ' ')
c    &           .OR. (fflags(listId)(1:1) .EQ. 'R')) THEN
c
c             CALL MNC_CW_RL_W('R',diag_mnc_bn,1,1,
c    &             tnam, stmp, myThid)
c
c           ELSEIF (fflags(listId)(1:1) .EQ. 'D') THEN

              CALL MNC_CW_RL_W('D',diag_mnc_bn,1,1,
     &             tnam, stmp, myThid)

c           else
c             write(0,*) myIter, ndId, listId
c             write(0,'(3A)') '>',cdiag(ndId),'<'
c             write(0,'(3A)') '>',fflags(listId),'<'
c             STOP ' in DIAGSTATS_MNC_OUT'
c           ENDIF

            CALL MNC_CW_DEL_VNAME(tnam, myThid)

c         ENDIF

          IF ( kdiag(ndId) .GT. 1 ) THEN

            ilen = ILNBLNK(cdiag(ndId))
            WRITE(tnam,'(a,a4,a3)')
     &           cdiag(ndId)(1:ilen),'_lv_',stat_typ(ist+1)

            CALL MNC_CW_ADD_VNAME(tnam, d_cw_gname,
     &           0,0, myThid)
            CALL MNC_CW_ADD_VATTR_TEXT(tnam,'description',
     &           tdiag(ndId),myThid)
            CALL MNC_CW_ADD_VATTR_TEXT(tnam,'units',
     &         udiag(ndId),myThid)

C           Copy the data into a temporary with the necessary shape
            DO j = 0,nRegions
              DO k = 1,kdiag(ndId)
                stmp(k,j+1) = statGlob(ist,k,j)
              ENDDO
            ENDDO

C-jmc: Always write real*8 (size of the output file will not be an issue here)
              CALL MNC_CW_RL_W('D',diag_mnc_bn,1,1,
     &             tnam, stmp, myThid)

            CALL MNC_CW_DEL_VNAME(tnam, myThid)

          ENDIF

        ENDDO

        CALL MNC_CW_DEL_GNAME(d_cw_gname, myThid)
        CALL MNC_CW_DEL_GNAME(d_cw_gname0, myThid)

      ENDIF

      _END_MASTER( myThid )

#endif /*  ALLOW_MNC  */

      RETURN
      END
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
