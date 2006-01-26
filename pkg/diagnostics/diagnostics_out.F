C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_out.F,v 1.26 2006/01/26 04:15:05 edhill Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: DIAGNOSTICS_OUT

C     !INTERFACE:
      SUBROUTINE  DIAGNOSTICS_OUT(
     I     listId,
     I     myIter,
     I     myTime,
     I     myThid )

C     !DESCRIPTION:
C     Write output for diagnostics fields.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
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
C     listId  :: Diagnostics list number being written
C     myIter  :: current iteration number
C     myTime  :: current time of simulation (s)
C     myThid  :: my Thread Id number
      _RL     myTime
      INTEGER listId, myIter, myThid
CEOP

C     !LOCAL VARIABLES:
C     i,j,k :: loop indices
C     md    :: field number in the list "listId".
C     ndId  :: diagnostics  Id number (in available diagnostics list)
C     mate  :: counter mate Id number (in available diagnostics list)
C     ip    :: diagnostics  pointer to storage array
C     im    :: counter-mate pointer to storage array
      INTEGER i, j, k
      INTEGER bi, bj
      INTEGER md, ndId, ip, im
      INTEGER mate, mVec
      CHARACTER*8 parms1
      CHARACTER*3 mate_index
      _RL qtmp1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+Nrphys,nSx,nSy)
      _RL undef, getcon
      EXTERNAL getcon
      INTEGER ILNBLNK
      EXTERNAL ILNBLNK
      INTEGER ilen
      INTEGER nlevsout

      INTEGER ioUnit
      CHARACTER*(MAX_LEN_FNAM) fn
      CHARACTER*(MAX_LEN_MBUF) suff
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL glf
#ifdef ALLOW_MNC
      INTEGER ii
      CHARACTER*(MAX_LEN_FNAM) diag_mnc_bn
      INTEGER CW_DIMS, NLEN
      PARAMETER ( CW_DIMS = 10 )
      PARAMETER ( NLEN    = 80 )
      INTEGER dim(CW_DIMS), ib(CW_DIMS), ie(CW_DIMS)
      CHARACTER*(NLEN) dn(CW_DIMS)
      CHARACTER*(NLEN) d_cw_name
      CHARACTER*(NLEN) dn_blnk
#ifdef DIAG_MNC_COORD_NEEDSWORK
      CHARACTER*(5) ctmp
      _RS ztmp(Nr+Nrphys)
#endif
#endif /*  ALLOW_MNC  */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      ioUnit= standardMessageUnit
      undef = getcon('UNDEF')
      glf = globalFiles
      WRITE(suff,'(I10.10)') myIter
      ilen = ILNBLNK(fnames(listId))
      WRITE( fn, '(A,A,A)' ) fnames(listId)(1:ilen),'.',suff(1:10)

#ifdef ALLOW_MNC
      IF (useMNC .AND. diag_mnc) THEN
        DO i = 1,MAX_LEN_FNAM
          diag_mnc_bn(i:i) = ' '
        ENDDO
        DO i = 1,NLEN
          dn_blnk(i:i) = ' '
        ENDDO
        WRITE( diag_mnc_bn, '(A)' ) fnames(listId)(1:ilen)

C       Update the record dimension by writing the iteration number
        CALL MNC_CW_SET_UDIM(diag_mnc_bn, -1, myThid)
        CALL MNC_CW_RL_W_S('D',diag_mnc_bn,0,0,'T',myTime,myThid)
        CALL MNC_CW_SET_UDIM(diag_mnc_bn, 0, myThid)
        CALL MNC_CW_I_W_S('I',diag_mnc_bn,0,0,'iter',myIter,myThid)

C       NOTE: at some point it would be a good idea to add a time_bounds
C       variable that has dimension (2,T) and clearly denotes the
C       beginning and ending times for each diagnostics period

        dn(1)(1:NLEN) = dn_blnk(1:NLEN)
        WRITE(dn(1),'(a,i6.6)') 'Zmd', nlevels(listId)
        dim(1) = nlevels(listId)
        ib(1)  = 1
        ie(1)  = nlevels(listId)

        CALL MNC_CW_ADD_GNAME('diag_levels', 1, 
     &       dim, dn, ib, ie, myThid)
        CALL MNC_CW_ADD_VNAME('diag_levels', 'diag_levels', 
     &       0,0, myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('diag_levels','description',
     &       'Idicies of vertical levels within the source arrays',
     &       myThid)
        
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
          CALL MNC_CW_RS_W('D',diag_mnc_bn,0,0, dn(1), ztmp, myThid)
          CALL MNC_CW_DEL_VNAME(dn(1), myThid)
          CALL MNC_CW_DEL_GNAME(dn(1), myThid)
        ENDDO
#endif /*  DIAG_MNC_COORD_NEEDSWORK  */

      ENDIF
#endif /*  ALLOW_MNC  */

      DO md = 1,nfields(listId)
        ndId = jdiag(md,listId)
        parms1 = gdiag(ndId)(1:8)
        IF ( idiag(md,listId).NE.0 .AND. parms1(5:5).NE.'D' ) THEN
C--     Start processing 1 Fld :

          ip = ABS(idiag(md,listId))
          im = mdiag(md,listId)
          IF ( ndiag(ip,1,1).EQ.0 ) THEN
C-        Empty diagnostics case :

            _BEGIN_MASTER( myThid )
            WRITE(msgBuf,'(A,I10)')
     &        '- WARNING - from DIAGNOSTICS_OUT at iter=', myIter
            CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                          SQUEEZE_RIGHT, myThid)
            WRITE(msgBuf,'(A,I4,3A,I3,2A)')
     &       '- WARNING -   diag.#',ndId, ' : ',flds(md,listId),
     &       ' (#',md,' ) in outp.Stream: ',fnames(listId)
            CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                          SQUEEZE_RIGHT, myThid)
            WRITE(msgBuf,'(A,I2,A)')
     &       '- WARNING -   has not been filled (ndiag=',
     &       ndiag(ip,1,1), ' )'
            CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                          SQUEEZE_RIGHT, myThid)
            WRITE(msgBuf,'(A)')
     &       'WARNING DIAGNOSTICS_OUT  => write ZEROS instead'
            CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                          SQUEEZE_RIGHT, myThid)
            _END_MASTER( myThid )
            DO bj = myByLo(myThid), myByHi(myThid)
              DO bi = myBxLo(myThid), myBxHi(myThid)
                DO k = 1,nlevels(listId)
                  DO j = 1-OLy,sNy+OLy
                    DO i = 1-OLx,sNx+OLx
                      qtmp1(i,j,k,bi,bj) = 0. _d 0
                    ENDDO
                  ENDDO
                ENDDO
              ENDDO
            ENDDO

          ELSE
C-        diagnostics is not empty :

            IF ( myThid.EQ.1 ) WRITE(ioUnit,'(A,I3,3A,I8,2A)')
     &         ' Computing Diagnostic # ', ndId, '  ', cdiag(ndId),
     &         '     Counter:',ndiag(ip,1,1),'   Parms: ',gdiag(ndId)

            IF ( parms1(5:5).EQ.'C' ) THEN
C             Check for Mate of a Counter Diagnostic
C             --------------------------------------
              mate_index = parms1(6:8)
              READ (mate_index,'(I3)') mate
              IF ( myThid.EQ.1 ) WRITE(ioUnit,'(3A,I3,2A)')
     &         '       use Counter Mate for  ', cdiag(ndId),
     &         '     Diagnostic # ',mate, '  ', cdiag(mate)

            ELSE
              mate = 0

C             Check for Mate of a Vector Diagnostic
C             -------------------------------------
              IF ( parms1(1:1).EQ.'U' .OR. parms1(1:1).EQ.'V' ) THEN
                mate_index = parms1(6:8)
                READ (mate_index,'(I3)') mVec
                IF ( im.GT.0 .AND. ndiag(MAX(1,im),1,1).GT.0 ) THEN
                 IF ( myThid.EQ.1 ) WRITE(ioUnit,'(3A,I3,3A)')
     &             '           Vector  Mate for  ', cdiag(ndId),
     &             '     Diagnostic # ',mVec, '  ', cdiag(mVec),
     &             ' exists '
                ELSE
                 IF ( myThid.EQ.1 ) WRITE(ioUnit,'(3A,I3,3A)')
     &             '           Vector  Mate for  ', cdiag(ndId),
     &             '     Diagnostic # ',mVec, '  ', cdiag(mVec),
     &             ' not enabled'
                ENDIF
              ENDIF
            ENDIF

            DO bj = myByLo(myThid), myByHi(myThid)
             DO bi = myBxLo(myThid), myBxHi(myThid)
              DO k = 1,nlevels(listId)
                CALL GETDIAG(
     I                       levs(k,listId),undef,
     O                       qtmp1(1-OLx,1-OLy,k,bi,bj),
     I                       ndId,mate,ip,im,bi,bj,myThid)
              ENDDO
             ENDDO
            ENDDO

C-        end of empty diag / not empty block
          ENDIF

          nlevsout = nlevels(listId)

C-----------------------------------------------------------------------
C         Check to see if we need to interpolate before output
C-----------------------------------------------------------------------
         IF ( fflags(listId)(2:2).EQ.'P' ) THEN
C-        Do vertical interpolation:
          CALL DIAGNOSTICS_INTERP_VERT(
     I                     listId, md, ndId, ip, im,
     U                     nlevsout,
     U                     qtmp1,
     I                     undef,
     I                     myTime, myIter, myThid )
         ENDIF

#ifdef ALLOW_MDSIO
C         Prepare for mdsio optionality
          IF (diag_mdsio) THEN
            IF (fflags(listId)(1:1) .EQ. 'R') THEN
C             Force it to be 32-bit precision
              CALL MDSWRITEFIELD_NEW(fn,precFloat32,glf,.FALSE.,
     &             'RL',Nr+Nrphys,nlevsout,qtmp1,md,myIter,myThid)
            ELSEIF (fflags(listId)(1:1) .EQ. 'D') THEN
C             Force it to be 64-bit precision
              CALL MDSWRITEFIELD_NEW(fn,precFloat64,glf,.FALSE.,
     &             'RL',Nr+Nrphys,nlevsout,qtmp1,md,myIter,myThid)
            ELSE
C             This is the old default behavior
              CALL MDSWRITEFIELD_NEW(fn,writeBinaryPrec,glf,.FALSE.,
     &             'RL',Nr+Nrphys,nlevsout,qtmp1,md,myIter,myThid)
            ENDIF
          ENDIF
#endif /*  ALLOW_MDSIO  */

#ifdef ALLOW_MNC
          IF (useMNC .AND. diag_mnc) THEN

            _BEGIN_MASTER( myThid )

            DO ii = 1,CW_DIMS
              d_cw_name(1:NLEN) = dn_blnk(1:NLEN)
              dn(ii)(1:NLEN) = dn_blnk(1:NLEN)
            ENDDO

C           Note that the "d_cw_name" variable is a hack that hides a
C           subtlety within MNC.  Basically, each MNC-wrapped file is
C           caching its own concept of what each "grid name" (that is, a
C           dimension group name) means.  So one cannot re-use the same
C           "grid" name for different collections of dimensions within a
C           given file.  By appending the "ndId" values to each name, we
C           guarantee uniqueness within each MNC-produced file.
            WRITE(d_cw_name,'(a,i6.6)') 'd_cw_',ndId

C           XY dimensions
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
            
C           Z is special since it varies
            WRITE(dn(3),'(a,i6.6)') 'Zd', nlevsout
            IF ( (gdiag(ndId)(10:10) .EQ. 'R')
     &           .AND. (gdiag(ndId)(9:9) .EQ. 'M') ) THEN
              WRITE(dn(3),'(a,i6.6)') 'Zmd', nlevsout
            ENDIF
            IF ( (gdiag(ndId)(10:10) .EQ. 'R')
     &           .AND. (gdiag(ndId)(9:9) .EQ. 'L') ) THEN
              WRITE(dn(3),'(a,i6.6)') 'Zld', nlevsout
            ENDIF
            IF ( (gdiag(ndId)(10:10) .EQ. 'R')
     &           .AND. (gdiag(ndId)(9:9) .EQ. 'U') ) THEN
              WRITE(dn(3),'(a,i6.6)') 'Zud', nlevsout
            ENDIF
            dim(3) = Nr+Nrphys
            ib(3)  = 1
            ie(3)  = nlevsout

C           Time dimension
            dn(4)(1:1) = 'T'
            dim(4) = -1
            ib(4)  = 1
            ie(4)  = 1

            CALL MNC_CW_ADD_GNAME(d_cw_name, 4, 
     &             dim, dn, ib, ie, myThid)
            CALL MNC_CW_ADD_VNAME(cdiag(ndId), d_cw_name, 
     &             4,5, myThid)
            CALL MNC_CW_ADD_VATTR_TEXT(cdiag(ndId),'description',
     &             tdiag(ndId),myThid)
            CALL MNC_CW_ADD_VATTR_TEXT(cdiag(ndId),'units',
     &             udiag(ndId),myThid)

            IF ( ( (writeBinaryPrec .EQ. precFloat32)
     &           .AND. (fflags(listId)(1:1) .NE. 'D')
     &           .AND. (fflags(listId)(1:1) .NE. 'R') )
     &           .OR. (fflags(listId)(1:1) .EQ. 'R')) THEN
              CALL MNC_CW_RL_W('R',diag_mnc_bn,0,0,
     &             cdiag(ndId), qtmp1, myThid)
            ELSEIF ( (writeBinaryPrec .EQ. precFloat64) 
     &             .OR. (fflags(listId)(1:1) .EQ. 'D') ) THEN
              CALL MNC_CW_RL_W('D',diag_mnc_bn,0,0,
     &             cdiag(ndId), qtmp1, myThid)
            ENDIF
            
            CALL MNC_CW_DEL_VNAME(cdiag(ndId), myThid)
            CALL MNC_CW_DEL_GNAME(d_cw_name, myThid)

            _END_MASTER( myThid )

          ENDIF
#endif /*  ALLOW_MNC  */

C--     end of Processing Fld # md
        ENDIF
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
