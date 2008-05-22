C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_out.F,v 1.37 2008/05/22 08:35:44 mlosch Exp $
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

      INTEGER NrMax
      PARAMETER( NrMax = numLevels )

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
C     lm    :: loop index (averageCycle)
C     md    :: field number in the list "listId".
C     ndId  :: diagnostics  Id number (in available diagnostics list)
C     mate  :: counter mate Id number (in available diagnostics list)
C     ip    :: diagnostics  pointer to storage array
C     im    :: counter-mate pointer to storage array
C
C--   COMMON /LOCAL_DIAGNOSTICS_OUT/ local common block (for multi-threaded)
C     qtmp1 :: thread-shared temporary array (needs to be in common block):
C              to write a diagnostic field to file, copy it first from (big)
C              diagnostic storage qdiag into it.
      COMMON /LOCAL_DIAGNOSTICS_OUT/ qtmp1
      _RL qtmp1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,NrMax,nSx,nSy)

      INTEGER i, j, k, lm, klev
      INTEGER bi, bj
      INTEGER md, ndId, ip, im
      INTEGER mate, mVec
      CHARACTER*10 gcode
      _RL undef, getcon
      _RL tmpLev
      EXTERNAL getcon
      INTEGER ILNBLNK
      EXTERNAL ILNBLNK
      INTEGER ilen

      INTEGER ioUnit
      CHARACTER*(MAX_LEN_FNAM) fn
      CHARACTER*(MAX_LEN_MBUF) suff
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER prec, nRec
#ifdef ALLOW_MDSIO
      LOGICAL glf
#endif
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
      _RS ztmp(NrMax)
#endif
      REAL*8 misvalLoc
      REAL*8 misval_r8(2)
      REAL*4 misval_r4(2)
      INTEGER misvalIntLoc, misval_int(2)
#endif /*  ALLOW_MNC  */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      ioUnit= standardMessageUnit
      undef = getcon('UNDEF')
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

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      DO md = 1,nfields(listId)
        ndId = jdiag(md,listId)
        gcode = gdiag(ndId)(1:10)
        mate = 0
        mVec = 0
        IF ( gcode(5:5).EQ.'C' ) THEN
C-      Check for Mate of a Counter Diagnostic
           mate = hdiag(ndId)
        ELSEIF ( gcode(1:1).EQ.'U' .OR. gcode(1:1).EQ.'V' ) THEN
C-      Check for Mate of a Vector Diagnostic
           mVec = hdiag(ndId)
        ENDIF
        IF ( idiag(md,listId).NE.0 .AND. gcode(5:5).NE.'D' ) THEN
C--     Start processing 1 Fld :
         DO lm=1,averageCycle(listId)

          ip = ABS(idiag(md,listId)) + kdiag(ndId)*(lm-1)
          im = mdiag(md,listId)
          IF (mate.GT.0) im = im + kdiag(mate)*(lm-1)
          IF (mVec.GT.0) im = im + kdiag(mVec)*(lm-1)

          IF ( ndiag(ip,1,1).EQ.0 ) THEN
C-        Empty diagnostics case :

            _BEGIN_MASTER( myThid )
            WRITE(msgBuf,'(A,I10)')
     &        '- WARNING - from DIAGNOSTICS_OUT at iter=', myIter
            CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                          SQUEEZE_RIGHT, myThid)
            WRITE(msgBuf,'(A,I6,3A,I4,2A)')
     &       '- WARNING -   diag.#',ndId, ' : ',flds(md,listId),
     &       ' (#',md,' ) in outp.Stream: ',fnames(listId)
            CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                          SQUEEZE_RIGHT, myThid)
            IF ( averageCycle(listId).GT.1 ) THEN
             WRITE(msgBuf,'(A,2(I3,A))')
     &        '- WARNING -   has not been filled (ndiag(lm=',lm,')=',
     &                                            ndiag(ip,1,1), ' )'
            ELSE
             WRITE(msgBuf,'(A,2(I3,A))')
     &        '- WARNING -   has not been filled (ndiag=',
     &                                            ndiag(ip,1,1), ' )'
            ENDIF
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

            IF ( debugLevel.GE.debLevA .AND. myThid.EQ.1 ) THEN
              WRITE(ioUnit,'(A,I6,3A,I8,2A)')
     &         ' Computing Diagnostic # ', ndId, '  ', cdiag(ndId),
     &         '     Counter:',ndiag(ip,1,1),'   Parms: ',gdiag(ndId)
              IF ( mate.GT.0 ) THEN
               WRITE(ioUnit,'(3A,I6,2A)')
     &         '       use Counter Mate for  ', cdiag(ndId),
     &         '     Diagnostic # ',mate, '  ', cdiag(mate)
              ELSEIF ( mVec.GT.0 ) THEN
                IF ( im.GT.0 .AND. ndiag(MAX(1,im),1,1).GT.0 ) THEN
                 WRITE(ioUnit,'(3A,I6,3A)')
     &             '           Vector  Mate for  ', cdiag(ndId),
     &             '     Diagnostic # ',mVec, '  ', cdiag(mVec),
     &             ' exists '
                ELSE
                 WRITE(ioUnit,'(3A,I6,3A)')
     &             '           Vector  Mate for  ', cdiag(ndId),
     &             '     Diagnostic # ',mVec, '  ', cdiag(mVec),
     &             ' not enabled'
                ENDIF
              ENDIF
            ENDIF

            IF ( fflags(listId)(2:2).EQ.'P' ) THEN
C-       get all the levels (for vertical interpolation)
              DO bj = myByLo(myThid), myByHi(myThid)
               DO bi = myBxLo(myThid), myBxHi(myThid)
                DO k = 1,kdiag(ndId)
                  tmpLev = k
                  CALL GETDIAG(
     I                         tmpLev,undef,
     O                         qtmp1(1-OLx,1-OLy,k,bi,bj),
     I                         ndId,mate,ip,im,bi,bj,myThid)
                ENDDO
               ENDDO
              ENDDO
            ELSE
C-       get only selected levels:
              DO bj = myByLo(myThid), myByHi(myThid)
               DO bi = myBxLo(myThid), myBxHi(myThid)
                DO k = 1,nlevels(listId)
                  CALL GETDIAG(
     I                         levs(k,listId),undef,
     O                         qtmp1(1-OLx,1-OLy,k,bi,bj),
     I                         ndId,mate,ip,im,bi,bj,myThid)
                ENDDO
               ENDDO
              ENDDO
            ENDIF

C-        end of empty diag / not empty block
          ENDIF

C-----------------------------------------------------------------------
C         Check to see if we need to interpolate before output
C-----------------------------------------------------------------------
          IF ( fflags(listId)(2:2).EQ.'P' ) THEN
C-        Do vertical interpolation:
           IF ( fluidIsAir ) THEN
C jmc: for now, this can only work in an atmospheric set-up (fluidIsAir);
            CALL DIAGNOSTICS_INTERP_VERT(
     I                     listId, md, ndId, ip, im, lm,
     U                     qtmp1,
     I                     undef, myTime, myIter, myThid )
           ELSE
             WRITE(msgBuf,'(2A)') 'DIAGNOSTICS_OUT: ',
     &         'INTERP_VERT not allowed in this config'
             CALL PRINT_ERROR( msgBuf , myThid )
             STOP 'ABNORMAL END: S/R DIAGNOSTICS_OUT'
           ENDIF
          ENDIF

C--    Ready to write field "md", element "lm" in averageCycle(listId)

C-        write to binary file, using MDSIO pkg:
          IF ( diag_mdsio ) THEN
            nRec = lm + (md-1)*averageCycle(listId)
C           default precision for output files
            prec = writeBinaryPrec
C           fFlag(1)=R(or D): force it to be 32-bit(or 64) precision
            IF ( fflags(listId)(1:1).EQ.'R' ) prec = precFloat32
            IF ( fflags(listId)(1:1).EQ.'D' ) prec = precFloat64
C         a hack not to write meta files now: pass -nRec < 0 to MDS_WRITE S/R
            CALL WRITE_REC_LEV_RL(
     I                            fn, prec,
     I                            NrMax, 1, nlevels(listId),
     I                            qtmp1, -nRec, myIter, myThid )
          ENDIF

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
            WRITE(dn(3),'(a,i6.6)') 'Zd', nlevels(listId)
            IF ( (gdiag(ndId)(10:10) .EQ. 'R')
     &           .AND. (gdiag(ndId)(9:9) .EQ. 'M') ) THEN
              WRITE(dn(3),'(a,i6.6)') 'Zmd', nlevels(listId)
            ENDIF
            IF ( (gdiag(ndId)(10:10) .EQ. 'R')
     &           .AND. (gdiag(ndId)(9:9) .EQ. 'L') ) THEN
              WRITE(dn(3),'(a,i6.6)') 'Zld', nlevels(listId)
            ENDIF
            IF ( (gdiag(ndId)(10:10) .EQ. 'R')
     &           .AND. (gdiag(ndId)(9:9) .EQ. 'U') ) THEN
              WRITE(dn(3),'(a,i6.6)') 'Zud', nlevels(listId)
            ENDIF
            dim(3) = NrMax
            ib(3)  = 1
            ie(3)  = nlevels(listId)

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

C     Handle missing value attribute (land points)
            IF ( misvalFlt(listId) .NE. UNSET_RL ) THEN
             misvalLoc = misvalFlt(listId)
            ELSE
             misvalLoc = undef
            ENDIF
C     Defaults to UNSET_I
            misvalIntLoc = misvalInt(listId)
            DO ii=1,2
C            misval_r4(ii)  = UNSET_FLOAT4
C            misval_r8(ii)  = UNSET_FLOAT8
             misval_r4(ii)  = misvalLoc
             misval_r8(ii)  = misvalLoc
             misval_int(ii) = UNSET_I
            ENDDO
C     Missing values only for scalar diagnostics at mass points (so far)
            IF ( gdiag(ndId)(1:2) .EQ. 'SM' ) THEN
C     assign missing values and set flag for adding the netCDF atttibute
             CALL MNC_CW_VATTR_MISSING(cdiag(ndId), 2,
     I            misval_r8, misval_r4, misval_int, 
     I            myThid )
C     and now use the missing values for masking out the land points
             DO bj = myByLo(myThid), myByHi(myThid)
              DO bi = myBxLo(myThid), myBxHi(myThid)
               DO k = 1,nlevels(listId)
                klev = NINT(levs(k,listId))
                DO j = 1-OLy,sNy+OLy
                 DO i = 1-OLx,sNx+OLx
                  IF ( _hFacC(I,J,klev,bi,bj) .EQ. 0. )
     &                 qtmp1(i,j,k,bi,bj) = misvalLoc
                 ENDDO
                ENDDO
               ENDDO
              ENDDO
             ENDDO
            ELSE
C     suppress the missing value attribute (iflag = 0)
C     Note: I have no idea, why we have to do this here, but if we do not
C     do it, variables get the missing_value attribute (the one of the 
C     previous varable) that are not supposed to get one. These is something
C     fishy with the flag array mnc_cw_vfmv.
             CALL MNC_CW_VATTR_MISSING(cdiag(ndId), 0,
     I            misval_r8, misval_r4, misval_int, 
     I            myThid )
            ENDIF

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

         ENDDO
C--     end of Processing Fld # md
        ENDIF
      ENDDO

#ifdef ALLOW_MDSIO
      IF (diag_mdsio) THEN
C-    Note: temporary: since it's a pain to add more arguments to
C     all MDSIO S/R, uses instead this specific S/R to write only
C     meta files but with more informations in it.
            glf = globalFiles
            nRec = nfields(listId)*averageCycle(listId)
            CALL MDS_WR_METAFILES(fn, prec, glf, .FALSE.,
     &              0, 0, nlevels(listId), ' ',
     &              nfields(listId), flds(1,listId), 1, myTime,
     &              nRec, myIter, myThid)
      ENDIF
#endif /*  ALLOW_MDSIO  */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
