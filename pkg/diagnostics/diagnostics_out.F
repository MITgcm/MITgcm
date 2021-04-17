#include "DIAG_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: DIAGNOSTICS_OUT

C     !INTERFACE:
      SUBROUTINE DIAGNOSTICS_OUT(
     I                       listId, myTime, myIter, myThid )

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

C     !FUNCTIONS:
      INTEGER ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
C     i,j,k :: loop indices
C     bi,bj :: tile indices
C     lm    :: loop index (averageCycle)
C     md    :: field number in the list "listId".
C     ndId  :: diagnostics  Id number (in available diagnostics list)
C     ip    :: diagnostics  pointer to storage array
C     im    :: counter-mate pointer to storage array
C     mate  :: counter mate Id number (in available diagnostics list)
C     mDbl  :: processing mate Id number (in case processing requires 2 diags)
C     mVec  :: vector mate Id number
C     ppFld :: post-processed diag or not (=0): =1 stored in qtmp1 ; =2 in qtmp2
C   isComputed :: previous post-processed diag (still available in qtmp)
C     nLevOutp :: number of levels to write in output file
C
C--   COMMON /LOCAL_DIAGNOSTICS_OUT/ local common block (for multi-threaded)
C     qtmp1 :: temporary array; used to store a copy of diag. output field.
C     qtmp2 :: temporary array; used to store a copy of a 2nd diag. field.
C-  Note: local common block no longer needed.
c     COMMON /LOCAL_DIAGNOSTICS_OUT/ qtmp1
      _RL qtmp1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,NrMax,nSx,nSy)
      _RL qtmp2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,NrMax,nSx,nSy)

      INTEGER i, j, k, lm
      INTEGER bi, bj
      INTEGER md, ndId, nn, ip, im
      INTEGER mate, mDbl, mVec
      INTEGER ppFld, isComputed
      CHARACTER*10 gcode
      _RL undefRL
      INTEGER nLevOutp, kLev

      INTEGER iLen,jLen
      INTEGER ioUnit
      CHARACTER*(MAX_LEN_FNAM) fn
      CHARACTER*(10) suff
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER prec, nRec, nTimRec
      _RL     timeRec(2)
      _RL     tmpLoc
#ifdef ALLOW_MDSIO
      LOGICAL glf
#endif
#ifdef ALLOW_MNC
      CHARACTER*(MAX_LEN_FNAM) diag_mnc_bn
#endif /*  ALLOW_MNC  */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C---  set file properties
      ioUnit= standardMessageUnit
      undefRL = misValFlt(listId)

      IF ( rwSuffixType.EQ.0 ) THEN
        WRITE(suff,'(I10.10)') myIter
      ELSE
        CALL RW_GET_SUFFIX( suff, myTime, myIter, myThid )
      ENDIF
      iLen = ILNBLNK(fnames(listId))
      WRITE( fn, '(A,A,A)' ) fnames(listId)(1:iLen),'.',suff
      IF ( diag_mdsio.AND.(diagMdsDir.NE.' ') ) THEN
        jLen = ILNBLNK(diagMdsDir)
        WRITE( fn, '(5A)' ) diagMdsDir(1:jLen),'/',
     &                       fnames(listId)(1:iLen),'.',suff
      ENDIF

C-    for now, if integrate vertically, output field has just 1 level:
      nLevOutp = nlevels(listId)
      IF ( fflags(listId)(2:2).EQ.'I' ) nLevOutp = 1

C--   Set time information:
      IF ( freq(listId).LT.0. ) THEN
C-    Snap-shot: store a unique time (which is consistent with State-Var timing)
        nTimRec = 1
        timeRec(1) = myTime
      ELSE
C-    Time-average: store the 2 edges of the time-averaging interval.
C      this time is consitent with intermediate Var (i.e., non-state, e.g, flux,
C      tendencies) timing. For State-Var, this is shifted by + halt time-step.
        nTimRec = 2

C-    end of time-averaging interval:
        timeRec(2) = myTime

C-    begining of time-averaging interval:
c       timeRec(1) = myTime - freq(listId)
C     a) find the time of the previous multiple of output freq:
        timeRec(1) = myTime-deltaTClock*0.5 _d 0
        timeRec(1) = (timeRec(1)-phase(listId))/freq(listId)
        tmpLoc     = DINT( timeRec(1) )
        IF ( timeRec(1).LT.zeroRL ) THEN
          IF ( timeRec(1).NE.tmpLoc ) tmpLoc = tmpLoc - 1. _d 0
        ENDIF
        timeRec(1) = phase(listId) + freq(listId)*tmpLoc
c       WRITE(0,'(3I5,A,2F17.4)') myProcId, myThid, listId,
c    &                     ' f ', tmpLoc, timeRec(1)/deltaTClock
        timeRec(1) = MAX( timeRec(1), startTime )

C     b) round off to nearest multiple of time-step:
        timeRec(1) = (timeRec(1)-baseTime)/deltaTClock
        tmpLoc = DNINT( timeRec(1) )
C     if just half way, NINT will return the next time-step: correct this
        IF ( (timeRec(1)+halfRL).EQ.tmpLoc ) tmpLoc = tmpLoc - 1. _d 0
        timeRec(1) = baseTime + deltaTClock*tmpLoc
c       WRITE(0,'(3I5,A,2F17.4)') myProcId, myThid, listId,
c    &                     '   ', tmpLoc, timeRec(1)/deltaTClock
      ENDIF
C--   Convert time to iteration number (debug)
c     DO i=1,nTimRec
c       timeRec(i) = timeRec(i)/deltaTClock
c     ENDDO

C--   Place the loop on lm (= averagePeriod) outside the loop on md (= field):
      DO lm=1,averageCycle(listId)

#ifdef ALLOW_MNC
       IF (useMNC .AND. diag_mnc) THEN
         CALL DIAGNOSTICS_MNC_SET(
     I                    nLevOutp, listId, lm,
     O                    diag_mnc_bn,
     I                    undefRL, myTime, myIter, myThid )
       ENDIF
#endif /*  ALLOW_MNC  */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

       isComputed = 0
       DO md = 1,nfields(listId)
        ndId = ABS(jdiag(md,listId))
        gcode = gdiag(ndId)(1:10)
        mate = 0
        mVec = 0
        mDbl = 0
        ppFld = 0
        IF ( gcode(5:5).EQ.'C' ) THEN
C-      Check for Mate of a Counter Diagnostic
           mate = hdiag(ndId)
        ELSEIF ( gcode(5:5).EQ.'P' ) THEN
           ppFld = 1
           IF ( gdiag(hdiag(ndId))(5:5).EQ.'P' ) ppFld = 2
C-      Also load the mate (if stored) for Post-Processing
           nn = ndId
           DO WHILE ( gdiag(nn)(5:5).EQ.'P' )
             nn = hdiag(nn)
           ENDDO
           IF ( mdiag(md,listId).NE.0 ) mDbl = hdiag(nn)
c          write(0,*) ppFld,' ndId=', ndId, nn, mDbl, isComputed
        ELSEIF ( gcode(1:1).EQ.'U' .OR. gcode(1:1).EQ.'V' ) THEN
C-      Check for Mate of a Vector Diagnostic
           mVec = hdiag(ndId)
        ENDIF
        IF ( idiag(md,listId).NE.0 .AND. gcode(5:5).NE.'D' ) THEN
C--     Start processing 1 Fld :

          ip = ABS(idiag(md,listId)) + kdiag(ndId)*(lm-1)
          im = mdiag(md,listId)
          IF (mate.GT.0) im = im + kdiag(mate)*(lm-1)
          IF (mDbl.GT.0) im = im + kdiag(mDbl)*(lm-1)
          IF (mVec.GT.0) im = im + kdiag(mVec)*(lm-1)

          IF ( ppFld.EQ.2 .AND. isComputed.EQ.hdiag(ndId) ) THEN
C-        Post-Processed diag from an other Post-Processed diag -and-
C         both of them have just been calculated and are still stored in qtmp:
C         => skip computation and just write qtmp2
            IF ( debugLevel.GE.debLevB .AND. myThid.EQ.1 ) THEN
               WRITE(ioUnit,'(A,I6,3A,I6)')
     &         '  get Post-Proc. Diag # ', ndId, '  ', cdiag(ndId),
     &         ' from previous computation of Diag # ', isComputed
            ENDIF
            isComputed = 0
          ELSEIF ( ndiag(ip,1,1).EQ.0 ) THEN
C-        Empty diagnostics case :
            isComputed = 0

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
                DO k = 1,nLevOutp
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
            isComputed = 0

            IF ( debugLevel.GE.debLevB .AND. myThid.EQ.1 ) THEN
              IF ( ppFld.GE.1 ) THEN
               WRITE(ioUnit,'(A,I6,7A,I8,2A)')
     &         ' Post-Processing Diag # ', ndId, '  ', cdiag(ndId),
     &         '   Parms: ',gdiag(ndId)
               IF ( mDbl.EQ.0 ) THEN
                WRITE(ioUnit,'(2(3A,I6,A,I8))') '   from diag: ',
     &            cdiag(nn), ' (#', nn, ') Cnt=', ndiag(ip,1,1)
               ELSE
                WRITE(ioUnit,'(2(3A,I6,A,I8))') '   from diag: ',
     &            cdiag(nn), ' (#', nn, ') Cnt=', ndiag(ip,1,1),
     &          ' and diag: ',
     &            cdiag(mDbl),' (#',mDbl,') Cnt=',ndiag(im,1,1)
               ENDIF
              ELSE
               WRITE(ioUnit,'(A,I6,3A,I8,2A)')
     &         ' Computing Diagnostic # ', ndId, '  ', cdiag(ndId),
     &         '     Counter:',ndiag(ip,1,1),'   Parms: ',gdiag(ndId)
              ENDIF
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

            IF ( fflags(listId)(2:2).EQ.' ' ) THEN
C-       get only selected levels:
              DO bj = myByLo(myThid), myByHi(myThid)
               DO bi = myBxLo(myThid), myBxHi(myThid)
                DO k = 1,nlevels(listId)
                  kLev = NINT(levs(k,listId))
                  CALL DIAGNOSTICS_GET_DIAG(
     I                         kLev, undefRL,
     O                         qtmp1(1-OLx,1-OLy,k,bi,bj),
     I                         ndId, mate, ip, im, bi, bj, myThid )
                ENDDO
               ENDDO
              ENDDO
              IF ( mDbl.GT.0 ) THEN
               DO bj = myByLo(myThid), myByHi(myThid)
                DO bi = myBxLo(myThid), myBxHi(myThid)
                 DO k = 1,nlevels(listId)
                  kLev = NINT(levs(k,listId))
                  CALL DIAGNOSTICS_GET_DIAG(
     I                         kLev, undefRL,
     O                         qtmp2(1-OLx,1-OLy,k,bi,bj),
     I                         mDbl, 0, im, 0, bi, bj, myThid )
                 ENDDO
                ENDDO
               ENDDO
              ENDIF
            ELSE
C-       get all the levels (for vertical post-processing)
              DO bj = myByLo(myThid), myByHi(myThid)
               DO bi = myBxLo(myThid), myBxHi(myThid)
                  CALL DIAGNOSTICS_GET_DIAG(
     I                         0, undefRL,
     O                         qtmp1(1-OLx,1-OLy,1,bi,bj),
     I                         ndId, mate, ip, im, bi, bj, myThid )
               ENDDO
              ENDDO
              IF ( mDbl.GT.0 ) THEN
               DO bj = myByLo(myThid), myByHi(myThid)
                DO bi = myBxLo(myThid), myBxHi(myThid)
                  CALL DIAGNOSTICS_GET_DIAG(
     I                         0, undefRL,
     O                         qtmp2(1-OLx,1-OLy,1,bi,bj),
     I                         mDbl, 0, im, 0, bi, bj, myThid )
                ENDDO
               ENDDO
              ENDIF
            ENDIF

C-----------------------------------------------------------------------
C--     Apply specific post-processing (e.g., interpolate) before output
C-----------------------------------------------------------------------
            IF ( fflags(listId)(2:2).EQ.'P' ) THEN
C-          Do vertical interpolation:
             IF ( fluidIsAir ) THEN
C jmc: for now, this can only work in an atmospheric set-up (fluidIsAir);
              CALL DIAGNOSTICS_INTERP_VERT(
     I                         listId, md, ndId, ip, im, lm,
     U                         qtmp1, qtmp2,
     I                         undefRL, myTime, myIter, myThid )
             ELSE
               WRITE(msgBuf,'(2A)') 'DIAGNOSTICS_OUT: ',
     &           'INTERP_VERT not allowed in this config'
               CALL PRINT_ERROR( msgBuf , myThid )
               STOP 'ABNORMAL END: S/R DIAGNOSTICS_OUT'
             ENDIF
            ENDIF
            IF ( fflags(listId)(2:2).EQ.'I' ) THEN
C-          Integrate vertically: for now, output field has just 1 level:
              CALL DIAGNOSTICS_SUM_LEVELS(
     I                         listId, md, ndId, ip, im, lm,
     U                         qtmp1,
     I                         undefRL, myTime, myIter, myThid )
            ENDIF
            IF ( ppFld.GE.1 ) THEN
C-          Do Post-Processing:
             IF ( flds(md,listId).EQ.'PhiVEL  '
     &       .OR. flds(md,listId).EQ.'PsiVEL  '
     &          ) THEN
              CALL DIAGNOSTICS_CALC_PHIVEL(
     I                         listId, md, ndId, ip, im, lm,
     I                         NrMax,
     U                         qtmp1, qtmp2,
     I                         myTime, myIter, myThid )
              isComputed = ndId
             ELSE
               WRITE(msgBuf,'(2A)') 'DIAGNOSTICS_OUT: ',
     &           'unknown Processing method for diag="',cdiag(ndId),'"'
               CALL PRINT_ERROR( msgBuf , myThid )
               STOP 'ABNORMAL END: S/R DIAGNOSTICS_OUT'
             ENDIF
            ENDIF

C--     End of empty diag / not-empty diag block
          ENDIF

C--     Ready to write field "md", element "lm" in averageCycle(listId)

C-        write to binary file, using MDSIO pkg:
          IF ( diag_mdsio ) THEN
c          nRec = lm + (md-1)*averageCycle(listId)
           nRec = md + (lm-1)*nfields(listId)
C         default precision for output files
           prec = writeBinaryPrec
C         fFlag(1)=R(or D): force it to be 32-bit(or 64) precision
           IF ( fflags(listId)(1:1).EQ.'R' ) prec = precFloat32
           IF ( fflags(listId)(1:1).EQ.'D' ) prec = precFloat64
C         a hack not to write meta files now: pass -nRec < 0 to MDS_WRITE S/R
           IF ( ppFld.LE.1 ) THEN
            CALL WRITE_REC_LEV_RL(
     I                            fn, prec,
     I                            NrMax, 1, nLevOutp,
     I                            qtmp1, -nRec, myIter, myThid )
           ELSE
            CALL WRITE_REC_LEV_RL(
     I                            fn, prec,
     I                            NrMax, 1, nLevOutp,
     I                            qtmp2, -nRec, myIter, myThid )
           ENDIF
          ENDIF

#ifdef ALLOW_MNC
          IF (useMNC .AND. diag_mnc) THEN
           IF ( ppFld.LE.1 ) THEN
            CALL DIAGNOSTICS_MNC_OUT(
     I                       NrMax, nLevOutp, listId, ndId, mate,
     I                       diag_mnc_bn, qtmp1,
     I                       undefRL, myTime, myIter, myThid )
           ELSE
            CALL DIAGNOSTICS_MNC_OUT(
     I                       NrMax, nLevOutp, listId, ndId, mate,
     I                       diag_mnc_bn, qtmp2,
     I                       undefRL, myTime, myIter, myThid )
           ENDIF
          ENDIF
#endif /*  ALLOW_MNC  */

C--     end of Processing Fld # md
        ENDIF
       ENDDO

C--   end loop on lm counter (= averagePeriod)
      ENDDO

#ifdef ALLOW_MDSIO
      IF (diag_mdsio) THEN
C-    Note: temporary: since it is a pain to add more arguments to
C     all MDSIO S/R, uses instead this specific S/R to write only
C     meta files but with more informations in it.
            glf = globalFiles
            nRec = averageCycle(listId)*nfields(listId)
            CALL MDS_WR_METAFILES(fn, prec, glf, .FALSE.,
     &              0, 0, nLevOutp, ' ',
     &              nfields(listId), flds(1,listId),
     &              nTimRec, timeRec, undefRL,
     &              nRec, myIter, myThid)
      ENDIF
#endif /*  ALLOW_MDSIO  */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
