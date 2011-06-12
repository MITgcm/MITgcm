C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_out.F,v 1.52 2011/06/12 19:16:09 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: DIAGNOSTICS_OUT

C     !INTERFACE:
      SUBROUTINE DIAGNOSTICS_OUT(
     I     listId,
     I     myTime,
     I     myIter,
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

C     !FUNCTIONS:
      INTEGER ILNBLNK
      EXTERNAL ILNBLNK
#ifdef ALLOW_FIZHI
      _RL   getcon
      EXTERNAL getcon
#endif

C     !LOCAL VARIABLES:
C     i,j,k :: loop indices
C     bi,bj :: tile indices
C     lm    :: loop index (averageCycle)
C     md    :: field number in the list "listId".
C     ndId  :: diagnostics  Id number (in available diagnostics list)
C     mate  :: counter mate Id number (in available diagnostics list)
C     ip    :: diagnostics  pointer to storage array
C     im    :: counter-mate pointer to storage array
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
      INTEGER md, ndId, ip, im
      INTEGER mate, mVec
      CHARACTER*10 gcode
      _RL undefRL
      INTEGER nLevOutp, kLev

      INTEGER iLen
      INTEGER ioUnit
      CHARACTER*(MAX_LEN_FNAM) fn
      CHARACTER*(MAX_LEN_MBUF) suff
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER prec, nRec, nTimRec
      _RL     timeRec(2)
      _RL     tmpLoc
#ifdef ALLOW_MDSIO
      LOGICAL glf
#endif
#ifdef ALLOW_MNC
      INTEGER ll, llMx, jj, jjMx
      CHARACTER*(MAX_LEN_FNAM) diag_mnc_bn
      LOGICAL useMissingValue
      REAL*8 misValLoc
#endif /*  ALLOW_MNC  */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C---  set file properties
      ioUnit= standardMessageUnit
      undefRL = UNSET_RL
#ifdef ALLOW_FIZHI
      IF ( useFIZHI ) undefRL = getcon('UNDEF')
#endif
      WRITE(suff,'(I10.10)') myIter
      iLen = ILNBLNK(fnames(listId))
      WRITE( fn, '(A,A,A)' ) fnames(listId)(1:iLen),'.',suff(1:10)
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
        i = INT( timeRec(1) )
        IF ( timeRec(1).LT.0. ) THEN
          tmpLoc = FLOAT(i)
          IF ( timeRec(1).NE.tmpLoc ) i = i - 1
        ENDIF
        timeRec(1) = phase(listId) + freq(listId)*FLOAT(i)
c       if ( listId.eq.2 ) write(0,*) 'f',i,timeRec(1)/deltaTClock
        timeRec(1) = MAX( timeRec(1), startTime )

C     b) round off to nearest multiple of time-step:
        timeRec(1) = (timeRec(1)-baseTime)/deltaTClock
        i = NINT( timeRec(1) )
C     if just half way, NINT will return the next time-step: correct this
        tmpLoc = FLOAT(i) - 0.5 _d 0
        IF ( timeRec(1).EQ.tmpLoc ) i = i - 1
        timeRec(1) = baseTime + deltaTClock*FLOAT(i)
c       if ( listId.eq.2 ) write(0,*) i,timeRec(1)/deltaTClock
      ENDIF
C--   Convert time to iteration number (debug)
c     DO i=1,nTimRec
c       timeRec(i) = timeRec(i)/deltaTClock
c     ENDDO

#ifdef ALLOW_MNC
C-- this is a trick to reverse the order of the loops on md (= field)
C   and lm (= averagePeriod): binary output: lm loop inside md loop ;
C                                 mnc ouput: md loop inside lm loop.
      IF (useMNC .AND. diag_mnc) THEN
        jjMx = averageCycle(listId)
        llMx = 1
      ELSE
        jjMx = 1
        llMx = averageCycle(listId)
      ENDIF
      DO jj=1,jjMx

       IF (useMNC .AND. diag_mnc) THEN
         misValLoc = undefRL
         IF ( misvalFlt(listId).NE.UNSET_RL )
     &        misValLoc = misvalFlt(listId)
         CALL DIAGNOSTICS_MNC_SET(
     I                    nLevOutp, listId, jj,
     O                    diag_mnc_bn, useMissingValue,
     I                    misValLoc, myTime, myIter, myThid )
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
#ifdef ALLOW_MNC
         DO ll=1,llMx
          lm = jj+ll-1
#else
         DO lm=1,averageCycle(listId)
#endif

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

            IF ( debugLevel.GE.debLevB .AND. myThid.EQ.1 ) THEN
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

            IF ( fflags(listId)(2:2).EQ.' ' ) THEN
C-       get only selected levels:
              DO bj = myByLo(myThid), myByHi(myThid)
               DO bi = myBxLo(myThid), myBxHi(myThid)
                DO k = 1,nlevels(listId)
                  kLev = NINT(levs(k,listId))
                  CALL DIAGNOSTICS_GET_DIAG(
     I                         kLev, undefRL,
     O                         qtmp1(1-OLx,1-OLy,k,bi,bj),
     I                         ndId,mate,ip,im,bi,bj,myThid)
                ENDDO
               ENDDO
              ENDDO
            ELSE
C-       get all the levels (for vertical post-processing)
              DO bj = myByLo(myThid), myByHi(myThid)
               DO bi = myBxLo(myThid), myBxHi(myThid)
                  CALL DIAGNOSTICS_GET_DIAG(
     I                         0, undefRL,
     O                         qtmp1(1-OLx,1-OLy,1,bi,bj),
     I                         ndId,mate,ip,im,bi,bj,myThid)
               ENDDO
              ENDDO
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

C--     End of empty diag / not-empty diag block
          ENDIF

C--     Ready to write field "md", element "lm" in averageCycle(listId)

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
     I                            NrMax, 1, nLevOutp,
     I                            qtmp1, -nRec, myIter, myThid )
          ENDIF

#ifdef ALLOW_MNC
          IF (useMNC .AND. diag_mnc) THEN
            CALL DIAGNOSTICS_MNC_OUT(
     I                       NrMax, nLevOutp, listId, ndId,
     I                       diag_mnc_bn,
     I                       useMissingValue, misValLoc,
     I                       qtmp1,
     I                       myTime, myIter, myThid )
          ENDIF
#endif /*  ALLOW_MNC  */

C--      end loop on lm (or ll if ALLOW_MNC) counter
         ENDDO
C--     end of Processing Fld # md
        ENDIF
       ENDDO

#ifdef ALLOW_MNC
C--   end loop on jj counter
      ENDDO
#endif

#ifdef ALLOW_MDSIO
      IF (diag_mdsio) THEN
C-    Note: temporary: since it is a pain to add more arguments to
C     all MDSIO S/R, uses instead this specific S/R to write only
C     meta files but with more informations in it.
            glf = globalFiles
            nRec = nfields(listId)*averageCycle(listId)
            CALL MDS_WR_METAFILES(fn, prec, glf, .FALSE.,
     &              0, 0, nLevOutp, ' ',
     &              nfields(listId), flds(1,listId), nTimRec, timeRec,
     &              nRec, myIter, myThid)
      ENDIF
#endif /*  ALLOW_MDSIO  */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
