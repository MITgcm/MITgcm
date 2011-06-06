C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_summary.F,v 1.7 2011/06/06 15:42:58 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

CBOP 0
C     !ROUTINE: DIAGNOSTICS_SUMMARY

C     !INTERFACE:
      SUBROUTINE DIAGNOSTICS_SUMMARY( myTime, myIter, myThid )

C     !DESCRIPTION:
C     Write a summary of diagnostics state to ASCII file unit "dUnit"
C     Notes: Only called after initialisation but could be called
C            from any place in the code.

C     !USES:
      IMPLICIT NONE

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"

C     !INPUT PARAMETERS:
C     myThid :: my Thread Id number
      _RL     myTime
      INTEGER myIter, myThid
CEOP

C     !LOCAL VARIABLES:
      INTEGER md, ld, ipt, im
      INTEGER j, k, k1, k2, l
      INTEGER  dUnit, stdUnit, iLen
      INTEGER xNew, xOld, ii, nDup
      CHARACTER*(2) cSep
      CHARACTER*(MAX_LEN_MBUF) msgBuf, tmpBuf
      CHARACTER*(MAX_LEN_FNAM) fn
      CHARACTER*(72) ccLine, ccFlds, ccList
      LOGICAL  outpSummary
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

      _BEGIN_MASTER( myThid )
      stdUnit = standardMessageUnit

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( debugLevel.GE.debLevB ) THEN
       IF ( myIter.EQ.nIter0 ) THEN
        outpSummary = .TRUE.
        dUnit = standardMessageUnit
        WRITE(msgBuf,'(A,I6)')
     &   ' write diagnostics summary to file ioUnit: ',dUnit
        CALL PRINT_MESSAGE( msgBuf, stdUnit,SQUEEZE_RIGHT, myThid)
       ELSE
        outpSummary = ( myXGlobalLo.EQ.1 .AND. myYGlobalLo.EQ.1 )
        IF ( outpSummary ) THEN
         WRITE(fn,'(A,I10.10,A)') 'diagnostics_status.',myIter,'.txt'
         iLen = ILNBLNK(fn)
         CALL MDSFINDUNIT( dUnit, mythid )
         OPEN(dUnit,file=fn(1:iLen),status='unknown',form='formatted')
         WRITE(msgBuf,'(2A)')
     &   ' write diagnostics summary to file: ',fn(1:iLen)
         CALL PRINT_MESSAGE( msgBuf, stdUnit,SQUEEZE_RIGHT, myThid)
        ENDIF
       ENDIF
      ELSE
       outpSummary = .FALSE.
      ENDIF

      IF ( outpSummary .AND. debugLevel.GE.debLevB ) THEN
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     write a summary diagnostics state:

        DO l=1,LEN(ccLine)
         ccLine(l:l) = '-'
        ENDDO
        WRITE(ccList,'(2A)')
     &  ' nFlds, nActive,       freq     &     phase        , nLev'
        WRITE(ccFlds,'(2A)')
     &  ' diag# | name   |   ipt  |  iMate | kLev|   count |   mate.C|'

        WRITE(dUnit,'(A,I10,A,1PE21.13)')
     &   'Iter.Nb:',myIter,' ; Time(s):', myTime
        WRITE(dUnit,'(A)') ccLine
        WRITE(dUnit,'(A,I6)')
     &   '2D/3D diagnostics: Number of lists:', nlists
        WRITE(dUnit,'(A)') ccLine

        DO ld=1,nlists
         iLen = ILNBLNK(fnames(ld))
         WRITE(dUnit,'(A,I5,2A)') 'listId=', ld,
     &                            ' ; file name: ',fnames(ld)(1:iLen)
         WRITE(dUnit,'(A)') ccList
         WRITE(dUnit,'(2(I5,A),2F17.6,A,I4)')
     &    nfields(ld), '  |',nActive(ld), '  |',
     &    freq(ld), phase(ld), ' |', nlevels(ld)
         IF ( fflags(ld)(2:2).EQ.'P' ) THEN
          DO k1=1,nlevels(ld),10
           k2 = MIN(nlevels(ld),k1+9)
           WRITE(dUnit,'(A,1P10E10.3)')' interp:', (levs(k,ld),k=k1,k2)
          ENDDO
         ELSE
          DO k1=1,nlevels(ld),25
           k2 = MIN(nlevels(ld),k1+24)
           WRITE(dUnit,'(A,25I4)')' levels:',(NINT(levs(k,ld)),k=k1,k2)
          ENDDO
         ENDIF
         WRITE(dUnit,'(A)') ccFlds
         DO md=1,nActive(ld)
           WRITE(msgBuf,'(I6,3A,2(I7,A),I4,A)')
     &       jdiag(md,ld),' |', flds(md,ld),'|',idiag(md,ld),' |',
     &       mdiag(md,ld),' |', kdiag(jdiag(md,ld)),' |'
           ipt = ABS(idiag(md,ld))
           IF (ipt.NE.0 .AND. averageCycle(ld).GT.1) THEN
             xOld=ndiag(ipt,1,1)
             nDup = 1
             cSep = ', '
             DO l=1,averageCycle(ld)
               ii = ipt+l*kdiag(jdiag(md,ld))
               IF (l.EQ.averageCycle(ld)) THEN
                 cSep = ' |'
                 xNew=xOld+1
               ELSE
                 xNew=ndiag(ii,1,1)
               ENDIF
               IF (xNew.EQ.xOld) THEN
                 nDup = nDup + 1
               ELSE
                 iLen = ILNBLNK(msgBuf)
                 tmpBuf(1:iLen) = msgBuf(1:iLen)
                 IF (nDup.EQ.1) THEN
                   WRITE(msgBuf,'(A,I7,A)') tmpBuf(1:iLen),xOld,cSep
                 ELSE
                   WRITE(msgBuf,'(A,I7,A,I3,2A)') tmpBuf(1:iLen),xOld,
     &              '(x',nDup,')',cSep
                 ENDIF
                 xOld = xNew
                 nDup = 1
               ENDIF
             ENDDO
           ELSEIF (ipt.NE.0) THEN
            iLen = ILNBLNK(msgBuf)
            tmpBuf(1:iLen) = msgBuf(1:iLen)
            WRITE(msgBuf,'(A,I8,A)') tmpBuf(1:iLen),ndiag(ipt,1,1),' |'
            im = mdiag(md,ld)
            IF (im.NE.0) THEN
             iLen = ILNBLNK(msgBuf)
             tmpBuf(1:iLen) = msgBuf(1:iLen)
             WRITE(msgBuf,'(A,I8,A)') tmpBuf(1:iLen),ndiag(im,1,1),' |'
            ENDIF
           ENDIF
           iLen = ILNBLNK(msgBuf)
           WRITE(dUnit,'(A)') msgBuf(1:iLen)
         ENDDO
c        WRITE(dUnit,'(A)') ccFlds
         WRITE(dUnit,'(A)') ccLine

        ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

c       WRITE(dUnit,'(A)') ccLine
        WRITE(dUnit,'(A,I6)')
     &   'Global & Regional Statistics diagnostics: Number of lists:',
     &     diagSt_nbLists
        WRITE(dUnit,'(A)') ccLine

        WRITE(ccList,'(2A)')
     &   ' nFlds, nActive,       freq     &     phase        |'
        WRITE(ccFlds,'(2A)')
     &   ' diag# | name   |   ipt  |  iMate |',
     &          '    Volume   |   mate-Vol. |'

        DO ld=1,diagSt_nbLists
         iLen = ILNBLNK(diagSt_Fname(ld))
         WRITE(dUnit,'(A,I4,2A)') 'listId=', ld,
     &                     ' ; file name: ',diagSt_Fname(ld)(1:iLen)
         WRITE(dUnit,'(A)') ccList
         WRITE(dUnit,'(2(I5,A),2F17.6,A,I4)')
     &    diagSt_nbFlds(ld), '  |',diagSt_nbActv(ld), '  |',
     &    diagSt_freq(ld), diagSt_phase(ld), ' |'
         WRITE(msgBuf,'(A)') ' Regions: '
         iLen = 10
         DO j=0,nRegions
           IF ( diagSt_region(j,ld).GE.1
     &          .AND. iLen+3.LE.MAX_LEN_MBUF) THEN
             tmpBuf(1:iLen) = msgBuf(1:iLen)
             WRITE(msgBuf,'(A,I3)') tmpBuf(1:iLen),j
             iLen = iLen+3
           ENDIF
         ENDDO
         WRITE(dUnit,'(A)') msgBuf(1:iLen)

         WRITE(dUnit,'(A)') ccFlds
         DO md=1,diagSt_nbActv(ld)
           WRITE(msgBuf,'(I6,3A,2(I7,A))')
     &       jSdiag(md,ld),' |', diagSt_Flds(md,ld),'|',iSdiag(md,ld),
     &       ' |', mSdiag(md,ld),' |'
           ipt = ABS(iSdiag(md,ld))
           IF (ipt.NE.0) THEN
            iLen = ILNBLNK(msgBuf)
            tmpBuf(1:iLen) = msgBuf(1:iLen)
            WRITE(msgBuf,'(A,1PE12.5,A)') tmpBuf(1:iLen),
     &                         qSdiag(0,0,ipt,1,1),' |'
           ENDIF
           im = mSdiag(md,ld)
           IF (im.NE.0) THEN
            iLen = ILNBLNK(msgBuf)
            tmpBuf(1:iLen) = msgBuf(1:iLen)
            WRITE(msgBuf,'(A,1PE12.5,A)') tmpBuf(1:iLen),
     &                         qSdiag(0,0,im, 1,1),' |'
           ENDIF
           iLen = ILNBLNK(msgBuf)
           WRITE(dUnit,'(A)') msgBuf(1:iLen)
         ENDDO
c        WRITE(dUnit,'(A)') ccFlds
         WRITE(dUnit,'(A)') ccLine

        ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
        IF ( dUnit.NE.standardMessageUnit ) CLOSE(dUnit)
      ENDIF

      _END_MASTER( myThid )

      RETURN
      END
