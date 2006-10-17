C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagstats_set_regions.F,v 1.4 2006/10/17 18:56:31 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

CBOP
C     !ROUTINE: DIAGSTATS_SET_REGIONS
C     !INTERFACE:
      SUBROUTINE DIAGSTATS_SET_REGIONS( myThid )

C     !DESCRIPTION: \bv
C     *==================================================================
C     | S/R DIAGSTATS_SET_REGIONS
C     | o set region-mask for regional statistics diagnostics
C     *==================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGSTATS_REGIONS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     == Local variables ==
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iLen
      INTEGER i, j
      INTEGER bi, bj
#ifdef DIAGSTATS_REGION_MASK
      CHARACTER*(MAX_LEN_MBUF) tmpBuf
      INTEGER ioUnit
      INTEGER k, nbReg
      _RS     tmpVar(1-OLx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      COMMON / SET_REGIONS_LOCAL / tmpVar
#else
      LOGICAL flag
#endif
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

#ifdef DIAGSTATS_REGION_MASK

C--   Initialize region-mask array to zero:
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k=1,sizRegMsk
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           diagSt_regMask(i,j,k,bi,bj) = 0.
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      ioUnit = -1

      _BEGIN_MASTER( myThid )
      ioUnit = standardMessageUnit
C--   Check size & parameter first:
      IF ( (diagSt_regMaskFile.NE.' ' .AND. nSetRegMskFile.EQ.0)
     & .OR.(diagSt_regMaskFile.EQ.' ' .AND. nSetRegMskFile.GT.0) ) THEN
        WRITE(msgBuf,'(2A)') 'DIAGSTATS_SET_REGIONS:',
     &   ' regMaskFile and nSetRegMskFile Not consistent'
        CALL PRINT_ERROR( msgBuf , myThid )
        STOP 'ABNORMAL END: S/R DIAGSTATS_SET_REGIONS'
      ENDIF
      IF ( nSetRegMskFile.GT.sizRegMsk ) THEN
        WRITE(msgBuf,'(2A,I4,A,I4)') 'DIAGSTATS_SET_REGIONS:',
     &   ' regMaskFile set-index number=', nSetRegMskFile,
     &   ' exceeds sizRegMsk=', sizRegMsk
        CALL PRINT_ERROR( msgBuf , myThid )
        STOP 'ABNORMAL END: S/R DIAGSTATS_SET_REGIONS'
      ENDIF
      _END_MASTER( myThid )

C--   Read region-mask from file
      IF ( diagSt_regMaskFile .NE. ' ' ) THEN
       _BARRIER
       iLen = ILNBLNK(diagSt_regMaskFile)
       IF (ioUnit.GE.0 ) WRITE(ioUnit,'(2A)')
     &   ' DIAGSTATS_SET_REGIONS: start reading region-mask file: ',
     &   diagSt_regMaskFile(1:iLen)
       DO k=1,nSetRegMskFile
C       _BEGIN_MASTER( myThid )
         IF (ioUnit.GE.0 )  WRITE(ioUnit,'(A,I3)')
     &   ' DIAGSTATS_SET_REGIONS:  reading set k=',k
         CALL READ_REC_XY_RS( diagSt_regMaskFile, tmpVar, k,
     &                        nIter0, myThid )
         IF (ioUnit.GE.0 ) WRITE(ioUnit,'(A,I3,A)')
     &   ' DIAGSTATS_SET_REGIONS:          set k=',k,' <= done'
C       _END_MASTER( myThid )
        _EXCH_XY_RS( tmpVar, myThid )
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j=1-Oly,sNy+Oly
           DO i=1-Olx,sNx+Olx
            diagSt_regMask(i,j,k,bi,bj) = tmpVar(i,j,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
C-     end of k loop
       ENDDO
      ENDIF

C--   Other way to define regions (e.g., latitude bands):
C      set corresponding set-index of the region-mask array,
C      starting from nSetRegMskFile+1 up to nSetRegMask
C note: for now, empty !
      _BEGIN_MASTER( myThid )
      nSetRegMask = nSetRegMskFile
      _END_MASTER( myThid )

C--   Region Identificator arrays
C       for now, directly filled when reading data.diagnostics

      _BEGIN_MASTER( myThid )
C--   Check defined regions
      nbReg = 0
      DO j=1,nRegions
C-      check for valid region-mask index:
        IF ( diagSt_kRegMsk(j).LT.0  .OR.
     &       diagSt_kRegMsk(j).GT.sizRegMsk ) THEN
          WRITE(msgBuf,'(2A,I3,A,I4)') 'DIAGSTATS_SET_REGIONS: ',
     &     '(region',j,') invalid region-mask index :',diagSt_kRegMsk(j)
          CALL PRINT_ERROR( msgBuf , myThid )
          STOP 'ABNORMAL END: S/R DIAGSTATS_SET_REGIONS'
C-      check for unset region-mask:
        ELSEIF ( diagSt_kRegMsk(j).GT.nSetRegMask ) THEN
          WRITE(msgBuf,'(2A,I3,A,I3,A)') 'DIAGSTATS_SET_REGIONS: ',
     &     'region',j,' , kRegMsk=', diagSt_kRegMsk(j),
     &     ' <- has not been set !'
          CALL PRINT_ERROR( msgBuf , myThid )
          STOP 'ABNORMAL END: S/R DIAGSTATS_SET_REGIONS'
        ELSEIF ( diagSt_kRegMsk(j).NE.0 ) THEN
          nbReg = nbReg + 1
C-      check for empty region: build temp mask 0 / 1 :
c         k = diagSt_kRegMsk(j)
c         IF ( diagSt_regMask(i,j,k,bi,bj).EQ.diagSt_vRegMsk(j) ) THEN
c           tmpVar(i,j,bi,bj) = 1.
c         ELSE
c           tmpVar(i,j,bi,bj) = 0.
c         ELSE
C-      print region mask:
c         IF ( debugLevel.GE.debLevA ) THEN
c           WRITE(msgBuf,'(A,I3,A)') 'DIAGSTAT Region',j,' mask:'
c           iLen = ILNBLNK(msgBuf)
c           CALL PLOT_FIELD_XYRS( tmpVar, msgBuf(1:iLen), -1, myThid )
c         ENDIF
        ENDIF
      ENDDO

C-    Global statistics (region # 0) <- done in diagnostics_readparams
c     diagSt_kRegMsk(0) = 1
c     diagSt_vRegMsk(0) = 0.


      WRITE(msgBuf,'(A,I4,A)') 'DIAGSTATS_SET_REGIONS: define',
     &                         nbReg,' regions:'
      iLen = ILNBLNK(msgBuf)
      DO j=1,nRegions
        IF ( diagSt_kRegMsk(j).NE.0 ) THEN
          iLen = MIN( iLen, MAX_LEN_MBUF -3 )
          tmpBuf(1:iLen) = msgBuf(1:iLen)
          WRITE(msgBuf,'(A,I3)') tmpBuf(1:iLen),j
          iLen = iLen+3
        ENDIF
      ENDDO
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT , myThid)
      WRITE(msgBuf,'(2A)')
     &   '------------------------------------------------------------'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT , myThid)

      _END_MASTER( myThid )

#else /* DIAGSTATS_REGION_MASK */

C--   Initialize region-mask array to zero:
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
c        DO j=1-Oly,sNy+Oly
c         DO i=1-Olx,sNx+Olx
         DO j=1-Oly,1-Oly
          DO i=1-Olx,1-Olx
           diagSt_regMask(i,j,1,bi,bj) = 0.
          ENDDO
         ENDDO
       ENDDO
      ENDDO

      _BEGIN_MASTER( myThid )
C--   Check parameter consitency:
      flag = .FALSE.
      DO j=1,nRegions
        flag = flag .OR. diagSt_kRegMsk(j).NE.0
     &              .OR. diagSt_vRegMsk(j).NE.0.
      ENDDO
      iLen = ILNBLNK(diagSt_regMaskFile)
      IF ( flag .OR. iLen.GE.1 .OR. nSetRegMskFile.NE.0 ) THEN
        WRITE(msgBuf,'(2A)') 'DIAGSTATS_SET_REGIONS:',
     &   ' #define DIAGSTATS_REGION_MASK missing in DIAG_OPTIONS.h'
        CALL PRINT_ERROR( msgBuf , myThid )
        STOP 'ABNORMAL END: S/R DIAGSTATS_SET_REGIONS'
      ENDIF

      WRITE(msgBuf,'(A)') 'DIAGSTATS_SET_REGIONS: define no region'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT , myThid)
      WRITE(msgBuf,'(2A)')
     &   '------------------------------------------------------------'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT , myThid)

      _END_MASTER( myThid )

#endif /* DIAGSTATS_REGION_MASK */

      RETURN
      END
