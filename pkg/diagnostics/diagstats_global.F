C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagstats_global.F,v 1.5 2009/04/28 18:10:47 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: DIAGSTATS_GLOBAL

C     !INTERFACE:
      SUBROUTINE DIAGSTATS_GLOBAL(
     O                    qtmp1, qtmp2,
     I                    undef, nLev, jReg,
     I                    ndId, mate, iSp, iSm, myThid )

C     !DESCRIPTION:
C     Retrieve averaged model diagnostic

C     !USES:
      IMPLICIT NONE
#include "EEPARAMS.h"
#include "SIZE.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"

C     !INPUT PARAMETERS:
C     undef     :: Undefined value
C     nLev      :: 2nd Dimension (max Nb of levels) of qtmp1,2 arrays
C     jReg      :: region Index to be process.
C     ndId      :: diagnostic Id number (in available diagnostics list)
C     mate      :: counter mate Id number if any ; 0 otherwise
C     iSp       :: diagnostics  pointer to storage array
C     iSm       :: counter-mate pointer to storage array
C     myThid    :: my thread Id number
      _RL undef
      INTEGER nLev, jReg, ndId, mate, iSp, iSm
      INTEGER myThid

C     !OUTPUT PARAMETERS:
C     qtmp1    ..... AVERAGED DIAGNOSTIC QUANTITY
C     qtmp2    ..... working array (used for counter mate statistics)
      _RL qtmp1(0:nStats,0:nLev)
      _RL qtmp2(0:nStats,0:nLev)
CEOP

C     !LOCAL VARIABLES:
      INTEGER im, ix, iv
      PARAMETER ( iv = nStats - 2 , im = nStats - 1 , ix = nStats )
      INTEGER bi, bj
      INTEGER i, k, kd, kCnt, klev, kMlev
      _RL     tmpMin, tmpMax, tmpVol

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Initialize to zero :
      DO k=0,nLev
        DO i=0,nStats
          qtmp1(i,k) = 0.
          qtmp2(i,k) = 0.
        ENDDO
      ENDDO

      klev = kdiag(ndId)
      IF ( mate.GT.0 ) kMlev = kdiag(mate)

      IF (klev.LE.nLev) THEN
C---    Compute global statistics :

C--     Retrieve tile statistics first
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)

          DO k=1,klev
            kd = iSp + k - 1
            IF ( qSdiag(0,jReg,kd,bi,bj).GT.0. ) THEN
             IF ( qtmp1(0,k).LE.0. ) THEN
              DO i=0,nStats
                qtmp1(i,k) = qSdiag(i,jReg,kd,bi,bj)
              ENDDO
             ELSE
              DO i=0,iv
                qtmp1(i,k) = qtmp1(i,k) + qSdiag(i,jReg,kd,bi,bj)
              ENDDO
              qtmp1(im,k) = MIN( qtmp1(im,k),qSdiag(im,jReg,kd,bi,bj) )
              qtmp1(ix,k) = MAX( qtmp1(ix,k),qSdiag(ix,jReg,kd,bi,bj) )
             ENDIF
            ENDIF
          ENDDO
          IF ( mate.GT.0 ) THEN
           DO k=1,kMlev
            kd = iSm + k - 1
            IF ( qSdiag(0,jReg,kd,bi,bj).GT.0. ) THEN
             IF ( qtmp2(0,k).LE.0. ) THEN
              DO i=0,1
                qtmp2(i,k) = qSdiag(i,jReg,kd,bi,bj)
              ENDDO
             ELSE
              DO i=0,1
                qtmp2(i,k) = qtmp2(i,k) + qSdiag(i,jReg,kd,bi,bj)
              ENDDO
             ENDIF
            ENDIF
           ENDDO
          ENDIF

C-       end tile index loops
         ENDDO
        ENDDO

C--     Global min,max & sum (at each level) over all thread & processors :
        DO k=1,klev
           tmpVol = qtmp1(0,k)
           DO i=0,iv
            _GLOBAL_SUM_RL(qtmp1(i,k),myThid)
           ENDDO
           IF ( qtmp1(0,k).GT.0. .AND. tmpVol.LE.0. ) THEN
C-      In case 1 processor has only empty tiles:
             tmpMax = qtmp1(1,k)/qtmp1(0,k)
             tmpmin = -tmpMax
           ELSE
             tmpMin = -qtmp1(im,k)
             tmpMax =  qtmp1(ix,k)
           ENDIF
           _GLOBAL_MAX_RL(tmpMin,myThid)
           _GLOBAL_MAX_RL(tmpMax,myThid)
           qtmp1(im,k) = -tmpMin
           qtmp1(ix,k) =  tmpMax
        ENDDO
        IF ( mate.GT.0 ) THEN
         DO k=1,kMlev
           DO i=0,1
            _GLOBAL_SUM_RL(qtmp2(i,k),myThid)
           ENDDO
         ENDDO
        ENDIF

C--     Vertical integral, min & max :
        DO k=1,klev
          IF ( qtmp1(0,k).GT.0. ) THEN
           IF ( qtmp1(0,0).LE.0. ) THEN
             DO i=0,nStats
              qtmp1(i,0) = qtmp1(i,k)
             ENDDO
           ELSE
             DO i=0,iv
              qtmp1(i,0) = qtmp1(i,0) + qtmp1(i,k)
             ENDDO
              qtmp1(im,0) = MIN(qtmp1(im,0),qtmp1(im,k))
              qtmp1(ix,0) = MAX(qtmp1(ix,0),qtmp1(ix,k))
           ENDIF
          ENDIF
        ENDDO
        IF ( mate.GT.0 ) THEN
         DO k=1,kMlev
          IF ( qtmp2(0,k).GT.0. ) THEN
           IF ( qtmp2(0,0).LE.0. ) THEN
             DO i=0,1
              qtmp2(i,0) = qtmp2(i,k)
             ENDDO
           ELSE
             DO i=0,1
              qtmp2(i,0) = qtmp2(i,0) + qtmp2(i,k)
             ENDDO
           ENDIF
          ENDIF
         ENDDO
        ENDIF

C--     Average, Standard.Dev.:
C-      no counter diagnostics => average = Sum / vol :
        IF ( mate.EQ.0 ) THEN
          DO k=0,klev
           IF ( qtmp1(0,k).LE.0. ) THEN
             DO i=1,nStats
              qtmp1(i,k) = undef
             ENDDO
           ELSE
             DO i=1,iv
              qtmp1(i,k) = qtmp1(i,k) / qtmp1(0,k)
             ENDDO
C            Variance :
             qtmp1(iv,k) = qtmp1(iv,k) - qtmp1(1,k)*qtmp1(1,k)
C            Standard deviation :
             IF (qtmp1(iv,k).GT.0.) qtmp1(iv,k) = SQRT(qtmp1(iv,k))
           ENDIF
          ENDDO
C         return global (& vertically integrated) volume in qtmp2(0,0):
          qtmp2(0,0) = qtmp1(0,0)
        ELSE
C       With counter diagnostics => average = Sum / Sum(counter) :
          DO k=0,klev
           kCnt = min(k,kMlev)
           IF ( qtmp2(0,kCnt).LE.0. ) THEN
             DO i=1,nStats
              qtmp1(i,k) = undef
             ENDDO
           ELSEIF ( qtmp2(1,kCnt).LE.0. ) THEN
             DO i=1,iv
              qtmp1(i,k) = undef
             ENDDO
           ELSE
             DO i=1,iv
              qtmp1(i,k) = qtmp1(i,k) / qtmp2(1,kCnt)
             ENDDO
C jmc: looks like there is a Pb with how Variance is computed
C            Variance :
             qtmp1(iv,k) = qtmp1(iv,k) - qtmp1(1,k)*qtmp1(1,k)
C            Standard deviation :
             IF (qtmp1(iv,k).GT.0.) qtmp1(iv,k) = SQRT(qtmp1(iv,k))
           ENDIF
          ENDDO
        ENDIF

      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      RETURN
      END
