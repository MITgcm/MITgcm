C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/aim_diagnostics.F,v 1.3 2003/01/03 03:51:27 jmc Exp $
C $Name:  $

#include "AIM_OPTIONS.h"

CStartOfInterFace
      SUBROUTINE AIM_DIAGNOSTICS( bi,bj, myTime, myIter, myThid )
C     *==========================================================*
C     | SUBROUTINE AIM_DIAGNOSTICS
C     | o Calculate AIM diagnostics
C     *==========================================================*
      IMPLICIT NONE

C     === Global variables ===
C-- size for MITgcm & Physics package :
#include "AIM_SIZE.h" 

#include "EEPARAMS.h"
#include "PARAMS.h"

#include "AIM_PARAMS.h"
#include "AIM_DIAGS.h"
#include "com_physvar.h"

      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     == Routine arguments ==
C     bi,bj  - Tile index
C     myTime - Current time of simulation ( s )
C     myIter - Current iteration number in simulation
C     myThid - Number of this instance of the routine
      INTEGER bi, bj, myIter, myThid
      _RL     myTime
CEndOfInterface

#ifdef ALLOW_AIM

C     == Local variables ==
      INTEGER I,J,K, I2,Katm
      _RL DDTT
      CHARACTER*(MAX_LEN_MBUF) suff

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF (
     &    DIFFERENT_MULTIPLE(aim_tendFreq,myTime+deltaTClock,myTime)
     &   ) THEN 
       WRITE(suff,'(I10.10)') myIter+1

C--   Write Tendencies to files :
       CALL AIM_WRITE_LOCAL('aim_dT_RSW.',suff,Nr,TT_RSW(1,1,myThid),
     &                      bi,bj,1,myIter,myThid)
       CALL AIM_WRITE_LOCAL('aim_dT_RLW.',suff,Nr,TT_RLW(1,1,myThid),
     &                      bi,bj,1,myIter,myThid)
       CALL AIM_WRITE_LOCAL('aim_dT_CNV.',suff,Nr,TT_CNV(1,1,myThid),
     &                      bi,bj,1,myIter,myThid)
       CALL AIM_WRITE_LOCAL('aim_dT_PBL.',suff,Nr,TT_PBL(1,1,myThid),
     &                      bi,bj,1,myIter,myThid)
       CALL AIM_WRITE_LOCAL('aim_dT_LSC.',suff,Nr,TT_LSC(1,1,myThid),
     &                      bi,bj,1,myIter,myThid)
       CALL AIM_WRITE_LOCAL('aim_dQ_CNV.',suff,Nr,QT_CNV(1,1,myThid),
     &                      bi,bj,1,myIter,myThid)
       CALL AIM_WRITE_LOCAL('aim_dQ_PBL.',suff,Nr,QT_PBL(1,1,myThid),
     &                      bi,bj,1,myIter,myThid)
       CALL AIM_WRITE_LOCAL('aim_dQ_LSC.',suff,Nr,QT_LSC(1,1,myThid),
     &                      bi,bj,1,myIter,myThid)
      ENDIF

      IF (
     &    DIFFERENT_MULTIPLE(aim_diagFreq,myTime+deltaTClock,myTime)
     &   ) THEN 
       WRITE(suff,'(I10.10)') myIter+1

C--   Write Relative Humidity :
       CALL AIM_WRITE_LOCAL('aim_RelHum.',suff,Nr,RH(1,1,myThid),
     &                      bi,bj,1,myIter,myThid)

C--   Write AIM Physics diagnostics (2D, all in 1 file) :
       CALL AIM_WRITE_LOCAL('aimPhyDiag.',suff,1,DRAG(1,3,myThid),
     &                      bi,bj, 1,myIter,myThid)
       CALL AIM_WRITE_LOCAL('aimPhyDiag.',suff,1,SPEED0(1,myThid),
     &                      bi,bj, 2,myIter,myThid)
       CALL AIM_WRITE_LOCAL('aimPhyDiag.',suff,1,TSR(1,myThid),
     &                      bi,bj, 3,myIter,myThid)
       CALL AIM_WRITE_LOCAL('aimPhyDiag.',suff,1,OLR(1,myThid),
     &                      bi,bj, 4,myIter,myThid)
       CALL AIM_WRITE_LOCAL('aimPhyDiag.',suff,1,SSR(1,myThid),
     &                      bi,bj, 5,myIter,myThid)
       CALL AIM_WRITE_LOCAL('aimPhyDiag.',suff,1,SLR(1,myThid),
     &                      bi,bj, 6,myIter,myThid)
       CALL AIM_WRITE_LOCAL('aimPhyDiag.',suff,1,SHF(1,3,myThid),
     &                      bi,bj, 7,myIter,myThid)
       CALL AIM_WRITE_LOCAL('aimPhyDiag.',suff,1,EVAP(1,3,myThid),
     &                      bi,bj, 8,myIter,myThid)
       CALL AIM_WRITE_LOCAL('aimPhyDiag.',suff,1,PRECNV(1,myThid),
     &                      bi,bj, 9,myIter,myThid)
       CALL AIM_WRITE_LOCAL('aimPhyDiag.',suff,1,PRECLS(1,myThid),
     &                      bi,bj,10,myIter,myThid)
       CALL AIM_WRITE_LOCAL('aimPhyDiag.',suff,1,CLOUDC(1,myThid),
     &                      bi,bj,11,myIter,myThid)
       CALL AIM_WRITE_LOCAL('aimPhyDiag.',suff,1,CLTOP(1,myThid),
     &                      bi,bj,12,myIter,myThid)
       CALL AIM_WRITE_LOCAL('aimPhyDiag.',suff,1,CBMF(1,myThid),
     &                      bi,bj,13,myIter,myThid)
       CALL AIM_WRITE_LOCAL('aimPhyDiag.',suff,1,T0(1,myThid),
     &                      bi,bj,14,myIter,myThid)
       CALL AIM_WRITE_LOCAL('aimPhyDiag.',suff,1,Q0(1,myThid),
     &                      bi,bj,15,myIter,myThid)
C--
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_AIM_TAVE

      IF (aim_taveFreq.GT.0.) THEN

        DDTT = deltaTclock

        DO J=1,sNy
         DO I=1,sNx
          I2 = I+(J-1)*sNx
          TSRtave(i,j,bi,bj)    = TSRtave(i,j,bi,bj)
     &                          + TSR(I2,myThid)*DDTT
          OLRtave(i,j,bi,bj)    = OLRtave(i,j,bi,bj)
     &                          + OLR(I2,myThid)*DDTT
          SSRtave(i,j,bi,bj)    = SSRtave(i,j,bi,bj)
     &                          + SSR(I2,myThid)*DDTT
          SLRtave(i,j,bi,bj)    = SLRtave(i,j,bi,bj)
     &                          + SLR(I2,myThid)*DDTT
          SHFtave(i,j,bi,bj)    = SHFtave(i,j,bi,bj)
     &                          + SHF(I2,3,myThid)*DDTT
          EVAPtave(i,j,bi,bj)   = EVAPtave(i,j,bi,bj)
     &                          + EVAP(I2,3,myThid)*DDTT
          PRECNVtave(i,j,bi,bj) = PRECNVtave(i,j,bi,bj)
     &                          + PRECNV(I2,myThid)*DDTT
          PRECLStave(i,j,bi,bj) = PRECLStave(i,j,bi,bj)
     &                          + PRECLS(I2,myThid)*DDTT
          CLOUDCtave(i,j,bi,bj) = CLOUDCtave(i,j,bi,bj)
     &                          + CLOUDC(I2,myThid)*DDTT
          CLTOPtave(i,j,bi,bj)  = CLTOPtave(i,j,bi,bj)
     &                          + CLTOP(I2,myThid)*DDTT
          CBMFtave(i,j,bi,bj)   = CBMFtave(i,j,bi,bj)
     &                          + CBMF(I2,myThid)*DDTT
          DRAGtave(i,j,bi,bj)   = DRAGtave(i,j,bi,bj)
     &                          + DRAG(I2,3,myThid)*DDTT
          aimV0tave(i,j,bi,bj)  = aimV0tave(i,j,bi,bj)
     &                          + SPEED0(I2,myThid)*DDTT
          aimT0tave(i,j,bi,bj)  = aimT0tave(i,j,bi,bj)
     &                          + T0(I2,myThid)*DDTT
          aimQ0tave(i,j,bi,bj)  = aimQ0tave(i,j,bi,bj)
     &                          + Q0(I2,myThid)*DDTT
         ENDDO
        ENDDO

C-      Relative Humidity :
        DO k=1,Nr
         Katm = _KD2KA( k )
         DO j=1,sNy
          DO i=1,sNx
           I2 = i+(j-1)*sNx
           aimRHtave(i,j,k,bi,bj) = aimRHtave(i,j,k,bi,bj)
     &                            + RH(I2,Katm,myThid)*DDTT
          ENDDO
         ENDDO
        ENDDO

C-    Keep record of how much time has been integrated over
        DO K=1,Nr
          aim_timeAve(k,bi,bj)=aim_timeAve(k,bi,bj)+DDTT
        ENDDO

      ENDIF

#endif /* ALLOW_AIM_TAVE */

#endif /* ALLOW_AIM */

      RETURN
      END
