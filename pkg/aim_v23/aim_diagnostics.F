C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/aim_diagnostics.F,v 1.8 2004/07/24 02:41:33 jmc Exp $
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
#include "AIM2DYN.h"
#include "AIM_DIAGS.h"
#include "com_physvar.h"
#include "com_forcing.h"

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
#ifdef ALLOW_DIAGNOSTICS
      _RL tmpVar(NGP)
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--  mean surf. temp. change:
       DO J=1,NGP
         dTsurf(J,1,myThid) = fMask1(J,1,myThid)*dTsurf(J,1,myThid)
     &                      + fMask1(J,2,myThid)*dTsurf(J,2,myThid)
     &                      + fMask1(J,3,myThid)*dTsurf(J,3,myThid)
         dTsurf(J,1,myThid) = ABS(dTsurf(J,1,myThid))
       ENDDO

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
#ifdef ALLOW_CLR_SKY_DIAG
C--   write clear-sky tendencies to files :
       IF ( aim_clrSkyDiag ) THEN
        CALL AIM_WRITE_LOCAL('aim_dT_clskySW.',suff,Nr,
     &                     TT_SWclr(1,1,myThid),bi,bj,1,myIter,myThid)
        CALL AIM_WRITE_LOCAL('aim_dT_clskyLW.',suff,Nr,
     &                     TT_LWclr(1,1,myThid),bi,bj,1,myIter,myThid)
       ENDIF
#endif /* ALLOW_CLR_SKY_DIAG */
      ENDIF

      IF (
     &    DIFFERENT_MULTIPLE(aim_diagFreq,myTime+deltaTClock,myTime)
     &   ) THEN 
       WRITE(suff,'(I10.10)') myIter+1

C--   Write Relative Humidity :
       CALL AIM_WRITE_LOCAL('aim_RelHum.',suff,Nr,RH(1,1,myThid),
     &                      bi,bj,1,myIter,myThid)

C--   Write AIM Physics diagnostics (2D, all in 1 file) :
       CALL AIM_WRITE_LOCAL('aimPhyDiag.',suff,1,DRAG(1,0,myThid),
     &                      bi,bj, 1,myIter,myThid)
       CALL AIM_WRITE_LOCAL('aimPhyDiag.',suff,1,SPEED0(1,myThid),
     &                      bi,bj, 2,myIter,myThid)
       CALL AIM_WRITE_LOCAL('aimPhyDiag.',suff,1,TSR(1,myThid),
     &                      bi,bj, 3,myIter,myThid)
       CALL AIM_WRITE_LOCAL('aimPhyDiag.',suff,1,OLR(1,myThid),
     &                      bi,bj, 4,myIter,myThid)
       CALL AIM_WRITE_LOCAL('aimPhyDiag.',suff,1,SSR(1,0,myThid),
     &                      bi,bj, 5,myIter,myThid)
       CALL AIM_WRITE_LOCAL('aimPhyDiag.',suff,1,SLR(1,0,myThid),
     &                      bi,bj, 6,myIter,myThid)
       CALL AIM_WRITE_LOCAL('aimPhyDiag.',suff,1,SHF(1,0,myThid),
     &                      bi,bj, 7,myIter,myThid)
       CALL AIM_WRITE_LOCAL('aimPhyDiag.',suff,1,EVAP(1,0,myThid),
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
       CALL AIM_WRITE_LOCAL('aimPhyDiag.',suff,1,EnPrec(1,myThid),
     &                      bi,bj,16,myIter,myThid)
       CALL AIM_WRITE_LOCAL('aimPhyDiag.',suff,1,alb1(1,0,myThid),
     &                      bi,bj,17,myIter,myThid)
       CALL AIM_WRITE_LOCAL('aimPhyDiag.',suff,1,dTsurf(1,1,myThid),
     &                      bi,bj,18,myIter,myThid)
#ifdef ALLOW_CLR_SKY_DIAG
C--   write clear-sky radiative fluxes to files :
      IF ( aim_clrSkyDiag ) THEN
       CALL AIM_WRITE_LOCAL('aimPhyDiag.',suff,1,TSWclr(1,myThid),
     &                      bi,bj, 19,myIter,myThid)
       CALL AIM_WRITE_LOCAL('aimPhyDiag.',suff,1,OLWclr(1,myThid),
     &                      bi,bj, 20,myIter,myThid)
       CALL AIM_WRITE_LOCAL('aimPhyDiag.',suff,1,SSWclr(1,myThid),
     &                      bi,bj, 21,myIter,myThid)
       CALL AIM_WRITE_LOCAL('aimPhyDiag.',suff,1,SLWclr(1,myThid),
     &                      bi,bj, 22,myIter,myThid)
      ENDIF
#endif /* ALLOW_CLR_SKY_DIAG */
C--
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   cloud-top pressure: multiplied by cloud fraction before averaging:
      DO J=1,NGP
        CLTOP(J,myThid)=CLTOP(J,mythid)*CLOUDC(J,myThid)
      ENDDO

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
     &                          + SSR(I2,0,myThid)*DDTT
          SLRtave(i,j,bi,bj)    = SLRtave(i,j,bi,bj)
     &                          + SLR(I2,0,myThid)*DDTT
          SHFtave(i,j,bi,bj)    = SHFtave(i,j,bi,bj)
     &                          + SHF(I2,0,myThid)*DDTT
          EVAPtave(i,j,bi,bj)   = EVAPtave(i,j,bi,bj)
     &                          + EVAP(I2,0,myThid)*DDTT
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
     &                          + DRAG(I2,0,myThid)*DDTT
          aimV0tave(i,j,bi,bj)  = aimV0tave(i,j,bi,bj)
     &                          + SPEED0(I2,myThid)*DDTT
          aimT0tave(i,j,bi,bj)  = aimT0tave(i,j,bi,bj)
     &                          + T0(I2,myThid)*DDTT
          aimQ0tave(i,j,bi,bj)  = aimQ0tave(i,j,bi,bj)
     &                          + Q0(I2,myThid)*DDTT
          EnFxPrtave(i,j,bi,bj) = EnFxPrtave(i,j,bi,bj)
     &                          + EnPrec(I2,myThid)
     &                          *(PRECNV(I2,myThid)+
     &                            PRECLS(I2,myThid))*DDTT
          albedotave(i,j,bi,bj) = albedotave(i,j,bi,bj)
     &                          + alb1(I2,0,myThid)*DDTT
          dTsurftave(i,j,bi,bj) = dTsurftave(i,j,bi,bj)
     &                          + dTsurf(I2,1,myThid)*DDTT
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

#ifdef ALLOW_DIAGNOSTICS
       IF (usediagnostics) THEN
        CALL FILL_DIAGNOSTICS(myThid,
     &            'DIABT   ',0,Nr,1,bi,bj,aim_dTdt)
        CALL FILL_DIAGNOSTICS(myThid,
     &            'DIABQ   ',0,Nr,1,bi,bj,aim_dSdt)
        CALL FILL_DIAGNOSTICS(myThid,
     &            'RADSW   ',0,-Nr,2,bi,bj,TT_RSW(1,1,myThid))
        CALL FILL_DIAGNOSTICS(myThid,
     &            'RADLW   ',0,-Nr,2,bi,bj,TT_RLW(1,1,myThid))
        CALL FILL_DIAGNOSTICS(myThid,
     &            'DTCONV  ',0,-Nr,2,bi,bj,TT_CNV(1,1,myThid))
        CALL FILL_DIAGNOSTICS(myThid,
     &            'TURBT   ',0,-Nr,2,bi,bj,TT_PBL(1,1,myThid))
        CALL FILL_DIAGNOSTICS(myThid,
     &            'DTLS    ',0,-Nr,2,bi,bj,TT_LSC(1,1,myThid))
        CALL FILL_DIAGNOSTICS(myThid,
     &            'DQCONV  ',0,-Nr,2,bi,bj,QT_CNV(1,1,myThid))
        CALL FILL_DIAGNOSTICS(myThid,
     &            'TURBQ   ',0,-Nr,2,bi,bj,QT_PBL(1,1,myThid))
        CALL FILL_DIAGNOSTICS(myThid,
     &            'DQLS    ',0,-Nr,2,bi,bj,QT_LSC(1,1,myThid))
        CALL FILL_DIAGNOSTICS(myThid,
     &            'RELHUM  ',0,-Nr,2,bi,bj,RH(1,1,myThid))
        CALL FILL_DIAGNOSTICS(myThid,
     &            'OSR     ',1,1, 2,bi,bj,TSR(1,myThid))
        CALL FILL_DIAGNOSTICS(myThid,
     &            'OLR     ',1,1, 2,bi,bj,OLR(1,myThid))
        CALL FILL_DIAGNOSTICS(myThid,
     &            'RADSWG  ',1,1, 2,bi,bj,SSR(1,0,myThid))
        CALL FILL_DIAGNOSTICS(myThid,
     &            'RADLWG  ',1,1, 2,bi,bj,SLR(1,0,myThid))
        CALL FILL_DIAGNOSTICS(myThid,
     &            'HFLUX   ',1,1, 2,bi,bj,SHF(1,0,myThid))
        CALL FILL_DIAGNOSTICS(myThid,
     &            'EVAP    ',1,1, 2,bi,bj,EVAP(1,0,myThid))
        CALL FILL_DIAGNOSTICS(myThid,
     &            'PRECON  ',1,1, 2,bi,bj,PRECNV(1,myThid))
        CALL FILL_DIAGNOSTICS(myThid,
     &            'PRECLS  ',1,1, 2,bi,bj,PRECLS(1,myThid))
        CALL FILL_DIAGNOSTICS(myThid,
     &            'CLDFRC  ',1,1, 2,bi,bj,CLOUDC(1,myThid))
        CALL FILL_DIAGNOSTICS(myThid,
     &            'CLDPRS  ',1,1, 2,bi,bj,CLTOP(1,myThid))
        CALL FILL_DIAGNOSTICS(myThid,
     &            'CTPCNT  ',1,1, 2,bi,bj,CLOUDC(1,myThid))
        CALL FILL_DIAGNOSTICS(myThid,
     &            'CLDMAS  ',1,1, 2,bi,bj,CBMF(1,myThid))
        CALL FILL_DIAGNOSTICS(myThid,
     &            'KM      ',1,1, 2,bi,bj,DRAG(1,0,myThid))
        CALL FILL_DIAGNOSTICS(myThid,
     &            'WINDS   ',1,1, 2,bi,bj,SPEED0(1,myThid))
        CALL FILL_DIAGNOSTICS(myThid,
     &            'TS      ',1,1, 2,bi,bj,T0(1,myThid))
        CALL FILL_DIAGNOSTICS(myThid,
     &            'QS      ',1,1, 2,bi,bj,Q0(1,myThid))
        DO J=1,NGP
          tmpVar(J) = EnPrec(J,myThid)
     &              *(PRECNV(J,myThid)+PRECLS(J,myThid))
        ENDDO
        CALL FILL_DIAGNOSTICS(myThid,
     &            'ENPREC  ',1,1, 2,bi,bj,tmpVar)
        CALL FILL_DIAGNOSTICS(myThid,
     &            'ALBVISDF',1,1, 2,bi,bj,alb1(1,0,myThid))
        CALL FILL_DIAGNOSTICS(myThid,
     &            'SDIAG1  ',1,1, 2,bi,bj,dTsurf(1,1,myThid))
#ifdef ALLOW_CLR_SKY_DIAG
        IF ( aim_clrSkyDiag ) THEN 
         CALL FILL_DIAGNOSTICS(myThid,
     &        'SWCLR   ',0,-Nr,2,bi,bj,TT_SWclr(1,1,myThid))
         CALL FILL_DIAGNOSTICS(myThid,
     &        'LWCLR   ',0,-Nr,2,bi,bj,TT_LWclr(1,1,myThid))
         CALL FILL_DIAGNOSTICS(myThid,
     &        'OSRCLR  ',1,1,2,bi,bj,TSWclr(1,myThid))
         CALL FILL_DIAGNOSTICS(myThid,
     &        'OLRCLR  ',1,1,2,bi,bj,OLWclr(1,myThid))
         CALL FILL_DIAGNOSTICS(myThid,
     &        'SWGCLR  ',1,1,2,bi,bj,SSWclr(1,myThid))
         CALL FILL_DIAGNOSTICS(myThid,
     &        'LWGCLR  ',1,1,2,bi,bj,SLWclr(1,myThid))
        ENDIF
#endif /* ALLOW_CLR_SKY_DIAG */

       ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif /* ALLOW_AIM */

      RETURN
      END
