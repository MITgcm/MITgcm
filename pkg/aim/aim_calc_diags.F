C     $Header: /u/gcmpack/MITgcm/pkg/aim/Attic/aim_calc_diags.F,v 1.8 2005/04/06 18:34:59 jmc Exp $
C     $Name:  $

#include "AIM_OPTIONS.h"

CStartOfInterFace
      SUBROUTINE AIM_CALC_DIAGS( bi, bj, myTime, myThid )
C     /==========================================================\
C     | SUBROUTINE AIM_CALC_DIAGS                                |
C     | o Calculate AIM diagnostics                              |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
C-- size for MITgcm & Physics package :
#include "AIM_SIZE.h" 

#include "EEPARAMS.h"
#include "PARAMS.h"

#include "AIM_DIAGS.h"
#include "com_physvar.h"

C     == Routine arguments ==
C     myTime - Current time of simulation ( s )
C     myThid - Number of this instance of the routine
C     bi,bj  - Tile index
      _RL     myTime
      INTEGER myThid
      INTEGER bi, bj
CEndOfInterface

#ifdef ALLOW_AIM

C     == Local variables ==
      INTEGER I,J,I2,K
      _RL DDTT

#ifdef ALLOW_TIMEAVE

      DDTT = deltaTclock

      DO J=1,sNy
       DO I=1,sNx
        I2 = sNx*(J-1)+I
c       USTRtave(i,j,bi,bj)   = USTRtave(i,j,bi,bj)
c    &                        + USTR(I2,3,myThid)*DDTT
c       VSTRtave(i,j,bi,bj)   = VSTRtave(i,j,bi,bj)
c    &                        + VSTR(I2,3,myThid)*DDTT
        TSRtave(i,j,bi,bj)    = TSRtave(i,j,bi,bj)
     &                        + TSR(I2,myThid)*DDTT
        OLRtave(i,j,bi,bj)    = OLRtave(i,j,bi,bj)
     &                        + OLR(I2,myThid)*DDTT
        SSRtave(i,j,bi,bj)    = SSRtave(i,j,bi,bj)
     &                        + SSR(I2,myThid)*DDTT
        SLRtave(i,j,bi,bj)    = SLRtave(i,j,bi,bj)
     &                        + SLR(I2,myThid)*DDTT
        SHFtave(i,j,bi,bj)    = SHFtave(i,j,bi,bj)
     &                        + SHF(I2,3,myThid)*DDTT
        EVAPtave(i,j,bi,bj)   = EVAPtave(i,j,bi,bj)
     &                        + EVAP(I2,3,myThid)*DDTT
        PRECNVtave(i,j,bi,bj) = PRECNVtave(i,j,bi,bj)
     &                        + PRECNV(I2,myThid)*DDTT
        PRECLStave(i,j,bi,bj) = PRECLStave(i,j,bi,bj)
     &                        + PRECLS(I2,myThid)*DDTT
        CLOUDCtave(i,j,bi,bj) = CLOUDCtave(i,j,bi,bj)
     &                        + CLOUDC(I2,myThid)*DDTT
       ENDDO
      ENDDO

C-    Keep record of how much time has been integrated over
      DO K=1,Nr
        AIM_TimeAve(k,bi,bj)=AIM_TimeAve(k,bi,bj)+DDTT
      ENDDO


#endif /* ALLOW_TIMEAVE */

#endif /* ALLOW_AIM */

      RETURN
      END
