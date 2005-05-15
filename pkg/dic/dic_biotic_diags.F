C $Header: /u/gcmpack/MITgcm/pkg/dic/dic_biotic_diags.F,v 1.6 2005/05/15 03:04:56 jmc Exp $
C $Name:  $

#include "DIC_OPTIONS.h"
#include "GCHEM_OPTIONS.h"

CStartOfInterface
      SUBROUTINE DIC_BIOTIC_DIAGS( myTime, myIter, myThid )
C     /==========================================================\
C     | SUBROUTINE DIC_BIOTIC_DIAGS                                  |
C     | o Do DIC diagnostic output                           |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DIC_BIOTIC.h"

C     == Routine arguments ==
C     myTime - Current time of simulation ( s )
C     myIter - Iteration number
C     myThid -  Number of this instance of INI_FORCING
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEndOfInterface


C     == Local variables ==
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
      INTEGER bi, bj, k
      CHARACTER*(MAX_LEN_MBUF) suff

#ifdef DIC_BIOTIC


#ifdef ALLOW_TIMEAVE

C     Dump files and restart average computation if needed
      IF ( DIFFERENT_MULTIPLE(taveFreq,myTime,deltaTClock) ) THEN
         
C      Normalize by integrated time
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         CALL TIMEAVE_NORMALIZ(BIOave, DIC_timeave, nR ,
     &                 bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(CARave, DIC_timeave, nR ,
     &                 bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(SURave,dic_timeave, 1 ,
     &                 bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(SUROave,dic_timeave, 1 ,
     &                 bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(pCO2ave,dic_timeave, 1 ,
     &                 bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(pHave,dic_timeave, 1 ,
     &                 bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(fluxCO2ave,dic_timeave, 1 ,
     &                 bi,bj,myThid)
        ENDDO
       ENDDO
         
       WRITE(suff,'(I10.10)') myIter

       CALL WRITE_FLD_XYZ_RL('DIC_Biotave.',suff,BIOave,         
     &     myIter,myThid)
       CALL WRITE_FLD_XYZ_RL('DIC_Cartave.',suff,CARave,
     &     myIter,myThid)
       CALL WRITE_FLD_XY_RL('DIC_Surtave.',suff,SURave,
     &     myIter,myThid)
       CALL WRITE_FLD_XY_RL('DIC_SurOtave.',suff,SUROave,
     &     myIter,myThid)
       CALL WRITE_FLD_XY_RL('DIC_pHtave.',suff,pHave,
     &     myIter,myThid)
       CALL WRITE_FLD_XY_RL('DIC_pCO2tave.',suff,pCO2ave,
     &     myIter,myThid)
       CALL WRITE_FLD_XY_RL('DIC_fluxCO2ave.',suff,fluxCO2ave,
     &     myIter,myThid)

         
C      Reset averages to zero
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         CALL TIMEAVE_RESET(BIOave,Nr,bi,bj,myThid)
         CALL TIMEAVE_RESET(CARave,Nr,bi,bj,myThid)
         CALL TIMEAVE_RESET(SURave,1,bi,bj,myThid)
         CALL TIMEAVE_RESET(SUROave,1,bi,bj,myThid)
         CALL TIMEAVE_RESET(pCO2ave,1,bi,bj,myThid)
         CALL TIMEAVE_RESET(pHave,1,bi,bj,myThid)
         CALL TIMEAVE_RESET(fluxCO2ave,1,bi,bj,myThid)
         do k=1,Nr
           DIC_TimeAve(bi,bj,k)=0.d0
         enddo
        ENDDO
       ENDDO
         
      ENDIF

#endif /* ALLOW_TIMEAVE */
      
#endif /* DIC_BIOTIC   */
      
      RETURN
      END
