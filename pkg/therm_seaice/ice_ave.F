#include "CPP_OPTIONS.h"

CStartofinterface
C     !ROUTINE: ICE_AVE
C     !INTERFACE:
      SUBROUTINE ICE_AVE( bi, bj, iMin, iMax, jMin, jMax,
     I                     myThid )
C     *==========================================================*
C     | S/R  ICE_AVE                                              
C     | o save values for timeaveraging
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
cswdice -- add ---
#ifdef ALLOW_THERM_SEAICE
#include "ICE.h"
#include "ICE_DIAGS.h"
#endif
cswdice -- end add ---

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      INTEGER myThid
CEndofinterface

#ifdef ALLOW_THERM_SEAICE

#ifdef ALLOW_TIMEAVE

c
       integer i,j 

C--     Time-average
       DO j=jMin,jMax
        DO i=iMin,iMax
        ICE_icemask_AVE(i,j,bi,bj)=ICE_icemask_AVE(i,j,bi,bj)
     &                       +iceMask(i,j,bi,bj)*deltaTclock
        ICE_iceheight_AVE(i,j,bi,bj)=ICE_iceheight_AVE(i,j,bi,bj)
     &                       +iceheight(i,j,bi,bj)*deltaTclock
        ICE_snowheight_AVE(i,j,bi,bj)=ICE_snowheight_AVE(i,j,bi,bj)
     &                       +snowHeight(i,j,bi,bj)*deltaTclock
        ICE_Tsrf_AVE(i,j,bi,bj)=ICE_Tsrf_AVE(i,j,bi,bj)
     &                       +Tsrf(i,j,bi,bj)*deltaTclock
        ICE_Tice1_AVE(i,j,bi,bj)=ICE_Tice1_AVE(i,j,bi,bj)
     &                       +Tice1(i,j,bi,bj)*deltaTclock
        ICE_Tice2_AVE(i,j,bi,bj)=ICE_Tice2_AVE(i,j,bi,bj)
     &                       +Tice2(i,j,bi,bj)*deltaTclock
        ICE_snow_AVE(i,j,bi,bj)=ICE_snow_AVE(i,j,bi,bj)
     &                       +snow(i,j,bi,bj)*deltaTclock
        ENDDO
       ENDDO

        ICE_timeave(bi,bj)=ICE_timeave(bi,bj)+deltaTclock
c       print*,'QQ end ice ave '
#endif /* ALLOW_TIMEAVE */

#endif 

      RETURN
      END
