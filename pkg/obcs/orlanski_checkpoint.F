C $Header: 
C $Name: 

#include "OBCS_OPTIONS.h"

CStartofinterface
      SUBROUTINE ORLANSKI_READ_CHECKPOINT( prec, myIt, suff, myThid )
C     /==========================================================\
C     | SUBROUTINE ORLANSKI_READ_CHECKPOINT                      |
C     | o Read Orlanski open boundary checkpoint arrays          |
C     |==========================================================|
C     |                                                          |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "OBCS.h"
#include "ORLANSKI.h"

C     == Routine arguments ==
      INTEGER prec
      INTEGER myIt
      CHARACTER*(10) suff
      INTEGER myThid
CEndofinterface

#ifdef ALLOW_ORLANSKI

C     == Local variables ==
      CHARACTER*(MAX_LEN_FNAM) fn

#ifdef ALLOW_OBCS_NORTH
C     READ N and S OB storage and phase speed arrays
      WRITE(fn,'(A,A10)') 'pickup_orlanskiN.',suff
      CALL MDSREADFIELDXZ(fn,prec,'RL',Nr,UN_STORE_1,1,myThid)
      CALL MDSREADFIELDXZ(fn,prec,'RL',Nr,UN_STORE_2,2,myThid)
      CALL MDSREADFIELDXZ(fn,prec,'RL',Nr,UN_STORE_3,3,myThid)
      CALL MDSREADFIELDXZ(fn,prec,'RL',Nr,UN_STORE_4,4,myThid)
      CALL MDSREADFIELDXZ(fn,prec,'RL',Nr,VN_STORE_1,5,myThid)
      CALL MDSREADFIELDXZ(fn,prec,'RL',Nr,VN_STORE_2,6,myThid)
      CALL MDSREADFIELDXZ(fn,prec,'RL',Nr,VN_STORE_3,7,myThid)
      CALL MDSREADFIELDXZ(fn,prec,'RL',Nr,VN_STORE_4,8,myThid)
      CALL MDSREADFIELDXZ(fn,prec,'RL',Nr,TN_STORE_1,9,myThid)
      CALL MDSREADFIELDXZ(fn,prec,'RL',Nr,TN_STORE_2,10,myThid)
      CALL MDSREADFIELDXZ(fn,prec,'RL',Nr,TN_STORE_3,11,myThid)
      CALL MDSREADFIELDXZ(fn,prec,'RL',Nr,TN_STORE_4,12,myThid)
      CALL MDSREADFIELDXZ(fn,prec,'RL',Nr,SN_STORE_1,13,myThid)
      CALL MDSREADFIELDXZ(fn,prec,'RL',Nr,SN_STORE_2,14,myThid)
      CALL MDSREADFIELDXZ(fn,prec,'RL',Nr,SN_STORE_3,15,myThid)
      CALL MDSREADFIELDXZ(fn,prec,'RL',Nr,SN_STORE_4,16,myThid)
      CALL MDSREADFIELDXZ(fn,prec,'RL',Nr,CVEL_UN,17,myThid)
      CALL MDSREADFIELDXZ(fn,prec,'RL',Nr,CVEL_VN,18,myThid)
      CALL MDSREADFIELDXZ(fn,prec,'RL',Nr,CVEL_TN,19,myThid)
      CALL MDSREADFIELDXZ(fn,prec,'RL',Nr,CVEL_SN,20,myThid)      
#ifdef ALLOW_NONHYDROSTATIC
      CALL MDSREADFIELDXZ(fn,prec,'RL',Nr,WN_STORE_1,21,myThid)
      CALL MDSREADFIELDXZ(fn,prec,'RL',Nr,WN_STORE_2,22,myThid)
      CALL MDSREADFIELDXZ(fn,prec,'RL',Nr,WN_STORE_3,23,myThid)
      CALL MDSREADFIELDXZ(fn,prec,'RL',Nr,WN_STORE_4,24,myThid)
      CALL MDSREADFIELDXZ(fn,prec,'RL',Nr,CVEL_WN,25,myThid)
#endif
#endif

#ifdef ALLOW_OBCS_SOUTH
      WRITE(fn,'(A,A10)') 'pickup_orlanskiS.',suff
      CALL MDSREADFIELDXZ(fn,prec,'RL',Nr,US_STORE_1,1,myThid)
      CALL MDSREADFIELDXZ(fn,prec,'RL',Nr,US_STORE_2,2,myThid)
      CALL MDSREADFIELDXZ(fn,prec,'RL',Nr,US_STORE_3,3,myThid)
      CALL MDSREADFIELDXZ(fn,prec,'RL',Nr,US_STORE_4,4,myThid)
      CALL MDSREADFIELDXZ(fn,prec,'RL',Nr,VS_STORE_1,5,myThid)
      CALL MDSREADFIELDXZ(fn,prec,'RL',Nr,VS_STORE_2,6,myThid)
      CALL MDSREADFIELDXZ(fn,prec,'RL',Nr,VS_STORE_3,7,myThid)
      CALL MDSREADFIELDXZ(fn,prec,'RL',Nr,VS_STORE_4,8,myThid)
      CALL MDSREADFIELDXZ(fn,prec,'RL',Nr,TS_STORE_1,9,myThid)
      CALL MDSREADFIELDXZ(fn,prec,'RL',Nr,TS_STORE_2,10,myThid)
      CALL MDSREADFIELDXZ(fn,prec,'RL',Nr,TS_STORE_3,11,myThid)
      CALL MDSREADFIELDXZ(fn,prec,'RL',Nr,TS_STORE_4,12,myThid)
      CALL MDSREADFIELDXZ(fn,prec,'RL',Nr,SS_STORE_1,13,myThid)
      CALL MDSREADFIELDXZ(fn,prec,'RL',Nr,SS_STORE_2,14,myThid)
      CALL MDSREADFIELDXZ(fn,prec,'RL',Nr,SS_STORE_3,15,myThid)
      CALL MDSREADFIELDXZ(fn,prec,'RL',Nr,SS_STORE_4,16,myThid)
      CALL MDSREADFIELDXZ(fn,prec,'RL',Nr,CVEL_US,17,myThid)
      CALL MDSREADFIELDXZ(fn,prec,'RL',Nr,CVEL_VS,18,myThid)
      CALL MDSREADFIELDXZ(fn,prec,'RL',Nr,CVEL_TS,19,myThid)
      CALL MDSREADFIELDXZ(fn,prec,'RL',Nr,CVEL_SS,20,myThid)
#ifdef ALLOW_NONHYDROSTATIC
      CALL MDSREADFIELDXZ(fn,prec,'RL',Nr,WS_STORE_1,21,myThid)
      CALL MDSREADFIELDXZ(fn,prec,'RL',Nr,WS_STORE_2,22,myThid)
      CALL MDSREADFIELDXZ(fn,prec,'RL',Nr,WS_STORE_3,23,myThid)
      CALL MDSREADFIELDXZ(fn,prec,'RL',Nr,WS_STORE_4,24,myThid)
      CALL MDSREADFIELDXZ(fn,prec,'RL',Nr,CVEL_WS,25,myThid)
#endif
#endif

C     READ E and W OB storage and phase speed arrays
#ifdef ALLOW_OBCS_EAST
      WRITE(fn,'(A,A10)') 'pickup_orlanskiE.',suff
      CALL MDSREADFIELDYZ(fn,prec,'RL',Nr,UE_STORE_1,1,myThid)
      CALL MDSREADFIELDYZ(fn,prec,'RL',Nr,UE_STORE_2,2,myThid)
      CALL MDSREADFIELDYZ(fn,prec,'RL',Nr,UE_STORE_3,3,myThid)
      CALL MDSREADFIELDYZ(fn,prec,'RL',Nr,UE_STORE_4,4,myThid)
      CALL MDSREADFIELDYZ(fn,prec,'RL',Nr,VE_STORE_1,5,myThid)
      CALL MDSREADFIELDYZ(fn,prec,'RL',Nr,VE_STORE_2,6,myThid)
      CALL MDSREADFIELDYZ(fn,prec,'RL',Nr,VE_STORE_3,7,myThid)
      CALL MDSREADFIELDYZ(fn,prec,'RL',Nr,VE_STORE_4,8,myThid)
      CALL MDSREADFIELDYZ(fn,prec,'RL',Nr,TE_STORE_1,9,myThid)
      CALL MDSREADFIELDYZ(fn,prec,'RL',Nr,TE_STORE_2,10,myThid)
      CALL MDSREADFIELDYZ(fn,prec,'RL',Nr,TE_STORE_3,11,myThid)
      CALL MDSREADFIELDYZ(fn,prec,'RL',Nr,TE_STORE_4,12,myThid)
      CALL MDSREADFIELDYZ(fn,prec,'RL',Nr,SE_STORE_1,13,myThid)
      CALL MDSREADFIELDYZ(fn,prec,'RL',Nr,SE_STORE_2,14,myThid)
      CALL MDSREADFIELDYZ(fn,prec,'RL',Nr,SE_STORE_3,15,myThid)
      CALL MDSREADFIELDYZ(fn,prec,'RL',Nr,SE_STORE_4,16,myThid)
      CALL MDSREADFIELDYZ(fn,prec,'RL',Nr,CVEL_UE,17,myThid)
      CALL MDSREADFIELDYZ(fn,prec,'RL',Nr,CVEL_VE,18,myThid)
      CALL MDSREADFIELDYZ(fn,prec,'RL',Nr,CVEL_TE,19,myThid)
      CALL MDSREADFIELDYZ(fn,prec,'RL',Nr,CVEL_SE,20,myThid)      
#ifdef ALLOW_NONHYDROSTATIC
      CALL MDSREADFIELDYZ(fn,prec,'RL',Nr,WE_STORE_1,21,myThid)
      CALL MDSREADFIELDYZ(fn,prec,'RL',Nr,WE_STORE_2,22,myThid)
      CALL MDSREADFIELDYZ(fn,prec,'RL',Nr,WE_STORE_3,23,myThid)
      CALL MDSREADFIELDYZ(fn,prec,'RL',Nr,WE_STORE_4,24,myThid)
      CALL MDSREADFIELDYZ(fn,prec,'RL',Nr,CVEL_WE,25,myThid)
#endif
#endif

#ifdef ALLOW_OBCS_WEST
      WRITE(fn,'(A,A10)') 'pickup_orlanskiW.',suff
      CALL MDSREADFIELDYZ(fn,prec,'RL',Nr,UW_STORE_1,1,myThid)
      CALL MDSREADFIELDYZ(fn,prec,'RL',Nr,UW_STORE_2,2,myThid)
      CALL MDSREADFIELDYZ(fn,prec,'RL',Nr,UW_STORE_3,3,myThid)
      CALL MDSREADFIELDYZ(fn,prec,'RL',Nr,UW_STORE_4,4,myThid)
      CALL MDSREADFIELDYZ(fn,prec,'RL',Nr,VW_STORE_1,5,myThid)
      CALL MDSREADFIELDYZ(fn,prec,'RL',Nr,VW_STORE_2,6,myThid)
      CALL MDSREADFIELDYZ(fn,prec,'RL',Nr,VW_STORE_3,7,myThid)
      CALL MDSREADFIELDYZ(fn,prec,'RL',Nr,VW_STORE_4,8,myThid)
      CALL MDSREADFIELDYZ(fn,prec,'RL',Nr,TW_STORE_1,9,myThid)
      CALL MDSREADFIELDYZ(fn,prec,'RL',Nr,TW_STORE_2,10,myThid)
      CALL MDSREADFIELDYZ(fn,prec,'RL',Nr,TW_STORE_3,11,myThid)
      CALL MDSREADFIELDYZ(fn,prec,'RL',Nr,TW_STORE_4,12,myThid)
      CALL MDSREADFIELDYZ(fn,prec,'RL',Nr,SW_STORE_1,13,myThid)
      CALL MDSREADFIELDYZ(fn,prec,'RL',Nr,SW_STORE_2,14,myThid)
      CALL MDSREADFIELDYZ(fn,prec,'RL',Nr,SW_STORE_3,15,myThid)
      CALL MDSREADFIELDYZ(fn,prec,'RL',Nr,SW_STORE_4,16,myThid)
      CALL MDSREADFIELDYZ(fn,prec,'RL',Nr,CVEL_UW,17,myThid)
      CALL MDSREADFIELDYZ(fn,prec,'RL',Nr,CVEL_VW,18,myThid)
      CALL MDSREADFIELDYZ(fn,prec,'RL',Nr,CVEL_TW,19,myThid)
      CALL MDSREADFIELDYZ(fn,prec,'RL',Nr,CVEL_SW,20,myThid)
#ifdef ALLOW_NONHYDROSTATIC
      CALL MDSREADFIELDYZ(fn,prec,'RL',Nr,WW_STORE_1,21,myThid)
      CALL MDSREADFIELDYZ(fn,prec,'RL',Nr,WW_STORE_2,22,myThid)
      CALL MDSREADFIELDYZ(fn,prec,'RL',Nr,WW_STORE_3,23,myThid)
      CALL MDSREADFIELDYZ(fn,prec,'RL',Nr,WW_STORE_4,24,myThid)
      CALL MDSREADFIELDYZ(fn,prec,'RL',Nr,CVEL_WW,25,myThid)
#endif
#endif


C     Fill in edge regions
c      _EXCH_XZ_R8(UN_STORE_1 , myThid)
c      _EXCH_XZ_R8(UN_STORE_2 , myThid)
c      _EXCH_XZ_R8(UN_STORE_3 , myThid)
c      _EXCH_XZ_R8(UN_STORE_4 , myThid)
c      _EXCH_XZ_R8(VN_STORE_1 , myThid)
c      _EXCH_XZ_R8(VN_STORE_2 , myThid)
c      _EXCH_XZ_R8(VN_STORE_3 , myThid)
c      _EXCH_XZ_R8(VN_STORE_4 , myThid)
c      _EXCH_XZ_R8(TN_STORE_1 , myThid)
c      _EXCH_XZ_R8(TN_STORE_2 , myThid)
c      _EXCH_XZ_R8(TN_STORE_3 , myThid)
c      _EXCH_XZ_R8(TN_STORE_4 , myThid)
c      _EXCH_XZ_R8(SN_STORE_1 , myThid)
c      _EXCH_XZ_R8(SN_STORE_2 , myThid)
c      _EXCH_XZ_R8(SN_STORE_3 , myThid)
c      _EXCH_XZ_R8(SN_STORE_4 , myThid)

c      _EXCH_XZ_R8(US_STORE_1 , myThid)
c      _EXCH_XZ_R8(US_STORE_2 , myThid)
c      _EXCH_XZ_R8(US_STORE_3 , myThid)
c      _EXCH_XZ_R8(US_STORE_4 , myThid)
c      _EXCH_XZ_R8(VS_STORE_1 , myThid)
c      _EXCH_XZ_R8(VS_STORE_2 , myThid)
c      _EXCH_XZ_R8(VS_STORE_3 , myThid)
c      _EXCH_XZ_R8(VS_STORE_4 , myThid)
c      _EXCH_XZ_R8(TS_STORE_1 , myThid)
c      _EXCH_XZ_R8(TS_STORE_2 , myThid)
c      _EXCH_XZ_R8(TS_STORE_3 , myThid)
c      _EXCH_XZ_R8(TS_STORE_4 , myThid)
c      _EXCH_XZ_R8(SS_STORE_1 , myThid)
c      _EXCH_XZ_R8(SS_STORE_2 , myThid)
c      _EXCH_XZ_R8(SS_STORE_3 , myThid)
c      _EXCH_XZ_R8(SS_STORE_4 , myThid)
      
c      _EXCH_XZ_R8(CVEL_UN , myThid)
c      _EXCH_XZ_R8(CVEL_VN , myThid)
c      _EXCH_XZ_R8(CVEL_TN , myThid)
c      _EXCH_XZ_R8(CVEL_SN , myThid)      
c      _EXCH_XZ_R8(CVEL_US , myThid)
c      _EXCH_XZ_R8(CVEL_VS , myThid)
c      _EXCH_XZ_R8(CVEL_TS , myThid)
c      _EXCH_XZ_R8(CVEL_SS , myThid)
c#ifdef ALLOW_NONHYDROSTATIC
c      _EXCH_XZ_R8(WN_STORE_1 , myThid)
c      _EXCH_XZ_R8(WN_STORE_2 , myThid)
c      _EXCH_XZ_R8(WN_STORE_3 , myThid)
c      _EXCH_XZ_R8(WN_STORE_4 , myThid)
c      _EXCH_XZ_R8(WS_STORE_1 , myThid)
c      _EXCH_XZ_R8(WS_STORE_2 , myThid)
c      _EXCH_XZ_R8(WS_STORE_3 , myThid)
c      _EXCH_XZ_R8(WS_STORE_4 , myThid)
c      _EXCH_XZ_R8(CVEL_WN , myThid)
c      _EXCH_XZ_R8(CVEL_WS , myThid)
c#endif

C     Fill in edge regions
c      _EXCH_YZ_R8(UE_STORE_1 , myThid)
c      _EXCH_YZ_R8(UE_STORE_2 , myThid)
c      _EXCH_YZ_R8(UE_STORE_3 , myThid)
c      _EXCH_YZ_R8(UE_STORE_4 , myThid)
c      _EXCH_YZ_R8(VE_STORE_1 , myThid)
c      _EXCH_YZ_R8(VE_STORE_2 , myThid)
c      _EXCH_YZ_R8(VE_STORE_3 , myThid)
c      _EXCH_YZ_R8(VE_STORE_4 , myThid)
c      _EXCH_YZ_R8(TE_STORE_1 , myThid)
c      _EXCH_YZ_R8(TE_STORE_2 , myThid)
c      _EXCH_YZ_R8(TE_STORE_3 , myThid)
c      _EXCH_YZ_R8(TE_STORE_4 , myThid)
c      _EXCH_YZ_R8(SE_STORE_1 , myThid)
c      _EXCH_YZ_R8(SE_STORE_2 , myThid)
c      _EXCH_YZ_R8(SE_STORE_3 , myThid)
c      _EXCH_YZ_R8(SE_STORE_4 , myThid)

c      _EXCH_YZ_R8(UW_STORE_1 , myThid)
c      _EXCH_YZ_R8(UW_STORE_2 , myThid)
c      _EXCH_YZ_R8(UW_STORE_3 , myThid)
c      _EXCH_YZ_R8(UW_STORE_4 , myThid)
c      _EXCH_YZ_R8(VW_STORE_1 , myThid)
c      _EXCH_YZ_R8(VW_STORE_2 , myThid)
c      _EXCH_YZ_R8(VW_STORE_3 , myThid)
c      _EXCH_YZ_R8(VW_STORE_4 , myThid)
c      _EXCH_YZ_R8(TW_STORE_1 , myThid)
c      _EXCH_YZ_R8(TW_STORE_2 , myThid)
c      _EXCH_YZ_R8(TW_STORE_3 , myThid)
c      _EXCH_YZ_R8(TW_STORE_4 , myThid)
c      _EXCH_YZ_R8(SW_STORE_1 , myThid)
c      _EXCH_YZ_R8(SW_STORE_2 , myThid)
c      _EXCH_YZ_R8(SW_STORE_3 , myThid)
c      _EXCH_YZ_R8(SW_STORE_4 , myThid)

c      _EXCH_YZ_R8(CVEL_UE , myThid)
c      _EXCH_YZ_R8(CVEL_VE , myThid)
c      _EXCH_YZ_R8(CVEL_TE , myThid)
c      _EXCH_YZ_R8(CVEL_SE , myThid)      
c      _EXCH_YZ_R8(CVEL_UW , myThid)
c      _EXCH_YZ_R8(CVEL_VW , myThid)
c      _EXCH_YZ_R8(CVEL_TW , myThid)
c      _EXCH_YZ_R8(CVEL_SW , myThid)
c#ifdef ALLOW_NONHYDROSTATIC
c      _EXCH_YZ_R8(WE_STORE_1 , myThid)
c      _EXCH_YZ_R8(WE_STORE_2 , myThid)
c      _EXCH_YZ_R8(WE_STORE_3 , myThid)
c      _EXCH_YZ_R8(WE_STORE_4 , myThid)
c      _EXCH_YZ_R8(WW_STORE_1 , myThid)
c      _EXCH_YZ_R8(WW_STORE_2 , myThid)
c      _EXCH_YZ_R8(WW_STORE_3 , myThid)
c      _EXCH_YZ_R8(WW_STORE_4 , myThid)          
c      _EXCH_YZ_R8(CVEL_WE , myThid)
c      _EXCH_YZ_R8(CVEL_WW , myThid)
c#endif


#endif /* ALLOW_ORLANSKI */
      RETURN
      END

CStartofinterface
      SUBROUTINE ORLANSKI_WRITE_CHECKPOINT( prec, lgf, permCheckPoint, 
     &                                      myIt, myThid )
C     /==========================================================\
C     | SUBROUTINE ORLANSKI_WRITE_CHECKPOINT                     |
C     | o Write Orlanski open boundary checkpoint arrays         |
C     |==========================================================|
C     |                                                          |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "RESTART.h"
#include "OBCS.h"
#include "ORLANSKI.h"

C     == Routine arguments ==
      INTEGER prec
      LOGICAL lgf
      LOGICAL permCheckPoint
      INTEGER myIt
      INTEGER myThid
CEndofinterface

#ifdef ALLOW_ORLANSKI

C     == Local variables ==
      CHARACTER*(MAX_LEN_FNAM) fn

C     Write N and S OB storage and phase speed arrays
#ifdef ALLOW_OBCS_NORTH
      IF ( permCheckPoint ) THEN
         WRITE(fn,'(A,I10.10)') 'pickup_orlanskiN.',myIt
      ELSE
         WRITE(fn,'(A,A)') 'pickup_orlanskiN.',checkPtSuff(nCheckLev)
      ENDIF
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RL',Nr,UN_STORE_1,1,
     &     myIt,myThid)
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RL',Nr,UN_STORE_2,2,
     &     myIt,myThid)
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RL',Nr,UN_STORE_3,3,
     &     myIt,myThid)
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RL',Nr,UN_STORE_4,4,
     &     myIt,myThid)
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RL',Nr,VN_STORE_1,5,
     &     myIt,myThid)
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RL',Nr,VN_STORE_2,6,
     &     myIt,myThid)
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RL',Nr,VN_STORE_3,7,
     &     myIt,myThid)
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RL',Nr,VN_STORE_4,8,
     &     myIt,myThid)
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RL',Nr,TN_STORE_1,9,
     &     myIt,myThid)
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RL',Nr,TN_STORE_2,10,
     &     myIt,myThid)
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RL',Nr,TN_STORE_3,11,
     &     myIt,myThid)
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RL',Nr,TN_STORE_4,12,
     &     myIt,myThid)
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RL',Nr,SN_STORE_1,13,
     &     myIt,myThid)
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RL',Nr,SN_STORE_2,14,
     &     myIt,myThid)
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RL',Nr,SN_STORE_3,15,
     &     myIt,myThid)
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RL',Nr,SN_STORE_4,16,
     &     myIt,myThid)
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RL',Nr,CVEL_UN,17,
     &     myIt,myThid)
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RL',Nr,CVEL_VN,18,
     &     myIt,myThid)
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RL',Nr,CVEL_TN,19,
     &     myIt,myThid)
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RL',Nr,CVEL_SN,20,
     &     myIt,myThid)      
#ifdef ALLOW_NONHYDROSTATIC
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RL',Nr,WN_STORE_1,21,
     &     myIt,myThid)
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RL',Nr,WN_STORE_2,22,
     &     myIt,myThid)
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RL',Nr,WN_STORE_3,23,
     &     myIt,myThid)
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RL',Nr,WN_STORE_4,24,
     &     myIt,myThid)
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RL',Nr,CVEL_WN,25,
     &     myIt,myThid)
#endif
#endif

#ifdef ALLOW_OBCS_SOUTH
      IF ( permCheckPoint ) THEN
         WRITE(fn,'(A,I10.10)') 'pickup_orlanskiS.',myIt
      ELSE
         WRITE(fn,'(A,A)') 'pickup_orlanskiS.',checkPtSuff(nCheckLev)
      ENDIF
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RL',Nr,US_STORE_1,1,
     &     myIt,myThid)
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RL',Nr,US_STORE_2,2,
     &     myIt,myThid)
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RL',Nr,US_STORE_3,3,
     &     myIt,myThid)
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RL',Nr,US_STORE_4,4,
     &     myIt,myThid)
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RL',Nr,VS_STORE_1,5,
     &     myIt,myThid)
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RL',Nr,VS_STORE_2,6,
     &     myIt,myThid)
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RL',Nr,VS_STORE_3,7,
     &     myIt,myThid)
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RL',Nr,VS_STORE_4,8,
     &     myIt,myThid)
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RL',Nr,TS_STORE_1,9,
     &     myIt,myThid)
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RL',Nr,TS_STORE_2,10,
     &     myIt,myThid)
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RL',Nr,TS_STORE_3,11,
     &     myIt,myThid)
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RL',Nr,TS_STORE_4,12,
     &     myIt,myThid)
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RL',Nr,SS_STORE_1,13,
     &     myIt,myThid)
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RL',Nr,SS_STORE_2,14,
     &     myIt,myThid)
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RL',Nr,SS_STORE_3,15,
     &     myIt,myThid)
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RL',Nr,SS_STORE_4,16,
     &     myIt,myThid)
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RL',Nr,CVEL_US,17,
     &     myIt,myThid)
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RL',Nr,CVEL_VS,18,
     &     myIt,myThid)
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RL',Nr,CVEL_TS,19,
     &     myIt,myThid)
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RL',Nr,CVEL_SS,20,
     &     myIt,myThid)
#ifdef ALLOW_NONHYDROSTATIC
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RL',Nr,WS_STORE_1,21,
     &     myIt,myThid)
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RL',Nr,WS_STORE_2,22,
     &     myIt,myThid)
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RL',Nr,WS_STORE_3,23,
     &     myIt,myThid)
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RL',Nr,WS_STORE_4,24,
     &     myIt,myThid)   
      CALL MDSWRITEFIELDXZ(fn,prec,lgf,'RL',Nr,CVEL_WS,25,
     &     myIt,myThid)
#endif
#endif

C     Write E and W OB storage and phase speed arrays
#ifdef ALLOW_OBCS_EAST
      IF ( permCheckPoint ) THEN
         WRITE(fn,'(A,I10.10)') 'pickup_orlanskiE.',myIt
      ELSE
         WRITE(fn,'(A,A)') 'pickup_orlanskiE.',checkPtSuff(nCheckLev)
      ENDIF
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RL',Nr,UE_STORE_1,1,
     &     myIt,myThid)
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RL',Nr,UE_STORE_2,2,
     &     myIt,myThid)
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RL',Nr,UE_STORE_3,3,
     &     myIt,myThid)
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RL',Nr,UE_STORE_4,4,
     &     myIt,myThid)
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RL',Nr,VE_STORE_1,5,
     &     myIt,myThid)
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RL',Nr,VE_STORE_2,6,
     &     myIt,myThid)
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RL',Nr,VE_STORE_3,7,
     &     myIt,myThid)
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RL',Nr,VE_STORE_4,8,
     &     myIt,myThid)
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RL',Nr,TE_STORE_1,9,
     &     myIt,myThid)
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RL',Nr,TE_STORE_2,10,
     &     myIt,myThid)
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RL',Nr,TE_STORE_3,11,
     &     myIt,myThid)
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RL',Nr,TE_STORE_4,12,
     &     myIt,myThid)
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RL',Nr,SE_STORE_1,13,
     &     myIt,myThid)
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RL',Nr,SE_STORE_2,14,
     &     myIt,myThid)
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RL',Nr,SE_STORE_3,15,
     &     myIt,myThid)
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RL',Nr,SE_STORE_4,16,
     &     myIt,myThid)
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RL',Nr,CVEL_UE,17,
     &     myIt,myThid)
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RL',Nr,CVEL_VE,18,
     &     myIt,myThid)
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RL',Nr,CVEL_TE,19,
     &     myIt,myThid)
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RL',Nr,CVEL_SE,20,
     &     myIt,myThid)      
#ifdef ALLOW_NONHYDROSTATIC
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RL',Nr,WE_STORE_1,21,
     &     myIt,myThid)
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RL',Nr,WE_STORE_2,22,
     &     myIt,myThid)
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RL',Nr,WE_STORE_3,23,
     &     myIt,myThid)
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RL',Nr,WE_STORE_4,24,
     &     myIt,myThid)
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RL',Nr,CVEL_WE,25,
     &     myIt,myThid)
#endif
#endif

#ifdef ALLOW_OBCS_WEST
      IF ( permCheckPoint ) THEN
         WRITE(fn,'(A,I10.10)') 'pickup_orlanskiW.',myIt
      ELSE
         WRITE(fn,'(A,A)') 'pickup_orlanskiW.',checkPtSuff(nCheckLev)
      ENDIF
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RL',Nr,UW_STORE_1,1,
     &     myIt,myThid)
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RL',Nr,UW_STORE_2,2,
     &     myIt,myThid)
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RL',Nr,UW_STORE_3,3,
     &     myIt,myThid)
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RL',Nr,UW_STORE_4,4,
     &     myIt,myThid)
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RL',Nr,VW_STORE_1,5,
     &     myIt,myThid)
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RL',Nr,VW_STORE_2,6,
     &     myIt,myThid)
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RL',Nr,VW_STORE_3,7,
     &     myIt,myThid)
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RL',Nr,VW_STORE_4,8,
     &     myIt,myThid)
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RL',Nr,TW_STORE_1,9,
     &     myIt,myThid)
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RL',Nr,TW_STORE_2,10,
     &     myIt,myThid)
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RL',Nr,TW_STORE_3,11,
     &     myIt,myThid)
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RL',Nr,TW_STORE_4,12,
     &     myIt,myThid)
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RL',Nr,SW_STORE_1,13,
     &     myIt,myThid)
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RL',Nr,SW_STORE_2,14,
     &     myIt,myThid)
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RL',Nr,SW_STORE_3,15,
     &     myIt,myThid)
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RL',Nr,SW_STORE_4,16,
     &     myIt,myThid)
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RL',Nr,CVEL_UW,17,
     &     myIt,myThid)
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RL',Nr,CVEL_VW,18,
     &     myIt,myThid)
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RL',Nr,CVEL_TW,19,
     &     myIt,myThid)
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RL',Nr,CVEL_SW,20,
     &     myIt,myThid)
#ifdef ALLOW_NONHYDROSTATIC
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RL',Nr,WW_STORE_1,21,
     &     myIt,myThid)
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RL',Nr,WW_STORE_2,22,
     &     myIt,myThid)
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RL',Nr,WW_STORE_3,23,
     &     myIt,myThid)
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RL',Nr,WW_STORE_4,24,
     &     myIt,myThid)
      CALL MDSWRITEFIELDYZ(fn,prec,lgf,'RL',Nr,CVEL_WW,25,
     &     myIt,myThid)
#endif
#endif

#endif /* ALLOW_ORLANSKI */
      RETURN
      END

