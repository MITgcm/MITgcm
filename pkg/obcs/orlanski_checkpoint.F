C $Header: /u/gcmpack/MITgcm/pkg/obcs/Attic/orlanski_checkpoint.F,v 1.8 2009/09/01 19:40:45 jmc Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

CStartofinterface
      SUBROUTINE ORLANSKI_READ_PICKUP( myIter, myThid )
C     *==========================================================*
C     | SUBROUTINE ORLANSKI_READ_PICKUP
C     | o Read Orlanski open boundary pickup file
C     *==========================================================*
C     |
C     *==========================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "OBCS.h"
#include "ORLANSKI.h"

C     == Routine arguments ==
      INTEGER myIter
      INTEGER myThid
CEndofinterface

#ifdef ALLOW_ORLANSKI

C     == Local variables ==
      CHARACTER*(MAX_LEN_FNAM) fn
      CHARACTER*(10) suff
      INTEGER prec

      IF ( pickupSuff.EQ.' ' ) THEN
        WRITE(suff,'(I10.10)') myIter
      ELSE
        suff = pickupSuff
      ENDIF
      prec = precFloat64

#ifdef ALLOW_OBCS_NORTH
C     READ N and S OB storage and phase speed arrays
      WRITE(fn,'(2A)') 'pickup_orlanskiN.',suff
      CALL READ_REC_XZ_RL(fn,prec,Nr,UN_STORE_1, 1,myIter,myThid)
      CALL READ_REC_XZ_RL(fn,prec,Nr,UN_STORE_2, 2,myIter,myThid)
      CALL READ_REC_XZ_RL(fn,prec,Nr,UN_STORE_3, 3,myIter,myThid)
      CALL READ_REC_XZ_RL(fn,prec,Nr,UN_STORE_4, 4,myIter,myThid)
      CALL READ_REC_XZ_RL(fn,prec,Nr,VN_STORE_1, 5,myIter,myThid)
      CALL READ_REC_XZ_RL(fn,prec,Nr,VN_STORE_2, 6,myIter,myThid)
      CALL READ_REC_XZ_RL(fn,prec,Nr,VN_STORE_3, 7,myIter,myThid)
      CALL READ_REC_XZ_RL(fn,prec,Nr,VN_STORE_4, 8,myIter,myThid)
      CALL READ_REC_XZ_RL(fn,prec,Nr,TN_STORE_1, 9,myIter,myThid)
      CALL READ_REC_XZ_RL(fn,prec,Nr,TN_STORE_2,10,myIter,myThid)
      CALL READ_REC_XZ_RL(fn,prec,Nr,TN_STORE_3,11,myIter,myThid)
      CALL READ_REC_XZ_RL(fn,prec,Nr,TN_STORE_4,12,myIter,myThid)
      CALL READ_REC_XZ_RL(fn,prec,Nr,SN_STORE_1,13,myIter,myThid)
      CALL READ_REC_XZ_RL(fn,prec,Nr,SN_STORE_2,14,myIter,myThid)
      CALL READ_REC_XZ_RL(fn,prec,Nr,SN_STORE_3,15,myIter,myThid)
      CALL READ_REC_XZ_RL(fn,prec,Nr,SN_STORE_4,16,myIter,myThid)
      CALL READ_REC_XZ_RL(fn,prec,Nr, CVEL_UN , 17,myIter,myThid)
      CALL READ_REC_XZ_RL(fn,prec,Nr, CVEL_VN , 18,myIter,myThid)
      CALL READ_REC_XZ_RL(fn,prec,Nr, CVEL_TN , 19,myIter,myThid)
      CALL READ_REC_XZ_RL(fn,prec,Nr, CVEL_SN , 20,myIter,myThid)
#ifdef ALLOW_NONHYDROSTATIC
      CALL READ_REC_XZ_RL(fn,prec,Nr,WN_STORE_1,21,myIter,myThid)
      CALL READ_REC_XZ_RL(fn,prec,Nr,WN_STORE_2,22,myIter,myThid)
      CALL READ_REC_XZ_RL(fn,prec,Nr,WN_STORE_3,23,myIter,myThid)
      CALL READ_REC_XZ_RL(fn,prec,Nr,WN_STORE_4,24,myIter,myThid)
      CALL READ_REC_XZ_RL(fn,prec,Nr, CVEL_WN , 25,myIter,myThid)
#endif
#endif /* ALLOW_OBCS_NORTH */

#ifdef ALLOW_OBCS_SOUTH
      WRITE(fn,'(2A)') 'pickup_orlanskiS.',suff
      CALL READ_REC_XZ_RL(fn,prec,Nr,US_STORE_1, 1,myIter,myThid)
      CALL READ_REC_XZ_RL(fn,prec,Nr,US_STORE_2, 2,myIter,myThid)
      CALL READ_REC_XZ_RL(fn,prec,Nr,US_STORE_3, 3,myIter,myThid)
      CALL READ_REC_XZ_RL(fn,prec,Nr,US_STORE_4, 4,myIter,myThid)
      CALL READ_REC_XZ_RL(fn,prec,Nr,VS_STORE_1, 5,myIter,myThid)
      CALL READ_REC_XZ_RL(fn,prec,Nr,VS_STORE_2, 6,myIter,myThid)
      CALL READ_REC_XZ_RL(fn,prec,Nr,VS_STORE_3, 7,myIter,myThid)
      CALL READ_REC_XZ_RL(fn,prec,Nr,VS_STORE_4, 8,myIter,myThid)
      CALL READ_REC_XZ_RL(fn,prec,Nr,TS_STORE_1, 9,myIter,myThid)
      CALL READ_REC_XZ_RL(fn,prec,Nr,TS_STORE_2,10,myIter,myThid)
      CALL READ_REC_XZ_RL(fn,prec,Nr,TS_STORE_3,11,myIter,myThid)
      CALL READ_REC_XZ_RL(fn,prec,Nr,TS_STORE_4,12,myIter,myThid)
      CALL READ_REC_XZ_RL(fn,prec,Nr,SS_STORE_1,13,myIter,myThid)
      CALL READ_REC_XZ_RL(fn,prec,Nr,SS_STORE_2,14,myIter,myThid)
      CALL READ_REC_XZ_RL(fn,prec,Nr,SS_STORE_3,15,myIter,myThid)
      CALL READ_REC_XZ_RL(fn,prec,Nr,SS_STORE_4,16,myIter,myThid)
      CALL READ_REC_XZ_RL(fn,prec,Nr, CVEL_US , 17,myIter,myThid)
      CALL READ_REC_XZ_RL(fn,prec,Nr, CVEL_VS , 18,myIter,myThid)
      CALL READ_REC_XZ_RL(fn,prec,Nr, CVEL_TS , 19,myIter,myThid)
      CALL READ_REC_XZ_RL(fn,prec,Nr, CVEL_SS , 20,myIter,myThid)
#ifdef ALLOW_NONHYDROSTATIC
      CALL READ_REC_XZ_RL(fn,prec,Nr,WS_STORE_1,21,myIter,myThid)
      CALL READ_REC_XZ_RL(fn,prec,Nr,WS_STORE_2,22,myIter,myThid)
      CALL READ_REC_XZ_RL(fn,prec,Nr,WS_STORE_3,23,myIter,myThid)
      CALL READ_REC_XZ_RL(fn,prec,Nr,WS_STORE_4,24,myIter,myThid)
      CALL READ_REC_XZ_RL(fn,prec,Nr, CVEL_WS , 25,myIter,myThid)
#endif
#endif /* ALLOW_OBCS_SOUTH */

C     READ E and W OB storage and phase speed arrays
#ifdef ALLOW_OBCS_EAST
      WRITE(fn,'(2A)') 'pickup_orlanskiE.',suff
      CALL READ_REC_YZ_RL(fn,prec,Nr,UE_STORE_1, 1,myIter,myThid)
      CALL READ_REC_YZ_RL(fn,prec,Nr,UE_STORE_2, 2,myIter,myThid)
      CALL READ_REC_YZ_RL(fn,prec,Nr,UE_STORE_3, 3,myIter,myThid)
      CALL READ_REC_YZ_RL(fn,prec,Nr,UE_STORE_4, 4,myIter,myThid)
      CALL READ_REC_YZ_RL(fn,prec,Nr,VE_STORE_1, 5,myIter,myThid)
      CALL READ_REC_YZ_RL(fn,prec,Nr,VE_STORE_2, 6,myIter,myThid)
      CALL READ_REC_YZ_RL(fn,prec,Nr,VE_STORE_3, 7,myIter,myThid)
      CALL READ_REC_YZ_RL(fn,prec,Nr,VE_STORE_4, 8,myIter,myThid)
      CALL READ_REC_YZ_RL(fn,prec,Nr,TE_STORE_1, 9,myIter,myThid)
      CALL READ_REC_YZ_RL(fn,prec,Nr,TE_STORE_2,10,myIter,myThid)
      CALL READ_REC_YZ_RL(fn,prec,Nr,TE_STORE_3,11,myIter,myThid)
      CALL READ_REC_YZ_RL(fn,prec,Nr,TE_STORE_4,12,myIter,myThid)
      CALL READ_REC_YZ_RL(fn,prec,Nr,SE_STORE_1,13,myIter,myThid)
      CALL READ_REC_YZ_RL(fn,prec,Nr,SE_STORE_2,14,myIter,myThid)
      CALL READ_REC_YZ_RL(fn,prec,Nr,SE_STORE_3,15,myIter,myThid)
      CALL READ_REC_YZ_RL(fn,prec,Nr,SE_STORE_4,16,myIter,myThid)
      CALL READ_REC_YZ_RL(fn,prec,Nr, CVEL_UE , 17,myIter,myThid)
      CALL READ_REC_YZ_RL(fn,prec,Nr, CVEL_VE , 18,myIter,myThid)
      CALL READ_REC_YZ_RL(fn,prec,Nr, CVEL_TE , 19,myIter,myThid)
      CALL READ_REC_YZ_RL(fn,prec,Nr, CVEL_SE , 20,myIter,myThid)
#ifdef ALLOW_NONHYDROSTATIC
      CALL READ_REC_YZ_RL(fn,prec,Nr,WE_STORE_1,21,myIter,myThid)
      CALL READ_REC_YZ_RL(fn,prec,Nr,WE_STORE_2,22,myIter,myThid)
      CALL READ_REC_YZ_RL(fn,prec,Nr,WE_STORE_3,23,myIter,myThid)
      CALL READ_REC_YZ_RL(fn,prec,Nr,WE_STORE_4,24,myIter,myThid)
      CALL READ_REC_YZ_RL(fn,prec,Nr, CVEL_WE , 25,myIter,myThid)
#endif
#endif /* ALLOW_OBCS_EAST */

#ifdef ALLOW_OBCS_WEST
      WRITE(fn,'(2A)') 'pickup_orlanskiW.',suff
      CALL READ_REC_YZ_RL(fn,prec,Nr,UW_STORE_1, 1,myIter,myThid)
      CALL READ_REC_YZ_RL(fn,prec,Nr,UW_STORE_2, 2,myIter,myThid)
      CALL READ_REC_YZ_RL(fn,prec,Nr,UW_STORE_3, 3,myIter,myThid)
      CALL READ_REC_YZ_RL(fn,prec,Nr,UW_STORE_4, 4,myIter,myThid)
      CALL READ_REC_YZ_RL(fn,prec,Nr,VW_STORE_1, 5,myIter,myThid)
      CALL READ_REC_YZ_RL(fn,prec,Nr,VW_STORE_2, 6,myIter,myThid)
      CALL READ_REC_YZ_RL(fn,prec,Nr,VW_STORE_3, 7,myIter,myThid)
      CALL READ_REC_YZ_RL(fn,prec,Nr,VW_STORE_4, 8,myIter,myThid)
      CALL READ_REC_YZ_RL(fn,prec,Nr,TW_STORE_1, 9,myIter,myThid)
      CALL READ_REC_YZ_RL(fn,prec,Nr,TW_STORE_2,10,myIter,myThid)
      CALL READ_REC_YZ_RL(fn,prec,Nr,TW_STORE_3,11,myIter,myThid)
      CALL READ_REC_YZ_RL(fn,prec,Nr,TW_STORE_4,12,myIter,myThid)
      CALL READ_REC_YZ_RL(fn,prec,Nr,SW_STORE_1,13,myIter,myThid)
      CALL READ_REC_YZ_RL(fn,prec,Nr,SW_STORE_2,14,myIter,myThid)
      CALL READ_REC_YZ_RL(fn,prec,Nr,SW_STORE_3,15,myIter,myThid)
      CALL READ_REC_YZ_RL(fn,prec,Nr,SW_STORE_4,16,myIter,myThid)
      CALL READ_REC_YZ_RL(fn,prec,Nr, CVEL_UW , 17,myIter,myThid)
      CALL READ_REC_YZ_RL(fn,prec,Nr, CVEL_VW , 18,myIter,myThid)
      CALL READ_REC_YZ_RL(fn,prec,Nr, CVEL_TW , 19,myIter,myThid)
      CALL READ_REC_YZ_RL(fn,prec,Nr, CVEL_SW , 20,myIter,myThid)
#ifdef ALLOW_NONHYDROSTATIC
      CALL READ_REC_YZ_RL(fn,prec,Nr,WW_STORE_1,21,myIter,myThid)
      CALL READ_REC_YZ_RL(fn,prec,Nr,WW_STORE_2,22,myIter,myThid)
      CALL READ_REC_YZ_RL(fn,prec,Nr,WW_STORE_3,23,myIter,myThid)
      CALL READ_REC_YZ_RL(fn,prec,Nr,WW_STORE_4,24,myIter,myThid)
      CALL READ_REC_YZ_RL(fn,prec,Nr, CVEL_WW , 25,myIter,myThid)
#endif
#endif /* ALLOW_OBCS_WEST */

C     Fill in edge regions
c      _EXCH_XZ_RL(UN_STORE_1 , myThid)
c      _EXCH_XZ_RL(UN_STORE_2 , myThid)
c      _EXCH_XZ_RL(UN_STORE_3 , myThid)
c      _EXCH_XZ_RL(UN_STORE_4 , myThid)
c      _EXCH_XZ_RL(VN_STORE_1 , myThid)
c      _EXCH_XZ_RL(VN_STORE_2 , myThid)
c      _EXCH_XZ_RL(VN_STORE_3 , myThid)
c      _EXCH_XZ_RL(VN_STORE_4 , myThid)
c      _EXCH_XZ_RL(TN_STORE_1 , myThid)
c      _EXCH_XZ_RL(TN_STORE_2 , myThid)
c      _EXCH_XZ_RL(TN_STORE_3 , myThid)
c      _EXCH_XZ_RL(TN_STORE_4 , myThid)
c      _EXCH_XZ_RL(SN_STORE_1 , myThid)
c      _EXCH_XZ_RL(SN_STORE_2 , myThid)
c      _EXCH_XZ_RL(SN_STORE_3 , myThid)
c      _EXCH_XZ_RL(SN_STORE_4 , myThid)

c      _EXCH_XZ_RL(US_STORE_1 , myThid)
c      _EXCH_XZ_RL(US_STORE_2 , myThid)
c      _EXCH_XZ_RL(US_STORE_3 , myThid)
c      _EXCH_XZ_RL(US_STORE_4 , myThid)
c      _EXCH_XZ_RL(VS_STORE_1 , myThid)
c      _EXCH_XZ_RL(VS_STORE_2 , myThid)
c      _EXCH_XZ_RL(VS_STORE_3 , myThid)
c      _EXCH_XZ_RL(VS_STORE_4 , myThid)
c      _EXCH_XZ_RL(TS_STORE_1 , myThid)
c      _EXCH_XZ_RL(TS_STORE_2 , myThid)
c      _EXCH_XZ_RL(TS_STORE_3 , myThid)
c      _EXCH_XZ_RL(TS_STORE_4 , myThid)
c      _EXCH_XZ_RL(SS_STORE_1 , myThid)
c      _EXCH_XZ_RL(SS_STORE_2 , myThid)
c      _EXCH_XZ_RL(SS_STORE_3 , myThid)
c      _EXCH_XZ_RL(SS_STORE_4 , myThid)

c      _EXCH_XZ_RL(CVEL_UN , myThid)
c      _EXCH_XZ_RL(CVEL_VN , myThid)
c      _EXCH_XZ_RL(CVEL_TN , myThid)
c      _EXCH_XZ_RL(CVEL_SN , myThid)
c      _EXCH_XZ_RL(CVEL_US , myThid)
c      _EXCH_XZ_RL(CVEL_VS , myThid)
c      _EXCH_XZ_RL(CVEL_TS , myThid)
c      _EXCH_XZ_RL(CVEL_SS , myThid)
c#ifdef ALLOW_NONHYDROSTATIC
c      _EXCH_XZ_RL(WN_STORE_1 , myThid)
c      _EXCH_XZ_RL(WN_STORE_2 , myThid)
c      _EXCH_XZ_RL(WN_STORE_3 , myThid)
c      _EXCH_XZ_RL(WN_STORE_4 , myThid)
c      _EXCH_XZ_RL(WS_STORE_1 , myThid)
c      _EXCH_XZ_RL(WS_STORE_2 , myThid)
c      _EXCH_XZ_RL(WS_STORE_3 , myThid)
c      _EXCH_XZ_RL(WS_STORE_4 , myThid)
c      _EXCH_XZ_RL(CVEL_WN , myThid)
c      _EXCH_XZ_RL(CVEL_WS , myThid)
c#endif

C     Fill in edge regions
c      _EXCH_YZ_RL(UE_STORE_1 , myThid)
c      _EXCH_YZ_RL(UE_STORE_2 , myThid)
c      _EXCH_YZ_RL(UE_STORE_3 , myThid)
c      _EXCH_YZ_RL(UE_STORE_4 , myThid)
c      _EXCH_YZ_RL(VE_STORE_1 , myThid)
c      _EXCH_YZ_RL(VE_STORE_2 , myThid)
c      _EXCH_YZ_RL(VE_STORE_3 , myThid)
c      _EXCH_YZ_RL(VE_STORE_4 , myThid)
c      _EXCH_YZ_RL(TE_STORE_1 , myThid)
c      _EXCH_YZ_RL(TE_STORE_2 , myThid)
c      _EXCH_YZ_RL(TE_STORE_3 , myThid)
c      _EXCH_YZ_RL(TE_STORE_4 , myThid)
c      _EXCH_YZ_RL(SE_STORE_1 , myThid)
c      _EXCH_YZ_RL(SE_STORE_2 , myThid)
c      _EXCH_YZ_RL(SE_STORE_3 , myThid)
c      _EXCH_YZ_RL(SE_STORE_4 , myThid)

c      _EXCH_YZ_RL(UW_STORE_1 , myThid)
c      _EXCH_YZ_RL(UW_STORE_2 , myThid)
c      _EXCH_YZ_RL(UW_STORE_3 , myThid)
c      _EXCH_YZ_RL(UW_STORE_4 , myThid)
c      _EXCH_YZ_RL(VW_STORE_1 , myThid)
c      _EXCH_YZ_RL(VW_STORE_2 , myThid)
c      _EXCH_YZ_RL(VW_STORE_3 , myThid)
c      _EXCH_YZ_RL(VW_STORE_4 , myThid)
c      _EXCH_YZ_RL(TW_STORE_1 , myThid)
c      _EXCH_YZ_RL(TW_STORE_2 , myThid)
c      _EXCH_YZ_RL(TW_STORE_3 , myThid)
c      _EXCH_YZ_RL(TW_STORE_4 , myThid)
c      _EXCH_YZ_RL(SW_STORE_1 , myThid)
c      _EXCH_YZ_RL(SW_STORE_2 , myThid)
c      _EXCH_YZ_RL(SW_STORE_3 , myThid)
c      _EXCH_YZ_RL(SW_STORE_4 , myThid)

c      _EXCH_YZ_RL(CVEL_UE , myThid)
c      _EXCH_YZ_RL(CVEL_VE , myThid)
c      _EXCH_YZ_RL(CVEL_TE , myThid)
c      _EXCH_YZ_RL(CVEL_SE , myThid)
c      _EXCH_YZ_RL(CVEL_UW , myThid)
c      _EXCH_YZ_RL(CVEL_VW , myThid)
c      _EXCH_YZ_RL(CVEL_TW , myThid)
c      _EXCH_YZ_RL(CVEL_SW , myThid)
c#ifdef ALLOW_NONHYDROSTATIC
c      _EXCH_YZ_RL(WE_STORE_1 , myThid)
c      _EXCH_YZ_RL(WE_STORE_2 , myThid)
c      _EXCH_YZ_RL(WE_STORE_3 , myThid)
c      _EXCH_YZ_RL(WE_STORE_4 , myThid)
c      _EXCH_YZ_RL(WW_STORE_1 , myThid)
c      _EXCH_YZ_RL(WW_STORE_2 , myThid)
c      _EXCH_YZ_RL(WW_STORE_3 , myThid)
c      _EXCH_YZ_RL(WW_STORE_4 , myThid)
c      _EXCH_YZ_RL(CVEL_WE , myThid)
c      _EXCH_YZ_RL(CVEL_WW , myThid)
c#endif


#endif /* ALLOW_ORLANSKI */
      RETURN
      END

CStartofinterface
      SUBROUTINE ORLANSKI_WRITE_PICKUP(
     &                          suff, myTime, myIter, myThid )
C     *==========================================================*
C     | SUBROUTINE ORLANSKI_WRITE_PICKUP
C     | o Write Orlanski open boundary pickup file
C     *==========================================================*
C     |
C     *==========================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "RESTART.h"
#include "OBCS.h"
#include "ORLANSKI.h"

C     == Routine arguments ==
      CHARACTER*(*) suff
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEndofinterface

#ifdef ALLOW_ORLANSKI

C     === Functions ====
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     == Local variables ==
      CHARACTER*(MAX_LEN_FNAM) fn
      INTEGER prec
      INTEGER iL

      iL = ILNBLNK(suff)
      iL = MAX(1,iL)
      prec = precFloat64

C     Write N and S OB storage and phase speed arrays
#ifdef ALLOW_OBCS_NORTH
      WRITE(fn,'(2A)') 'pickup_orlanskiN.',suff(1:iL)
      CALL WRITE_REC_XZ_RL(fn,prec,Nr,UN_STORE_1, 1,myIter,myThid)
      CALL WRITE_REC_XZ_RL(fn,prec,Nr,UN_STORE_2, 2,myIter,myThid)
      CALL WRITE_REC_XZ_RL(fn,prec,Nr,UN_STORE_3, 3,myIter,myThid)
      CALL WRITE_REC_XZ_RL(fn,prec,Nr,UN_STORE_4, 4,myIter,myThid)
      CALL WRITE_REC_XZ_RL(fn,prec,Nr,VN_STORE_1, 5,myIter,myThid)
      CALL WRITE_REC_XZ_RL(fn,prec,Nr,VN_STORE_2, 6,myIter,myThid)
      CALL WRITE_REC_XZ_RL(fn,prec,Nr,VN_STORE_3, 7,myIter,myThid)
      CALL WRITE_REC_XZ_RL(fn,prec,Nr,VN_STORE_4, 8,myIter,myThid)
      CALL WRITE_REC_XZ_RL(fn,prec,Nr,TN_STORE_1, 9,myIter,myThid)
      CALL WRITE_REC_XZ_RL(fn,prec,Nr,TN_STORE_2,10,myIter,myThid)
      CALL WRITE_REC_XZ_RL(fn,prec,Nr,TN_STORE_3,11,myIter,myThid)
      CALL WRITE_REC_XZ_RL(fn,prec,Nr,TN_STORE_4,12,myIter,myThid)
      CALL WRITE_REC_XZ_RL(fn,prec,Nr,SN_STORE_1,13,myIter,myThid)
      CALL WRITE_REC_XZ_RL(fn,prec,Nr,SN_STORE_2,14,myIter,myThid)
      CALL WRITE_REC_XZ_RL(fn,prec,Nr,SN_STORE_3,15,myIter,myThid)
      CALL WRITE_REC_XZ_RL(fn,prec,Nr,SN_STORE_4,16,myIter,myThid)
      CALL WRITE_REC_XZ_RL(fn,prec,Nr, CVEL_UN , 17,myIter,myThid)
      CALL WRITE_REC_XZ_RL(fn,prec,Nr, CVEL_VN , 18,myIter,myThid)
      CALL WRITE_REC_XZ_RL(fn,prec,Nr, CVEL_TN , 19,myIter,myThid)
      CALL WRITE_REC_XZ_RL(fn,prec,Nr, CVEL_SN , 20,myIter,myThid)
#ifdef ALLOW_NONHYDROSTATIC
      CALL WRITE_REC_XZ_RL(fn,prec,Nr,WN_STORE_1,21,myIter,myThid)
      CALL WRITE_REC_XZ_RL(fn,prec,Nr,WN_STORE_2,22,myIter,myThid)
      CALL WRITE_REC_XZ_RL(fn,prec,Nr,WN_STORE_3,23,myIter,myThid)
      CALL WRITE_REC_XZ_RL(fn,prec,Nr,WN_STORE_4,24,myIter,myThid)
      CALL WRITE_REC_XZ_RL(fn,prec,Nr, CVEL_WN , 25,myIter,myThid)
#endif
#endif /* ALLOW_OBCS_NORTH */

#ifdef ALLOW_OBCS_SOUTH
      WRITE(fn,'(2A)') 'pickup_orlanskiS.',suff(1:iL)
      CALL WRITE_REC_XZ_RL(fn,prec,Nr,US_STORE_1, 1,myIter,myThid)
      CALL WRITE_REC_XZ_RL(fn,prec,Nr,US_STORE_2, 2,myIter,myThid)
      CALL WRITE_REC_XZ_RL(fn,prec,Nr,US_STORE_3, 3,myIter,myThid)
      CALL WRITE_REC_XZ_RL(fn,prec,Nr,US_STORE_4, 4,myIter,myThid)
      CALL WRITE_REC_XZ_RL(fn,prec,Nr,VS_STORE_1, 5,myIter,myThid)
      CALL WRITE_REC_XZ_RL(fn,prec,Nr,VS_STORE_2, 6,myIter,myThid)
      CALL WRITE_REC_XZ_RL(fn,prec,Nr,VS_STORE_3, 7,myIter,myThid)
      CALL WRITE_REC_XZ_RL(fn,prec,Nr,VS_STORE_4, 8,myIter,myThid)
      CALL WRITE_REC_XZ_RL(fn,prec,Nr,TS_STORE_1, 9,myIter,myThid)
      CALL WRITE_REC_XZ_RL(fn,prec,Nr,TS_STORE_2,10,myIter,myThid)
      CALL WRITE_REC_XZ_RL(fn,prec,Nr,TS_STORE_3,11,myIter,myThid)
      CALL WRITE_REC_XZ_RL(fn,prec,Nr,TS_STORE_4,12,myIter,myThid)
      CALL WRITE_REC_XZ_RL(fn,prec,Nr,SS_STORE_1,13,myIter,myThid)
      CALL WRITE_REC_XZ_RL(fn,prec,Nr,SS_STORE_2,14,myIter,myThid)
      CALL WRITE_REC_XZ_RL(fn,prec,Nr,SS_STORE_3,15,myIter,myThid)
      CALL WRITE_REC_XZ_RL(fn,prec,Nr,SS_STORE_4,16,myIter,myThid)
      CALL WRITE_REC_XZ_RL(fn,prec,Nr, CVEL_US , 17,myIter,myThid)
      CALL WRITE_REC_XZ_RL(fn,prec,Nr, CVEL_VS , 18,myIter,myThid)
      CALL WRITE_REC_XZ_RL(fn,prec,Nr, CVEL_TS , 19,myIter,myThid)
      CALL WRITE_REC_XZ_RL(fn,prec,Nr, CVEL_SS , 20,myIter,myThid)
#ifdef ALLOW_NONHYDROSTATIC
      CALL WRITE_REC_XZ_RL(fn,prec,Nr,WS_STORE_1,21,myIter,myThid)
      CALL WRITE_REC_XZ_RL(fn,prec,Nr,WS_STORE_2,22,myIter,myThid)
      CALL WRITE_REC_XZ_RL(fn,prec,Nr,WS_STORE_3,23,myIter,myThid)
      CALL WRITE_REC_XZ_RL(fn,prec,Nr,WS_STORE_4,24,myIter,myThid)
      CALL WRITE_REC_XZ_RL(fn,prec,Nr, CVEL_WS , 25,myIter,myThid)
#endif
#endif /* ALLOW_OBCS_SOUTH */

C     Write E and W OB storage and phase speed arrays
#ifdef ALLOW_OBCS_EAST
      WRITE(fn,'(2A)') 'pickup_orlanskiE.',suff(1:iL)
      CALL WRITE_REC_YZ_RL(fn,prec,Nr,UE_STORE_1, 1,myIter,myThid)
      CALL WRITE_REC_YZ_RL(fn,prec,Nr,UE_STORE_2, 2,myIter,myThid)
      CALL WRITE_REC_YZ_RL(fn,prec,Nr,UE_STORE_3, 3,myIter,myThid)
      CALL WRITE_REC_YZ_RL(fn,prec,Nr,UE_STORE_4, 4,myIter,myThid)
      CALL WRITE_REC_YZ_RL(fn,prec,Nr,VE_STORE_1, 5,myIter,myThid)
      CALL WRITE_REC_YZ_RL(fn,prec,Nr,VE_STORE_2, 6,myIter,myThid)
      CALL WRITE_REC_YZ_RL(fn,prec,Nr,VE_STORE_3, 7,myIter,myThid)
      CALL WRITE_REC_YZ_RL(fn,prec,Nr,VE_STORE_4, 8,myIter,myThid)
      CALL WRITE_REC_YZ_RL(fn,prec,Nr,TE_STORE_1, 9,myIter,myThid)
      CALL WRITE_REC_YZ_RL(fn,prec,Nr,TE_STORE_2,10,myIter,myThid)
      CALL WRITE_REC_YZ_RL(fn,prec,Nr,TE_STORE_3,11,myIter,myThid)
      CALL WRITE_REC_YZ_RL(fn,prec,Nr,TE_STORE_4,12,myIter,myThid)
      CALL WRITE_REC_YZ_RL(fn,prec,Nr,SE_STORE_1,13,myIter,myThid)
      CALL WRITE_REC_YZ_RL(fn,prec,Nr,SE_STORE_2,14,myIter,myThid)
      CALL WRITE_REC_YZ_RL(fn,prec,Nr,SE_STORE_3,15,myIter,myThid)
      CALL WRITE_REC_YZ_RL(fn,prec,Nr,SE_STORE_4,16,myIter,myThid)
      CALL WRITE_REC_YZ_RL(fn,prec,Nr, CVEL_UE , 17,myIter,myThid)
      CALL WRITE_REC_YZ_RL(fn,prec,Nr, CVEL_VE , 18,myIter,myThid)
      CALL WRITE_REC_YZ_RL(fn,prec,Nr, CVEL_TE , 19,myIter,myThid)
      CALL WRITE_REC_YZ_RL(fn,prec,Nr, CVEL_SE , 20,myIter,myThid)
#ifdef ALLOW_NONHYDROSTATIC
      CALL WRITE_REC_YZ_RL(fn,prec,Nr,WE_STORE_1,21,myIter,myThid)
      CALL WRITE_REC_YZ_RL(fn,prec,Nr,WE_STORE_2,22,myIter,myThid)
      CALL WRITE_REC_YZ_RL(fn,prec,Nr,WE_STORE_3,23,myIter,myThid)
      CALL WRITE_REC_YZ_RL(fn,prec,Nr,WE_STORE_4,24,myIter,myThid)
      CALL WRITE_REC_YZ_RL(fn,prec,Nr, CVEL_WE , 25,myIter,myThid)
#endif
#endif /* ALLOW_OBCS_EAST */

#ifdef ALLOW_OBCS_WEST
      WRITE(fn,'(2A)') 'pickup_orlanskiW.',suff(1:iL)
      CALL WRITE_REC_YZ_RL(fn,prec,Nr,UW_STORE_1, 1,myIter,myThid)
      CALL WRITE_REC_YZ_RL(fn,prec,Nr,UW_STORE_2, 2,myIter,myThid)
      CALL WRITE_REC_YZ_RL(fn,prec,Nr,UW_STORE_3, 3,myIter,myThid)
      CALL WRITE_REC_YZ_RL(fn,prec,Nr,UW_STORE_4, 4,myIter,myThid)
      CALL WRITE_REC_YZ_RL(fn,prec,Nr,VW_STORE_1, 5,myIter,myThid)
      CALL WRITE_REC_YZ_RL(fn,prec,Nr,VW_STORE_2, 6,myIter,myThid)
      CALL WRITE_REC_YZ_RL(fn,prec,Nr,VW_STORE_3, 7,myIter,myThid)
      CALL WRITE_REC_YZ_RL(fn,prec,Nr,VW_STORE_4, 8,myIter,myThid)
      CALL WRITE_REC_YZ_RL(fn,prec,Nr,TW_STORE_1, 9,myIter,myThid)
      CALL WRITE_REC_YZ_RL(fn,prec,Nr,TW_STORE_2,10,myIter,myThid)
      CALL WRITE_REC_YZ_RL(fn,prec,Nr,TW_STORE_3,11,myIter,myThid)
      CALL WRITE_REC_YZ_RL(fn,prec,Nr,TW_STORE_4,12,myIter,myThid)
      CALL WRITE_REC_YZ_RL(fn,prec,Nr,SW_STORE_1,13,myIter,myThid)
      CALL WRITE_REC_YZ_RL(fn,prec,Nr,SW_STORE_2,14,myIter,myThid)
      CALL WRITE_REC_YZ_RL(fn,prec,Nr,SW_STORE_3,15,myIter,myThid)
      CALL WRITE_REC_YZ_RL(fn,prec,Nr,SW_STORE_4,16,myIter,myThid)
      CALL WRITE_REC_YZ_RL(fn,prec,Nr, CVEL_UW , 17,myIter,myThid)
      CALL WRITE_REC_YZ_RL(fn,prec,Nr, CVEL_VW , 18,myIter,myThid)
      CALL WRITE_REC_YZ_RL(fn,prec,Nr, CVEL_TW , 19,myIter,myThid)
      CALL WRITE_REC_YZ_RL(fn,prec,Nr, CVEL_SW , 20,myIter,myThid)
#ifdef ALLOW_NONHYDROSTATIC
      CALL WRITE_REC_YZ_RL(fn,prec,Nr,WW_STORE_1,21,myIter,myThid)
      CALL WRITE_REC_YZ_RL(fn,prec,Nr,WW_STORE_2,22,myIter,myThid)
      CALL WRITE_REC_YZ_RL(fn,prec,Nr,WW_STORE_3,23,myIter,myThid)
      CALL WRITE_REC_YZ_RL(fn,prec,Nr,WW_STORE_4,24,myIter,myThid)
      CALL WRITE_REC_YZ_RL(fn,prec,Nr, CVEL_WW , 25,myIter,myThid)
#endif
#endif /* ALLOW_OBCS_WEST */

#endif /* ALLOW_ORLANSKI */
      RETURN
      END
