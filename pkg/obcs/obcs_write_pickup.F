C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_write_pickup.F,v 1.6 2011/12/02 15:27:02 mlosch Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: OBCS_WRITE_PICKUP

C     !INTERFACE:
      SUBROUTINE OBCS_WRITE_PICKUP(
     &                          suff, myTime, myIter, myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE OBCS_WRITE_PICKUP
C     | o Write open boundary pickup file
C     *==========================================================*
C     |   Note: only Orlanski OBC needs pickup files to restart
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "RESTART.h"
#include "OBCS_PARAMS.h"
#include "OBCS_FIELDS.h"
#include "ORLANSKI.h"

C     !INPUT/OUTPUT PARAMETERS:
      CHARACTER*(*) suff
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#if (defined ALLOW_ORLANSKI) || (defined ALLOW_OBCS_STEVENS)

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      CHARACTER*(MAX_LEN_FNAM) fn
      INTEGER prec
      INTEGER iL

      iL = ILNBLNK(suff)
      iL = MAX(1,iL)
      prec = precFloat64

C--   Unlike horizontal 2D or 3D IO routines, no thread synchronisation is
C     done within  Vertical Section IO routines. Since only thread one is
C     doing IO, we need a barrier here to wait for every one to finish.
      _BARRIER

#ifdef ALLOW_ORLANSKI
C     Write N and S OB storage and phase speed arrays
#ifdef ALLOW_OBCS_NORTH
      IF ( useOrlanskiNorth ) THEN
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
       IF ( nonHydrostatic ) THEN
        CALL WRITE_REC_XZ_RL(fn,prec,Nr,WN_STORE_1,21,myIter,myThid)
        CALL WRITE_REC_XZ_RL(fn,prec,Nr,WN_STORE_2,22,myIter,myThid)
        CALL WRITE_REC_XZ_RL(fn,prec,Nr,WN_STORE_3,23,myIter,myThid)
        CALL WRITE_REC_XZ_RL(fn,prec,Nr,WN_STORE_4,24,myIter,myThid)
        CALL WRITE_REC_XZ_RL(fn,prec,Nr, CVEL_WN , 25,myIter,myThid)
        CALL WRITE_REC_XZ_RL(fn,prec,Nr, OBNw    , 26,myIter,myThid)
       ENDIF
#endif
      ENDIF
#endif /* ALLOW_OBCS_NORTH */

#ifdef ALLOW_OBCS_SOUTH
      IF ( useOrlanskiSouth ) THEN
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
       IF ( nonHydrostatic ) THEN
        CALL WRITE_REC_XZ_RL(fn,prec,Nr,WS_STORE_1,21,myIter,myThid)
        CALL WRITE_REC_XZ_RL(fn,prec,Nr,WS_STORE_2,22,myIter,myThid)
        CALL WRITE_REC_XZ_RL(fn,prec,Nr,WS_STORE_3,23,myIter,myThid)
        CALL WRITE_REC_XZ_RL(fn,prec,Nr,WS_STORE_4,24,myIter,myThid)
        CALL WRITE_REC_XZ_RL(fn,prec,Nr, CVEL_WS , 25,myIter,myThid)
        CALL WRITE_REC_XZ_RL(fn,prec,Nr, OBSw    , 26,myIter,myThid)
       ENDIF
#endif
      ENDIF
#endif /* ALLOW_OBCS_SOUTH */

C     Write E and W OB storage and phase speed arrays
#ifdef ALLOW_OBCS_EAST
      IF ( useOrlanskiEast ) THEN
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
       IF ( nonHydrostatic ) THEN
        CALL WRITE_REC_YZ_RL(fn,prec,Nr,WE_STORE_1,21,myIter,myThid)
        CALL WRITE_REC_YZ_RL(fn,prec,Nr,WE_STORE_2,22,myIter,myThid)
        CALL WRITE_REC_YZ_RL(fn,prec,Nr,WE_STORE_3,23,myIter,myThid)
        CALL WRITE_REC_YZ_RL(fn,prec,Nr,WE_STORE_4,24,myIter,myThid)
        CALL WRITE_REC_YZ_RL(fn,prec,Nr, CVEL_WE , 25,myIter,myThid)
        CALL WRITE_REC_YZ_RL(fn,prec,Nr, OBEw    , 26,myIter,myThid)
       ENDIF
#endif
      ENDIF
#endif /* ALLOW_OBCS_EAST */

#ifdef ALLOW_OBCS_WEST
      IF ( useOrlanskiWest ) THEN
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
       IF ( nonHydrostatic ) THEN
        CALL WRITE_REC_YZ_RL(fn,prec,Nr,WW_STORE_1,21,myIter,myThid)
        CALL WRITE_REC_YZ_RL(fn,prec,Nr,WW_STORE_2,22,myIter,myThid)
        CALL WRITE_REC_YZ_RL(fn,prec,Nr,WW_STORE_3,23,myIter,myThid)
        CALL WRITE_REC_YZ_RL(fn,prec,Nr,WW_STORE_4,24,myIter,myThid)
        CALL WRITE_REC_YZ_RL(fn,prec,Nr, CVEL_WW , 25,myIter,myThid)
        CALL WRITE_REC_YZ_RL(fn,prec,Nr, OBWw    , 26,myIter,myThid)
       ENDIF
#endif
      ENDIF
#endif /* ALLOW_OBCS_WEST */
#endif /* ALLOW_ORLANSKI */

#ifdef ALLOW_OBCS_STEVENS
#ifdef ALLOW_OBCS_NORTH
      IF ( useStevensNorth ) THEN
        WRITE(fn,'(2A)') 'pickup_stevensN.',suff(1:iL)
        CALL WRITE_REC_XZ_RL(fn,prec,Nr,OBNvStevens, 1,myIter,myThid)
        CALL WRITE_REC_XZ_RL(fn,prec,Nr,OBNtStevens, 2,myIter,myThid)
        CALL WRITE_REC_XZ_RL(fn,prec,Nr,OBNsStevens, 3,myIter,myThid)
      ENDIF
#endif /* ALLOW_OBCS_NORTH */

#ifdef ALLOW_OBCS_SOUTH
      IF ( useStevensSouth ) THEN
        WRITE(fn,'(2A)') 'pickup_stevensS.',suff(1:iL)
        CALL WRITE_REC_XZ_RL(fn,prec,Nr,OBSvStevens, 1,myIter,myThid)
        CALL WRITE_REC_XZ_RL(fn,prec,Nr,OBStStevens, 2,myIter,myThid)
        CALL WRITE_REC_XZ_RL(fn,prec,Nr,OBSsStevens, 3,myIter,myThid)
      ENDIF
#endif /* ALLOW_OBCS_SOUTH */

#ifdef ALLOW_OBCS_EAST
      IF ( useStevensEast ) THEN
        WRITE(fn,'(2A)') 'pickup_stevensE.',suff(1:iL)
        CALL WRITE_REC_YZ_RL(fn,prec,Nr,OBEuStevens, 1,myIter,myThid)
        CALL WRITE_REC_YZ_RL(fn,prec,Nr,OBEtStevens, 2,myIter,myThid)
        CALL WRITE_REC_YZ_RL(fn,prec,Nr,OBEsStevens, 3,myIter,myThid)
      ENDIF
#endif /* ALLOW_OBCS_EAST */

#ifdef ALLOW_OBCS_WEST
      IF ( useStevensWest ) THEN
        WRITE(fn,'(2A)') 'pickup_stevensW.',suff(1:iL)
        CALL WRITE_REC_YZ_RL(fn,prec,Nr,OBWuStevens, 1,myIter,myThid)
        CALL WRITE_REC_YZ_RL(fn,prec,Nr,OBWtStevens, 2,myIter,myThid)
        CALL WRITE_REC_YZ_RL(fn,prec,Nr,OBWsStevens, 3,myIter,myThid)
      ENDIF
#endif /* ALLOW_OBCS_WEST */
#endif /* ALLOW_OBCS_STEVENS */

C--   Every one wait for thread 1 to finish (see comment @ the top)
      _BARRIER

#endif /* ALLOW_ORLANSKI or ALLOW_OBCS_STEVENS */
      RETURN
      END
