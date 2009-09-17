C $Header: /u/gcmpack/MITgcm/pkg/obcs/orlanski_init.F,v 1.3 2009/09/17 16:30:07 jmc Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

      SUBROUTINE ORLANSKI_INIT(bi, bj, myThid )
C     /==========================================================\
C     | SUBROUTINE ORLANSKI_INIT                                 |
C     | o Initialise storage and phase speed arrays              |
C     |   for Orlanski radiation condition                       |
C     |==========================================================|
C     |                                                          |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_ORLANSKI
#include "ORLANSKI.h"
#endif /* ALLOW_ORLANSKI */

C     == Routine arguments ==
      INTEGER bi, bj, myThid

#ifdef ALLOW_ORLANSKI

C     == Local variables ==
      INTEGER I, J, K

      DO K=1,Nr
        DO I=1-Olx,sNx+Olx
          UN_STORE_1(I,K,bi,bj)=0.
          VN_STORE_1(I,K,bi,bj)=0.
          TN_STORE_1(I,K,bi,bj)=0.
          SN_STORE_1(I,K,bi,bj)=0.
          UN_STORE_2(I,K,bi,bj)=0.
          VN_STORE_2(I,K,bi,bj)=0.
          TN_STORE_2(I,K,bi,bj)=0.
          SN_STORE_2(I,K,bi,bj)=0.
          UN_STORE_3(I,K,bi,bj)=0.
          VN_STORE_3(I,K,bi,bj)=0.
          TN_STORE_3(I,K,bi,bj)=0.
          SN_STORE_3(I,K,bi,bj)=0.
          UN_STORE_4(I,K,bi,bj)=0.
          VN_STORE_4(I,K,bi,bj)=0.
          TN_STORE_4(I,K,bi,bj)=0.
          SN_STORE_4(I,K,bi,bj)=0.
          US_STORE_1(I,K,bi,bj)=0.
          VS_STORE_1(I,K,bi,bj)=0.
          TS_STORE_1(I,K,bi,bj)=0.
          SS_STORE_1(I,K,bi,bj)=0.
          US_STORE_2(I,K,bi,bj)=0.
          VS_STORE_2(I,K,bi,bj)=0.
          TS_STORE_2(I,K,bi,bj)=0.
          SS_STORE_2(I,K,bi,bj)=0.
          US_STORE_3(I,K,bi,bj)=0.
          VS_STORE_3(I,K,bi,bj)=0.
          TS_STORE_3(I,K,bi,bj)=0.
          SS_STORE_3(I,K,bi,bj)=0.
          US_STORE_4(I,K,bi,bj)=0.
          VS_STORE_4(I,K,bi,bj)=0.
          TS_STORE_4(I,K,bi,bj)=0.
          SS_STORE_4(I,K,bi,bj)=0.
          CVEL_UN(I,K,bi,bj)=0.
          CVEL_VN(I,K,bi,bj)=0.
          CVEL_TN(I,K,bi,bj)=0.
          CVEL_SN(I,K,bi,bj)=0.
          CVEL_US(I,K,bi,bj)=0.
          CVEL_VS(I,K,bi,bj)=0.
          CVEL_TS(I,K,bi,bj)=0.
          CVEL_SS(I,K,bi,bj)=0.
#ifdef ALLOW_NONHYDROSTATIC
          WN_STORE_1(I,K,bi,bj)=0.
          WN_STORE_2(I,K,bi,bj)=0.
          WN_STORE_3(I,K,bi,bj)=0.
          WN_STORE_4(I,K,bi,bj)=0.
          WS_STORE_1(I,K,bi,bj)=0.
          WS_STORE_2(I,K,bi,bj)=0.
          WS_STORE_3(I,K,bi,bj)=0.
          WS_STORE_4(I,K,bi,bj)=0.
          CVEL_WN(I,K,bi,bj)=0.
          CVEL_WS(I,K,bi,bj)=0.
#endif
        ENDDO
        DO J=1-Oly,sNy+Oly
          UE_STORE_1(J,K,bi,bj)=0.
          VE_STORE_1(J,K,bi,bj)=0.
          TE_STORE_1(J,K,bi,bj)=0.
          SE_STORE_1(J,K,bi,bj)=0.
          UE_STORE_2(J,K,bi,bj)=0.
          VE_STORE_2(J,K,bi,bj)=0.
          TE_STORE_2(J,K,bi,bj)=0.
          SE_STORE_2(J,K,bi,bj)=0.
          UE_STORE_3(J,K,bi,bj)=0.
          VE_STORE_3(J,K,bi,bj)=0.
          TE_STORE_3(J,K,bi,bj)=0.
          SE_STORE_3(J,K,bi,bj)=0.
          UE_STORE_4(J,K,bi,bj)=0.
          VE_STORE_4(J,K,bi,bj)=0.
          TE_STORE_4(J,K,bi,bj)=0.
          SE_STORE_4(J,K,bi,bj)=0.
          UW_STORE_1(J,K,bi,bj)=0.
          VW_STORE_1(J,K,bi,bj)=0.
          TW_STORE_1(J,K,bi,bj)=0.
          SW_STORE_1(J,K,bi,bj)=0.
          UW_STORE_2(J,K,bi,bj)=0.
          VW_STORE_2(J,K,bi,bj)=0.
          TW_STORE_2(J,K,bi,bj)=0.
          SW_STORE_2(J,K,bi,bj)=0.
          UW_STORE_3(J,K,bi,bj)=0.
          VW_STORE_3(J,K,bi,bj)=0.
          TW_STORE_3(J,K,bi,bj)=0.
          SW_STORE_3(J,K,bi,bj)=0.
          UW_STORE_4(J,K,bi,bj)=0.
          VW_STORE_4(J,K,bi,bj)=0.
          TW_STORE_4(J,K,bi,bj)=0.
          SW_STORE_4(J,K,bi,bj)=0.
          CVEL_UE(J,K,bi,bj)=0.
          CVEL_VE(J,K,bi,bj)=0.
          CVEL_TE(J,K,bi,bj)=0.
          CVEL_SE(J,K,bi,bj)=0.
          CVEL_UW(J,K,bi,bj)=0.
          CVEL_VW(J,K,bi,bj)=0.
          CVEL_TW(J,K,bi,bj)=0.
          CVEL_SW(J,K,bi,bj)=0.
#ifdef ALLOW_NONHYDROSTATIC
          WE_STORE_1(J,K,bi,bj)=0.
          WE_STORE_2(J,K,bi,bj)=0.
          WE_STORE_3(J,K,bi,bj)=0.
          WE_STORE_4(J,K,bi,bj)=0.
          WW_STORE_1(J,K,bi,bj)=0.
          WW_STORE_2(J,K,bi,bj)=0.
          WW_STORE_3(J,K,bi,bj)=0.
          WW_STORE_4(J,K,bi,bj)=0.
          CVEL_WE(J,K,bi,bj)=0.
          CVEL_WW(J,K,bi,bj)=0.
#endif
        ENDDO
      ENDDO

#endif /* ALLOW_ORLANSKI */
      RETURN
      END
