C $Header: /u/gcmpack/MITgcm/pkg/obcs/ORLANSKI.h,v 1.3 2004/07/06 18:25:52 adcroft Exp $
C $Name:  $

C SPK 6/2/00: Added storage arrays for salinity. Removed some 
C             unneeded arrays.
C             
C SPK 7/18/00: Added dimensional phase speed arrays CVEL_**, 
C              where **=Variable(U,V,T,S,W)Boundary(E,W,N,S). 
C
cc

#ifdef ALLOW_ORLANSKI

      COMMON /ORLANSKI_PARAMS/
     &      CMax, cVelTimeScale, CFix, useFixedCEast, useFixedCWest
      _RL   CMax, cVelTimeScale, CFix
      LOGICAL useFixedCEast, useFixedCWest

C     Storage arrays
      COMMON /ORLANSKI_STORE/ 
     &      UE_STORE_1,UE_STORE_2,
     &      UE_STORE_3,UE_STORE_4,
     &      VE_STORE_1,VE_STORE_2,
     &      VE_STORE_3,VE_STORE_4,
     &      TE_STORE_1,TE_STORE_2,
     &      TE_STORE_3,TE_STORE_4,
     &      SE_STORE_1,SE_STORE_2,
     &      SE_STORE_3,SE_STORE_4,
     &      WE_STORE_1,WE_STORE_2,
     &      WE_STORE_3,WE_STORE_4,
     &      UW_STORE_1,UW_STORE_2,
     &      UW_STORE_3,UW_STORE_4,
     &      VW_STORE_1,VW_STORE_2,
     &      VW_STORE_3,VW_STORE_4,
     &      TW_STORE_1,TW_STORE_2,
     &      TW_STORE_3,TW_STORE_4,
     &      SW_STORE_1,SW_STORE_2,
     &      SW_STORE_3,SW_STORE_4,
     &      WW_STORE_1,WW_STORE_2,
     &      WW_STORE_3,WW_STORE_4,
     &      UN_STORE_1,UN_STORE_2,
     &      UN_STORE_3,UN_STORE_4,
     &      VN_STORE_1,VN_STORE_2,
     &      VN_STORE_3,VN_STORE_4,
     &      TN_STORE_1,TN_STORE_2,
     &      TN_STORE_3,TN_STORE_4,
     &      SN_STORE_1,SN_STORE_2,
     &      SN_STORE_3,SN_STORE_4,
     &      WN_STORE_1,WN_STORE_2,
     &      WN_STORE_3,WN_STORE_4,
     &      US_STORE_1,US_STORE_2,
     &      US_STORE_3,US_STORE_4,
     &      VS_STORE_1,VS_STORE_2,
     &      VS_STORE_3,VS_STORE_4,
     &      TS_STORE_1,TS_STORE_2,
     &      TS_STORE_3,TS_STORE_4,
     &      SS_STORE_1,SS_STORE_2,
     &      SS_STORE_3,SS_STORE_4,
     &      WS_STORE_1,WS_STORE_2,
     &      WS_STORE_3,WS_STORE_4

      _RL UE_STORE_1(1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL UE_STORE_2(1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL UE_STORE_3(1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL UE_STORE_4(1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL VE_STORE_1(1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL VE_STORE_2(1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL VE_STORE_3(1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL VE_STORE_4(1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL TE_STORE_1(1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL TE_STORE_2(1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL TE_STORE_3(1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL TE_STORE_4(1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL SE_STORE_1(1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL SE_STORE_2(1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL SE_STORE_3(1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL SE_STORE_4(1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL WE_STORE_1(1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL WE_STORE_2(1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL WE_STORE_3(1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL WE_STORE_4(1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL UW_STORE_1(1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL UW_STORE_2(1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL UW_STORE_3(1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL UW_STORE_4(1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL VW_STORE_1(1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL VW_STORE_2(1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL VW_STORE_3(1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL VW_STORE_4(1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL TW_STORE_1(1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL TW_STORE_2(1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL TW_STORE_3(1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL TW_STORE_4(1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL SW_STORE_1(1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL SW_STORE_2(1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL SW_STORE_3(1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL SW_STORE_4(1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL WW_STORE_1(1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL WW_STORE_2(1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL WW_STORE_3(1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL WW_STORE_4(1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL UN_STORE_1(1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL UN_STORE_2(1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL UN_STORE_3(1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL UN_STORE_4(1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL VN_STORE_1(1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL VN_STORE_2(1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL VN_STORE_3(1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL VN_STORE_4(1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL TN_STORE_1(1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL TN_STORE_2(1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL TN_STORE_3(1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL TN_STORE_4(1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL SN_STORE_1(1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL SN_STORE_2(1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL SN_STORE_3(1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL SN_STORE_4(1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL WN_STORE_1(1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL WN_STORE_2(1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL WN_STORE_3(1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL WN_STORE_4(1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL US_STORE_1(1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL US_STORE_2(1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL US_STORE_3(1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL US_STORE_4(1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL VS_STORE_1(1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL VS_STORE_2(1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL VS_STORE_3(1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL VS_STORE_4(1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL TS_STORE_1(1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL TS_STORE_2(1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL TS_STORE_3(1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL TS_STORE_4(1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL SS_STORE_1(1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL SS_STORE_2(1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL SS_STORE_3(1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL SS_STORE_4(1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL WS_STORE_1(1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL WS_STORE_2(1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL WS_STORE_3(1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL WS_STORE_4(1-OLx:sNx+OLx,Nr,nSx,nSy)

C     Phase speed arrays
      COMMON /ORLANSKI_CVEL/ 
     &      CVEL_UE,CVEL_VE,CVEL_TE,CVEL_SE,CVEL_WE,
     &      CVEL_UW,CVEL_VW,CVEL_TW,CVEL_SW,CVEL_WW,
     &      CVEL_UN,CVEL_VN,CVEL_TN,CVEL_SN,CVEL_WN,
     &      CVEL_US,CVEL_VS,CVEL_TS,CVEL_SS,CVEL_WS

      _RL CVEL_UE(1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL CVEL_VE(1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL CVEL_TE(1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL CVEL_SE(1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL CVEL_WE(1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL CVEL_UW(1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL CVEL_VW(1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL CVEL_TW(1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL CVEL_SW(1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL CVEL_WW(1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL CVEL_UN(1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL CVEL_VN(1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL CVEL_TN(1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL CVEL_SN(1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL CVEL_WN(1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL CVEL_US(1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL CVEL_VS(1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL CVEL_TS(1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL CVEL_SS(1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RL CVEL_WS(1-OLx:sNx+OLx,Nr,nSx,nSy)

#endif /* ALLOW_ORLANSKI */
