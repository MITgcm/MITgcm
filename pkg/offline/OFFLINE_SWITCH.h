C $Header: /u/gcmpack/MITgcm/pkg/offline/OFFLINE_SWITCH.h,v 1.1 2013/07/07 22:25:13 jmc Exp $
C $Name:  $

C     !ROUTINE: OFFLINE_SWITCH.h
C -------------------------------
C   OFFLINE_SWITCH.h
C  variable for switching on/off some calculations
C -------------------------------

C     offlineLoadGMRedi :: load from file GMRedi tensor (do not compute it)
C     offlineLoadKPP    :: load from file KPP mixing coeff (do not compute it)
C     offlineLoadConvec :: load from file Convective mixing (do not compute it)
      COMMON /OFFLINE_SWITCH_L/
     &       offlineLoadGMRedi, offlineLoadKPP, offlineLoadConvec
      LOGICAL offlineLoadGMRedi
      LOGICAL offlineLoadKPP
      LOGICAL offlineLoadConvec

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
