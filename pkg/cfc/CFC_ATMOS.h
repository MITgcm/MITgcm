C $Header: /u/gcmpack/MITgcm/pkg/cfc/CFC_ATMOS.h,v 1.1 2013/06/10 02:51:08 jmc Exp $
C $Name:  $

CBOP
C     !ROUTINE: CFC_ATMOS.h
C     !INTERFACE:
C     #include CFC_ATMOS.h

C     !DESCRIPTION:
C     Header file defining Atmospheric CFC time series.
CEOP

C     ACFCrecSize :: maximum length of Atmospheric CFC time series to be loaded
      INTEGER ACFCrecSize
      PARAMETER ( ACFCrecSize = 100 )

C atmospheric CFC timseries
C     ACFCyear :: calendar year of Atmospheric CFC time series
      COMMON /CFC_ATMOS_R/
     &                  ACFCyear, ACFC11, ACFC12
      _RL ACFCyear(ACFCrecSize)
      _RL ACFC11(ACFCrecSize,2)
      _RL ACFC12(ACFCrecSize,2)

C     ACFCnRec :: current number of records in Atmospheric CFC time series
      INTEGER ACFCnRec
      COMMON /CFC_ATMOS_I/ ACFCnRec

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
