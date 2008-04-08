C $Header: /u/gcmpack/MITgcm/pkg/cfc/CFC.h,v 1.3 2008/04/08 20:23:05 dfer Exp $
C $Name:  $

C     /==========================================================\
C     | CFC.h                                                |
C     |==========================================================|

       COMMON /CFC_NEEDS/
     &              AtmosCFC11, AtmosCFC12, AtmosP,
     &              pisvel,fice
      _RL  AtmosCFC11(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  AtmosCFC12(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  Atmosp(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  pisvel(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  fice(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

       COMMON /CFC11_CHEM/
     &              A1_11,A2_11,A3_11,A4_11,
     &              B1_11,B2_11,B3_11,
     &              A1_12,A2_12,A3_12,A4_12,
     &              B1_12,B2_12,B3_12
      _RL A1_11,A2_11,A3_11,A4_11
      _RL B1_11,B2_11,B3_11
      _RL A1_12,A2_12,A3_12,A4_12
      _RL B1_12,B2_12,B3_12


C schmidt number coefficients
      COMMON /SCHMIDT_NO/
     &                    sca_11_1, sca_11_2, sca_11_3, sca_11_4,
     &                    sca_12_1, sca_12_2, sca_12_3, sca_12_4
      _RL sca_11_1, sca_11_2, sca_11_3, sca_11_4
      _RL sca_12_1, sca_12_2, sca_12_3, sca_12_4

c atmospheric CFC timseries
      COMMON /CFC_atmosdata/
     &                  ACFC11, ACFC12,
     &                  cfc_yearbeg, cfc_yearend
      _RL ACFC11(100,2)
      _RL ACFC12(100,2)
      INTEGER cfc_yearbeg, cfc_yearend

      COMMON /cfc_load/
     &    wind0, wind1, ice0, ice1, atmosp0,
     &    atmosp1
      _RS wind0 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS wind1 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS wind    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS ice0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS ice1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS atmosp0 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS atmosp1 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

C--   COMMON /DIC_FILENAMES/
C  WindFileCfc    :: file name of wind speeds
C  AtmospFileCfc  :: file name of atmospheric pressure
C  IceFileCfc     :: file name of seaice fraction
C  IronFileCfc    :: file name of aeolian iron flux
C  SilicaFileCfc  :: file name of surface silica
C  cfc_ForcingPeriod :: periodic forcing parameter specific for cfc (seconds)
C  cfc_ForcingCycle  :: periodic forcing parameter specific for cfc (seconds)
C  dic_int*          :: place holder to read in a integer number, set at run time

      COMMON /DIC_FILENAMES/
     &        WindFileCfc, AtmospFileCfc, IceFileCfc,
     &        IronFileCfc, SilicaFileCfc,
     &        cfc_ForcingPeriod, cfc_ForcingCycle,
     &        dic_int1, dic_int2, dic_int3, dic_int4

      CHARACTER*(MAX_LEN_FNAM) WindFileCfc
      CHARACTER*(MAX_LEN_FNAM) AtmospFileCfc
      CHARACTER*(MAX_LEN_FNAM) IceFileCfc
      CHARACTER*(MAX_LEN_FNAM) IronFileCfc
      CHARACTER*(MAX_LEN_FNAM) SilicaFileCfc
      _RL     cfc_ForcingPeriod
      _RL     cfc_ForcingCycle
      INTEGER dic_int1
      INTEGER dic_int2
      INTEGER dic_int3
      INTEGER dic_int4

