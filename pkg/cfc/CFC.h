C     *==========================================================*
C     | CFC.h
C     *==========================================================*

       COMMON /CFC_2D_FIELDS/
     &              AtmosP,
     &              pisVel, fIce
      _RL  Atmosp(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  pisVel(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  fIce  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

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
      COMMON /CFC_SCHMIDT_NO/
     &                    sca_11_1, sca_11_2, sca_11_3, sca_11_4,
     &                    sca_12_1, sca_12_2, sca_12_3, sca_12_4
      _RL sca_11_1, sca_11_2, sca_11_3, sca_11_4
      _RL sca_12_1, sca_12_2, sca_12_3, sca_12_4

C--   parameter for CFC forcing and atmospheric CFC time-series interpolation
C--   COMMON /CFC_PARAMS_R/
C     atmCFC_recSepTime :: time spacing between 2 records of atmos CFC [s]
C     atmCFC_timeOffset :: time offset for atmos CFC (cfcTime = myTime + offSet)
C     atmCFC_yNorthBnd  :: Northern Lat boundary for interpolation [y-unit]
C     atmCFC_ySouthBnd  :: Southern Lat boundary for interpolation [y-unit]
C     CFC_monFreq       :: frequency (s) to monitor CFC

      COMMON /CFC_PARAMS_R/
     &    atmCFC_recSepTime, atmCFC_timeOffset,
     &    atmCFC_yNorthBnd, atmCFC_ySouthBnd,
     &    CFC_monFreq
      _RL atmCFC_recSepTime
      _RL atmCFC_timeOffset
      _RL atmCFC_yNorthBnd
      _RL atmCFC_ySouthBnd
      _RL CFC_monFreq

C--   COMMON /CFC_FILENAMES/
C  atmCFC_inpFile  :: file name of Atmospheric CFC time series (ASCII file)

      COMMON /CFC_FILENAMES/
     &        atmCFC_inpFile
      CHARACTER*(MAX_LEN_FNAM) atmCFC_inpFile

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
