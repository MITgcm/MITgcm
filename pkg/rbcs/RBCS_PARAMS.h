C $Header: /u/gcmpack/MITgcm/pkg/rbcs/RBCS_PARAMS.h,v 1.2 2012/06/26 22:19:17 gforget Exp $
C $Name:  $

#ifdef ALLOW_RBCS

CBOP
C    !ROUTINE: RBCS_PARAMS.h
C    !INTERFACE:

C    !DESCRIPTION:
C Contains RBCS parameters and input files for 3-D relaxation
C Some of these can be read in from data.rbcs
CEOP

C---  RBCS Parameters:
C     rbcsForcingPeriod :: period of rbc data (in seconds)
C     rbcsForcingCycle  :: cycle of rbc data (in seconds)
C     rbcsForcingOffset :: model time at beginning of first rbc period
C     rbcsSingleTimeFiles :: if .TRUE., rbc fields are given 1 file per time
C                         :: labeled by iteration number (see rbcsIter0,deltaTrbcs)
C     deltaTrbcs :: time step used to compute iteration numbers for singleTimeFiles
C     rbcsIter0  :: singleTimeFile iteration number corresponding to rbcsForcingOffset
C     rbcsVanishingTime :: when rbcsVanishingTime .NE. 0. the relaxation strength reduces
C                       :: linearly to vanish at myTime .EQ. rbcsVanishingTime
C
      _RL tauRelaxU
      _RL tauRelaxV
      _RL tauRelaxT
      _RL tauRelaxS
      _RL rbcsForcingPeriod
      _RL rbcsForcingCycle
      _RL rbcsForcingOffset
      _RL deltaTrbcs
      _RL rbcsVanishingTime
      INTEGER rbcsIniter
      INTEGER rbcsIter0
      LOGICAL rbcsSingleTimeFiles
      LOGICAL useRBCuVel
      LOGICAL useRBCvVel
      LOGICAL useRBCtemp
      LOGICAL useRBCsalt
      CHARACTER*(MAX_LEN_FNAM) relaxMaskUFile
      CHARACTER*(MAX_LEN_FNAM) relaxMaskVFile
      CHARACTER*(MAX_LEN_FNAM) relaxMaskFile(maskLEN)
      CHARACTER*(MAX_LEN_FNAM) relaxUFile
      CHARACTER*(MAX_LEN_FNAM) relaxVFile
      CHARACTER*(MAX_LEN_FNAM) relaxTFile
      CHARACTER*(MAX_LEN_FNAM) relaxSFile

      COMMON /RBCS_PARM01_R/
     &          tauRelaxU,
     &          tauRelaxV,
     &          tauRelaxT,
     &          tauRelaxS,
     &          rbcsForcingPeriod,
     &          rbcsForcingCycle,
     &          rbcsForcingOffset,
     &          rbcsVanishingTime,
     &          deltaTrbcs
      COMMON /RBCS_PARM01_I/
     &          rbcsIniter,
     &          rbcsIter0
      COMMON /RBCS_PARM01_L/
     &          rbcsSingleTimeFiles,
     &          useRBCuVel,
     &          useRBCvVel,
     &          useRBCtemp,
     &          useRBCsalt
      COMMON /RBCS_PARM01_C/
     &          relaxMaskUFile,
     &          relaxMaskVFile,
     &          relaxMaskFile,
     &          relaxUFile,
     &          relaxVFile,
     &          relaxTFile,
     &          relaxSFile

#ifdef ALLOW_PTRACERS
      LOGICAL useRBCptrnum(PTRACERS_num)
      _RL tauRelaxPTR(PTRACERS_num)
      CHARACTER*(MAX_LEN_FNAM) relaxPtracerFile(PTRACERS_num)

      COMMON /RBCS_PARM02_L/
     &          useRBCptrnum
      COMMON /RBCS_PARM02_R/
     &          tauRelaxPTR
      COMMON /RBCS_PARM02_C/
     &          relaxPtracerFile
#endif /* ALLOW_PTRACERS */

#endif /* ALLOW_RBCS */
