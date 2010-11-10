C $Header: /u/gcmpack/MITgcm/pkg/rbcs/Attic/RBCS.h,v 1.9 2010/11/10 00:34:21 jahn Exp $
C $Name:  $

#ifdef ALLOW_RBCS

CBOP
C    !ROUTINE: RBCS.h
C    !INTERFACE:

C    !DESCRIPTION:
C Contains tracer parameters and input files for 3-D relaxation
C Some of these can be read in from data.rbcs

c number of mask to read
      INTEGER maskLEN
      PARAMETER(maskLEN = 3 )

C     rbcsForcingPeriod :: period of rbc data (in seconds)
C     rbcsForcingCycle  :: cycle of rbc data (in seconds)
C     rbcsForcingOffset :: model time at beginning of first rbc period
C     rbcsSingleTimeFiles :: if .TRUE., rbc fields are given 1 file per time
C                         :: labeled by iteration number (see rbcsIter0,deltaTrbcs)
C     deltaTrbcs :: time step used to compute iteration numbers for singleTimeFiles
C     rbcsIter0  :: singleTimeFile iteration number corresponding to rbcsForcingOffset
C
      _RS RBC_mask(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,maskLEN)
      _RL RBCtemp(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL RBCsalt(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL tauRelaxT
      _RL tauRelaxS
      _RL rbcsForcingPeriod
      _RL rbcsForcingCycle
      _RL rbcsForcingOffset
      _RL deltaTrbcs
      INTEGER rbcsIniter
      INTEGER rbcsIter0
      LOGICAL rbcsSingleTimeFiles
      LOGICAL useRBCtemp
      LOGICAL useRBCsalt
      CHARACTER*(MAX_LEN_FNAM) relaxMaskFile(maskLEN)
      CHARACTER*(MAX_LEN_FNAM) relaxTFile
      CHARACTER*(MAX_LEN_FNAM) relaxSFile

      COMMON /RBCS_PARM01_RS/
     &          RBC_mask
      COMMON /RBCS_PARM01_R/
     &          RBCtemp,
     &          RBCsalt,
     &          tauRelaxT,
     &          tauRelaxS,
     &          rbcsForcingPeriod,
     &          rbcsForcingCycle,
     &          rbcsForcingOffset,
     &          deltaTrbcs
      COMMON /RBCS_PARM01_I/
     &          rbcsIniter,
     &          rbcsIter0
      COMMON /RBCS_PARM01_L/
     &          rbcsSingleTimeFiles,
     &          useRBCtemp,
     &          useRBCsalt
      COMMON /RBCS_PARM01_C/
     &          relaxMaskFile,
     &          relaxTFile,
     &          relaxSFile

#ifdef ALLOW_PTRACERS
      LOGICAL useRBCptrnum(PTRACERS_num)
      _RL RBC_ptracers(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,
     &                PTRACERS_num)
      _RL tauRelaxPTR(PTRACERS_num)
      CHARACTER*(MAX_LEN_FNAM) relaxPtracerFile(PTRACERS_num)

      COMMON /RBCS_PARM02_L/
     &          useRBCptrnum
      COMMON /RBCS_PARM02_R/
     &          RBC_ptracers,
     &          tauRelaxPTR
      COMMON /RBCS_PARM02_C/
     &          relaxPtracerFile
#endif /* ALLOW_PTRACERS */

      COMMON /RBCFFIELDS/
     &                 rbct0, rbcs0,
     &                 rbct1, rbcs1
      _RS  rbct0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  rbct1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  rbcs0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  rbcs1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

#ifdef ALLOW_PTRACERS
       COMMON /RBCFFTRACER/
     &               rbcptr0, rbcptr1
       _RS rbcptr0(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,
     &              PTRACERS_num)
       _RS rbcptr1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,
     &              PTRACERS_num)
#endif /* ALLOW_PTRACERS */

#endif /* ALLOW_RBCS */
