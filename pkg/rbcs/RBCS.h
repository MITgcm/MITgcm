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
 
      _RL RBC_mask(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,maskLEN)
      _RL RBCtemp(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL RBCsalt(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL tauRelaxT
      _RL tauRelaxS
      _RL rbcsForcingPeriod
      _RL rbcsForcingCycle
      INTEGER rbcsIniter
      LOGICAL useRBCtemp
      LOGICAL useRBCsalt
      LOGICAL useRBCptracers
      CHARACTER*(MAX_LEN_FNAM) relaxMaskFile(maskLEN)
      CHARACTER*(MAX_LEN_FNAM) relaxTFile
      CHARACTER*(MAX_LEN_FNAM) relaxSFile
#ifdef ALLOW_PTRACERS
      LOGICAL useRBCptrnum(PTRACERS_num)
      _RL RBC_ptracers(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,
     &              PTRACERS_num)
      _RL tauRelaxPTR(PTRACERS_num)
      CHARACTER*(MAX_LEN_FNAM) relaxPtracerFile(PTRACERS_num)
#endif

      COMMON /RBCS_PARM01_R/
     &          tauRelaxT,
     &          tauRelaxS,
     &          rbcsForcingPeriod,
     &          rbcsForcingCycle,
     &          RBC_mask,
     &          RBCtemp,
     &          RBCsalt
      COMMON /RBCS_PARM01_I/
     &          rbcsIniter
      COMMON /RBCS_PARM01_L/
     &          useRBCtemp,
     &          useRBCsalt,
     &          useRBCptracers
      COMMON /RBCS_PARM01_C/
     &          relaxMaskFile,
     &          relaxTFile,
     &          relaxSFile

#ifdef ALLOW_PTRACERS
      COMMON /RBCS_PARM02/
     &          useRBCptrnum,
     &          tauRelaxPTR,
     &          relaxPtracerFile,
     &          RBC_ptracers
#endif

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
#endif

#endif
