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

      

      COMMON /RBCS_PARM01/
     &          tauRelaxT,
     &          tauRelaxS,
     &          relaxMaskFile,
     &          relaxTFile,
     &          relaxSFile,
     &          rbcsIniter,
     &          rbcsForcingPeriod,
     &          rbcsForcingCycle,
     &          RBC_mask,
     &          RBCtemp,
     &          RBCsalt,
     &          useRBCtemp,
     &          useRBCsalt,
     &          useRBCptracers

#ifdef ALLOW_PTRACERS
      COMMON /RBCS_PARM02/
     &          useRBCptrnum,
     &          tauRelaxPTR,
     &          relaxPtracerFile,
     &          RBC_ptracers
#endif

#endif
