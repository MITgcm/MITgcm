#ifdef ALLOW_RBCS

CBOP
C    !ROUTINE: RBCS.h
C    !INTERFACE:

C    !DESCRIPTION:
C Contains tracer parameters and input files for relaxed boundary
C conditions
C Some of these can be read in from data.rbcs
 
      _RL RBC_mask(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL RBCtemp(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL RBCsalt(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  tauRelax
      _RL rbcsForcingPeriod
      _RL rbcsForcingCycle
      INTEGER rbcsIniter
      LOGICAL useRBCtemp
      LOGICAL useRBCsalt
      LOGICAL useRBCptracers
      CHARACTER*(MAX_LEN_FNAM) relaxBoundaryFile
      CHARACTER*(MAX_LEN_FNAM) relaxTFile
      CHARACTER*(MAX_LEN_FNAM) relaxSFile
#ifdef ALLOW_PTRACERS
      LOGICAL useRBCptrnum(PTRACERS_num)
      _RL RBC_ptracers(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,
     &              PTRACERS_num)
      CHARACTER*(MAX_LEN_FNAM) relaxPtracerFile(PTRACERS_num)
#endif

      

      COMMON /RBCS_PARM01/
     &          tauRelax,
     &          relaxBoundaryFile,
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
     &          relaxPtracerFile,
     &          RBC_ptracers
#endif

#endif
