#ifdef ALLOW_STEEP_ICECAVITY

CBOP
C !ROUTINE: STIC_COST.h

C !DESCRIPTION: \bv
C     /==========================================================\
C     | STIC_COST.h                                          |
C     | o stic cost terms                                    |
C     \==========================================================/

C \ev
CEOP

#ifdef ALLOW_COST
      COMMON /STIC_COST_FIELDS_RL/ 
     &     cMeanSHIforT,
     &     cMeanSHIforS,
     &     objf_stic 
      _RL cMeanSHIforT (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL cMeanSHIforS (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL objf_stic(nSx,nSy)

      COMMON /STIC_COST_C/ shifwflx_errfile
      CHARACTER*(MAX_LEN_FNAM) shifwflx_errfile

      COMMON /STIC_COST_WEIGHTS/ wshifwflx
      _RL wshifwflx(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      COMMON /STIC_COST_SCALAR_RL/
     &     mult_stic,
     &     mult_shifwflx,
     &     wshifwflx0
      _RL mult_stic
      _RL mult_shifwflx
      _RL wshifwflx0
#endif

#endif /* ALLOW_STEEP_ICECAVITY */
