#ifdef ALLOW_SHELFICE

CBOP
C !ROUTINE: SHELFICE_COST.h

C !DESCRIPTION: \bv
C     /==========================================================\
C     | SHELFICE_COST.h                                          |
C     | o shelfice cost terms                                    |
C     \==========================================================/

C \ev
CEOP

#ifdef ALLOW_COST
      COMMON /SHELFICE_COST_FIELDS_RL/ 
     &     cMeanSHIforT,
     &     cMeanSHIforS,
     &     objf_shelfice,
     &     objf_shifwflx,
     &     num_shifwflx
      _RL cMeanSHIforT (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL cMeanSHIforS (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL objf_shelfice(nSx,nSy)
      _RL objf_shifwflx(nSx,nSy)
      _RL  num_shifwflx(nSx,nSy)

      COMMON /SHELFICE_COST_C/ shifwflx_errfile
      CHARACTER*(MAX_LEN_FNAM) shifwflx_errfile

      COMMON /SHELFICE_COST_WEIGHTS/ wshifwflx
      _RL wshifwflx(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      COMMON /SHELFICE_COST_SCALAR_RL/
     &     mult_shelfice,
     &     mult_shifwflx,
     &     wshifwflx0
      _RL mult_shelfice
      _RL mult_shifwflx
      _RL wshifwflx0
#endif

#endif /* ALLOW_SHELFICE */
