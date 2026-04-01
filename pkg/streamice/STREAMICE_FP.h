C---+----1--+-+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C STREAMICE_FP.h
#ifdef ALLOW_STREAMICE

#ifdef ALLOW_STREAMICE_FP_ADJ
#ifdef ALLOW_PETSC
      LOGICAL STREAMICE_need2createmat
      LOGICAL STREAMICE_need2destroymat
      LOGICAL STREAMICE_OAD_petsc_reuse
      CHARACTER*(MAX_LEN_FNAM) PETSC_PRECOND_TMP
      CHARACTER*(MAX_LEN_FNAM) PETSC_PRECOND_OAD

      COMMON /STREAMICE_PARM_FP_PETSC_L/
     & STREAMICE_need2createmat,
     & STREAMICE_need2destroymat,
     & STREAMICE_OAD_petsc_reuse

      COMMON /STREAMICE_PARM_FP_PETSC_C/
     & PETSC_PRECOND_TMP,
     & PETSC_PRECOND_OAD
#endif
#endif

      _RL streamice_nonlin_tol_adjoint
      _RL streamice_nonlin_tol_adjoint_rl
#ifdef ALLOW_TAPENADE
      _RL refCumuls(5)
      _RL prevCumuls(5)
      INTEGER adjIters(5)
      INTEGER fpDepth
#endif

      COMMON /STREAMICE_PARM_FP_R/
     & streamice_nonlin_tol_adjoint,
     & streamice_nonlin_tol_adjoint_rl
#ifdef ALLOW_TAPENADE
     & ,refCumuls,
     & prevCumuls
#endif

#ifdef ALLOW_TAPENADE
      COMMON /STREAMICE_PARM_FP_I/
     & adjIters,
     & fpDepth
#endif

#endif
