#ifdef ALLOW_AUTODIFF_TAMC
C     *==========================================================*
C     | tamc_keys.h keys required by TAMC for record computation |
C     *==========================================================*
      integer           key, ikey
      common /tamckeys/ key, ikey
#ifdef ALLOW_CG2D_NSA
      integer           icg2dkey
      common /tamckeys_cg2d/ icg2dkey
#endif /* ALLOW_CG2D_NSA */
#endif /* ALLOW_AUTODIFF_TAMC */
