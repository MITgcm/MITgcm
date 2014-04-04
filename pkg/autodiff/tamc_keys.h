C $Header: /u/gcmpack/MITgcm/pkg/autodiff/tamc_keys.h,v 1.5 2014/04/04 23:06:34 jmc Exp $
C $Name:  $

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
