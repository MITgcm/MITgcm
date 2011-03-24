C $Header: /u/gcmpack/MITgcm/pkg/ptracers/ptracers_ad_check_lev2_dir.h,v 1.5 2011/03/24 21:39:55 heimbach Exp $
C $Name:  $

#ifdef ALLOW_PTRACERS
# ifdef AUTODIFF_PTRACERS_SPLIT_FILES
CADJ STORE ptracer(:,:,:,:,:,1:PTRACERS_num)
CADJ &     = tapelev2, key = ilev_2
CADJ STORE gptrnm1(:,:,:,:,:,1:PTRACERS_num)
CADJ &     = tapelev2, key = ilev_2
CADJ STORE gptr(:,:,:,:,:,1:PTRACERS_num)
CADJ &     = tapelev2, key = ilev_2
# else
CADJ STORE ptracer = tapelev2, key = ilev_2
CADJ STORE gptrnm1 = tapelev2, key = ilev_2
CADJ STORE gptr    = tapelev2, key = ilev_2
# endif
#endif /* ALLOW_PTRACERS */
