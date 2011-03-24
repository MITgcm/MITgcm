C $Header: /u/gcmpack/MITgcm/pkg/ptracers/ptracers_ad_check_lev3_dir.h,v 1.5 2011/03/24 21:39:55 heimbach Exp $
C $Name:  $

#ifdef ALLOW_PTRACERS
# ifdef AUTODIFF_PTRACERS_SPLIT_FILES
CADJ STORE ptracer(:,:,:,:,:,1:PTRACERS_num)
CADJ &     = tapelev3, key = ilev_3
CADJ STORE gptrnm1(:,:,:,:,:,1:PTRACERS_num)
CADJ &     = tapelev3, key = ilev_3
CADJ STORE gptr(:,:,:,:,:,1:PTRACERS_num)
CADJ &     = tapelev3, key = ilev_3
# else
CADJ STORE ptracer = tapelev3, key = ilev_3
CADJ STORE gptrnm1 = tapelev3, key = ilev_3
CADJ STORE gptr    = tapelev3, key = ilev_3
# endif
#endif /* ALLOW_PTRACERS */
