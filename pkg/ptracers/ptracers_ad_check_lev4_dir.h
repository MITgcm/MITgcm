C $Header: /u/gcmpack/MITgcm/pkg/ptracers/ptracers_ad_check_lev4_dir.h,v 1.3 2011/03/24 12:17:39 heimbach Exp $
C $Name:  $

#ifdef ALLOW_PTRACERS
# ifdef AUTODIFF_PTRACERS_SPLIT_FILES
CADJ STORE ptracer(:,:,:,:,:,1:PTRACERS_num)
CADJ &     = tapelev4_ptr, key = ilev_4
CADJ STORE gptrnm1(:,:,:,:,:,1:PTRACERS_num)
CADJ &     = tapelev4_ptr, key = ilev_4
CADJ STORE gptr(:,:,:,:,:,1:PTRACERS_num)
CADJ &     = tapelev4_ptr, key = ilev_4
# else
CADJ STORE ptracer = tapelev4, key = ilev_4
CADJ STORE gptrnm1 = tapelev4, key = ilev_4
CADJ STORE gptr    = tapelev4, key = ilev_4
# endif
#endif /* ALLOW_PTRACERS */
