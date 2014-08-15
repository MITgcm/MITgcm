C $Header: /u/gcmpack/MITgcm/pkg/ptracers/ptracers_ad_check_lev4_dir.h,v 1.5 2014/08/15 19:18:12 jmc Exp $
C $Name:  $

#ifdef ALLOW_PTRACERS
# ifdef AUTODIFF_PTRACERS_SPLIT_FILES
CADJ STORE pTracer(:,:,:,:,:,1:PTRACERS_num)
CADJ &     = tapelev4, key = ilev_4
CADJ STORE gpTrNm1(:,:,:,:,:,1:PTRACERS_num)
CADJ &     = tapelev4, key = ilev_4
# else
CADJ STORE pTracer = tapelev4, key = ilev_4
CADJ STORE gpTrNm1 = tapelev4, key = ilev_4
# endif
#endif /* ALLOW_PTRACERS */
