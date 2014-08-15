C $Header: /u/gcmpack/MITgcm/pkg/ptracers/ptracers_ad_check_lev3_dir.h,v 1.6 2014/08/15 19:18:12 jmc Exp $
C $Name:  $

#ifdef ALLOW_PTRACERS
# ifdef AUTODIFF_PTRACERS_SPLIT_FILES
CADJ STORE pTracer(:,:,:,:,:,1:PTRACERS_num)
CADJ &     = tapelev3, key = ilev_3
CADJ STORE gpTrNm1(:,:,:,:,:,1:PTRACERS_num)
CADJ &     = tapelev3, key = ilev_3
# else
CADJ STORE pTracer = tapelev3, key = ilev_3
CADJ STORE gpTrNm1 = tapelev3, key = ilev_3
# endif
#endif /* ALLOW_PTRACERS */
