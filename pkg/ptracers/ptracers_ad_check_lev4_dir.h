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
