#ifdef ALLOW_PTRACERS
# ifdef AUTODIFF_PTRACERS_SPLIT_FILES
CADJ STORE pTracer(:,:,:,:,:,1:PTRACERS_num)
CADJ &     = tapelev2, key = ilev_2
CADJ STORE gpTrNm1(:,:,:,:,:,1:PTRACERS_num)
CADJ &     = tapelev2, key = ilev_2
# else
CADJ STORE pTracer = tapelev2, key = ilev_2
CADJ STORE gpTrNm1 = tapelev2, key = ilev_2
# endif
#endif /* ALLOW_PTRACERS */
