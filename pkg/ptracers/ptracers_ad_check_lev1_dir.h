#ifdef ALLOW_PTRACERS
#if ( defined ALLOW_ECCO || defined NONLIN_FRSURF )
CADJ STORE pTracer   = comlev1, key = ikey_dynamics, kind = isbyte
CADJ STORE gpTrNm1   = comlev1, key = ikey_dynamics, kind = isbyte
#endif
#endif /* ALLOW_PTRACERS */
