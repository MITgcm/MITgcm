C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/GAD_FLUX_LIMITER.h,v 1.2 2001/06/20 16:15:07 jamous Exp $
C $Name:  $

C Statement function to describe flux limiter
      _RL Limiter
C Upwind        Limiter(Cr)=0.
C Lax-Wendroff  Limiter(Cr)=1.
C Suberbee      Limiter(Cr)=max(0.,max(min(1.,2*Cr),min(2.,Cr)))

c     Limiter(Cr)=0.
c     Limiter(Cr)=1.
      Limiter(Cr)=max(0.d0,max(min(1.d0,2.d0*Cr),min(2.d0,Cr)))
