C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/GAD_FLUX_LIMITER.h,v 1.4 2001/07/30 20:42:45 heimbach Exp $
C $Name:  $

C Statement function to describe flux limiter
      _RL Limiter
C Upwind        Limiter(Cr)=0.
C Lax-Wendroff  Limiter(Cr)=1.
C Suberbee      Limiter(Cr)=max(0.,max(min(1.,2*Cr),min(2.,Cr)))

c     Limiter(Cr)=0.
c     Limiter(Cr)=1.
      Limiter(Cr)=max(0.D0,max(min(1.D0,2.D0*Cr),
     &                         min(2.D0,Cr))) 
