C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/GAD_FLUX_LIMITER.h,v 1.5 2001/09/19 20:45:09 adcroft Exp $
C $Name:  $

CBOP
C !ROUTINE: GAD_FLUX_LIMITER.h

C !INTERFACE:
C #include "GAD_FLUX_LIMITER.h"
C       _RL Limiter
C       Limiter(Cr)

C !DESCRIPTION:
C Contains statement function defining limiter function.
C
C A trivial limit for the limiter recovers the upwind scheme:
C \begin{equation*}
C Limiter(Cr)=0
C \end{equation*}
C Lax-Wendroff is recovered with:
C \begin{equation*}
C Limiter(Cr)=1
C \end{equation*}
C The current limiter of choice is the "Superbee" limiter:
C \begin{equation*}
C Limiter(Cr)=max(0,max(min(1,2*Cr),min(2,Cr)))
C \end{equation*}
C which is the default.
CEOP

C Statement function to describe flux limiter
      _RL Limiter
C Upwind        Limiter(Cr)=0.
C Lax-Wendroff  Limiter(Cr)=1.
C Suberbee      Limiter(Cr)=max(0.,max(min(1.,2*Cr),min(2.,Cr)))

c     Limiter(Cr)=0.
c     Limiter(Cr)=1.
      Limiter(Cr)=max(0.D0,max(min(1.D0,2.D0*Cr),
     &                         min(2.D0,Cr))) 
