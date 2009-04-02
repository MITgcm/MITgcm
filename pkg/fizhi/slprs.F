C $Header: /u/gcmpack/MITgcm/pkg/fizhi/slprs.F,v 1.4 2009/04/02 20:54:03 jmc Exp $
C $Name:  $

#include "FIZHI_OPTIONS.h"
      subroutine slprs (PHIS,PLE,THZ,lwmask,im,jm,lm,SLP)
C**********************************************************************
C  INPUT
C    PHIS .... SURFACE GEOPOTENTIAL (M2/S2)
C    THZ ..... POTENTIAL TEMPERATURE (K) ON  Model LEVELS
C    grid .... Dynamics Grid Structure
C    lwmask .. Land:(0.0) Water:(1.0) Mask
C
C  OUTPUT
C    SLP ..... SEA LEVEL PRESSURE (MB)
C
C NOTE: Level counting here for thz and ple is top down (thz(1) is top)
C***********************************************************************

      implicit none

      integer im,jm,lm
      _RL  SLP   (im*jm)
      _RL  PHIS  (im*jm),  THZ  (im*jm,lm)
      _RL  lwmask(im*jm)
      _RL  ple(im*jm,lm+1)

      _RL TWO, BETA
      PARAMETER(TWO  = 2.0)
      PARAMETER(BETA = 0.0065)

      _RL  getcon,g,r,ak
      integer i,L
      _RL  tm (im*jm)
      integer Ltop (im*jm)

      G  = GETCON('GRAVITY')
      R  = GETCON('RGAS')
      AK = GETCON('KAPPA')
C***********************************************************************
C*                COMPUTE MEAN THETA IN PBL (100 MB)                   *
C***********************************************************************

      do i=1,im*jm
      tm(i) = 0.0
      Ltop(i) = lm
      enddo

      do L = lm,1,-1
       do i=1,im*jm
        if ( ple(i,L+1).ge.(ple(i,lm+1)-100.) ) then
         Ltop(i) = L
         tm(i) = tm(i) + thz(i,L)*(ple(i,L+1)-ple(i,L))
        endif
       enddo
      enddo

      do i=1,im*jm
      tm(i) = tm(i)/(ple(i,lm+1)-ple(i,Ltop(i)))
      enddo

C***********************************************************************
C*                   COMPUTE SEA LEVEL PRESSURE                        *
C***********************************************************************

      do i=1,im*jm
      if( lwmask(i).ne.0.0 ) then
      TM(I) = TM(I) * (PLE(I,LM+1)/1000.)**AK + BETA*PHIS(I)/(TWO*G)
      else
      TM(I) = THZ(I,LM)*(PLE(I,LM+1)/1000.)**AK + BETA*PHIS(I)/(TWO*G)
      endif

      SLP(I) = PHIS(I) / ( R*TM(I) )
      SLP(I) = PLE(I,LM+1) * EXP( SLP(I) )
      enddo

      RETURN
      END
