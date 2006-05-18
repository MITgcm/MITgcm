C $Header: /u/gcmpack/MITgcm/pkg/fizhi/slprs.F,v 1.1 2006/05/18 19:38:32 molod Exp $
C $Name:  $

#include "FIZHI_OPTIONS.h"
      subroutine slprs (SLP,PZ,PTOP,PHIS,THZ,PLE,lwmask,im,jm,lm)
C***********************************************************************        
C  INPUT                                                                        
C    PZ ...... SURFACE   PRESSURE (MB) - PTOP                                   
C    PTOP .... MODEL TOP PRESSURE (MB)                                          
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
      _RL  SLP   (im*jm),  PZ   (im*jm)
      _RL  PHIS  (im*jm),  THZ  (im*jm,lm)
      _RL  lwmask(im*jm)
      _RL  ple(im*jm,lm+1)

      _RL ZERO, ONE, TWO, BETA
      PARAMETER(ZERO = 0.0)
      PARAMETER(ONE  = 1.0)
      PARAMETER(TWO  = 2.0)
      PARAMETER(BETA = 0.0065)

      _RL  getcon,g,r,ak,cp,delp,ptop
      integer i,L
      _RL  tm (im*jm)
      integer Ltop (im*jm)
                                                                                
      G  = GETCON('GRAVITY')                                                    
      R  = GETCON('RGAS')                                                       
      AK = GETCON('KAPPA')                                                      
      CP = GETCON('CP')                                                      
                                                                                
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
      if( lwmask(i).eq.0.0 ) then
      TM(I) = TM(I) * ((PZ(I)+PTOP)/1000.)**AK + BETA*PHIS(I)/(TWO*G)
      else
      TM(I) = THZ(I,LM)*((PZ(I)+PTOP)/1000.)**AK + BETA*PHIS(I)/(TWO*G)
      endif

      SLP(I) = PHIS(I) / ( R*TM(I) )                                            
      SLP(I) = ( PZ(I)+PTOP ) * EXP( SLP(I) )                                   
      enddo
                                                                                
      RETURN                                                                    
      END                                                                       
