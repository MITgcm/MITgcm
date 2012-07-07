C $Header: /u/gcmpack/MITgcm/pkg/gridalt/phys2dyn.F,v 1.8 2012/07/07 00:08:09 jmc Exp $
C $Name:  $

#include "GRIDALT_OPTIONS.h"

      subroutine phys2dyn(qphy,pephy,im1,im2,jm1,jm2,lmphy,Nsx,Nsy,
     .     idim1,idim2,jdim1,jdim2,bi,bj,pedyn,Lbot,lmdyn,nlperdyn,qdyn)
C***********************************************************************
C Purpose:
C   To interpolate an arbitrary quantity from the 'dynamics' eta (pstar)
C               grid to the higher resolution physics grid
C Algorithm:
C   Routine works one layer (edge to edge pressure) at a time.
C   Physics -> Dynamics computes the physics layer mean value,
C   weighted by dp**kappa (interp1) or by dp.
C
C Input:
C   qphy..... [im,jm,lmphy] Arbitrary Quantity on Input Grid
C   pephy.... [im,jm,lmphy+1] Pressures at bottom edges of input levels
C   im1,2 ... Limits for Longitude Dimension of Input
C   jm1,2 ... Limits for Latitude  Dimension of Input
C   lmphy.... Vertical  Dimension of Input
C   Nsx...... Number of processes in x-direction
C   Nsy...... Number of processes in y-direction
C   idim1,2.. Beginning and ending i-values to calculate
C   jdim1,2.. Beginning and ending j-values to calculate
C   bi....... Index of process number in x-direction
C   bj....... Index of process number in x-direction
C   pedyn.... [im,jm,lmdyn+1] Pressures at bottom edges of output levels
C   lmdyn.... Vertical  Dimension of Output
C   nlperdyn. Mapping Array-Highest Physics level in each dynmics level
C
C Output:
C   qdyn..... [im,jm,lmdyn] Quantity at output grid (physics grid)
C
C Notes:
C   1) This algorithm assumes that the output (physics) grid levels
C      fit exactly into the input (dynamics) grid levels
C***********************************************************************
      implicit none

      integer  im1, im2, jm1, jm2, lmdyn, lmphy, Nsx, Nsy
      integer idim1, idim2, jdim1, jdim2, bi, bj
      _RL qphy(im1:im2,jm1:jm2,lmphy,Nsx,Nsy)
      _RL pedyn(im1:im2,jm1:jm2,lmdyn+1,Nsx,Nsy)
      _RL pephy(im1:im2,jm1:jm2,lmphy+1,Nsx,Nsy)
      integer nlperdyn(im1:im2,jm1:jm2,lmdyn,Nsx,Nsy)
      _RL qdyn(im1:im2,jm1:jm2,lmdyn,Nsx,Nsy)
      integer Lbot(im1:im2,jm1:jm2,Nsx,Nsy)

      integer  i,j,L,Lout1,Lout1p1,Lout2,Lphy
      _RL dpkephy, dpkedyn, sum

cinterp1  _RL kappa
#ifdef ALLOW_FIZHI
cinterp1  _RL getcon
#else
cinterp1 #include 'SIZE.h'
cinterp1 #include 'EEPARAMS.h'
cinterp1 #include 'PARAMS.h'
#endif

#ifdef ALLOW_FIZHI
cinterp1  kappa = getcon('KAPPA')
#else
cinterp1  kappa = atm_kappa
#endif

c do loop for all dynamics (output) levels
      do L = 1,lmdyn
c do loop for all grid points
       do j = jdim1,jdim2
        do i = idim1,idim2
         qdyn(i,j,L,bi,bj) = 0.
c Check to make sure we are above ground - otherwise do nothing
         if(L.ge.Lbot(i,j,bi,bj))then
          if(L.eq.Lbot(i,j,bi,bj)) then
           Lout1 = 0
          else
           Lout1 = nlperdyn(i,j,L-1,bi,bj)
          endif
          Lout2 = nlperdyn(i,j,L,bi,bj)
c do loop for all physics levels contained in this dynamics level
cinterp1  dpkedyn = (pedyn(i,j,L,bi,bj)**kappa)-
cinterp1                                   (pedyn(i,j,L+1,bi,bj)**kappa)
          dpkedyn = pedyn(i,j,L,bi,bj)-pedyn(i,j,L+1,bi,bj)
          sum = 0.
          Lout1p1 = Lout1+1
          do Lphy = Lout1p1,Lout2
cinterp1   dpkephy = (pephy(i,j,Lphy,bi,bj)**kappa)-
cinterp1                                (pephy(i,j,Lphy+1,bi,bj)**kappa)
           dpkephy = pephy(i,j,Lphy,bi,bj)-pephy(i,j,Lphy+1,bi,bj)
           sum=sum+qphy(i,j,Lphy,bi,bj)*(dpkephy/dpkedyn)
          enddo
          qdyn(i,j,L,bi,bj) = sum
         endif
        enddo
       enddo
      enddo

      return
      end
