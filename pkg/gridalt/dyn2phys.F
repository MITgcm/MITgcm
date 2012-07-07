C $Header: /u/gcmpack/MITgcm/pkg/gridalt/dyn2phys.F,v 1.7 2012/07/07 00:08:08 jmc Exp $
C $Name:  $

#include "GRIDALT_OPTIONS.h"

      subroutine dyn2phys(qdyn,pedyn,im1,im2,jm1,jm2,lmdyn,Nsx,Nsy,
     . idim1,idim2,jdim1,jdim2,bi,bj,windphy,pephy,Lbot,lmphy,nlperdyn,
     . flg,qphy)
C***********************************************************************
C Purpose:
C   To interpolate an arbitrary quantity from the 'dynamics' eta (pstar)
C               grid to the higher resolution physics grid
C Algorithm:
C   Routine works one layer (edge to edge pressure) at a time.
C   Dynamics -> Physics retains the dynamics layer mean value,
C   weights the field either with the profile of the physics grid
C   wind speed (for U and V fields), or uniformly (T and Q)
C
C Input:
C   qdyn..... [im,jm,lmdyn] Arbitrary Quantity on Input Grid
C   pedyn.... [im,jm,lmdyn+1] Pressures at bottom edges of input levels
C   im1,2 ... Limits for Longitude Dimension of Input
C   jm1,2 ... Limits for Latitude  Dimension of Input
C   lmdyn.... Vertical  Dimension of Input
C   Nsx...... Number of processes in x-direction
C   Nsy...... Number of processes in y-direction
C   idim1,2.. Beginning and ending i-values to calculate
C   jdim1,2.. Beginning and ending j-values to calculate
C   bi....... Index of process number in x-direction
C   bj....... Index of process number in x-direction
C   windphy.. [im,jm,lmphy] Magnitude of the wind on the output levels
C   pephy.... [im,jm,lmphy+1] Pressures at bottom edges of output levels
C   lmphy.... Vertical  Dimension of Output
C   nlperdyn. [im,jm,lmdyn] Highest Physics level in each dynamics level
C   flg...... Flag to indicate field type (0 for T or Q, 1 for U or V)
C
C Output:
C   qphy..... [im,jm,lmphy] Quantity at output grid (physics grid)
C
C Notes:
C   1) This algorithm assumes that the output (physics) grid levels
C      fit exactly into the input (dynamics) grid levels
C***********************************************************************
      implicit none

      integer  im1, im2, jm1, jm2, lmdyn, lmphy, Nsx, Nsy, flg
      integer idim1, idim2, jdim1, jdim2, bi, bj
      _RL qdyn(im1:im2,jm1:jm2,lmdyn,Nsx,Nsy)
      _RL pedyn(im1:im2,jm1:jm2,lmdyn+1,Nsx,Nsy)
      _RL pephy(im1:im2,jm1:jm2,lmphy+1,Nsx,Nsy)
      _RL windphy(im1:im2,jm1:jm2,lmphy,Nsx,Nsy)
      integer nlperdyn(im1:im2,jm1:jm2,lmdyn,Nsx,Nsy)
      _RL qphy(im1:im2,jm1:jm2,lmphy,Nsx,Nsy)
      integer Lbot(im1:im2,jm1:jm2,Nsx,Nsy)

      _RL weights(im1:im2,jm1:jm2,lmphy)
      _RL pphy(im1:im2,jm1:jm2,lmphy)
      _RL dpkedyn, dpkephy, windsum, qd
      integer  i,j,L,Lout1,Lout2,Lphy
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

C define physics grid mid level pressures
      do Lphy = 1,lmphy
       do j = jdim1,jdim2
        do i = idim1,idim2
         pphy(i,j,Lphy) =
     .       (pephy(i,j,Lphy,bi,bj)+pephy(i,j,Lphy+1,bi,bj))/2.
        enddo
       enddo
      enddo

c do loop for all dynamics (input) levels
      do L = 1,lmdyn
c do loop for all grid points
       do j = jdim1,jdim2
        do i = idim1,idim2
         qd = qdyn(i,j,L,bi,bj)
c Check to make sure we are above ground - if not, do nothing
         if(L.ge.Lbot(i,j,bi,bj))then
          if(L.eq.Lbot(i,j,bi,bj)) then
           Lout1 = 0
          else
           Lout1 = nlperdyn(i,j,L-1,bi,bj)
          endif
          Lout2 = nlperdyn(i,j,L,bi,bj)
c for U and V fields, need to compute for the weights:
cinterp1  dpkedyn = (pedyn(i,j,L,bi,bj)**kappa)-
cinterp1                                  (pedyn(i,j,L+1,bi,bj)**kappa)
          dpkedyn = pedyn(i,j,L,bi,bj)-pedyn(i,j,L+1,bi,bj)
          if(flg.eq.1)then
           windsum = 0.
           do Lphy = Lout1+1,Lout2
cinterp1    dpkephy = (pephy(i,j,Lphy,bi,bj)**kappa)-
cinterp1                                (pephy(i,j,Lphy+1,bi,bj)**kappa)
            dpkephy = pephy(i,j,Lphy,bi,bj)-pephy(i,j,Lphy+1,bi,bj)
            windsum = windsum+(windphy(i,j,Lphy,bi,bj)*dpkephy)/dpkedyn
           enddo
          endif
c do loop for all physics levels contained in this dynamics level
          do Lphy = Lout1+1,Lout2
           weights(i,j,Lphy) = 1.
           if( (flg.eq.1).and.(windsum.ne.0.) )
     .                weights(i,j,Lphy)=windphy(i,j,Lphy,bi,bj)/windsum
           if( (flg.eq.2) .and. (pedyn(i,j,L,bi,bj).lt.10000.)) then
            weights(i,j,Lphy) =
     .   (qd-5. + (10.*(pedyn(i,j,L,bi,bj)-pphy(i,j,Lphy))/dpkedyn))/qd
           elseif( (flg.eq.2) .and. (pedyn(i,j,L,bi,bj).ge.10000.)) then
            weights(i,j,Lphy) = 1.
           endif
           qphy(i,j,Lphy,bi,bj) = qd * weights(i,j,Lphy)
          enddo
         endif
        enddo
       enddo
      enddo

      return
      end
