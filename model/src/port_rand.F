C $Header: /u/gcmpack/MITgcm/model/src/port_rand.F,v 1.3 2001/09/26 18:09:16 cnh Exp $
C $Name:  $

C Portable random number generator

#undef _USE_INTEGERS

! ==============================================================================
CBOP
C     !ROUTINE: port_rand
C     !INTERFACE:
      real*8 function port_rand()

C     !DESCRIPTION: \bv
C     Portable random number generator
C     \ev

C     !LOCAL VARIABLES:
      implicit none
      integer nff
      parameter(nff=55)
#ifdef _USE_INTEGERS
      integer mbig,mseed,mZ
#else
      real*8 mbig,mseed,mz
#endif
      real*8 fac
#ifdef _USE_INTEGERS
      parameter (mbig=1000000000,mseed=161803398,mz=0,fac=1./mbig)
#else
      parameter (mbig=4000000.,mseed=1618033.,mz=0.,fac=1./mbig)
#endif
      integer i,ii,inext,inextp,k,idum
      parameter(idum=-2)
#ifdef _USE_INTEGERS
      integer mj,mk,ma(nff)
#else
      real*8 mj,mk,ma(nff)
#endif
      logical firstCall
      save firstCall,inext,inextp,ma
      data firstCall /.true./
CEOP
! ------------------------------------------------------------------------------
      if(firstCall)then
        firstCall=.false.
        mj=mseed-iabs(idum)
        mj=mod(mj,mbig)
        ma(nff)=mj
        mk=1
        do i=1,nff-1
          ii=mod(21*i,nff)
          ma(ii)=mk
          mk=mj-mk
          if(mk.lt.mz)mk=mk+mbig
          mj=ma(ii)
        enddo
        do k=1,4
          do i=1,nff
            ma(i)=ma(i)-ma(1+mod(i+30,nff))
            if(ma(i).lt.MZ)ma(i)=ma(i)+mbig
          enddo
        enddo
        inext=0
        inextp=31
      endif
      inext=mod(inext,nff)+1
      inextp=mod(inextp,nff)+1
      mj=ma(inext)-ma(inextp)
      if(mj.lt.MZ)mj=mj+mbig
      ma(inext)=mj
      port_rand=mj*fac
      return
! ------------------------------------------------------------------------------
      end
! ==============================================================================

! ==============================================================================
      subroutine port_ranarr(n,arr)
      implicit none
      integer n,i
      real arr(n)
      real*8 port_rand
! ------------------------------------------------------------------------------
      do i=1,n
       arr(i)=port_rand()
      enddo

      return
! ------------------------------------------------------------------------------
      end
! ==============================================================================
