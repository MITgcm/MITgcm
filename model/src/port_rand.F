C $Header: /u/gcmpack/MITgcm/model/src/port_rand.F,v 1.5 2004/09/07 15:16:58 edhill Exp $
C $Name:  $

C Portable random number generator

#undef _USE_INTEGERS

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: port_rand
C     !INTERFACE:
      real*8 function port_rand(seed)

C     !DESCRIPTION:
C     Portable random number generator
      
C     !USES:
      implicit none
CEOP

C     !LOCAL VARIABLES:
      integer nff
      parameter(nff=55)
#ifdef _USE_INTEGERS
      integer mbig,mseed,mZ,seed
#else
      real*8 mbig,mseed,mz,seed
#endif
      real*8 fac
#ifdef _USE_INTEGERS
      parameter (mbig=1000000000,mz=0,fac=1./mbig)
      data mseed/161803398/
#else
      parameter (mbig=4000000.,mz=0.,fac=1./mbig)
      data mseed/1618033./
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

      mseed = seed
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

      end

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      subroutine port_ranarr(n,arr)
      implicit none
      integer n,i
      real arr(n)
      real*8 port_rand,seed
      seed=1618033.0d0
      do i=1,n
       arr(i)=port_rand(seed)
      enddo

      return
      end

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: port_rand_norm
C     !INTERFACE:
      real*8 function port_rand_norm()

C     !DESCRIPTION:
C     This function generates a normally distributed random number with
C     the so called polar algorithm. The algorithm actually generates 2
C     numbers, but only 1 is returned for maximum compatibility with old
C     code.  The most obvious way to improve this function would be to
C     make sure that the second number is not wasted.

C     Changed: 2004.09.06 antti.westerlund@fimr.fi

C     !USES:
      implicit none
CEOP

C     !LOCAL VARIABLES:
      real*8 port_rand,seed
      real*8 x1, x2, xs, t           
      integer i

      seed=1618033.0d0
C     first generate 2 equally distributed random numbers (-1,1) 
      DO WHILE (1 .eq. 1)
         x1=2.0*port_rand(seed)-1.0
         x2=2.0*port_rand(seed)-1.0
         xs=x1**2+x2**2
         if(xs .lt. 1.0 .and. xs .ne. 0.0) then
            goto 100      
         end if
      END DO
 100  continue

      t = sqrt(-2.0*log(xs)/xs)
      port_rand_norm = t*x1
C
C     also t*x2 would be a gaussian random number and could be returned

      return
      end
