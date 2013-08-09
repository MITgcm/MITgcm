C $Header: /u/gcmpack/MITgcm/model/src/port_rand.F,v 1.8 2013/08/09 15:00:46 jmc Exp $
C $Name:  $

#undef _USE_INTEGERS

C--  File port_rand.F: Portable random number generator functions
C--   Contents
C--   o PORT_RAND
C--   o PORT_RANARR
C--   o PORT_RAND_NORM

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: PORT_RAND
C     !INTERFACE:
      real*8 FUNCTION PORT_RAND(seed)

C     !DESCRIPTION:
C     Portable random number generator
C      seed >=0 :: initialise using this seed ; and return 0
C      seed < 0 :: if first call then initialise using the default seed (=mseed)
C                  and always return a random number

C     !USES:
      IMPLICIT NONE

C     !INPUT PARAMETERS:
#ifdef _USE_INTEGERS
      INTEGER seed
#else
      real*8  seed
#endif
CEOP

C     !LOCAL VARIABLES:
      INTEGER nff,idum
      PARAMETER(nff=55)
      PARAMETER(idum=-2)
      real*8 fac
#ifdef _USE_INTEGERS
      INTEGER mbig,mseed,mZ
      PARAMETER (mbig=1000000000,mz=0,fac=1.d0/mbig)
      INTEGER mj,mk,ma(nff)
      DATA mseed/161803398/
#else
      real*8 mbig,mseed,mz
      PARAMETER (mbig=4000000.,mz=0.,fac=1.d0/mbig)
      real*8 mj,mk,ma(nff)
      DATA mseed/1618033./
#endif
      LOGICAL firstCall
      INTEGER i,ii,inext,inextp,k
      DATA firstCall /.true./
      SAVE firstCall,inext,inextp,ma

C-    Initialise the random number generator
      IF (firstCall .OR. seed.GE.mz) THEN
        IF (seed.GE.mz) mseed = seed
        firstCall=.false.
        mj=mseed-iabs(idum)
        mj=mod(mj,mbig)
        ma(nff)=mj
        mk=1
        DO i=1,nff-1
          ii=mod(21*i,nff)
          ma(ii)=mk
          mk=mj-mk
          IF (mk.LT.mz) mk=mk+mbig
          mj=ma(ii)
        ENDDO
        DO k=1,4
          DO i=1,nff
            ma(i)=ma(i)-ma(1+mod(i+30,nff))
            IF (ma(i).LT.mz) ma(i)=ma(i)+mbig
          ENDDO
        ENDDO
        inext=0
        inextp=31
      ENDIF

C-    Compute a random number (only if seed < 0)
      IF (seed.GE.mz) THEN
        port_rand=0.d0
      ELSE
        inext=mod(inext,nff)+1
        inextp=mod(inextp,nff)+1
        mj=ma(inext)-ma(inextp)
        IF (mj.LT.mz) mj=mj+mbig
        ma(inext)=mj
        port_rand=mj*fac
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: PORT_RANARR
C     !INTERFACE:
      SUBROUTINE PORT_RANARR(n,arr)

C     !DESCRIPTION:
C     Portable random number array generator

C     !USES:
      IMPLICIT NONE

C     !INPUT PARAMETERS:
      INTEGER n
      real*8 arr(n)
CEOP

C     !LOCAL VARIABLES:
      INTEGER i
      real*8 port_rand
#ifdef _USE_INTEGERS
      INTEGER seed
      seed=-1
#else
      real*8  seed
      seed=-1.d0
#endif
c     seed=1618033.0d0
      DO i=1,n
       arr(i)=port_rand(seed)
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: PORT_RAND_NORM
C     !INTERFACE:
      real*8 FUNCTION PORT_RAND_NORM()

C     !DESCRIPTION:
C     This function generates a normally distributed random number with
C     the so called polar algorithm. The algorithm actually generates 2
C     numbers, but only 1 is returned for maximum compatibility with old
C     code.  The most obvious way to improve this function would be to
C     make sure that the second number is not wasted.

C     Changed: 2004.09.06 antti.westerlund@fimr.fi

C     !USES:
      IMPLICIT NONE
CEOP

C     !LOCAL VARIABLES:
      real*8 port_rand
      real*8 x1, x2, xs, t

#ifdef _USE_INTEGERS
      INTEGER seed
      seed=-1
#else
      real*8  seed
      seed=-1.d0
#endif
c     seed=1618033.0d0

C     first generate 2 equally distributed random numbers (-1,1)
      xs = 0.0
      DO WHILE ( xs.GE.1.0 .OR. xs.EQ.0.0 )
         x1=2.0*port_rand(seed)-1.0
         x2=2.0*port_rand(seed)-1.0
         xs=x1**2+x2**2
      ENDDO

      t = SQRT(-2.0*LOG(xs)/xs)
      port_rand_norm = t*x1

C     also t*x2 would be a gaussian random number and could be returned

      RETURN
      END
