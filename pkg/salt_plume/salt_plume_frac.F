C $Header: /u/gcmpack/MITgcm/pkg/salt_plume/salt_plume_frac.F,v 1.9 2014/05/21 10:46:03 heimbach Exp $
C $Name:  $

#include "SALT_PLUME_OPTIONS.h"

CBOP
C     !ROUTINE: SALT_PLUME_FRAC
C     !INTERFACE:
      SUBROUTINE SALT_PLUME_FRAC(
     I                  imax, fact,SPDepth,
#ifdef SALT_PLUME_SPLIT_BASIN
     I                  lon,lat,
#endif
     U                  plumek,
     I                  myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SALT_PLUME_FRAC
C     | o Compute saltplume penetration.
C     *==========================================================*
C     | Compute fraction of saltplume (flux) penetrating to
C     | specified depth, plumek, due to rejected salt
C     | during freezing.
C     | For example, if surface value is Saltplume0,
C     | and each level gets equal fraction 1/5 down to SPDepth=5,
C     | SALT_PLUME_FRAC will report plumek = 1/5,2/5,3/5,4/5,5/5 on
C     | output if input plumek = 1,2,3,4,5. Else, output plumek = 0.                
C     | Reference : Duffy et al, (GRL 1999)
C     |
C     | =====
C     | Written by   : ATN (based on SWFRAC)
C     | Date         : Sep 13, 2007
C     | Modified     : Mar 16, 2014 by atn to improve/clean up
C     | -> replace 1-[cummulative plumefrac] code which was based 
C     |    on swfrac with cleaner [cummulative plumefrac] on output
C     |    in order to avoid 1-[1-[cummulative_plumefrac]] whenever
C     |    SALT_PLUME_FRAC is called and used from outside.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "SALT_PLUME.h"

C     !INPUT/OUTPUT PARAMETERS:
C     input arguments
C     imax    :: number of vertical grid points
C     fact    :: scale  factor to apply to depth array
C     SPDpeth :: corresponding SaltPlumeDepth(i,j) at this grid point
C     myTime  :: Current time in simulation
C     myIter  :: Current iteration number in simulation
C     myThid  :: My Thread Id. number
      INTEGER imax
      _RL     fact
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
C     input/output arguments
C     plumek :: on input: vertical depth for desired plume fraction
C               (fact*plumek) is negative distance (m) from surface
C     plumek :: on output: saltplume contribution fraction
      _RL     plumek(imax), SPDepth(imax)
#ifdef SALT_PLUME_SPLIT_BASIN
      _RL     lon(imax), lat(imax)
#endif
CEOP

#ifdef ALLOW_SALT_PLUME

C     !LOCAL VARIABLES:
      _RL facz, dd, dd20
      INTEGER i, Npowerloc
#ifndef TARGET_NEC_SX
      INTEGER kk
#endif
      _RL     one, two, three, S, So, zero
      parameter( one = 1. _d 0, two = 2. _d 0, three = 3. _d 0 )
      parameter( zero = 0. _d 0 )
C     This is an abbreviation of 1./(exp(1.)-1.)
      _RL     recip_expOneM1
      parameter( recip_expOneM1 = 0.581976706869326343 )

      DO i = 1,imax
       facz = abs(fact*plumek(i))
#ifdef SALT_PLUME_SPLIT_BASIN
       IF(SaltPlumeSplitBasin) THEN
         Npowerloc = Npower(2)
         IF(lat(imax).LT. 85.0 .AND. lat(imax).GT. 71.0
     &      .AND. lon(imax) .LT. -90.0) Npowerloc = Npower(1)
       ELSE
         Npowerloc = Npower(1)
       ENDIF
#else
         Npowerloc = Npower
#endif

       IF (SPDepth(i).GE.facz .and. SPDepth(i) .GT. zero) THEN

C     Default: uniform distribution, PlumeMethod=1, Npower=0
        IF (PlumeMethod .EQ. 1) THEN
         dd20 = (abs(SPDepth(i)))
#ifdef TARGET_NEC_SX
         IF ( dd20 .GT. zero ) THEN
          S   = (facz/dd20)
C     crazy attempt to make the code faster and raise S to (Npower+1)
          IF (Npowerloc .GT. 0) S = S*S**Npowerloc
         ELSE
          S = zero
         ENDIF
         plumek(i) = max(zero,S)
#else
         S  = one                  !input depth temp
         So = one
         DO kk=1,Npowerloc+1
          S  = facz*S              !raise to the Npowerloc+1
          So = dd20*So
         ENDDO
         plumek(i) = max(zero,S/So)
#endif /* TARGET_NEC_SX */

        ELSEIF (PlumeMethod .EQ. 2) THEN !exponential distribution
         dd = abs(SPDepth(i))
         S  = exp(facz/dd)-one
         So = recip_expOneM1       !So = exp(one)-one
         plumek(i) = max(zero,S*So)

C     PlumeMethod = 3, distribute salt LINEARLY between SPDepth and 
C     SPDepth/SPovershoot
C     (1-SPovershoot)percent has already been taken into account in
C     SPDepth calculation, i.e., SPDepth = SPovershoot*SPDepth.
        ELSEIF (PlumeMethod .EQ. 3) THEN !overshoot 20%
         dd20 = (abs(SPDepth(i)))
         dd   = dd20/SPovershoot
         So=dd20-dd
         S =facz-dd
         IF( (facz.GE.dd).AND.(facz.LT.dd20) ) THEN
          plumek(i) = max(zero,S/So)
         ELSE
          plumek(i) = zero
         ENDIF

C     PlumeMethod = 5, dumping all salt at the top layer
        ELSEIF (PlumeMethod .EQ. 5) THEN
         dd   = zero
         dd20 = one
         IF( (facz.GE.dd).AND.(facz.LT.dd20) ) THEN
          plumek(i) = zero 
         ELSE
          plumek(i) = one
         ENDIF
        ELSEIF (PlumeMethod .EQ. 6) THEN
C     PLumeMethod = 6, currently only works for Npower = 1 and 2.
         dd20 = (abs(SPDepth(i)))
#ifdef TARGET_NEC_SX
         IF ( dd20 .GT. zero ) THEN
          S  = (facz/dd20)
C     crazy attempt to make the code faster and raise S to (Npower+1)
          IF (Npowerloc .GT. 0) S = S*S**Npowerloc
          So = 1. _d 0/dd20
         ELSE
          S  = zero
          So = zero
         ENDIF
         IF(Npowerloc.EQ.1) THEN   !Npower=1
          plumek(i) = max(zero,two*So*facz-S)
         ELSE                      !Npower=2
          plumek(i) = max(zero,
     &         three*So*facz - three*So*So*facz*facz + S)
         ENDIF
#else
         S  = one                  !input depth temp
         So = one
         DO kk=1,Npowerloc+1
          S  = facz*S              !raise to the Npower+1
          So = dd20*So
         ENDDO
         IF(Npowerloc.EQ.1) THEN   !Npower=1
          plumek(i) = max(zero,two/dd20*facz-S/So)
         ELSE                      !Npower=2
          plumek(i) = max(zero,
     &         three/dd20*facz - three/(dd20*dd20)*facz*facz + S/So)
         ENDIF
#endif /* TARGET_NEC_SX */
          
catn: 15.Mar.2014
catn: this is a new method by atn. After fixing adjoint compiling error,
catn: will switch this on.  Currently commenting out for purpose of 
catn: comparing old (1-abc) vs new (abc) way of coding
c        ELSEIF (PlumeMethod .EQ. 7) THEN 
cC     PLumeMethod = 7, dump an offset parabolla with more salt at surface
cC        tapered to zero at depth SPDepth/2, then increased until SPDepth
cC        need input SPDepth, NPower = percentage of SPDepth
cC        Ex: if Npower = 10 -> (10/2)=5% of SPDepth
cC        NPower can be negative integer here.
cC        0 -> parabola centered at SPDepth/2;
cC        + -> parabola offset, salt @ surface < @ SPDepth
cC        - -> parabola offset, salt @ surface > @ SPDepth
cC        S and So are dummy variables
c         dd   = (abs(SPDepth(i)))
c         dd20 = dd*(one/two-Npower/200. _d 0)
c         So   = (dd*dd*dd/three)
c     &            -(dd*dd)      *dd20
c     &            +(dd)         *dd20*dd20
c         S    = (facz*facz *facz/three)
c     &             - (facz*facz)*dd20
c     &             + (facz)     *dd20*dd20
c         plumek(i) = max(zero,(S/So))
c
#ifndef TARGET_NEC_SX
        ELSE
         WRITE(*,*) 'salt_plume_frac: PLumeMethod =', PLumeMethod,
     &        'not implemented'
         STOP 'ABNORMAL in S/R SALT_PLUME_FRAC'
#endif /* not TARGET_NEC_SX */
        ENDIF
       ELSE
        plumek(i) = one
       ENDIF
      ENDDO
      
#endif /* ALLOW_SALT_PLUME */

      RETURN
      END
