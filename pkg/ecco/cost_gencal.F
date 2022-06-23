#include "ECCO_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: cost_gencal
C     !INTERFACE:
      subroutine cost_gencal(
     I     localbarfile, localobsfile,
     I     irec, localstartdate, localperiod,
     O     fname1, fname2, localrec, obsrec, exst,
     I     mythid )

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE cost_gencal
C     ==================================================================
C     reads and pre-processes bar file records
C     ==================================================================
C     SUBROUTINE cost_gencal
C     ==================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#ifdef ALLOW_CAL
# include "cal.h"
#endif
#ifdef ALLOW_ECCO
# include "ECCO_SIZE.h"
# include "ECCO.h"
#endif

c     == routine arguments ==
      character*(MAX_LEN_FNAM) localbarfile
      character*(MAX_LEN_FNAM) localobsfile
      integer irec, localstartdate(4)
      _RL localperiod
      character*(128) fname1, fname2
      integer localrec, obsrec
      logical exst
      integer mythid

#ifdef ALLOW_ECCO
c     == external functions ==
      integer  ilnblnk
      external ilnblnk

c     == local variables ==
c      CHARACTER*(MAX_LEN_MBUF) msgBuf
      integer k, il
      _RL daytime
      _RL diffsecs
      integer dayiter
      integer daydate(4)
      integer difftime(4)
      integer tempDate_1
      integer middate(4)
      integer yday, ymod
      integer md, dd, sd, ld, wd
      integer mody, modm

c     == end of interface ==
CEOP

      write(fname1(1:128),'(80a)') ' '
      il=ilnblnk( localbarfile )
      write(fname1(1:128),'(2a,i10.10)')
     &     localbarfile(1:il),'.',eccoiter

        if ( localperiod.EQ.dTtracerLev(1) ) then
           localrec = irec
           obsrec = irec
           yday = 0
        elseif ( localperiod .EQ. 86400. ) then
c-- assume daily fields
           obsrec = irec
           daytime = FLOAT(secondsperday*(irec-1)) + modelstart
           dayiter = hoursperday*(irec-1) + modeliter0
           call cal_getdate( dayiter, daytime, daydate, mythid )
           call cal_convdate( daydate,yday,md,dd,sd,ld,wd,mythid )
           ymod = modelstartdate(1)/10000
           do k=1,4
              middate(k)=0
           enddo
           tempDate_1 = yday*10000+100+1
           if ( ymod .GE. yday ) then
              call cal_FullDate( modelstartdate(1), 0, middate, mythid)
           else
              call cal_FullDate( tempDate_1, 0, middate, mythid)
           endif
           call cal_TimePassed( middate, daydate, difftime, mythid )
           call cal_ToSeconds( difftime, diffsecs, mythid )
c           localrec = floor(diffsecs/localperiod) + 1
           localrec = int(diffsecs/localperiod) + 1
        else
c-- assume monthly fields
           obsrec = irec
           mody   = modelstartdate(1)/10000
           modm   = modelstartdate(1)/100 - mody*100
           yday   = mody + INT((modm-1+irec-1)/12)
           localrec = 1 + MOD(modm-1+irec-1,12)
        endif

        il=ilnblnk(localobsfile)
        write(fname2(1:128),'(2a,i4)')
     &       localobsfile(1:il), '_', yday
        inquire( file=fname2, exist=exst )
        if (.NOT. exst) then
c          assume that the data set is cyclic
           write(fname2(1:128),'(a)') localobsfile(1:il)
           inquire( file=fname2, exist=exst )
        endif

#endif /* ALLOW_ECCO */

      RETURN
      END

