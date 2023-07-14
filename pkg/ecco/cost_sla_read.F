#include "ECCO_OPTIONS.h"

      subroutine cost_sla_read(
     I                sla_file, sla_startdate, sla_period, use_mon,
     I                sla_intercept, sla_slope,
     O                sla_obs, sla_mask,
     I                irec, myThid )

c     ==================================================================
c     SUBROUTINE cost_sla_read
c     ==================================================================
c
c     o Read a given record of the SLA data.
c
c     started: Gael Forget 20-Oct-2009
c
c     Apr-2023: argument use_mon, switches read daily to monthly if true
c
c     ==================================================================
c     SUBROUTINE cost_sla_read
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"

#include "cal.h"
#include "ECCO_SIZE.h"
#include "ECCO.h"

c     == routine arguments ==
C     use_mon :: switch from using daily to monthly data if true
      character*(MAX_LEN_FNAM) sla_file
      integer sla_startdate(4)
      _RL sla_period
      logical use_mon
      _RL sla_intercept
      _RL sla_slope
      _RL sla_obs (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL sla_mask(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      integer irec
      integer myThid

c     == external functions ==
      integer  ilnblnk
      external ilnblnk

c     == local variables ==
      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer sshrec
      integer difftime(4)
      integer tempDate_1
      integer middate(4)
      _RL diffsecs
      _RL spval
      _RL factor
cnew(
      integer  il
      _RL obstime
      integer obsiter
      integer obsdate(4)
      integer yobs, ymod
      integer md, dd, sd, ld, wd
      character*(MAX_LEN_FNAM) fnametmp
      logical exst
cnew)
      integer mody, modm
      _RL daysperavgperiod

c     == end of interface ==

      jtlo = myByLo(myThid)
      jthi = myByHi(myThid)
      itlo = myBxLo(myThid)
      ithi = myBxHi(myThid)
      jmin = 1
      jmax = sNy
      imin = 1
      imax = sNx

      factor = 0.01
      spval = -9990.

      IF (use_mon) THEN
        daysperavgperiod = 30.5
        mody = modelstartdate(1)/10000
        modm = modelstartdate(1)/100 - mody*100
        yobs = mody + INT((modm-1+irec-1)/12)
        sshrec = 1 + MOD(modm-1+irec-1,12)

      ELSE

        daysperavgperiod = 1.0
c select data record to read
        obstime = FLOAT(secondsperday*(irec-1)) + modelstart
        obsiter = hoursperday*(irec-1)+modeliter0
        call cal_getdate( obsiter, obstime, obsdate, myThid )
        call cal_convdate( obsdate,yobs,md,dd,sd,ld,wd,myThid )
        ymod = sla_startdate(1)/10000

        IF ( ymod .GE. yobs ) THEN
           call cal_FullDate( sla_startdate(1), 0, middate, myThid)
        ELSE
           tempDate_1 = yobs*10000+100+1
           call cal_FullDate( tempDate_1, 0, middate, myThid)
        ENDIF

        call cal_TimePassed( middate, obsdate, difftime, myThid )
        call cal_ToSeconds( difftime, diffsecs, myThid )
c        sshrec = floor(diffsecs/sla_period) + 1
        sshrec = int(diffsecs/sla_period) + 1
      ENDIF !use_mon

      il=ilnblnk(sla_file)
      WRITE(fnametmp,'(2a,i4)')
     &     sla_file(1:il), '_', yobs
      inquire( file=fnametmp, exist=exst )

c read data:
      IF ( (sshrec .GT. 0).AND.(exst) ) THEN
       CALL READ_REC_3D_RL( fnametmp, cost_iprec, 1,
     &                      sla_obs, sshrec, 1, myThid )
      ELSE
       DO bj = jtlo,jthi
        DO bi = itlo,ithi
         DO j = jmin,jmax
          DO i = imin,imax
            sla_obs(i,j,bi,bj) = spval
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF

c mask data:
      DO bj = jtlo,jthi
        DO bi = itlo,ithi
          k = 1
          DO j = jmin,jmax
            DO i = imin,imax
              IF (_hFacC(i,j,k,bi,bj) .EQ. 0.) THEN
                 sla_mask(i,j,bi,bj) = 0. _d 0
              ELSE
                 sla_mask(i,j,bi,bj) = 1. _d 0
              ENDIF
              IF (sla_obs(i,j,bi,bj) .LE. spval) THEN
                sla_mask(i,j,bi,bj) = 0. _d 0
              ENDIF
              IF (abs(sla_obs(i,j,bi,bj)) .LT. 1.d-8 ) THEN
                 sla_mask(i,j,bi,bj) = 0. _d 0
              ENDIF

#ifndef ALLOW_SHALLOW_ALTIMETRY
              IF ( R_low(i,j,bi,bj) .GT. -200. ) THEN
                sla_mask(i,j,bi,bj) = 0. _d 0
              ENDIF
#endif
#ifndef ALLOW_HIGHLAT_ALTIMETRY
              IF ( abs(YC(i,j,bi,bj)) .GT. 66. ) THEN
                sla_mask(i,j,bi,bj) = 0. _d 0
              ENDIF
#endif

              sla_mask(i,j,bi,bj) = sla_mask(i,j,bi,bj)*frame(i,j)
              sla_obs(i,j,bi,bj)  = sla_mask(i,j,bi,bj)*factor*
     &             ( sla_obs(i,j,bi,bj) -
     &               ( sla_intercept + sla_slope*irec*hoursperday
     &                *daysperavgperiod  ) )
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      RETURN
      END
