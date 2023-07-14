#include "ECCO_OPTIONS.h"

      subroutine cost_sla_read_yd(
     I                sla_file, sla_startdate, use_mon,
     O                sla_obs, sla_mask,
     I                year, irec, myThid )

c     ==================================================================
c     SUBROUTINE cost_sla_read_yd
c     ==================================================================
c
c     o Read a given record of the SLA data.
c
c     started: Gael Forget 20-Oct-2009
c
c     Apr-2023: argument use_mon, switches read daily to monthly if true
c
c     ==================================================================
c     SUBROUTINE cost_sla_read_yd
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
      logical use_mon
      _RL sla_obs (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL sla_mask(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      integer year, irec
      integer myThid

c     == external functions ==
      integer  ilnblnk
      external ilnblnk
      integer  cal_IsLeap
      external cal_IsLeap

c     == local variables ==
      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      _RL spval
      _RL factor
cnew(
      integer  il
      character*(MAX_LEN_FNAM) fnametmp
      logical exst
cnew)
      logical read_rec

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

      read_rec=.FALSE.
      IF (use_mon) THEN
        IF ( (irec.GE.1).AND.(irec.LE.12 ) ) THEN
          read_rec = .TRUE.
        ENDIF
      ELSE

        IF ( (irec.GE.1).AND.( (
     &   (cal_IsLeap(year,myThid).EQ.2).AND.(irec.LE.366)
     &   ).OR.(irec.LE.365) ) ) THEN
          read_rec = .TRUE.
        ENDIF
      ENDIF

      IF (read_rec) THEN
        il=ilnblnk(sla_file)
        WRITE(fnametmp,'(2a,i4)')
     &     sla_file(1:il), '_', year
        inquire( file=fnametmp, exist=exst )
        IF (.NOT. exst) THEN
           STOP
        ENDIF

        CALL READ_REC_3D_RL( fnametmp, cost_iprec, 1,
     &                     sla_obs, irec, 1, myThid )

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
     &               sla_obs(i,j,bi,bj)

              ENDDO
            ENDDO
          ENDDO
        ENDDO

      ELSE !IF ( (irec.GE.1).AND...

        DO bj = jtlo,jthi
          DO bi = itlo,ithi
            DO j = jmin,jmax
              DO i = imin,imax
                   sla_obs(i,j,bi,bj) = 0. _d 0
                   sla_mask(i,j,bi,bj) = 0. _d 0
              ENDDO
            ENDDO
          ENDDO
        ENDDO

      ENDIF !IF ( (irec.GE.1).AND...

      RETURN
      END
