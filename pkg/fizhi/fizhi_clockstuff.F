C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_clockstuff.F,v 1.31 2012/03/22 14:22:32 jmc Exp $
C $Name:  $

#include "FIZHI_OPTIONS.h"

C--  File fizhi_clockstuff.F:
C--   Contents
C--   o SET_ALARM
C--   o GET_ALARM
C--   o ALARM      (function)
C--   o ALARM2     (function)
C--   o ALARM2NEXT (function)
C--   o SET_TIME
C--   o GET_TIME
C--   o NSECF      (function)
C--   o NHMSF      (function)
C--   o NSECF2     (function)
C--   o FIXDATE
C--   o INTERP_TIME
C--   o TICK
C--   o TIC_TIME
C--   o NALARM     (function)
C--   o NALARM2    (function)
C--   o INCYMD     (function)
C--   o ASTRO
C--   o TIME_BOUND
C--   o TIME2FREQ2

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      subroutine set_alarm (tag,datein,timein,freq)
C***********************************************************************
C  Purpose
C  -------
C     Utility to Set Internal Alarms
C
C  Argument Description
C  --------------------
C     tag ....... Character String Tagging Alarm Process
C     date ...... Begining Date for Alarm
C     time ...... Begining Time for Alarm
C     freq ...... Repeating Frequency Interval for Alarm
C
C***********************************************************************

      implicit none
#include "EEPARAMS.h"
#include "chronos.h"

      character*(*) tag
      integer       freq,datein,timein

C-    functions:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C-    local variables:
      integer myid
      logical first,set
      data          first /.true./
      integer n, iL

      myid = 1
      if(first) then
         ntags    = 1
          tags(1) = tag
         freqs(1) = freq
         dates(1) = datein
         times(1) = timein
         iL = ILNBLNK(tag)
         WRITE(standardMessageUnit,'(A,I8,A,I6.6,A,I10,2A)')
     &    '  Set Alarm for: ', datein, '  ', timein,
     &    ', with frequency: ', freq, ', and Tag: ',tag(1:iL)
      else

      set = .false.
      do n=1,ntags
       if(tag.eq.tags(n)) then
        if( myid.eq.1 ) then
         print *, 'Warning!  Alarm has already been set for Tag: ',tag
         print *, 'Changing  Alarm Information:'
         print *, 'Frequency: ',freqs(n),' (Old) ',freq,' (New)'
         print *, '    Date0: ',dates(n),' (Old) ',datein,' (New)'
         print *, '    Time0: ',times(n),' (Old) ',timein,' (New)'
        endif
        freqs(n) = freq
        dates(n) = datein
        times(n) = timein
        set = .true.
       endif
      enddo
      if(.not.set) then
            ntags = ntags+1
         if(ntags.gt.maxtag ) then
            if( myid.eq.1 ) then
            print *, 'Too many Alarms are Set!!'
            print *, 'Maximum Number of Alarms = ',maxtag
            endif
         call my_finalize
         call my_exit (101)
         endif
          tags(ntags) = tag
         freqs(ntags) = freq
         dates(ntags) = datein
         times(ntags) = timein
         iL = ILNBLNK(tag)
         WRITE(standardMessageUnit,'(A,I8,A,I6.6,A,I10,2A)')
     &    '  Set Alarm for: ', datein, '  ', timein,
     &    ', with frequency: ', freq, ', and Tag: ',tag(1:iL)
      endif
      endif

      first = .false.
      return
      end

      subroutine get_alarm (tag,datein,timein,freq,tleft)
C***********************************************************************
C  Purpose
C  -------
C     Utility to Get Internal Alarm Information
C
C  Input
C  -----
C     tag ....... Character String Tagging Alarm Process
C
C  Output
C  ------
C     datein ...... Begining  Date for Alarm
C     timein ...... Begining  Time for Alarm
C     freq ........ Frequency Interval for Alarm
C     tleft ....... Time Remaining (seconds) before Alarm is TRUE
C
C***********************************************************************

      implicit none
      character*(*) tag
      integer freq,datein,timein,tleft

#include "chronos.h"

      logical set,alarm
      external alarm
      integer myid,n,nalarm,nsecf

      myid = 1
      set = .false.
      do n=1,ntags
       if (tag.eq.tags(n)) then
         freq   = freqs(n)
         datein = dates(n)
         timein = times(n)

         if ( alarm(tag) ) then
          tleft = 0
         else
          call get_time (nymd,nhms)
          tleft = nsecf(freq) - nalarm(freq,nymd,nhms,datein,timein )
         endif

         set = .true.
       endif
      enddo

      if(.not.set) then
      if( myid.eq.1 ) print *, 'Alarm has not been set for Tag: ',tag
      freq  = 0
      datein  = 0
      timein  = 0
      tleft = 0
      endif

      return
      end

      LOGICAL FUNCTION ALARM (tag)
      implicit none
      character*(*) tag
#include "chronos.h"

      integer datein,timein
      integer n,nalarm
      external nalarm

      call get_time (datein,timein)

      alarm = .false.
      do n=1,ntags
       if( tags(n).eq.tag  ) then
        if( freqs(n).eq.0 ) then
          alarm = (dates(n).eq.datein) .and. (times(n).eq.timein)
        else
          alarm = ( datein.gt.dates(n) .or.
     &             (datein.eq.dates(n) .and. timein.ge.times(n)) )
     &    .and. nalarm( freqs(n),datein,timein,dates(n),times(n) ).eq.0
        endif
       endif
      enddo

      return
      end

      LOGICAL FUNCTION ALARM2 (tag)
      implicit none
      character*(*) tag
#include "chronos.h"

      integer datein,timein
      integer n,nalarm2
      external nalarm2

      call get_time (datein,timein)

      alarm2 = .false.
      do n=1,ntags
       if( tags(n).eq.tag  ) then
        if( freqs(n).eq.0 ) then
          alarm2 = (dates(n).eq.datein) .and. (times(n).eq.timein)
        else
          alarm2 = ( datein.gt.dates(n) .or.
     &              (datein.eq.dates(n) .and. timein.ge.times(n)) )
     &    .and. nalarm2( freqs(n),datein,timein,dates(n),times(n) ).eq.0
        endif
       endif
      enddo

      return
      end

      LOGICAL FUNCTION ALARM2NEXT (tag,deltat)
      implicit none
      character*(*) tag
      _RL deltat
#include "chronos.h"

      integer datein,timein,ndt
      integer dateminus,timeminus
      integer n,nalarm2
      external nalarm2

      ndt = int(deltat)
      call get_time (dateminus,timeminus)
      datein = dateminus
      timein = timeminus
      call tick(datein,timein,ndt)

      alarm2next = .false.
      do n=1,ntags
       if( tags(n).eq.tag  ) then
        if( freqs(n).eq.0 ) then
          alarm2next = (dates(n).eq.datein) .and. (times(n).eq.timein)
        else
          alarm2next = ( datein.gt.dates(n) .or.
     &                  (datein.eq.dates(n) .and. timein.ge.times(n)) )
     &    .and. nalarm2( freqs(n),datein,timein,dates(n),times(n) ).eq.0
        endif
       endif
      enddo

      return
      end

      subroutine set_time (datein,timein)
      implicit none
      integer  datein,timein

#include "chronos.h"

      integer myid

      myid = 1
      if(  myid.eq.1 ) then
      print *, 'Setting Clock'
      print *, 'Date: ',datein
      print *, 'Time: ',timein
      endif

      nymd = datein
      nhms = timein
      return
      end

      subroutine get_time (datein,timein)
      implicit none
      integer datein,timein

#include "chronos.h"

      datein = nymd
      timein = nhms
      return
      end

      function nsecf (nhms)
C***********************************************************************
C  Purpose
C     Converts NHMS format to Total Seconds
C
C***********************************************************************
      implicit none
      integer  nhms, nsecf
      nsecf =  nhms/10000*3600 + mod(nhms,10000)/100*60 + mod(nhms,100)
      return
      end

      function nhmsf (nsec)
C***********************************************************************
C  Purpose
C     Converts Total Seconds to NHMS format
C
C***********************************************************************
      implicit none
      integer  nhmsf, nsec
      nhmsf =  nsec/3600*10000 + mod(nsec,3600)/60*100 + mod(nsec,60)
      return
      end

      function nsecf2 (nhhmmss,nmmdd,nymd)
C***********************************************************************
C  Purpose
C     Computes the Total Number of seconds from NYMD using NHHMMSS & NMMDD
C
C  Arguments   Description
C     NHHMMSS  IntervaL Frequency (HHMMSS)
C     NMMDD    Interval Frequency (MMDD)
C     NYMD     Current  Date      (YYMMDD)
C
C  NOTE:
C     IF (NMMDD.ne.0), THEN HOUR FREQUENCY HH MUST BE < 24
C
C***********************************************************************
      implicit none

      integer nsecf2,nhhmmss,nmmdd,nymd

      INTEGER NSDAY, NCYCLE
      PARAMETER ( NSDAY  = 86400 )
      PARAMETER ( NCYCLE = 1461*24*3600 )

      INTEGER YEAR, MONTH, DAY

c     INTEGER  MNDY(12,4)
      INTEGER  MNDY(12*4)
      DATA MNDY /0,31,60,91,121,152,182,213,244,274,305,335,366,
     &           397,34*0 /

      integer nsecf,i,nsegm,nsegd,iday,iday2,nday

C***********************************************************************
C*                 COMPUTE # OF SECONDS FROM NHHMMSS                   *
C***********************************************************************

      nsecf2 = nsecf( nhhmmss )

      if( nmmdd.eq.0 ) return

C***********************************************************************
C*                 COMPUTE # OF DAYS IN A 4-YEAR CYCLE                 *
C***********************************************************************

      DO I=15,48
c     MNDY(I,1) = MNDY(I-12,1) + 365
        MNDY(I) = MNDY(I-12) + 365
      ENDDO

C***********************************************************************
C*                 COMPUTE # OF SECONDS FROM NMMDD                     *
C***********************************************************************

      nsegm =     nmmdd/100
      nsegd = mod(nmmdd,100)

      YEAR   = NYMD / 10000
      MONTH  = MOD(NYMD,10000) / 100
      DAY    = MOD(NYMD,100)

c     IDAY   = MNDY( MONTH ,MOD(YEAR ,4)+1 )
      IDAY   = MNDY( MONTH +12*MOD(YEAR ,4) )
      month = month + nsegm
      If( month.gt.12 ) then
      month = month - 12
      year = year + 1
      endif
c     IDAY2  = MNDY( MONTH ,MOD(YEAR ,4)+1 )
      IDAY2  = MNDY( MONTH +12*MOD(YEAR ,4) )

                    nday = iday2-iday
      if(nday.lt.0) nday = nday + 1461
                    nday = nday + nsegd

      nsecf2 = nsecf2 + nday*nsday

      return
      end

      subroutine fixdate (nymd)
      implicit none
      integer nymd

C Modify 6-digit YYMMDD for dates between 1950-2050
C -------------------------------------------------
      if (nymd .lt. 500101) then
        nymd = 20000000 + nymd
      else if (nymd .le. 991231) then
        nymd = 19000000 + nymd
      endif

      return
      end

      subroutine interp_time ( nymd ,nhms ,
     &                         nymd1,nhms1, nymd2,nhms2, fac1,fac2 )
C***********************************************************************
C
C  PURPOSE:
C  ========
C    Compute interpolation factors, fac1 & fac2, to be used in the
C    calculation of the instantanious boundary conditions, ie:
C
C               q(i,j) = fac1*q1(i,j) + fac2*q2(i,j)
C    where:
C               q(i,j) => Boundary Data valid    at (nymd  , nhms )
C              q1(i,j) => Boundary Data centered at (nymd1 , nhms1)
C              q2(i,j) => Boundary Data centered at (nymd2 , nhms2)
C
C  INPUT:
C  ======
C    nymd     : Date (yymmdd) of Current Timestep
C    nhms     : Time (hhmmss) of Current Timestep
C    nymd1    : Date (yymmdd) of Boundary Data 1
C    nhms1    : Time (hhmmss) of Boundary Data 1
C    nymd2    : Date (yymmdd) of Boundary Data 2
C    nhms2    : Time (hhmmss) of Boundary Data 2
C
C  OUTPUT:
C  =======
C    fac1     : Interpolation factor for Boundary Data 1
C    fac2     : Interpolation factor for Boundary Data 2
C
C
C***********************************************************************
      implicit none

      integer nhms,nymd,nhms1,nymd1,nhms2,nymd2
      _RL fac1,fac2

      INTEGER  YEAR , MONTH , DAY , SEC
      INTEGER  YEAR1, MONTH1, DAY1, SEC1
      INTEGER  YEAR2, MONTH2, DAY2, SEC2

      _RL time00, time1, time2

      INTEGER    DAYSCY
      parameter ( dayscy   = 365*4 + 1 )

      INTEGER MNDY(12*4)

      LOGICAL FIRST
      DATA    FIRST/.TRUE./

      DATA MNDY /0,31,60,91,121,152,182,213,244,274,305,335,366,
     &           397,34*0 /

      integer i,nsecf

C***********************************************************************
C*                         SET TIME BOUNDARIES                         *
C***********************************************************************

      YEAR   = NYMD / 10000
      MONTH  = MOD(NYMD,10000) / 100
      DAY    = MOD(NYMD,100)
      SEC    = NSECF(NHMS)

      YEAR1  = NYMD1 / 10000
      MONTH1 = MOD(NYMD1,10000) / 100
      DAY1   = MOD(NYMD1,100)
      SEC1   = NSECF(NHMS1)

      YEAR2  = NYMD2 / 10000
      MONTH2 = MOD(NYMD2,10000) / 100
      DAY2   = MOD(NYMD2,100)
      SEC2   = NSECF(NHMS2)

C***********************************************************************
C*                    COMPUTE DAYS IN 4-YEAR CYCLE                     *
C***********************************************************************

      IF(FIRST) THEN
      DO I=15,48
        MNDY(I) = MNDY(I-12) + 365
      ENDDO
      FIRST=.FALSE.
      ENDIF

C***********************************************************************
C*                     COMPUTE INTERPOLATION FACTORS                   *
C***********************************************************************

      time00 = DAY + MNDY(MONTH +12*MOD(YEAR ,4)) + float(sec )/86400.
      time1 = DAY1 + MNDY(MONTH1+12*MOD(YEAR1,4)) + float(sec1)/86400.
      time2 = DAY2 + MNDY(MONTH2+12*MOD(YEAR2,4)) + float(sec2)/86400.

      if( time00 .lt.time1 ) time00  = time00  + dayscy
      if( time2.lt.time1 ) time2 = time2 + dayscy

      fac1  = (time2-time00)/(time2-time1)
      fac2  = (time00-time1)/(time2-time1)

      RETURN
      END

      subroutine tick (nymd,nhms,ndt)
C***********************************************************************
C  Purpose
C     Tick the Date (nymd) and Time (nhms) by NDT (seconds)
C
C***********************************************************************
      implicit none

      integer nymd,nhms,ndt

      integer nsec,nsecf,incymd,nhmsf

      IF(NDT.NE.0) THEN
      NSEC = NSECF(NHMS) + NDT

      IF (NSEC.GT.86400)  THEN
      DO WHILE (NSEC.GT.86400)
      NSEC = NSEC - 86400
      NYMD = INCYMD (NYMD,1)
      ENDDO
      ENDIF

      IF (NSEC.EQ.86400)  THEN
      NSEC = 0
      NYMD = INCYMD (NYMD,1)
      ENDIF

      IF (NSEC.LT.00000)  THEN
      DO WHILE (NSEC.LT.0)
      NSEC = 86400 + NSEC
      NYMD = INCYMD (NYMD,-1)
      ENDDO
      ENDIF

      NHMS = NHMSF (NSEC)
      ENDIF

#ifdef FIZHI_USE_FIXED_DAY
      NYMD = 20040321
#endif

      RETURN
      END

      subroutine tic_time (mymd,mhms,ndt)
C***********************************************************************
C  PURPOSE
C     Tick the Clock by NDT (seconds)
C
C***********************************************************************
      implicit none
#include "chronos.h"

      integer mymd,mhms,ndt

      integer nsec,nsecf,incymd,nhmsf

      IF(NDT.NE.0) THEN
      NSEC = NSECF(NHMS) + NDT

      IF (NSEC.GT.86400)  THEN
      DO WHILE (NSEC.GT.86400)
      NSEC = NSEC - 86400
      NYMD = INCYMD (NYMD,1)
      ENDDO
      ENDIF

      IF (NSEC.EQ.86400)  THEN
      NSEC = 0
      NYMD = INCYMD (NYMD,1)
      ENDIF

      IF (NSEC.LT.00000)  THEN
      DO WHILE (NSEC.LT.0)
      NSEC = 86400 + NSEC
      NYMD = INCYMD (NYMD,-1)
      ENDDO
      ENDIF

      NHMS = NHMSF (NSEC)
      ENDIF

C Pass Back Current Updated Time
C ------------------------------
      mymd = nymd
      mhms = nhms

      RETURN
      END

      FUNCTION NALARM (MHMS,NYMD,NHMS,NYMD0,NHMS0)
C***********************************************************************
C  PURPOSE
C     COMPUTES MODULO-FRACTION BETWEEN MHHS AND TOTAL TIME
C  USAGE
C  ARGUMENTS   DESCRIPTION
C     MHMS     INTERVAL FREQUENCY (HHMMSS)
C     NYMD     CURRENT   YYMMDD
C     NHMS     CURRENT   HHMMSS
C     NYMD0    BEGINNING YYMMDD
C     NHMS0    BEGINNING HHMMSS
C
C***********************************************************************
      implicit none

      integer nalarm,MHMS,NYMD,NHMS,NYMD0,NHMS0

      integer nsday, ncycle
      PARAMETER ( NSDAY  = 86400 )
      PARAMETER ( NCYCLE = 1461*24*3600 )

      INTEGER YEAR, MONTH, DAY, SEC, YEAR0, MONTH0, DAY0, SEC0

      integer MNDY(12*4)
      DATA MNDY /0,31,60,91,121,152,182,213,244,274,305,335,366,
     &           397,34*0 /

      integer i,nsecf,iday,iday0,nsec,nsec0,ntime

C***********************************************************************
C*                 COMPUTE # OF DAYS IN A 4-YEAR CYCLE                 *
C***********************************************************************

      DO I=15,48
        MNDY(I) = MNDY(I-12) + 365
      ENDDO

C***********************************************************************
C*                   SET CURRENT AND BEGINNING TIMES                   *
C***********************************************************************

      YEAR   = NYMD / 10000
      MONTH  = MOD(NYMD,10000) / 100
      DAY    = MOD(NYMD,100)
      SEC    = NSECF(NHMS)

      YEAR0  = NYMD0 / 10000
      MONTH0 = MOD(NYMD0,10000) / 100
      DAY0   = MOD(NYMD0,100)
      SEC0   = NSECF(NHMS0)

C***********************************************************************
C*      COMPUTE POSITIONS IN CYCLE FOR CURRENT AND BEGINNING TIMES     *
C***********************************************************************

      IDAY   = (DAY -1) + MNDY( MONTH +12*MOD(YEAR ,4) )
      IDAY0  = (DAY0-1) + MNDY( MONTH0+12*MOD(YEAR0,4) )

      NSEC   = IDAY *NSDAY + SEC
      NSEC0  = IDAY0*NSDAY + SEC0

                       NTIME  = NSEC-NSEC0
      IF (NTIME.LT.0 ) NTIME  = NTIME + NCYCLE
                       NALARM = NTIME
      IF ( MHMS.NE.0 ) NALARM = MOD( NALARM,NSECF(MHMS) )

      RETURN
      END

      FUNCTION NALARM2(MHMS,NYMD,NHMS,NYMD0,NHMS0)
C***********************************************************************
C  PURPOSE
C     COMPUTES MODULO-FRACTION BETWEEN MHHS AND TOTAL TIME
C  USAGE
C  ARGUMENTS   DESCRIPTION
C     MHMS     INTERVAL FREQUENCY (MMDDHHMMSS)
C     NYMD     CURRENT   YYMMDD
C     NHMS     CURRENT   HHMMSS
C     NYMD0    BEGINNING YYMMDD
C     NHMS0    BEGINNING HHMMSS
C
C***********************************************************************
      implicit none

      integer nalarm2,MHMS,NYMD,NHMS,NYMD0,NHMS0

      integer nsday, ncycle
      PARAMETER ( NSDAY  = 86400 )
      PARAMETER ( NCYCLE = 1461*24*3600 )

      INTEGER YEAR, MONTH, DAY, SEC, YEAR0, MONTH0, DAY0, SEC0

      integer MNDY(12*4)
      DATA MNDY /0,31,60,91,121,152,182,213,244,274,305,335,366,
     &           397,34*0 /
      INTEGER NDPM(12)
      DATA NDPM /31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/

      integer i,nsecf,iday,iday0,nsec,nsec0,ntime
      integer NHMMSS,NMMDD
      integer iloop
      integer testnymd,testnhms,testndpm

C***********************************************************************
C*                 COMPUTE # OF DAYS IN A 4-YEAR CYCLE                 *
C***********************************************************************

      DO I=15,48
        MNDY(I) = MNDY(I-12) + 365
      ENDDO

C***********************************************************************
C*                   SET CURRENT AND BEGINNING TIMES                   *
C***********************************************************************

      YEAR   = NYMD / 10000
      MONTH  = MOD(NYMD,10000) / 100
      DAY    = MOD(NYMD,100)
      SEC    = NSECF(NHMS)

      YEAR0  = NYMD0 / 10000
      MONTH0 = MOD(NYMD0,10000) / 100
      DAY0   = MOD(NYMD0,100)
      SEC0   = NSECF(NHMS0)

C***********************************************************************
C*      COMPUTE POSITIONS IN CYCLE FOR CURRENT AND BEGINNING TIMES     *
C***********************************************************************

      IDAY   = (DAY -1) + MNDY( MONTH +12*MOD(YEAR ,4) )
      IDAY0  = (DAY0-1) + MNDY( MONTH0+12*MOD(YEAR0,4) )

      NSEC   = IDAY *NSDAY + SEC
      NSEC0  = IDAY0*NSDAY + SEC0

      NTIME  = NSEC-NSEC0
      IF(NTIME.LT.0) NTIME  = NTIME + NCYCLE
      NALARM2 = NTIME
      IF(MHMS.NE.0)NALARM2 = MOD( NALARM2,NSECF(MHMS) )
      IF(MHMS.GE.1000000) THEN
       testnymd=nymd0
       testnhms=nhms0
       NMMDD = MHMS / 1000000
       NHMMSS = MOD(MHMS,1000000)
       do iloop=1,100000
        testnymd=testnymd + nmmdd
        testnhms=testnhms + nhmmss
        year0=testnymd/10000
        month0=mod(testnymd,10000)/100
        day0 = mod(testnymd,100)
        testndpm = ndpm(month0)
        if( month0.eq.2  .and. mod(year0,4).eq.0) testndpm = 29
        if(testnhms.ge.240000) then
         testnhms = testnhms-240000
         testnymd = testnymd + 1
         day0 = day0 + 1
        endif
        if(day0.gt.testndpm) then
         testnymd = testnymd - testndpm
         testnymd = testnymd + 100
         day0 = day0 - testndpm
         month0 = month0 + 1
        endif
        if(month0.gt.12) then
         month0 = month0 - 12
         year0 = year0 + 1
         testnymd = testnymd + 10000 - 1200
        endif
        sec0 = nsecf(testnhms)
        iday0 = (day0-1) + MNDY(month0+12*mod(year0,4) )
        nsec0 = iday0 *nsday + sec0
        if( (testnymd.gt.nymd) .or.
     &         (testnymd.eq.testnymd) .and. (testnhms.gt.nhms) )
     &                    go to 200
        nalarm2 = nsec-nsec0
       enddo
 200   continue
      ENDIF

      RETURN
      END

      FUNCTION INCYMD (NYMD,M)
C***********************************************************************
C  PURPOSE
C     INCYMD:  NYMD CHANGED BY ONE DAY
C     MODYMD:  NYMD CONVERTED TO JULIAN DATE
C  DESCRIPTION OF INPUT VARIABLES
C     NYMD     CURRENT DATE IN YYMMDD FORMAT
C     M        +/- 1 (DAY ADJUSTMENT)
C
C***********************************************************************
      implicit none
      integer incymd,nymd,m

      integer ny,nm,nd,ny00,modymd

      INTEGER NDPM(12)
      DATA NDPM /31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/
      LOGICAL LEAP
      DATA NY00 /1900 /
      LEAP(NY) = MOD(NY,4).EQ.0 .AND. (NY.NE.0 .OR. MOD(NY00,400).EQ.0)

C***********************************************************************
C
      NY = NYMD / 10000
      NM = MOD(NYMD,10000) / 100
      ND = MOD(NYMD,100) + M

      IF (ND.EQ.0) THEN
      NM = NM - 1
      IF (NM.EQ.0) THEN
          NM = 12
          NY = NY - 1
      ENDIF
      ND = NDPM(NM)
      IF (NM.EQ.2 .AND. LEAP(NY))  ND = 29
      ENDIF

      IF (ND.EQ.29 .AND. NM.EQ.2 .AND. LEAP(NY))  GO TO 20

      IF (ND.GT.NDPM(NM)) THEN
      ND = 1
      NM = NM + 1
      IF (NM.GT.12) THEN
          NM = 1
          NY = NY + 1
      ENDIF
      ENDIF

   20 CONTINUE
      INCYMD = NY*10000 + NM*100 + ND

      RETURN

C***********************************************************************
C                      E N T R Y    M O D Y M D
C***********************************************************************

      ENTRY MODYMD (NYMD)

      NY = NYMD / 10000
      NM = MOD(NYMD,10000) / 100
      ND = MOD(NYMD,100)

   40 CONTINUE
      IF (NM.LE.1)  GO TO 60
      NM = NM - 1
      ND = ND + NDPM(NM)
      IF (NM.EQ.2 .AND. LEAP(NY))  ND = ND + 1
      GO TO 40

   60 CONTINUE
      MODYMD = ND

      RETURN
      END

      SUBROUTINE ASTRO ( NYMD,NHMS,ALAT,ALON,IRUN,COSZ,RA )
C***********************************************************************
C
C  INPUT:
C  ======
C    NYMD      : CURRENT YYMMDD
C    NHMS      : CURRENT HHMMSS
C    ALAT(IRUN):LATITUDES  IN DEGREES.
C    ALON(IRUN):LONGITUDES IN DEGREES. (0 = GREENWICH, + = EAST).
C    IRUN      : # OF POINTS TO CALCULATE
C
C  OUTPUT:
C  =======
C    COSZ(IRUN)  : COSINE OF ZENITH ANGLE.
C    RA          : EARTH-SUN DISTANCE IN UNITS OF
C                  THE ORBITS SEMI-MAJOR AXIS.
C
C  NOTE:
C  =====
C  THE INSOLATION AT THE TOP OF THE ATMOSPHERE IS:
C
C  S(I) = (SOLAR CONSTANT)*(1/RA**2)*COSZ(I),
C
C  WHERE:
C  RA AND COSZ(I) ARE THE TWO OUTPUTS OF THIS SUBROUTINE.
C
C***********************************************************************

      implicit none

C Input Variables
C ---------------
      integer nymd, nhms, irun
      _RL getcon, cosz(irun), alat(irun), alon(irun), ra

C Local Variables
C ---------------
      integer year, day, sec, month, iday, idayp1
      integer dayscy
      integer i,nsecf,k,km,kp

      _RL hc
      _RL pi, zero, one, two, six, dg2rd, yrlen, eqnx, ob, ecc, per
      _RL daylen, fac, thm, thp, thnow, zs, zc, sj, cj

      parameter ( zero  = 0.0 )
      parameter ( one   = 1.0 )
      parameter ( two   = 2.0 )
      parameter ( six   = 6.0 )
      parameter ( dayscy   = 365*4 + 1 )

      _RL      TH(DAYSCY),T0,T1,T2,T3,T4,FUN,Y
      INTEGER  MNDY(12*4)

      LOGICAL FIRST
      DATA    FIRST/.TRUE./
      SAVE

      DATA MNDY /0,31,60,91,121,152,182,213,244,274,305,335,366,
     &           397,34*0 /

      FUN(Y,PI,ECC,YRLEN,PER) = (TWO*PI/((ONE-ECC**2)**1.5))*(ONE/YRLEN)
     &       * (ONE - ECC*COS(Y-PER)) ** 2

C***********************************************************************
C*                         SET SOME CONSTANTS                          *
C***********************************************************************
      pi = getcon('PI')
      dg2rd = getcon('DEG2RAD')
      yrlen = getcon('YRLEN')
      ob =  getcon('OBLDEG') * dg2rd
      daylen = getcon('SDAY')
      eqnx = getcon('VERNAL EQUINOX')
      ecc = getcon('ECCENTRICITY')
      per = getcon('PERIHELION') * dg2rd

C***********************************************************************
C*                          SET CURRENT TIME                           *
C***********************************************************************

      YEAR  = NYMD / 10000
      MONTH = MOD(NYMD,10000) / 100
      DAY   = MOD(NYMD,100)
      SEC   = NSECF(NHMS)

C***********************************************************************
C*                 COMPUTE DAY-ANGLES FOR 4-YEAR CYCLE                 *
C***********************************************************************

      IF(FIRST) THEN
           DO 100 I=15,48
             MNDY(I) = MNDY(I-12) + 365
100        CONTINUE

           KM  = INT(EQNX) + 1
           FAC = KM-EQNX
           T0 = ZERO
           T1 = FUN(T0,PI,ECC,YRLEN,PER         )*FAC
           T2 = FUN(ZERO+T1/TWO,PI,ECC,YRLEN,PER)*FAC
           T3 = FUN(ZERO+T2/TWO,PI,ECC,YRLEN,PER)*FAC
           T4 = FUN(ZERO+T3,PI,ECC,YRLEN,PER    )*FAC
           TH(KM) = (T1 + TWO*(T2 + T3) + T4) / SIX

           DO 200 K=2,DAYSCY
           T1 = FUN(TH(KM),PI,ECC,YRLEN,PER       )
           T2 = FUN(TH(KM)+T1/TWO,PI,ECC,YRLEN,PER)
           T3 = FUN(TH(KM)+T2/TWO,PI,ECC,YRLEN,PER)
           T4 = FUN(TH(KM)+T3,PI,ECC,YRLEN,PER    )
           KP = MOD(KM,DAYSCY) + 1
           TH(KP) = TH(KM) + (T1 + TWO*(T2 + T3) + T4) / SIX
           KM = KP
 200       CONTINUE

           FIRST=.FALSE.
      ENDIF

C***********************************************************************
C*            COMPUTE EARTH-SUN DISTANCE TO CURRENT SECOND             *
C***********************************************************************

      IDAY   = DAY + MNDY(MONTH+12*MOD(YEAR,4) )
      IDAYP1 = MOD( IDAY,DAYSCY) + 1
      THM    = MOD( TH(IDAY)  ,TWO*PI)
      THP    = MOD( TH(IDAYP1),TWO*PI)

      IF(THP.LT.THM) THP = THP + TWO*PI
      FAC   = FLOAT(SEC)/DAYLEN
      THNOW = THM*(ONE-FAC) + THP*FAC

      ZS = SIN(THNOW) * SIN(OB)
      ZC = SQRT(ONE-ZS*ZS)
      RA = (1.-ECC*ECC) / ( ONE-ECC*COS(THNOW-PER) )

C***********************************************************************
C*                 COMPUTE COSINE OF THE ZENITH ANGLE                  *
C***********************************************************************

      FAC  = FAC*TWO*PI + PI
      DO I = 1,IRUN

      HC = COS( FAC+ALON(I)*DG2RD )
      SJ = SIN(ALAT(I)*DG2RD)
      CJ = SQRT(ONE-SJ*SJ)

          COSZ(I) = SJ*ZS + CJ*ZC*HC
      IF( COSZ(I).LT.ZERO ) COSZ(I) = ZERO
      ENDDO

      RETURN
      END

      subroutine time_bound(nymd,nhms,nymd1,nhms1,nymd2,nhms2,imnm,imnp)
C***********************************************************************
C  PURPOSE
C     Compute Date and Time boundaries.
C
C  ARGUMENTS   DESCRIPTION
C     nymd .... Current    Date
C     nhms .... Current    Time
C     nymd1 ... Previous   Date Boundary
C     nhms1 ... Previous   Time Boundary
C     nymd2 ... Subsequent Date Boundary
C     nhms2 ... Subsequent Time Boundary
C
C     imnm .... Previous   Time Index for Interpolation
C     imnp .... Subsequent Time Index for Interpolation
C
C***********************************************************************

      implicit none
      integer  nymd,nhms, nymd1,nhms1, nymd2,nhms2

C Local Variables
C ---------------
      integer  month,day,nyear,midmon1,midmon,midmon2
      integer  imnm,imnp
      INTEGER  DAYS(14), daysm, days0, daysp
      DATA     DAYS /31,31,28,31,30,31,30,31,31,30,31,30,31,31/

      integer nmonf,ndayf,n
      NMONF(N) = MOD(N,10000)/100
      NDAYF(N) = MOD(N,100)

C*********************************************************************
C**** Find Proper Month and Time Boundaries for Climatological Data **
C*********************************************************************

      MONTH  = NMONF(NYMD)
      DAY    = NDAYF(NYMD)

      daysm  = days(month  )
      days0  = days(month+1)
      daysp  = days(month+2)

C Check for Leap Year
C -------------------
      nyear = nymd/10000
      if( 4*(nyear/4).eq.nyear ) then
      if( month.eq.3 ) daysm = daysm+1
      if( month.eq.2 ) days0 = days0+1
      if( month.eq.1 ) daysp = daysp+1
      endif

      MIDMON1 = daysm/2 + 1
      MIDMON  = days0/2 + 1
      MIDMON2 = daysp/2 + 1


      IF(DAY.LT.MIDMON) THEN
         imnm = month
         imnp = month + 1
         nymd2 = (nymd/10000)*10000 + month*100 + midmon
         nhms2 = 000000
         nymd1 = nymd2
         nhms1 = nhms2
         call tick ( nymd1,nhms1,       -midmon  *86400 )
         call tick ( nymd1,nhms1,-(daysm-midmon1)*86400 )
      ELSE
         IMNM = MONTH + 1
         IMNP = MONTH + 2
         nymd1 = (nymd/10000)*10000 + month*100 + midmon
         nhms1 = 000000
         nymd2 = nymd1
         nhms2 = nhms1
         call tick ( nymd2,nhms2,(days0-midmon)*86400 )
         call tick ( nymd2,nhms2,       midmon2*86400 )
      ENDIF

C -------------------------------------------------------------
C Note:  At this point, imnm & imnp range between 01-14, where
C        01    -> Previous years December
C        02-13 -> Current  years January-December
C        14    -> Next     years January
C -------------------------------------------------------------

      imnm = imnm-1
      imnp = imnp-1

      if( imnm.eq.0  ) imnm = 12
      if( imnp.eq.0  ) imnp = 12
      if( imnm.eq.13 ) imnm = 1
      if( imnp.eq.13 ) imnp = 1

      return
      end
      subroutine time2freq2(MMDD,NYMD,NHMS,timeleft)
C***********************************************************************
C  PURPOSE
C     COMPUTES TIME IN SECONDS UNTIL WE REACH THE NEXT MMDD
C       (ASSUME that the target time is 0Z)
C
C  ARGUMENTS   DESCRIPTION
C     MMDD     FREQUENCY (MMDDHHMMSS)
C     NYMD     CURRENT   YYMMDD
C     NHMS     CURRENT   HHMMSS
C     TIMELEFT TIME LEFT (SECONDS)
C
C  NOTES - Only used when the frequency is in units of months
C          Assumes that we always want to be at a month boundary
C***********************************************************************
      implicit none

      integer mmdd,nymd,nhms,timeleft,daysleft

      integer nsday
      PARAMETER ( NSDAY  = 86400 )
      integer year, month, day, sec
      integer yearnext, monthnext, daynext
      integer i,nsecf,iday,idaynext,nsec
      integer testnymd
      integer MNDY(12*4)
      DATA MNDY /0,31,60,91,121,152,182,213,244,274,305,335,366,
     &           397,34*0 /

C***********************************************************************
C*                 COMPUTE # OF DAYS IN A 4-YEAR CYCLE                 *
C***********************************************************************
      DO I=15,48
        MNDY(I) = MNDY(I-12) + 365
      ENDDO
C***********************************************************************
C*                   SET CURRENT TIME ELEMENTS                                 *
C***********************************************************************
      YEAR   = NYMD / 10000
      MONTH  = MOD(NYMD,10000) / 100
      DAY    = MOD(NYMD,100)
      SEC    = NSECF(NHMS)
C***********************************************************************
C*      COMPUTE POSITIONS IN CYCLE FOR CURRENT AND BEGINNING TIMES     *
C***********************************************************************
      IDAY   = (DAY -1) + MNDY( MONTH +12*MOD(YEAR ,4) )
      NSEC   = IDAY *NSDAY + SEC

      testnymd=nymd + mmdd
      yearnext=testnymd/10000
      monthnext=mod(testnymd,10000)/100
      daynext = 1
      if(monthnext.gt.12) then
       monthnext = monthnext - 12
       yearnext = yearnext + 1
      endif
      testnymd = yearnext*10000 + monthnext*100 + daynext
      idaynext = MNDY(monthnext+12*mod(yearnext,4) )
      daysleft = idaynext - iday
      if(daysleft.lt.0) daysleft = daysleft + 1461

      timeleft = daysleft * nsday - sec

      RETURN
      END
