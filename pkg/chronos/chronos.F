C $Header: /u/gcmpack/MITgcm/pkg/chronos/chronos.F,v 1.1 2004/07/28 01:26:03 molod Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
      subroutine set_alarm (tag,date,time,freq)
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
      character*(*) tag
      integer       freq,date,time

#ifdef ALLOW_USE_MPI
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#endif

#include "chronos.h"

#ifdef ALLOW_USE_MPI
c MPI Utilities
c -------------
#include "mpif.h"
      integer  mpi_comm_model,ierror
#endif

      integer myid
      logical first,set
      data          first /.true./

      integer n
#ifdef ALLOW_USE_MPI
      call mpi_comm_rank ( mpi_comm_model,myid,ierror )
#else
      myid = 1
#endif

      if(first) then
         ntags    = 1
          tags(1) = tag
         freqs(1) = freq
         dates(1) = date
         times(1) = time
         if( myid.eq.1 ) write(6,100) date,time,freq,tags(1)
      else

      set = .false.
      do n=1,ntags
       if(tag.eq.tags(n)) then
        if( myid.eq.1 ) then
         print *, 'Warning!  Alarm has already been set for Tag: ',tag
         print *, 'Changing  Alarm Information:'
         print *, 'Frequency: ',freqs(n),' (Old) ',freq,' (New)'
         print *, '    Date0: ',dates(n),' (Old) ',date,' (New)'
         print *, '    Time0: ',times(n),' (Old) ',time,' (New)'
        endif
        freqs(n) = freq
        dates(n) = date
        times(n) = time
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
         dates(ntags) = date
         times(ntags) = time
         if( myid.eq.1 ) write(6,100) date,time,freq,tags(ntags)
      endif
      endif

      first = .false.
  100 format(1x,'Setting Alarm for: ',i8,2x,i6.6,',  with frequency: ',
     .       i8,', and Tag: ',a80)
      return
      end

      subroutine get_alarm (tag,date,time,freq,tleft)
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
C     date ...... Begining  Date for Alarm
C     time ...... Begining  Time for Alarm
C     freq ...... Frequency Interval for Alarm
C     tleft ..... Time Remaining (seconds) before Alarm is TRUE
C
C***********************************************************************        

      implicit none
      character*(*) tag
      integer freq,date,time,tleft

#ifdef ALLOW_USE_MPI
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#endif

#include "chronos.h"

#ifdef ALLOW_USE_MPI
c MPI Utilities
c -------------
#include "mpif.h"
      integer  mpi_comm_model,ierror
#endif

      logical set,alarm
      external alarm
      integer myid,n,nalarm,nsecf

#ifdef ALLOW_USE_MPI
      call mpi_comm_rank ( mpi_comm_model,myid,ierror )
#else
      myid = 1
#endif

      set = .false.
      do n=1,ntags
      if(tag.eq.tags(n)) then
       freq  = freqs(n)
       date  = dates(n)
       time  = times(n)

       if( alarm(tag) ) then
       tleft = 0
       else
       call get_time (nymd,nhms)
       tleft = nsecf(freq) - nalarm(freq,nymd,nhms,date,time )
       endif

       set = .true.
      endif
      enddo

      if(.not.set) then
      if( myid.eq.1 ) print *, 'Alarm has not been set for Tag: ',tag
      freq  = 0
      date  = 0
      time  = 0
      tleft = 0
      endif

      return
      end

      function alarm (tag)
      implicit none
      character*(*) tag
      integer date,time
      logical alarm
#include "chronos.h"

      integer n,modalarm,nalarm,freq,date0,time0
      modalarm(freq,date0,time0) = nalarm (freq,date,time,date0,time0 )

      call get_time (date,time)

      alarm = .false.
      do n=1,ntags
      if( tags(n).eq.tag  ) then
          if( freqs(n).eq.0 ) then
          alarm = (dates(n).eq.date) .and. (times(n).eq.time)
          else
          alarm = ( date.gt.dates(n) .or.
     .             (date.eq.dates(n) .and. time.ge.times(n)) ) .and.
     .              modalarm( freqs(n),dates(n),times(n) ).eq.0
          endif
      endif
      enddo

      return
      end

      subroutine set_time (date,time)
      implicit none
      integer  date,time

#ifdef ALLOW_USE_MPI
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#endif

#include "chronos.h"

#ifdef ALLOW_USE_MPI
c MPI Utilities
c -------------
#include "mpif.h"
      integer  mpi_comm_model,ierror
#endif
      integer myid

#ifdef ALLOW_USE_MPI
      call mpi_comm_rank ( mpi_comm_model,myid,ierror )
#else
      myid = 1
#endif
      if(  myid.eq.1 ) then
      print *, 'Setting Clock'
      print *, 'Date: ',date
      print *, 'Time: ',time
      endif

      nymd = date
      nhms = time
      return
      end

      subroutine get_time (date,time)
      implicit none
      integer date,time

#include "chronos.h"

      date = nymd
      time = nhms
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

      INTEGER  MNDY(12,4)
      DATA MNDY /0,31,60,91,121,152,182,213,244,274,305,335,366,
     .           397,34*0 /

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
      MNDY(I,1) = MNDY(I-12,1) + 365
      ENDDO

C***********************************************************************
C*                 COMPUTE # OF SECONDS FROM NMMDD                     *
C***********************************************************************

      nsegm =     nmmdd/100
      nsegd = mod(nmmdd,100)

      YEAR   = NYMD / 10000
      MONTH  = MOD(NYMD,10000) / 100
      DAY    = MOD(NYMD,100)

      IDAY   = MNDY( MONTH ,MOD(YEAR ,4)+1 )
      month = month + nsegm
      If( month.gt.12 ) then
      month = month - 12
      year = year + 1
      endif
      IDAY2  = MNDY( MONTH ,MOD(YEAR ,4)+1 )

                    nday = iday2-iday
      if(nday.lt.0) nday = nday + 1461
                    nday = nday + nsegd

      nsecf2 = nsecf2 + nday*nsday

      return
      end

      subroutine fixdate (nymd)
      implicit none
      integer nymd

c Modify 6-digit YYMMDD for dates between 1950-2050
c -------------------------------------------------
      if (nymd .lt. 500101) then
        nymd = 20000000 + nymd
      else if (nymd .le. 991231) then
        nymd = 19000000 + nymd
      endif

      return
      end

      subroutine interp_time ( nymd ,nhms , 
     .                         nymd1,nhms1, nymd2,nhms2, fac1,fac2 )
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

      _RL time, time1, time2
                                                                                
      INTEGER    DAYSCY                                                         
      PARAMETER (DAYSCY = 365*4+1)                                   

      INTEGER MNDY(12,4)              
                                                                                
      LOGICAL FIRST                                                             
      DATA    FIRST/.TRUE./                                                     
                                                                                
      DATA MNDY /0,31,60,91,121,152,182,213,244,274,305,335,366,                
     .           397,34*0 /

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
      MNDY(I,1) = MNDY(I-12,1) + 365
      ENDDO
      FIRST=.FALSE.
      ENDIF
                                                                                
C***********************************************************************        
C*                     COMPUTE INTERPOLATION FACTORS                   *        
C***********************************************************************        
                                                                                
      time  = DAY  + MNDY(MONTH ,MOD(YEAR ,4)+1) + float(sec )/86400.
      time1 = DAY1 + MNDY(MONTH1,MOD(YEAR1,4)+1) + float(sec1)/86400.
      time2 = DAY2 + MNDY(MONTH2,MOD(YEAR2,4)+1) + float(sec2)/86400.

      if( time .lt.time1 ) time  = time  + dayscy
      if( time2.lt.time1 ) time2 = time2 + dayscy

      fac1  = (time2-time)/(time2-time1)
      fac2  = (time-time1)/(time2-time1)

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

c Pass Back Current Updated Time
c ------------------------------
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

      integer MNDY(12,4)
      DATA MNDY /0,31,60,91,121,152,182,213,244,274,305,335,366,
     .           397,34*0 /

      integer i,nsecf,iday,iday0,nsec,nsec0,ntime

C***********************************************************************        
C*                 COMPUTE # OF DAYS IN A 4-YEAR CYCLE                 *        
C***********************************************************************        

      DO I=15,48
      MNDY(I,1) = MNDY(I-12,1) + 365
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
                                                                                
      IDAY   = (DAY -1) + MNDY( MONTH ,MOD(YEAR ,4)+1 )                         
      IDAY0  = (DAY0-1) + MNDY( MONTH0,MOD(YEAR0,4)+1 )                         
                                                                                
      NSEC   = IDAY *NSDAY + SEC                                                
      NSEC0  = IDAY0*NSDAY + SEC0                                               
                                                                                
                       NTIME  = NSEC-NSEC0                                      
      IF (NTIME.LT.0 ) NTIME  = NTIME + NCYCLE                                  
                       NALARM = NTIME                                           
      IF ( MHMS.NE.0 ) NALARM = MOD( NALARM,NSECF(MHMS) )                       
                                                                                
      RETURN                                                                    
      END                                                                       

      FUNCTION INCYMD (NYMD,M)                                                  
C***********************************************************************        
C  PURPOSE                                                                      
C     INCYMD:  NYMD CHANGED BY ONE DAY                                          
C     MODYMD:  NYMD CONVERTED TO JULIAN DATE                                    
C  DESCRIPTION OF PARAMETERS                                                    
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

c Input Variables
c ---------------
      integer nymd, nhms, irun
      _RL    cosz(irun), alat(irun), alon(irun), ra

c Local Variables
c ---------------
      integer year, day, sec, month, iday, idayp1
      integer dayscy
      integer i,nsecf,k,km,kp

      _RL hc
      _RL pi, zero, one, two, six, dg2rd, yrlen, eqnx, ob, ecc, per
      _RL daylen, fac, thm, thp, thnow, zs, zc, sj, cj

      parameter ( pi    = 3.1415926535898)
      parameter ( zero  = 0.0 )
      parameter ( one   = 1.0 )
      parameter ( two   = 2.0 )
      parameter ( six   = 6.0 )
      parameter ( dg2rd = pi/180. )

      parameter ( yrlen  = 365.25  )
      parameter ( dayscy = 365*4+1 )
      parameter ( eqnx   =  80.9028)
      parameter ( ob     =  23.45*dg2rd )
      parameter ( ecc    =   0.0167 )
      parameter ( per    = 102.0*dg2rd)
      parameter ( daylen = 86400.)

      _RL      TH(DAYSCY),T0,T1,T2,T3,T4,FUN,Y,MNDY(12,4)

      LOGICAL FIRST
      DATA    FIRST/.TRUE./
      SAVE

      DATA MNDY /0,31,60,91,121,152,182,213,244,274,305,335,366,
     .           397,34*0 /

      FUN(Y) = (TWO*PI/((ONE-ECC**2)**1.5))*(ONE/YRLEN)
     .       * (ONE - ECC*COS(Y-PER)) ** 2

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
           MNDY(I,1) = MNDY(I-12,1) + 365
100        CONTINUE

           KM  = INT(EQNX) + 1
           FAC = KM-EQNX
           T0 = ZERO
           T1 = FUN(T0         )*FAC
           T2 = FUN(ZERO+T1/TWO)*FAC
           T3 = FUN(ZERO+T2/TWO)*FAC
           T4 = FUN(ZERO+T3    )*FAC
           TH(KM) = (T1 + TWO*(T2 + T3) + T4) / SIX

           DO 200 K=2,DAYSCY
           T1 = FUN(TH(KM)       )
           T2 = FUN(TH(KM)+T1/TWO)
           T3 = FUN(TH(KM)+T2/TWO)
           T4 = FUN(TH(KM)+T3    )
           KP = MOD(KM,DAYSCY) + 1
           TH(KP) = TH(KM) + (T1 + TWO*(T2 + T3) + T4) / SIX
           KM = KP
 200       CONTINUE

           FIRST=.FALSE.
      ENDIF

C***********************************************************************
C*            COMPUTE EARTH-SUN DISTANCE TO CURRENT SECOND             *
C***********************************************************************

      IDAY   = DAY + MNDY(MONTH,MOD(YEAR,4)+1)
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

c Local Variables
c ---------------
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

c Check for Leap Year
c -------------------
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

c -------------------------------------------------------------
c Note:  At this point, imnm & imnp range between 01-14, where
c        01    -> Previous years December
c        02-13 -> Current  years January-December
c        14    -> Next     years January
c -------------------------------------------------------------

      imnm = imnm-1
      imnp = imnp-1

      if( imnm.eq.0  ) imnm = 12
      if( imnp.eq.0  ) imnp = 12
      if( imnm.eq.13 ) imnm = 1
      if( imnp.eq.13 ) imnp = 1

      return
      end
