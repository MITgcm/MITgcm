C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_init_chem.F,v 1.15 2004/10/20 16:41:21 molod Exp $
C $Name:  $

#include "FIZHI_OPTIONS.h"
      subroutine fizhi_init_chem(mythid,nozlats,nozlevs,ntimesoz,
     . ozlats,ozlevs,o3,
     . nwatlats,nwatlevs,ntimesq,watlats,watlevs,water,
     . Nrphys,pressure,n2o,methane,co2,cfc11,cfc12,cfc22)
C***********************************************************************
C Subroutine fizhi_init_chem - routine to read in the ozone and upper
C      atmosphere water vapor, and set the methane, n2o and cfc values
C
C INPUT:
C 
C mythid   - thread number (processor number)
C
C OUTPUT:
C
C***********************************************************************
      implicit none
      integer mythid,nozlevs,nozlats,nwatlevs,nwatlats,Nrphys
      integer ntimesoz,ntimesq
      _RL o3(nozlats,nozlevs,ntimesoz)
      _RL water(nwatlats,nwatlevs,ntimesq)
      _RL ozlats(nozlats), ozlevs(nozlevs)
      _RL watlats(nwatlats), watlevs(nwatlevs)
      _RL pressure(Nrphys),methane(Nrphys),n2o(Nrphys)
      _RL co2,cfc11,cfc12,cfc22

      _RL getcon
      integer kqz,koz
      call mdsfindunit( kqz, myThid )
      call mdsfindunit( koz, myThid )

      call read_qz (kqz,water,watlats,watlevs,nwatlats,nwatlevs,ntimesq)
      call read_oz (koz,o3,ozlats,ozlevs,nozlats,nozlevs,ntimesoz)

      call get_methane_n2o (pressure,Nrphys,n2o,methane)

      co2   = getcon('CO2'  )*1.e-6
      cfc11 = getcon('CFC11')*1.e-9
      cfc12 = getcon('CFC12')*1.e-9
      cfc22 = getcon('CFC22')*1.e-9

      RETURN
      END

      subroutine read_qz (ku,qz,lats,levs,nlat,nlev,ntime)
C***********************************************************************
C  PURPOSE
C     To Read Stratospheric Moisture Data
C
C  ARGUMENTS   DESCRIPTION
C     ku ...... Unit to Read  Moisture Data
C     qz ...... Stratospheric Moisture Data
C     lats .... Stratospheric Moisture Data Latitudes (degrees)
C     levs .... Stratospheric Moisture Data Levels    (mb)
C     nlat .... Number of ozone latitudes
C     nlev .... Number of ozone levels
C     ntime ... Number of ozone time values
C
C***********************************************************************

      implicit none
      integer  ku,nlat,nlev,ntime

      _RL qz(nlat,nlev,ntime)
      _RL lats(nlat)
      _RL levs(nlev)

      integer time0
      integer lat
      integer lev

      _RL voltomas
      parameter ( voltomas = 0.622e-6 )

      open(ku,file='data.sage',form='formatted')
      rewind ku

c Set Moisture Data Latitudes
c ---------------------------
      do   lat = 1,nlat
      lats(lat) = -85. + (lat-1)*10.
      enddo

c Read Moisture Pressure Levels
c -----------------------------
      read(ku,1000) (levs(lev),lev=1,nlev)

c Read Moisture Amounts by Month and Level
c ----------------------------------------
      do time0=1,ntime
      read (ku,1001)
      do  lat=1,nlat
      read(ku,1000) (qz(lat,lev,time0),lev=1,nlev)
      enddo
      enddo

c Convert from Volume Mixing Ratio to Mass Mixing Ratio
c -----------------------------------------------------
      do time0 = 1,ntime
      do  lev = 1,nlev
      do  lat = 1,nlat
      qz(lat,lev,time0) = qz(lat,lev,time0)*voltomas
      enddo
      enddo
      enddo

 1000 format (3(5x,7(2x,f6.1)/))
 1001 format (1x)
      return
      end

      subroutine read_oz (ku,oz,lats,levs,nlat,nlev,ntime)
C***********************************************************************
C  PURPOSE
C     To Read Ozone Value
C
C  ARGUMENTS   DESCRIPTION
C     ku ...... Unit to Read Ozone Data
C     oz ...... Ozone Data
C     lats .... Ozone Data Latitudes (degrees)
C     levs .... Ozone Data Levels    (mb)
C     nlat .... Number of ozone latitudes
C     nlev .... Number of ozone levels
C     ntime ... Number of ozone time values
C
C***********************************************************************

      implicit none
      integer  ku,nlat,nlev,ntime

      _RL   oz(nlat,nlev,ntime)
      real*4 o3(nlat)
      _RL lats(nlat)
      _RL levs(nlev)

      integer time0
      integer lat
      integer lev
      integer nrec

      _RL plevs(34)
      data plevs/ 0.003, 0.005, 0.007, 0.01, 0.015, 0.02, 0.03, 0.05,
     .            0.07, 0.1, 0.15, 0.2, 0.3, 0.5, 0.7, 1.0, 1.5, 2.0,
     .            3.0, 5.0, 7.0, 10.0, 15.0, 20.0, 30.0, 50.0, 70.0,
     .            100.0, 150.0, 200.0, 300.0, 500.0, 700.0, 1000.0 /

c Set Ozone Data Latitudes
c ------------------------
      do   lat = 1,nlat
      lats(lat) = -90. + (lat-1)*5.
      enddo

c Set Ozone Data Levels
c ------------------------
      do   lev = 1,nlev
      levs(lev) = plevs(lev)
      enddo

c Read Ozone Amounts by Month and Level
c -------------------------------------
      close (ku)
      open(ku,file='data.gcmo3',form='unformatted',access='direct',
     .                                                 recl=nlat*4)

      do time0=1,ntime
      do lev=1,nlev
C Note: 2 quantities in Ozone Dataset
      nrec = lev+(time0-1)*nlev*2   
      read(ku,rec=nrec) o3
#if defined( _BYTESWAPIO )
      call mds_byteswapr4(nlat,o3)
#endif
           do lat=1,nlat
           oz(lat,nlev-lev+1,time0) = o3(lat)
           enddo
      enddo
      enddo

      close (ku)
      return
      end

      subroutine get_methane_n2o (pres,Nrphys,n2o,methane)
C***********************************************************************
C  PURPOSE
C     Compute methane and n2o
C
C  ARGUMENTS   DESCRIPTION
C
C***********************************************************************
C*         Climatological Annual and Global Mean Height Data           *
C***********************************************************************

      implicit none

      integer Nrphys
      _RL n2o(Nrphys),methane(Nrphys)
      _RL pres(Nrphys)
      _RL hght(Nrphys), slope,pr1,pr2,hpr1,hpr2
      integer L,L1,L2,lup,ldn

      _RL plevc (46), plevz(46)
      _RL hghtc (46), hghtz(46)

      data plevc /1000.00, 975.00, 950.00, 925.00, 900.00,
     .             875.00, 850.00, 825.00, 800.00, 750.00,
     .             700.00, 650.00, 600.00, 550.00, 500.00,
     .             450.00, 400.00, 350.00, 300.00, 250.00,
     .             200.00, 150.00, 100.00,  70.00,  50.00,
     .              40.00,  30.00,  20.00,  10.00,   7.00,
     .               5.00,   4.00,   3.00,   2.00,   1.00,
     .               0.70,   0.50,   0.40,   0.30,   0.20,
     .               0.10,   0.07,   0.05,   0.04,   0.03,
     .               0.02 /

      data hghtc/ 0.128733 , 0.316985 , 0.528275 , 0.749515 , 0.976471 ,
     .            1.208910 , 1.446800 , 1.690980 , 1.941630 , 2.463530 ,
     .            3.016200 , 3.603490 , 4.229410 , 4.899870 , 5.622320 ,
     .            6.405940 , 7.263450 , 8.211920 , 9.275540 , 10.49150 ,
     .            11.92420 , 13.70200 , 16.12980 , 18.24120 , 20.26480 ,
     .            21.63100 , 23.41250 , 25.96570 , 30.45890 , 32.85240 ,
     .            35.17360 , 36.75040 , 38.82900 , 41.84600 , 47.15580 ,
     .            49.90100 , 52.46230 , 54.13890 , 56.26340 , 59.17640 ,
     .            63.89980 , 66.20240 , 68.29210 , 69.63550 , 71.32330 ,
     .            73.62110 /

      do L=1,46
      plevz(L) = plevc(47-L)
      hghtz(L) = hghtc(47-L)
      enddo

C **********************************************************************
C                     Interpolate Heights to Model Pressures        ****
C **********************************************************************

      do L2 = 1,Nrphys

         do L1 = 1,46
            if( plevz(L1).gt.pres(L2) ) then
                if( L1.eq.1 ) then
                    lup = 1
                    ldn = 2
                else
                    lup = L1-1
                    ldn = L1
                endif
                goto 10
            endif
         enddo
         lup = 45
         ldn = 46

   10 continue
       pr1 = plevz(lup)
       pr2 = plevz(ldn)
      hpr1 = hghtz(lup)
      hpr2 = hghtz(ldn)

      slope = ( hpr1-hpr2 )/( pr1-pr2 )
      hght(L2) = hpr2 + ( pres(L2)-pr2 )*slope

      enddo

C **********************************************************************
C  Set the profiles of N2O and CH4 based on Bresser and Pawson 1996 ****
C **********************************************************************

      do L = 1,Nrphys
       if( hght(L).gt.26. ) then
        n2o(L) = 120.* exp( (26.- hght(L)) / 5.69 ) * 1.e-9
       else if( hght(L).gt.16. ) then
        n2o(L) = 307.* exp( (16.- hght(L)) /10.47 ) * 1.e-9
       else
        n2o(L) = 307.e-9
       endif
      enddo

      do L = 1,Nrphys
       if( hght(L).gt.55. ) then
        methane(L) = 0.2e-6
       else if( hght(L).gt.14. ) then
        methane(L) = 1.7* exp( (14.- hght(L)) /19.16 ) * 1.e-6
       else
        methane(L) = 1.7e-6
       endif
      enddo

      return
      end
