C $Header: /u/gcmpack/MITgcm/pkg/fizhi/update_chemistry_exports.F,v 1.14 2010/03/16 00:19:33 jmc Exp $
C $Name:  $

#include "FIZHI_OPTIONS.h"
       subroutine update_chemistry_exports (myTime, myIter, myThid)
c----------------------------------------------------------------------
c  Subroutine update_chemistry_exports - 'Wrapper' routine to update
c        the fields related to the earth chemistry that are needed
c        by fizhi.
c        Also: Set up "bi, bj loop" and some timers and clocks here.
c
c Call:  interp_chemistry
c-----------------------------------------------------------------------
       implicit none
#include "SIZE.h"
#include "fizhi_SIZE.h"
#include "fizhi_land_SIZE.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "fizhi_chemistry_coms.h"
#include "fizhi_coms.h"
#include "gridalt_mapping.h"
#include "EEPARAMS.h"
#include "chronos.h"

      integer myIter, myThid
      _RL myTime

c pe on physics grid refers to bottom edge
      _RL pephy(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nrphys+1,nSx,nSy)
      _RL pphy(sNx,sNy,Nrphys,nSx,nSy)
      _RL oz1(nlatsoz,nlevsoz), strq1(nlatsq,nlevsq)
      _RL waterin(sNx,sNy,Nrphys), xlat(sNx,sNy)

      integer i, j, L, LL, bi, bj
      integer im1, im2, jm1, jm2
      integer nhms1,nymd1,nhms2,nymd2,imns,ipls
      _RL facm, facp
      logical alarm
      external alarm

      im1 = 1
      im2 = sNx
      jm1 = 1
      jm2 = sNy

      if( alarm('radsw').or.alarm('radlw') ) then

       do bj = myByLo(myThid), myByHi(myThid)
       do bi = myBxLo(myThid), myBxHi(myThid)

c  Construct the physics grid pressures - count pephy levels top down
c                                         (even though dpphy counted bottom up)
        do j = 1,sNy
        do i = 1,sNx
         pephy(i,j,Nrphys+1,bi,bj)=(Ro_surf(i,j,bi,bj)+etaH(i,j,bi,bj))
         do L = 2,Nrphys+1
         LL = Nrphys+2-L
         pephy(i,j,LL,bi,bj)=pephy(i,j,LL+1,bi,bj)-dpphys(i,j,L-1,bi,bj)
         enddo
        enddo
        enddo
        do j = 1,sNy
        do i = 1,sNx
         do L = 1,Nrphys
          pphy(i,j,L,bi,bj)=(pephy(i,j,L+1,bi,bj)+pephy(i,j,L,bi,bj))
     .                                              /200.
         enddo
        enddo
        enddo

        do j = 1,sNy
        do i = 1,sNx
         xlat(i,j) = yC(i,j,bi,bj)
         do L = 1,Nrphys
          waterin(i,j,L) = sphy(i,j,L,bi,bj)
         enddo
        enddo
        enddo

        call time_bound(nymd,nhms,nymd1,nhms1,nymd2,nhms2,imns,ipls)
        call interp_time(nymd,nhms,nymd1,nhms1,nymd2,nhms2,facm,facp)

        do L = 1,nlevsoz
        do j = 1,nlatsoz
         oz1(j,L) = ozone(j,L,imns)*facm + ozone(j,L,ipls)*facp
        enddo
        enddo

        do L = 1,nlevsq
        do j = 1,nlatsq
         strq1(j,L) = stratq(j,L,imns)*facm + stratq(j,L,ipls)*facp
        enddo
        enddo

        call interp_chemistry(strq1,nlevsq,nlatsq,levsq,latsq,
     .   oz1,nlevsoz,nlatsoz,levsoz,latsoz,
     .   waterin,pphy(1,1,1,bi,bj),xlat,
     .   im2,jm2,Nrphys,nSx,nSy,bi,bj,o3,qstr)

       enddo
       enddo

      endif

      return
      end

      subroutine interp_chemistry (stratq,nwatlevs,nwatlats,watlevs,
     . watlats,ozone,nozlevs,nozlats,ozlevs,ozlats,
     . qz,plz,xlat,im,jm,lm,nSx,nSy,bi,bj,ozrad,qzrad)

      implicit none

c Input Variables
c ---------------
      integer nwatlevs,nwatlats,nozlevs,nozlats,nSx,nSy,bi,bj
      _RL stratq(nwatlats,nwatlevs),ozone(nozlats,nozlevs)
      _RL watlevs(nwatlevs),watlats(nwatlats)
      _RL ozlevs(nozlevs),ozlats(nozlats)
      integer im,jm,lm
      _RL qz(im,jm,lm),plz(im,jm,lm)
      _RL xlat(im,jm)
      _RL ozrad(im,jm,lm,nSx,nSy)
      _RL qzrad(im,jm,lm,nSx,nSy)

C **********************************************************************
C ****           Get Ozone and Stratospheric Moisture Data          ****
C **********************************************************************

      call interp_qz (stratq,nwatlevs,nwatlats,watlevs,watlats,im*jm,
     .                         bi,bj, xlat,lm,plz,qz,qzrad(1,1,1,bi,bj))
      call interp_oz (ozone ,nozlevs,nozlats,ozlevs,ozlats,im*jm,
     .                         bi,bj, xlat,lm,plz,ozrad(1,1,1,bi,bj))

      return
      end

      subroutine interp_qz(stratq,nwatlevs,nwatlats,watlevs,watlats,
     .                         irun,bi,bj,xlat,nlevs,pres,qz_in,qz_out )
C***********************************************************************
C  Purpose
C     To Interpolate Chemistry Moisture from Chemistry Grid to Physics Grid
C
C  INPUT Argument Description
C     stratq .... Climatological SAGE Stratospheric Moisture
C     irun ...... Number of Columns to be filled
C     xlat ...... Latitude in Degrees
C     nlevs ..... Vertical   Dimension
C     pres ...... PRES (IM,JM,nlevs) Three-dimensional array of pressures
C     qz_in ..... Model Moisture (kg/kg mass mixing radtio)
C     qz_out .... Combination of Chemistry Moisture and Model Moisture
C                 (kg/kg mass mixing ratio)
C
C***********************************************************************

      implicit none
      integer nwatlevs,nwatlats
      integer bi,bj
      _RL stratq ( nwatlats,nwatlevs )
      _RL watlats (nwatlats)
      _RL watlevs (nwatlevs)

      integer irun,nlevs
      _RL xlat  (irun)
      _RL pres  (irun,nlevs)
      _RL qz_in (irun,nlevs)
      _RL qz_out(irun,nlevs)

c Local Variables
c ---------------
      integer     pqu,pql,dpq
      parameter ( pqu = 100.    )
      parameter ( pql = 300.    )
      parameter ( dpq = pql-pqu )

      integer i,k,L1,L2,LM,LP
      _RL h2o_time_lat (irun,nwatlevs)
      _RL       qz_clim(irun,nlevs)

      _RL  qpr1(irun), qpr2(irun), slope(irun)
      _RL   pr1(irun),  pr2(irun)

      integer  jlat,jlatm,jlatp

C **********************************************************************
C ****         Interpolate Moisture data to model latitudes          ***
C **********************************************************************

      DO 32 k = 1, nwatlevs
        DO 34   i = 1,irun

        DO 36 jlat = 1, nwatlats
           IF( watlats(jlat).gt.xlat(i) ) THEN
              IF( jlat.EQ.1 ) THEN
                  jlatm    = 1
                  jlatp    = 1
                  slope(i) = 0
                    ELSE
                  jlatm    = jlat -1
                  jlatp    = jlat
                  slope(i) = ( xlat(i)        -watlats(jlat-1) )
     .                     / ( watlats(jlat)-watlats(jlat-1) )
              ENDIF
              GOTO 37
           ENDIF
   36   CONTINUE
        jlatm    = nwatlats
        jlatp    = nwatlats
        slope(i) =  1
   37   CONTINUE
        QPR1(i) = stratq(jlatm,k)
        QPR2(i) = stratq(jlatp,k)
   34   CONTINUE

        do  i = 1,irun
        h2o_time_lat(i,k) = qpr1(i) + slope(i)*(qpr2(i)-qpr1(i))
        enddo

   32 CONTINUE

C **********************************************************************
C ****     Interpolate Latitude Moisture data to model pressures     ***
C **********************************************************************

      DO 40 L2 = 1,nlevs

        DO 44 i= 1, irun
        DO 46 L1 = 1,nwatlevs
           IF( watlevs(L1).GT.pres(i,L2) ) THEN
             IF( L1.EQ.1 ) THEN
                 LM = 1
                 LP = 2
               ELSE
                 LM = L1-1
                 LP = L1
             ENDIF
             GOTO 47
           ENDIF
   46   CONTINUE
        LM = nwatlevs-1
        LP = nwatlevs
   47   CONTINUE
         PR1(i) =     watlevs (LM)
         PR2(i) =     watlevs (LP)
        QPR1(i) = h2o_time_lat(i,LM)
        QPR2(i) = h2o_time_lat(i,LP)
   44   CONTINUE

      do i= 1, irun
           slope(i) =(QPR1(i)-QPR2(i)) / (PR1(i)-PR2(i))
      qz_clim(i,L2) = QPR2(i) + (pres(i,L2)-PR2(i))*SLOPE(i)
      enddo

   40 CONTINUE

c
c ... Above 100 mb, using climatological  water data set ...................
c ... Below 300 mb, using model predicted water data set ...................
c ... In between, using linear interpolation ...............................
c
      do k= 1, nlevs
      do i= 1, irun
           if( pres(i,k).ge.pqu  .and. pres(i,k).le. pql) then
             qz_out(i,k) = qz_clim(i,k)+(qz_in(i,k)-
     1                     qz_clim(i,k))*(pres(i,k)-pqu)/dpq
      else if( pres(i,k) .gt. pql ) then
             qz_out(i,k) = qz_in  (i,k)
      else
             qz_out(i,k) = qz_clim(i,k)
           endif
      enddo
      enddo

      return
      end

      subroutine interp_oz (ozone,nozlevs,nozlats,ozlevs,ozlats,
     .                            irun,bi,bj,xlat,nlevs,plevs,ozrad)
C***********************************************************************
C  Purpose
C     To Interpolate Chemistry Ozone from Chemistry Grid to Physics Grid
C
C  INPUT Argument Description
C     ozone ..... Climatological Ozone
C     chemistry .. Chemistry State Data Structure
C     irun ....... Number of Columns to be filled
C     xlat ....... Latitude in Degrees
C     nlevs ...... Vertical   Dimension
C     pres ....... Three-dimensional array of pressures
C     ozrad ...... Ozone on Physics Grid (kg/kg mass mixing radtio)
C
C***********************************************************************
      implicit none
      integer nozlevs,nozlats,irun,nlevs
      integer bi,bj
      _RL ozone(nozlats,nozlevs)
      _RL xlat(irun)
      _RL plevs(irun,nlevs)
      _RL ozrad(irun,nlevs)
      _RL ozlevs(nozlevs),ozlats(nozlats)

c Local Variables
c ---------------
      _RL zero,one,o3min,voltomas
      PARAMETER ( ZERO     = 0.0 )
      PARAMETER ( ONE      = 1.0 )
      PARAMETER ( O3MIN    = 1.0E-10  )
      PARAMETER ( VOLTOMAS = 1.655E-6 )

      integer  i,k,L1,L2,LM,LP
      integer  jlat,jlatm,jlatp
      _RL  O3INT1(IRUN,nozlevs)
      _RL    QPR1(IRUN), QPR2(IRUN), SLOPE(IRUN)
      _RL     PR1(IRUN),  PR2(IRUN)

C **********************************************************************
C ****           INTERPOLATE ozone data to model latitudes           ***
C **********************************************************************

      DO 32 K=1,nozlevs
      DO 34 I=1,IRUN

      DO 36 jlat = 1,nozlats
      IF( ozlats(jlat).gt.xlat(i) ) THEN
      IF( jlat.EQ.1 ) THEN
      jlatm    = 1
      jlatp    = 1
      slope(i) = zero
        ELSE
      jlatm    = jlat-1
      jlatp    = jlat
      slope(i) = ( XLAT(I)        -ozlats(jlat-1) )
     .         / ( ozlats(jlat)-ozlats(jlat-1) )
      ENDIF
      GOTO 37
      ENDIF
   36 CONTINUE
      jlatm    = nozlats
      jlatp    = nozlats
      slope(i) = one
   37 CONTINUE
      QPR1(I) = ozone(jlatm,k)
      QPR2(I) = ozone(jlatp,k)
   34 CONTINUE

      DO 38 I=1,IRUN
      o3int1(i,k) = qpr1(i) + slope(i)*( qpr2(i)-qpr1(i) )
   38 CONTINUE

   32 CONTINUE

C **********************************************************************
C ****     INTERPOLATE latitude ozone data to model pressures        ***
C **********************************************************************

      DO 40 L2 = 1,NLEVS

      DO 44 I  = 1,IRUN
      DO 46 L1 = 1,nozlevs
      IF( ozlevs(L1).GT.PLEVS(I,L2) ) THEN
      IF( L1.EQ.1 ) THEN
          LM = 1
          LP = 2
        ELSE
          LM = L1-1
          LP = L1
      ENDIF
      GOTO 47
      ENDIF
   46 CONTINUE
            LM = nozlevs-1
            LP = nozlevs
   47 CONTINUE
       PR1(I) = ozlevs (LM)
       PR2(I) = ozlevs (LP)
      QPR1(I) =   O3INT1(I,LM)
      QPR2(I) =   O3INT1(I,LP)
   44 CONTINUE

      DO 48 I=1,IRUN
         SLOPE(I) = ( QPR1(I)-QPR2(I) )
     .            / (  PR1(I)- PR2(I) )
      ozrad(I,L2) =   QPR2(I) + ( PLEVS(I,L2)-PR2(I) )*SLOPE(I)

      if( ozrad(i,l2).lt.o3min ) then
          ozrad(i,l2) =  o3min
      endif

   48 CONTINUE
   40 CONTINUE

C **********************************************************************
C ****     CONVERT FROM VOLUME MIXING RATIO TO MASS MIXING RATIO     ***
C **********************************************************************

      DO 60 L2=1,NLEVS
      DO 60 I=1,IRUN
c     DO 60 I=1,IRUN*NLEVS
c     ozrad (I,1) = ozrad(I,1) * VOLTOMAS
      ozrad (I,L2) = ozrad(I,L2) * VOLTOMAS
  60  CONTINUE

      RETURN
      END

