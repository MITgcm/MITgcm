C $Header: /u/gcmpack/MITgcm/pkg/fizhi/update_chemistry_exports.F,v 1.3 2004/06/10 21:50:33 molod Exp $
C $Name:  $

       subroutine update_chemistry_exports (myTime, myIter, myThid)
c----------------------------------------------------------------------
c  Subroutine update_chemistry_exports - 'Wrapper' routine to update
c        the fields related to the earth's chemistry that are needed
c        by fizhi.
c        Also: Set up "bi, bj loop" and some timers and clocks here.
c
c Call:  interp_chemistry
c-----------------------------------------------------------------------
       implicit none
#include "CPP_OPTIONS.h"
#include "SIZE.h"
#include "fizhi_SIZE.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "fizhi_chemistry_coms.h"
#include "gridalt_mapping.h"
#include "EEPARAMS.h"

       integer myTime, myIter, myThid

c pe on physics grid refers to bottom edge
       _RL pephy(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nrphys+1,nSx,nSy)

       integer i, j, L, bi, bj
       integer im1, im2, jm1, jm2, idim1, idim2, jdim1, jdim2
                                                                                    
       im1 = 1-OLx
       im2 = sNx+OLx
       jm1 = 1-OLy
       jm2 = sNy+OLy
       idim1 = 1
       idim2 = sNx
       jdim1 = 1
       jdim2 = sNy
                                                                                    
       do bj = myByLo(myThid), myByHi(myThid)
       do bi = myBxLo(myThid), myBxHi(myThid)

C Compute pressures on physics grid
        do j = 1,sNy
        do i = 1,sNx
         pephy(i,j,1,bi,bj)=Ro_surf(i,j,bi,bj) + etaH(i,j,bi,bj)
         do L = 2,Nrphys+1
          pephy(i,j,L,bi,bj)=pephy(i,j,L-1,bi,bj)-dpphys(i,j,L-1,bi,bj)
         enddo
c Do not use a zero field as the top edge pressure for interpolation
         if(pephy(i,j,Nrphys+1,bi,bj).lt.1.e-5)
     .                               pephy(i,j,Nrphys+1,bi,bj) = 1.e-5
        enddo
        enddo

call time-bound
call interp_time
call interp chemistry
c reminder - lats are in yC and lons in xC in GRID.h

       enddo
       enddo

       return
       end

      subroutine interp_chemistry (stratq,nwatlevs,nwatlats,watlevs,
     . watlats,ozone,nozolevs,nozolats,ozolevs,ozolats,
     . qz,plz,ptop,xlat,im,jm,lm,ozrad,qzrad)

      implicit none

c Input Variables
c ---------------
      integer nwatlevs,nwatlats,nozolevs,nozolats
      real stratq(nwatlats,nwatlevs),ozone(nozlats,nozlevs)
      integer watlevs(nwatlevs),watlats(nwatlats)
      integer ozlevs(nozlevs),ozlats(nozlats)
      real qz(im,jm,lm),plz(im,jm,lm)
      real ptop, xlat(im,jm)
      integer im,jm,lm
      real ozrad(im,jm,lm)
      real qzrad(im,jm,lm)

c Local Variables
c ---------------
      integer   i,j,L
      real      pi,fjeq,pi180

C **********************************************************************
C ****           Get Ozone and Stratospheric Moisture Data          ****
C **********************************************************************

      call interp_qz (stratq,nwatlevs,nwatlats,watlevs,watlats,im*jm,
     .                                             xlat,lm,plz,qz,qzrad)
      call interp_oz (ozone ,nozolevs,nozolats,ozolevs,ozolats,im*jm,
     .                                             xlat,lm,plz,   ozrad)
      return
      end

      subroutine interp_qz(stratq,nwatlevs,nwatlats,watlevs,watlats,
     .                               irun,xlat,nlevs,pres,qz_in,qz_out )
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
C     qz_out .... Combination of Chemistry Moisture and Model Moisture (kg/kg mass mixing ratio)
C
C***********************************************************************
C*                  GODDARD LABORATORY FOR ATMOSPHERES                 *
C***********************************************************************

c Declare Modules and Data Structures
c -----------------------------------
      implicit none 
      integer nwatlevs,nwatlats
      real stratq ( nwatlats,nwatlevs )
      real watlats (nwatlats)
      real watlevs (nwatlevs)

      integer irun,nlevs
      real xlat  (irun)
      real pres  (irun,nlevs)
      real qz_in (irun,nlevs)
      real qz_out(irun,nlevs)

c Local Variables
c ---------------
      integer     pqu,pql,dpq
      parameter ( pqu = 100.    )
      parameter ( pql = 300.    )
      parameter ( dpq = pql-pqu )

      integer i,k,L1,L2,LM,LP
      real h2o_time_lat (irun,nwatlevs)
      real       qz_clim(irun,nlevs)

      real  qpr1(irun), qpr2(irun), slope(irun)
      real   pr1(irun),  pr2(irun)

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

      subroutine interp_oz (ozone,nozolevs,nozolats,ozolevs,ozolats,
     .                                      irun,xlat,nlevs,plevs,ozrad)
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
C*                  GODDARD LABORATORY FOR ATMOSPHERES                 *
C***********************************************************************

c Declare Modules and Data Structures
c -----------------------------------
      implicit none
      real     ozone ( nozolats,nozolevs )

      integer irun,nlevs
      real xlat  (irun)
      real plevs (irun,nlevs)
      real ozrad (irun,nlevs)

c Local Variables
c ---------------
      real zero,one,o3min,voltomas
      PARAMETER ( ZERO     = 0.0 )
      PARAMETER ( ONE      = 1.0 )
      PARAMETER ( O3MIN    = 1.0E-10  )
      PARAMETER ( VOLTOMAS = 1.655E-6 )

      integer  i,k,L1,L2,LM,LP
      integer  jlat,jlatm,jlatp
      real  O3INT1(IRUN,nozolevs)
      real    QPR1(IRUN), QPR2(IRUN), SLOPE(IRUN)
      real     PR1(IRUN),  PR2(IRUN)

C **********************************************************************
C ****           INTERPOLATE ozone data to model latitudes           ***
C **********************************************************************

      DO 32 K=1,nozolevs
      DO 34 I=1,IRUN

      DO 36 jlat = 1,nozolats
      IF( ozolats(jlat).gt.xlat(i) ) THEN
      IF( jlat.EQ.1 ) THEN
      jlatm    = 1
      jlatp    = 1
      slope(i) = zero
        ELSE
      jlatm    = jlat-1
      jlatp    = jlat
      slope(i) = ( XLAT(I)        -ozolats(jlat-1) )
     .         / ( ozolats(jlat)-ozolats(jlat-1) )
      ENDIF
      GOTO 37
      ENDIF
   36 CONTINUE
      jlatm    = nozolats
      jlatp    = nozolats
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
      DO 46 L1 = 1,nozolevs
      IF( ozolevs(L1).GT.PLEVS(I,L2) ) THEN
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
            LM = nozolevs-1
            LP = nozolevs
   47 CONTINUE
       PR1(I) = ozolevs (LM)
       PR2(I) = ozolevs (LP)
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

      DO 60 I=1,IRUN*NLEVS
      ozrad (I,1) = ozrad(I,1) * VOLTOMAS
  60  CONTINUE

      RETURN
      END

