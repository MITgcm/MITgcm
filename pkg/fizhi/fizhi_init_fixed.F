C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_init_fixed.F,v 1.5 2004/06/08 15:09:01 molod Exp $
C $Name:  $

       subroutine fizhi_init_fixed (myThid)
c-----------------------------------------------------------------------
c  Routine to initialise the fizhi package.
c  
c  Input: myThid       - Process number calling this routine
c
c  Notes: 
c   1) This routine is the interface to read input datasets and set
c      other fixed variables for fizhi
c      the datasets are:
c         vegetation (data for each tile at every grid point)
c         ozone (varies with lat, height and time - read it all in
c                   now and interpolate between values later)
c      the other fixed parameters are:
c         N2O, Methane (vary with space)
c         CO2, CFC11, CFC12, CFC22 (set to a global value)
c   3) For now, the fizhi package contains the alarms and clocks
c      routines, so this routine will also initialize the alarms.
c-----------------------------------------------------------------------
       implicit none
#include "CPP_OPTIONS.h"
#include "SIZE.h"
#include "fizhi_SIZE.h"
#include "fizhi_land_SIZE.h"
#include "EEPARAMS.h"
#include "fizhi_chemistry_coms.h"
#include "fizhi_earth_coms.h"
#include "fizhi_land_coms.h"
#include "chronos.h"
#include "gridalt_mapping.h"

       integer myThid

       integer bi, bj
       integer im1, im2, jm1, jm2, idim1, idim2, jdim1, jdim2
       integer nymdb,nhmsb
       character*40 vegdata
       _RL pressure(Nrphys)

       im1 = 1-OLx
       im2 = sNx+OLx
       jm1 = 1-OLy
       jm2 = sNy+OLy
       idim1 = 1
       idim2 = sNx
       jdim1 = 1
       jdim2 = sNy
       nymdb = nymd0
       nhmsb = nhms0

       call fizhi_alarms(nymdb,nhmsb,deltaTClock)
       call fizhi_init_veg ( mythid, vegdata,idim2,jdim2,Nsx,Nsy,maxtyp,
     .            surftype,tilefrac,igrd,ityp,chfr )

C Compute pressure profile to get methane and n2o values (bottom-up)

       pressure(1)=1000.
       do L = 2,Nrphys+1
        pressure(L)=pressure(L-1)-dpphys0(1,1,L-1,1,1)
       enddo

       call fizhi_init_chem(mythid,
     .      nlatsoz,nlevsoz,ntimesoz,latsoz,levsoz,ozone,
     .      nlatsq,nlevsq,ntimesq,latsq,levsq,stratq,
     .      Nrphys,pressure,n20,methane,co2,cfc11,cfc12,cfc22)

       return
       end
