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
c   2) This routine will also read the data.fizhi namelist interface
c      to obtain information about diagnostic selection.
c-----------------------------------------------------------------------
       implicit none
#include "CPP_OPTIONS.h"
#include "SIZE.h"
#include "fizhi_SIZE.h"
#include "land_SIZE.h"
#include "EEPARAMS.h"
#include "chemistry_coms.h"
#include "earth_coms.h"

       integer myThid

       integer bi, bj
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
C Dummy routine - nothing to do for now
       enddo
       enddo

       return
       end
