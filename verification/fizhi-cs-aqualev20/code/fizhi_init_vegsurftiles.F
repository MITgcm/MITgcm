#include "FIZHI_OPTIONS.h"
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: FIZHI_INIT_VEGSURFTILES
C     !INTERFACE:
      subroutine fizhi_init_vegsurftiles(globalArr,xsize,ysize,
     &                                   nymd,nhms,prec,myThid)


C     !DESCRIPTION:
C      Read in grid space values of the land state
C      and then convert to vegetation tile space

C     !USES:
C      Calls routine grd2msc to do grid to tile space for each bi bj
      implicit none
#include "SIZE.h"
#include "fizhi_SIZE.h"
#include "fizhi_land_SIZE.h"
#include "fizhi_coms.h"
#include "fizhi_land_coms.h"
#include "fizhi_earth_coms.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
      integer xsize, ysize
      Real*8 globalArr(xsize,ysize,8)
      CHARACTER*1 prec
      INTEGER nhms,nymd
      INTEGER myThid

CEOP
C     !LOCAL VARIABLES:
      integer n,bi,bj

      DO bj = myByLo(myThid), myByHi(myThid)
      DO bi = myBxLo(myThid), myBxHi(myThid)

       do n = 1,nchp
        tcanopy(n,bi,bj) = 300.
        ecanopy(n,bi,bj) = 0.
        tdeep(n,bi,bj) = 295.
        swetshal(n,bi,bj) = 0.1
        swetroot(n,bi,bj) = 0.1
        swetdeep(n,bi,bj) = 0.1
        snodep(n,bi,bj) = 0.
        capac(n,bi,bj) = 0.
       enddo

      enddo
      enddo

      RETURN
      END
