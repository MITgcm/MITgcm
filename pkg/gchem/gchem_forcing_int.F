#include "CPP_OPTIONS.h"
#include "PTRACERS_OPTIONS.h"
#include "GCHEM_OPTIONS.h"

C !INTERFACE: ==========================================================
      SUBROUTINE GCHEM_FORCING_INT(bi,bj,iMin,iMax,jMin,jMax,k,
     &                           iTracer, myTime,myIter, myThid )

      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_PTRACERS
#include "PTRACERS.h"
#endif

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
      INTEGER myThid, myIter
      _RL myTime
      INTEGER iTracer
      INTEGER bi,bj,imin,imax,jmin,jmax
      INTEGER i,j,k
      INTEGER niter

#ifdef ALLOW_PTRACERS
#ifdef ALLOW_GCHEM
c

ccccccccccccccccccccccccc
c chemical forcing      c
ccccccccccccccccccccccccc
ccccccccccccccccccccccccccc DIC cccccccccccccccccccccccccccccccc
c
#ifdef ALLOW_CFC
         if (iTracer.eq.1.and.k.eq.1)  
     &    call CFC11_Forcing( Ptracer(1-Olx,1-Oly,1,bi,bj,1),
     &                          gPtr(1-Olx,1-Oly,1,bi,bj,1),
     &                          bi,bj,imin,imax,jmin,jmax,
     &                          myIter,myTime,myThid)
        if (iTracer.eq.2.and.k.eq.1) 
     &    call CFC12_Forcing( Ptracer(1-Olx,1-Oly,1,bi,bj,2),
     &                          gPtr(1-Olx,1-Oly,1,bi,bj,2),
     &                          bi,bj,imin,imax,jmin,jmax,
     &                          myIter,myTime,myThid)
#endif
cccccccccccccccccccccccccc END DIC cccccccccccccccccccccccccccccccccc
c
#endif
#endif /* ALLOW_PTRACERS */

      RETURN
      END
