#include "CPP_OPTIONS.h"
#include "PTRACERS_OPTIONS.h"
#include "GCHEM_OPTIONS.h"

C !INTERFACE: ==========================================================
      SUBROUTINE GCHEM_FORCING(myTime,myIter, myThid )

      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_PTRACERS
#include "PTRACERS.h"
#endif
#ifdef DIC_BIOTIC
#include "DIC_BIOTIC.h"
#include "DIC_ABIOTIC.h"
#endif

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
      INTEGER myThid, myIter
      _RL myTime
      INTEGER bi,bj,imin,imax,jmin,jmax
      INTEGER i,j
      INTEGER niter

#ifdef ALLOW_PTRACERS

         _BEGIN_MASTER( mythid )
c
c
       if (myIter.le.nIter0+1) then
         call GCHEM_INIT(myTime, myIter,mythid)
       endif

         
c
cccccccccccccccccccccccccc
c load external data     c
cccccccccccccccccccccccccc
#ifdef DIC_BIOTIC
       call  DIC_FIELDS_LOAD (myIter,myTime,myThid)
#endif

         _END_MASTER(mythid)

         _BARRIER
         _BEGIN_MASTER( mythid )

ccccccccccccccccccccccccc
c global calculations   c
ccccccccccccccccccccccccc
#ifdef DIC_BIOTIC
c find global surface averages
       call tracer_meanarea(myIter,myTime,myThid,salt, 1,
     &                    gsm_s)
       call tracer_meanarea(myIter,myTime,myThid,
     &                           ptracer(1-Olx,1-Oly,1,1,1,1), 1,
     &                    gsm_dic)
       print*,'mean surface dic', gsm_dic,gsm_s
       call tracer_meanarea(myIter,myTime,myThid,
     &                           ptracer(1-Olx,1-Oly,1,1,1,2), 1,
     &                    gsm_alk)

#endif
ccccccccccccccccccccccccccccccccccccccccccc

      _END_MASTER( mythid )
      _BARRIER


ccccccccccccccccccccccccc
c chemical forcing      c
ccccccccccccccccccccccccc
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)

        jMin=1-OLy
        jMax=sNy+OLy
        iMin=1-OLx
        iMax=sNx+OLx
c
ccccccccccccccccccccccccccc DIC cccccccccccccccccccccccccccccccc
c
#ifdef DIC_BIOTIC
#ifdef ALLOW_FE
          call DIC_BIOTIC_Forcing( Ptracer(1-Olx,1-Oly,1,bi,bj,1),
     &                          Ptracer(1-Olx,1-Oly,1,bi,bj,2),
     &                          Ptracer(1-Olx,1-Oly,1,bi,bj,3),
     &                          Ptracer(1-Olx,1-Oly,1,bi,bj,4),
     &                          Ptracer(1-Olx,1-Oly,1,bi,bj,5),
     &                          Ptracer(1-Olx,1-Oly,1,bi,bj,6),
     &                          bi,bj,imin,imax,jmin,jmax,
     &                          myIter,myTime,myThid)
#else
          call DIC_BIOTIC_Forcing( Ptracer(1-Olx,1-Oly,1,bi,bj,1),
     &                          Ptracer(1-Olx,1-Oly,1,bi,bj,2),
     &                          Ptracer(1-Olx,1-Oly,1,bi,bj,3),
     &                          Ptracer(1-Olx,1-Oly,1,bi,bj,4),
     &                          Ptracer(1-Olx,1-Oly,1,bi,bj,5),
     &                          bi,bj,imin,imax,jmin,jmax,
     &                          myIter,myTime,myThid)
#endif
#endif
cccccccccccccccccccccccccc END DIC cccccccccccccccccccccccccccccccccc
c
        ENDDO
       ENDDO

#endif /* ALLOW_PTRACERS */

      RETURN
      END
