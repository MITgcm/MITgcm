#include "CPP_OPTIONS.h"
#include "PTRACERS_OPTIONS.h"
#include "GCHEM_OPTIONS.h"

C !INTERFACE: ==========================================================
      SUBROUTINE GCHEM_FORCING_SEP(myTime,myIter, myThid )

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
#ifdef ALLOW_GCHEM

       _BARRIER
       _BEGIN_MASTER( mythid )

ccccccccccccccccccccccccc
c global calculations   c
ccccccccccccccccccccccccc
#ifdef DIC_BIOTIC
c find global surface averages
       gsm_s = 0.
       gsm_dic = 0.
       gsm_alk = 0.
       call tracer_meanarea(myThid,salt, 1,gsm_s)
       call tracer_meanarea(myThid,
     &               ptracer(1-Olx,1-Oly,1,1,1,1), 1, gsm_dic)
       print*,'mean surface dic', gsm_dic,gsm_s
       call tracer_meanarea(myThid,
     &               ptracer(1-Olx,1-Oly,1,1,1,2), 1, gsm_alk)
#endif
ccccccccccccccccccccccccccccccccccccccccccc

      _END_MASTER( mythid )
      _BARRIER


ccccccccccccccccccccccccc
c chemical forcing      c
ccccccccccccccccccccccccc
C$taf loop = parallel
       DO bj=myByLo(myThid),myByHi(myThid)
C$taf loop = parallel
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

#ifdef PTRACERS_SEPARATE_FORCING
C     time averaging directly, here otherwise in the_correction_step
           CALL PTRACERS_STATVARS(myTime, myIter, bi, bj, myThid)
#endif /* PTRACERS_SEPARATE_FORCING */

c
        ENDDO
       ENDDO

#endif /* ALLOW_GCHEM */
#endif /* ALLOW_PTRACERS */

      RETURN
      END
