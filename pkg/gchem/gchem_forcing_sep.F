C $Header: /u/gcmpack/MITgcm/pkg/gchem/gchem_forcing_sep.F,v 1.14 2006/08/09 01:25:40 jmc Exp $
C $Name:  $

#include "GCHEM_OPTIONS.h"

CBOP
C !ROUTINE: GCHEM_FORCING_SEP
C !INTERFACE: ==========================================================
      SUBROUTINE GCHEM_FORCING_SEP(myTime,myIter, myThid )

C !DESCRIPTION:
C     calls subroutine that will update passive tracers values
C     with a separate timestep. Since GCHEM_FORCING_SEP is now
C     called before DO_FIELDS_BLOCKING_EXCHANGES, the passive 
C     tracer values in the halo regions are not up to date and 
C     must not be used.

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS.h"
#ifdef ALLOW_DIC
#include "DIC_BIOTIC.h"
#include "DIC_ABIOTIC.h"
#endif /* ALLOW_DIC */

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
      INTEGER myThid, myIter
      _RL myTime

c!LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
C  bi,bj                :: tile indices
C  k                    :: vertical level
      INTEGER bi,bj,imin,imax,jmin,jmax
      INTEGER i,j
      INTEGER niter
CEOP

#ifdef ALLOW_GCHEM
#ifdef GCHEM_SEPARATE_FORCING

ccccccccccccccccccccccccc
c global calculations   c
ccccccccccccccccccccccccc
#ifdef ALLOW_DIC
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


ccccccccccccccccccccccccc
c chemical forcing      c
ccccccccccccccccccccccccc
C$taf loop = parallel
       DO bj=myByLo(myThid),myByHi(myThid)
C$taf loop = parallel
        DO bi=myBxLo(myThid),myBxHi(myThid)

        jMin=1
        jMax=sNy
        iMin=1
        iMax=sNx
c
ccccccccccccccccccccccccccc DIC cccccccccccccccccccccccccccccccc
c
#ifdef ALLOW_DIC
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

#endif /* GCHEM_SEPARATE_FORCING */
#endif /* ALLOW_GCHEM */

      RETURN
      END
