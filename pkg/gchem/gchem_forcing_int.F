C $Header: /u/gcmpack/MITgcm/pkg/gchem/Attic/gchem_forcing_int.F,v 1.6 2004/11/23 20:20:46 mlosch Exp $
C $Name:  $

#include "GCHEM_OPTIONS.h"

CBOP
C !ROUTINE: GCHEM_FORCING_INT

C !INTERFACE: ==========================================================
      SUBROUTINE GCHEM_FORCING_INT(bi,bj,iMin,iMax,jMin,jMax,k,
     &                           iTracer, myTime,myIter, myThid )

C !DESCRIPTION:
C calls subroutine that will update tracer tendencies
C this will be incorporated into regular timestepping in
C ptracers_intergrate.F

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS.h"

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
C  myIter               :: current timestep
C  myTime               :: current time
C  iTracer              :: ptracer number
C  bi,bj                :: tile indices
C  k                    :: vertical level
      INTEGER myThid, myIter
      _RL myTime
      INTEGER iTracer
      INTEGER bi,bj,imin,imax,jmin,jmax,k

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
      INTEGER i,j
      INTEGER niter
CEOP

#ifdef ALLOW_GCHEM
# ifndef GCHEM_SEPARATE_FORCING
c

ccccccccccccccccccccccccc
c chemical forcing      c
ccccccccccccccccccccccccc
ccccccccccccccccccccccccccc CFC cccccccccccccccccccccccccccccccc
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
#endif /* ALLOW_CFC */
cccccccccccccccccccccccccc END CFC cccccccccccccccccccccccccccccccccc
c
# endif /* GCHEM_SEPARATE_FORCING */
#endif /* ALLOW_GCHEM */

      RETURN
      END
