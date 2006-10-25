C $Header: /u/gcmpack/MITgcm/pkg/ptracers/ptracers_init_varia.F,v 1.2 2006/10/25 20:41:21 jmc Exp $
C $Name:  $

#include "PTRACERS_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: PTRACERS_INIT_VARIA

C     !INTERFACE:
      SUBROUTINE PTRACERS_INIT_VARIA( myThid )

C     !DESCRIPTION:
C     Initialize PTRACERS data structures

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS.h"

C     !INPUT PARAMETERS:
C     myThid               :: thread number
      INTEGER myThid

#ifdef ALLOW_PTRACERS

C     !LOCAL VARIABLES:
C     i,j,k,bi,bj,iTracer  :: loop indices
      INTEGER i,j,k,bi,bj,iTracer
      CHARACTER*(10) suff
CEOP

C     Loop over tracers
      DO iTracer = 1, PTRACERS_num

C     Loop over tiles
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)

C     Initialize arrays in common blocks :
         DO k=1,Nr
          DO j=1-Oly,sNy+OLy
           DO i=1-Olx,sNx+Olx
            pTracer(i,j,k,bi,bj,iTracer) = PTRACERS_ref(k,iTracer)
            gPtr(i,j,k,bi,bj,iTracer)    = 0. _d 0
            gPtrNM1(i,j,k,bi,bj,iTracer) = 0. _d 0
           ENDDO
          ENDDO
         ENDDO
         DO j=1-Oly,sNy+OLy
          DO i=1-Olx,sNx+Olx
           surfaceForcingPtr(i,j,bi,bj,iTracer) = 0. _d 0
          ENDDO
         ENDDO

C     end bi,bj loops
        ENDDO
       ENDDO

C     end of Tracer loop
      ENDDO

      _BARRIER

C     Now read initial conditions and always exchange
      IF (nIter0.EQ.PTRACERS_Iter0) THEN
       DO iTracer = 1, PTRACERS_numInUse
        IF ( PTRACERS_initialFile(iTracer) .NE. ' ' ) THEN
         _BEGIN_MASTER( myThid )
         CALL READ_FLD_XYZ_RL(PTRACERS_initialFile(iTracer),' ',
     &        pTracer(1-Olx,1-Oly,1,1,1,iTracer),0,myThid)
         _END_MASTER(myThid)
         _EXCH_XYZ_R8(pTracer(1-Olx,1-Oly,1,1,1,iTracer),myThid)
        ENDIF
       ENDDO
      ENDIF

C     Apply mask
      DO iTracer = 1, PTRACERS_numInUse
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO k=1,Nr
          DO j=1,sNy
           DO i=1,sNx
            IF (maskC(i,j,k,bi,bj).EQ.0.)
     &           pTracer(i,j,k,bi,bj,iTracer)=0. _d 0
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      _BARRIER

C Read from a pickup file if needed
      IF (nIter0.GT.PTRACERS_Iter0) THEN
C     Suffix for pickup files
       IF (pickupSuff.EQ.' ') THEN
        WRITE(suff,'(I10.10)') nIter0
       ELSE
        WRITE(suff,'(A10)') pickupSuff
       ENDIF
       CALL PTRACERS_READ_PICKUP( nIter0,myThid )
      ENDIF

#endif /* ALLOW_PTRACERS */

      RETURN
      END
