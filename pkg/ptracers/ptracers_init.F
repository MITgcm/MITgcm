C $Header: /u/gcmpack/MITgcm/pkg/ptracers/Attic/ptracers_init.F,v 1.7 2003/09/24 06:50:26 dimitri Exp $
C $Name:  $

#include "PTRACERS_OPTIONS.h"
#ifdef ALLOW_GCHEM
# include "GCHEM_OPTIONS.h"
#endif

CBOP
C !ROUTINE: PTRACERS_INIT

C !INTERFACE: ==========================================================
      SUBROUTINE PTRACERS_INIT( myThid )

C !DESCRIPTION:
C     Initialize PTRACERS data structures

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "PTRACERS.h"
cswdptr -- add ---
#ifdef ALLOW_GCHEM
# include "GCHEM.h"
#endif
cswdptr --- end add --


C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  none

#ifdef ALLOW_PTRACERS

C !LOCAL VARIABLES: ====================================================
C  i,j,k,bi,bj,iTracer  :: loop indices
      INTEGER i,j,k,bi,bj,iTracer
      CHARACTER*(10) suff
#ifndef ALLOW_GCHEM
      INTEGER tIter0
      PARAMETER ( tIter0 = 0 )
#endif
CEOP
        
C Loop over tracers
      DO iTracer = 1, PTRACERS_num

C Loop over tiles
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)

C Initialize arrays in common blocks :
         DO k=1,Nr
          DO j=1-Oly,sNy+OLy
           DO i=1-Olx,sNx+Olx
            pTracer(i,j,k,bi,bj,iTracer) = 0. _d 0
            gPtr(i,j,k,bi,bj,iTracer)    = 0. _d 0
            gPtrNM1(i,j,k,bi,bj,iTracer) = 0. _d 0
           ENDDO
          ENDDO
         ENDDO
         DO j=1-Oly,sNy+OLy
          DO i=1-Olx,sNx+Olx
           surfaceTendencyPtr(i,j,bi,bj,iTracer) = 0. _d 0
          ENDDO
         ENDDO

C end bi,bj loops
        ENDDO
       ENDDO

C end of Tracer loop
      ENDDO

C Now read initial conditions and always exchange
cswdptr IF (nIter0.EQ.0) THEN
cswdptr -- change ---
      IF (nIter0.EQ.tIter0) THEN
cswdptr -- end change ---
       DO iTracer = 1, PTRACERS_numInUse
        IF ( PTRACERS_initialFile(iTracer) .NE. ' ' ) THEN
         _BEGIN_MASTER( myThid )
         CALL READ_FLD_XYZ_RL(PTRACERS_initialFile(iTracer),' ',
     &            pTracer(1-Olx,1-Oly,1,1,1,iTracer) ,0,myThid)
         _END_MASTER(myThid)

C        Apply mask
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO K=1,Nr
            DO J=1,sNy
             DO I=1,sNx
              IF(hFacC(I,J,K,bi,bj).EQ.0)
     &         pTracer(i,j,k,bi,bj,iTracer)=0.
             ENDDO
            ENDDO
           ENDDO
          ENDDO
         ENDDO

        ELSE
cswdptr -- add ---
cswdptr -- end add ---
        ENDIF
        _EXCH_XYZ_R8(pTracer(1-Olx,1-Oly,1,1,1,iTracer),myThid)
       ENDDO
      ENDIF

C Read from a pickup file if needed
cswdptr IF (nIter0.NE.0) THEN
cswdptr -- change --
      IF (nIter0.GT.tIter0) THEN
C--     Suffix for pickup files
       IF (pickupSuff.EQ.' ') THEN
         WRITE(suff,'(I10.10)') nIter0
       ELSE
         WRITE(suff,'(A10)') pickupSuff
       ENDIF
       CALL PTRACERS_READ_CHECKPOINT( nIter0,myThid )
      ENDIF

#endif /* ALLOW_PTRACERS */

      RETURN
      END
