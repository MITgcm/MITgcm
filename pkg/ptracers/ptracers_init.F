C $Header: /u/gcmpack/MITgcm/pkg/ptracers/Attic/ptracers_init.F,v 1.2 2003/06/25 21:03:28 stephd Exp $
C $Name:  $

#include "PTRACERS_OPTIONS.h"
#include "GCHEM_OPTIONS.h"

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
#include "PTRACERS.h"
cswdptr -- add ---
#include "GRID.h"
#include "GCHEM.h"
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
           pTracer(i,j,k,bi,bj,iTracer) = 0.
           gPtr(i,j,k,bi,bj,iTracer) = 0.
           gPtrNM1(i,j,k,bi,bj,iTracer) = 0.
          ENDDO
         ENDDO
        ENDDO

#ifdef ALLOW_TIMEAVE
C     Initialize averages to zero
c       CALL TIMEAVE_RESET(GM_Kwx_T,Nr, bi,bj,myThid)
c       CALL TIMEAVE_RESET(GM_Kwy_T,Nr, bi,bj,myThid)
c       CALL TIMEAVE_RESET(GM_Kwz_T,Nr, bi,bj,myThid)
c       DO k=1,Nr
c        GM_TimeAve(k,bi,bj)=0.
c       ENDDO
#endif /* ALLOW_TIMEAVE */

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
cswdptr -- add ---
        ELSE
         DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           DO k=1,nR
            pTracer(i,j,k,bi,bj,iTracer)=0.d0
cswddic QQQ
            if (iTracer.eq.1)
     &        pTracer(i,j,k,bi,bj,iTracer)=2.23d0 !dic mol/m3
            if (iTracer.eq.2)
     &        pTracer(i,j,k,bi,bj,iTracer)=2.37d0 !alk moleq/m3
            if (iTracer.eq.3)
     &        pTracer(i,j,k,bi,bj,iTracer)=2.17d-3 !po4 mol/m3
            if (iTracer.eq.4)
     &        pTracer(i,j,k,bi,bj,iTracer)=1.d-5 !dop mol/m3
            if (iTracer.eq.5)
     &        pTracer(i,j,k,bi,bj,iTracer)=0.17d0  !o2 mol/m3
            if (iTracer.eq.6)
     &        pTracer(i,j,k,bi,bj,iTracer)=4.d-7  !fe mol/m3
           enddo
          ENDDO
          ENDDO
         ENDDO
         ENDDO
cswdptr -- end add ---
        ENDIF
        _EXCH_XYZ_R8(pTracer(1-Olx,1-Oly,1,1,1,iTracer),myThid)
       ENDDO
      ENDIF

C Read from a pickup file if nIter0
cswdptr IF (nIter0.NE.0) THEN
cswdptr -- change --
      IF (nIter0.GT.tIter0) THEN
cswdptr -- end change
       CALL PTRACERS_READ_CHECKPOINT( nIter0,myThid )
      ENDIF

#endif /* ALLOW_PTRACERS */

      RETURN
      END
