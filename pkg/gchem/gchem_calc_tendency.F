C $Header: /u/gcmpack/MITgcm/pkg/gchem/gchem_calc_tendency.F,v 1.1 2004/11/28 23:48:30 mlosch Exp $
C $Name:  $

#include "GCHEM_OPTIONS.h"

CBOP
C !ROUTINE: GCHEM_CALC_TENDENCY

C !INTERFACE: ==========================================================
      SUBROUTINE GCHEM_CALC_TENDENCY(
     I     myTime, myIter, myThid )

C !DESCRIPTION:
C In the case of GCHEM_SEPARATE_FORCING not defined,
C this subroutine computes the tracer tendencies due to a
C bio-geogchemistry or ecosystem model and stores them on an array
C gchemTendency, that will be incorporated into regular timestepping in
C in ptracers_intergrate.F
C The current example uses the CFC package, but his is meant to 
C be replaced by anything that the user provides.

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS.h"
#include "GCHEM.h"
#include "GCHEM_FIELDS.h"

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
C  myIter               :: current timestep
C  myTime               :: current time
      INTEGER myThid, myIter
      _RL myTime

#ifdef ALLOW_GCHEM
# ifndef GCHEM_SEPARATE_FORCING
C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
C  iTracer              :: ptracer number
C  bi,bj                :: tile indices
C  k                    :: vertical level
      INTEGER i,j
      INTEGER niter
      INTEGER iTracer
      INTEGER bi,bj,imin,imax,jmin,jmax,k
#ifdef ALLOW_CFC 
      INTEGER kLoc
      _RL  PtrLoc(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL gPtrLoc(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
#endif /* ALLOW_CFC */
CEOP

C     gchemTendency is re-initialized here
      DO iTracer = 1, PTRACERS_numInUse
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO K = 1, Nr
          DO J = 1-Oly, sNy+Oly
           DO I = 1-Olx, sNx+Olx
            gchemTendency(I,J,K,bi,bj,iTracer) = 0. _d 0
           ENDDO
          ENDDO
         ENDDO	
        ENDDO
       ENDDO
      ENDDO
C
C     Here is the place for code to compute bio-geochemical
C     tendency terms (sometimes referred to as source-minus-sink
C     terms). The tendencies are stored on gchemTendency, as show
C     in the CFC-example.
C
C     loop over tiles
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
C     define horizontal loop ranges
Cml        iMin = 1-OLx+2
Cml        iMax = sNx+OLx-1
Cml        jMin = 1-OLy+2
Cml        jMax = sNy+OLy-1
        iMin = 1
        iMax = sNx
        jMin = 1
        jMax = sNy
C        DO iTracer = 1, PTRACER_NumInUse
C         DO K = 1, Nr
C          DO J = 1-Oly, sNy+Oly
C           DO I = 1-Olx, sNx+Olx
C            gchemTendency(I,J,K,bi,bj,iTracer) = your specific model
C           ENDDO
C          ENDDO
C         ENDDO	
C        ENDDO
        
#ifdef ALLOW_CFC
ccccccccccccccccccccccccc
c chemical forcing      c
ccccccccccccccccccccccccc
ccccccccccccccccccccccccccc CFC cccccccccccccccccccccccccccccccc
c
        K = 1
CML         DO kLoc=1,Nr
CML          DO j=1-OLy,sNy+OLy
CML           DO i=1-OLx,sNx+OLx
CML            PtrLoc (I,J,kLoc) = Ptracer(I,J,kLoc,bi,bj,1)
CML            gPtrLoc(I,J,kLoc) = gPtr(I,J,kLoc,bi,bj,1)
CML           ENDDO
CML          ENDDO
CML         ENDDO	
CML         call CFC11_Forcing( PtrLoc, gPtrLoc,
CML     &        bi,bj,imin,imax,jmin,jmax,
CML     &        myIter,myTime,myThid)
CML         DO kLoc=1,Nr
CML          DO j=1-OLy,sNy+OLy
CML           DO i=1-OLx,sNx+OLx
CML            Ptracer(I,J,kLoc,bi,bj,1) = PtrLoc (I,J,kLoc) 
CML            gPtr(I,J,kLoc,bi,bj,1)    = gPtrLoc(I,J,kLoc)
CML           ENDDO
CML          ENDDO
CML         ENDDO	
CML        ENDIF
CML        if (iTracer.eq.2.and.k.eq.1) THEN
CML         DO kLoc=1,Nr
CML          DO j=1-OLy,sNy+OLy
CML           DO i=1-OLx,sNx+OLx
CML            PtrLoc (I,J,kLoc) = Ptracer(I,J,kLoc,bi,bj,2)
CML            gPtrLoc(I,J,kLoc) = gPtr(I,J,kLoc,bi,bj,2)
CML           ENDDO
CML          ENDDO
CML         ENDDO	
CML         call CFC12_Forcing( PtrLoc, gPtrLoc,
CML     &        bi,bj,imin,imax,jmin,jmax,
CML     &        myIter,myTime,myThid)
CML         DO kLoc=1,Nr
CML          DO j=1-OLy,sNy+OLy
CML           DO i=1-OLx,sNx+OLx
CML            Ptracer(I,J,kLoc,bi,bj,2) = PtrLoc (I,J,kLoc) 
CML            gPtr(I,J,kLoc,bi,bj,2)    = gPtrLoc(I,J,kLoc)
CML           ENDDO
CML          ENDDO
CML         ENDDO	
CML        ENDIF
       CALL CFC11_FORCING( Ptracer     (1-Olx,1-Oly,1,bi,bj,1),
     &                        gchemTendency(1-Olx,1-Oly,1,bi,bj,1),
     &                        bi, bj, imin, imax, jmin, jmax,
     &                        myIter, myTime, myThid )
        CALL CFC12_FORCING( Ptracer     (1-Olx,1-Oly,1,bi,bj,2),
     &                        gchemTendency(1-Olx,1-Oly,1,bi,bj,2),
     &                        bi, bj, imin, imax, jmin, jmax,
     &                        myIter, myTime, myThid )

cccccccccccccccccccccccccc END CFC cccccccccccccccccccccccccccccccccc
#endif /* ALLOW_CFC */
C     end of tile-loops
       ENDDO
      ENDDO
# endif /* GCHEM_SEPARATE_FORCING */
#endif /* ALLOW_GCHEM */

      RETURN
      END
