#include "DIC_OPTIONS.h"

CBOP
C !ROUTINE: DIC_INI_FORCING

C !INTERFACE: ==========================================================
      SUBROUTINE DIC_INI_FORCING( myThid )

C !DESCRIPTION:
c initializes forcing fields to zero
c (or to reasonable values)

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DIC_VARS.h"
#include "DIC_LOAD.h"

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
      INTEGER myThid
CEOP

#ifdef ALLOW_DIC

c !LOCAL VARIABLES: ====================================================
       INTEGER bi, bj, i, j
       INTEGER intimeP, intime0, intime1
       _RL aWght, bWght
#ifdef DIC_CALCITE_SAT
      INTEGER k
#endif

C--   Initialise forcing arrays in common blocks
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

C-    Initialise DIC_VARS.h forcing-field arrays:
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           wind      (i,j,bi,bj) = 0. _d 0
           fIce      (i,j,bi,bj) = 0. _d 0
           AtmosP    (i,j,bi,bj) = 0. _d 0
           silicaSurf(i,j,bi,bj) = 0. _d 0
#ifdef DIC_BIOTIC
           par       (i,j,bi,bj) = 0. _d 0
           CHL       (i,j,bi,bj) = 0. _d 0
           InputFe   (i,j,bi,bj) = 0. _d 0
#endif
         ENDDO
        ENDDO
#ifdef DIC_CALCITE_SAT
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           silicaDeep(i,j,k,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO
#endif

C-    Initialise DIC_LOAD.h arrays:
        DIC_ldRec(bi,bj) = 0
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           dicwind0(i,j,bi,bj) = 0. _d 0
           dicwind1(i,j,bi,bj) = 0. _d 0
           atmosp0 (i,j,bi,bj) = 0. _d 0
           atmosp1 (i,j,bi,bj) = 0. _d 0
           silicaSurf0(i,j,bi,bj) = 0. _d 0
           silicaSurf1(i,j,bi,bj) = 0. _d 0
           ice0    (i,j,bi,bj) = 0. _d 0
           ice1    (i,j,bi,bj) = 0. _d 0
#ifdef READ_PAR
           par0    (i,j,bi,bj) = 0. _d 0
           par1    (i,j,bi,bj) = 0. _d 0
#endif
#ifdef ALLOW_FE
           feinput0(i,j,bi,bj) = 0. _d 0
           feinput1(i,j,bi,bj) = 0. _d 0
#endif
#ifdef LIGHT_CHL
           chlinput(i,j,bi,bj) = 0. _d 0
#endif
         ENDDO
        ENDDO
#ifdef DIC_CALCITE_SAT
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           silicaDeep0(i,j,k,bi,bj) = 0. _d 0
           silicaDeep1(i,j,k,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO
#endif
C-   end bi,bj loops:
       ENDDO
      ENDDO

C ======================================================================

C--   Set reasonable values to those that need at least something
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            wind(i,j,bi,bj)       = 5. _d 0*maskC(i,j,1,bi,bj)
            AtmosP(i,j,bi,bj)     = 1. _d 0*maskC(i,j,1,bi,bj)
            silicaSurf(i,j,bi,bj) = 7.6838 _d -3*maskC(i,j,1,bi,bj)
            fIce(i,j,bi,bj)       = 0. _d 0
#ifdef READ_PAR
            par(i,j,bi,bj)        = 100. _d 0*maskC(i,j,1,bi,bj)
#endif
#ifdef LIGHT_CHL
C If the chlorophyll climatology is not provided, use this default value.
            CHL(i,j,bi,bj)        = 1. _d -2*maskC(i,j,1,bi,bj)
#endif
#ifdef ALLOW_FE
            InputFe(i,j,bi,bj)    = 1. _d -11*maskC(i,j,1,bi,bj)
#endif
          ENDDO
         ENDDO
#ifdef DIC_CALCITE_SAT
         IF ( useCalciteSaturation ) THEN
          DO k=1,Nr
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
              silicaDeep(i,j,k,bi,bj) = 3. _d -2*maskC(i,j,k,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ENDIF
#endif
C-    end bi,bj loops
       ENDDO
      ENDDO

C ======================================================================

C--   Load constant forcing-file (used if DIC_forcingCycle=0 )
c     IF ( DIC_forcingCycle .LE. zeroRL ) THEN
       IF ( DIC_windFile .NE. ' '  ) THEN
         CALL READ_REC_XY_RL( DIC_windFile, wind,
     &                        1, nIter0, myThid )
         _EXCH_XY_RL( wind, myThid )
       ENDIF
       IF ( DIC_atmospFile .NE. ' '  ) THEN
         CALL READ_REC_XY_RL( DIC_atmospFile, AtmosP,
     &                        1, nIter0, myThid )
         _EXCH_XY_RL( AtmosP, myThid )
       ENDIF
       IF ( DIC_silicaFile .NE. ' '  ) THEN
         CALL READ_REC_XY_RL( DIC_silicaFile, silicaSurf,
     &                        1, nIter0, myThid )
         _EXCH_XY_RL( silicaSurf, myThid )
       ENDIF
       IF ( DIC_iceFile .NE. ' '  ) THEN
         CALL READ_REC_XY_RL( DIC_iceFile, fIce,
     &                        1, nIter0, myThid )
         _EXCH_XY_RL( fIce, myThid )
       ENDIF
#ifdef DIC_BIOTIC
c#ifdef READ_PAR
       IF ( DIC_parFile .NE. ' '  ) THEN
         CALL READ_REC_XY_RL( DIC_parFile, par,
     &                        1, nIter0, myThid )
         _EXCH_XY_RL( par, myThid )
       ENDIF
c#endif
c#ifdef LIGHT_CHL
C--   Load chlorophyll climatology data, unit for chlorophyll : mg/m3
       IF ( DIC_chlaFile .NE. ' '  ) THEN
         CALL READ_REC_XY_RL( DIC_chlaFile, CHL,
     &                        1, nIter0, myThid )
         _EXCH_XY_RL( CHL, myThid )
       ENDIF
c#endif
c#ifdef ALLOW_FE
       IF ( DIC_ironFile .NE. ' '  ) THEN
         CALL READ_REC_XY_RL( DIC_ironFile, InputFe,
     &                        1, nIter0, myThid )
         _EXCH_XY_RL( InputFe, myThid )
       ENDIF
c#endif
#endif /* DIC_BIOTIC */
#ifdef DIC_CALCITE_SAT
       IF ( useCalciteSaturation .AND.
     &      DIC_deepSilicaFile .NE. ' '  ) THEN
         CALL READ_REC_XYZ_RL( DIC_deepSilicaFile, silicaDeep,
     &                        1, nIter0, myThid )
         _EXCH_XYZ_RL( silicaDeep, myThid )
       ENDIF
#endif
c     ENDIF

C ======================================================================

      IF ( DIC_forcingCycle .GT. zeroRL ) THEN
C--   Load some initial forcing from file

C-    Read in surface silica field (used to compute initial surf. pH)
C     Note: For this purpose, might be accurate enough to use first record
C           in silica file (already loaded) and skip all this block.

C-    Get reccord number and weight for time interpolation:
       CALL GET_PERIODIC_INTERVAL(
     O                   intimeP, intime0, intime1, bWght, aWght,
     I                   DIC_forcingCycle, DIC_forcingPeriod,
     I                   deltaTClock, startTime, myThid )

       _BARRIER
       _BEGIN_MASTER(myThid)
        WRITE(standardMessageUnit,'(A,I10,A,2(2I5,A))')
     &   ' DIC_INI_FORCING, it=', nIter0,
     &   ' : Reading new data, i0,i1=', intime0, intime1
       _END_MASTER(myThid)

       IF ( DIC_silicaFile .NE. ' '  ) THEN
C-    If file provided for surface silicate, read it in
         CALL READ_REC_XY_RS( DIC_silicaFile,silicaSurf0,intime0,
     &        nIter0,myThid )
         CALL READ_REC_XY_RS( DIC_silicaFile,silicaSurf1,intime1,
     &        nIter0,myThid )
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
              silicaSurf(i,j,bi,bj) = bWght*silicaSurf0(i,j,bi,bj)
     &                              + aWght*silicaSurf1(i,j,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ENDDO
#ifndef ALLOW_AUTODIFF
#ifdef DIC_CALCITE_SAT
       ELSEIF ( DIC_deepSilicaFile .NE. ' '  ) THEN
C-    If no surface silicate file but deep (3d) silicate provided, use top level
         k = 1
         CALL READ_REC_XYZ_RS( DIC_deepSilicaFile, silicaDeep0,
     &                         intime0, nIter0, myThid )
         CALL READ_REC_XYZ_RS( DIC_deepSilicaFile, silicaDeep1,
     &                         intime1, nIter0, myThid )
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
              silicaSurf(i,j,bi,bj) = bWght*silicaDeep0(i,j,k,bi,bj)
     &                              + aWght*silicaDeep1(i,j,k,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ENDDO
#endif /* DIC_CALCITE_SAT */
#endif /* ndef ALLOW_AUTODIFF */
       ENDIF
       _EXCH_XY_RL( silicaSurf, myThid )

C end if DIC_forcingCycle > 0
      ENDIF

#endif /* ALLOW_DIC */
      RETURN
      END
