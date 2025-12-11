#include "DIC_OPTIONS.h"
#include "GCHEM_OPTIONS.h"
CBOP
C ROUTINE: DIC_FIELDS_LOAD

C INTERFACE: ==========================================================
      SUBROUTINE DIC_FIELDS_LOAD (
     I           myTime, myIter, myThid )

C DESCRIPTION:
C  Read in fields needed for CO2,O2 fluxterms, silica for pH calculation

C USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DIC_VARS.h"
#include "DIC_LOAD.h"
#ifdef ALLOW_GCHEM
# include "GCHEM_FIELDS.h"
#endif

C INPUT PARAMETERS: ===================================================
C  myTime               :: current time
C  myIter               :: current timestep
C  myThid               :: thread number
      _RL myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_DIC

c LOCAL VARIABLES: ===================================================
      INTEGER bi, bj, i, j
      INTEGER intimeP, intime0, intime1
      _RL aWght,bWght
#ifdef DIC_CALCITE_SAT
      INTEGER k
#endif
CEOP

#ifdef GCHEM_ALLOW_FFIELDS
C Access pkg/gchem forcing fields
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          fIce(i,j,bi,bj)       = gchemIce(i,j,bi,bj)
          wind(i,j,bi,bj)       = gchemWind(i,j,bi,bj)
          AtmospCO2(i,j,bi,bj)  = gchemapCO2(i,j,bi,bj)
          silicaSurf(i,j,bi,bj) = gchemSi(i,j,bi,bj)
#ifndef USE_PLOAD
C     Atm pressure in Pascals, convert to atm
          AtmosP(i,j,bi,bj)     = gchemAtmosp(i,j,bi,bj)/Pa2atm
#endif
#ifdef READ_PAR
          PAR(i,j,bi,bj)        = gchemPAR(i,j,bi,bj)
#endif
#ifdef ALLOW_FE
          inputFe(i,j,bi,bj)    = gchemFe(i,j,bi,bj)
#endif
#ifdef LIGHT_CHL
          CHL(i,j,bi,bj)        = gchemCHL(i,j,bi,bj)
#endif
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#else /* GCHEM_ALLOW_FFIELDS */
      IF (  DIC_forcingCycle.GT.0. _d 0 ) THEN

C--   Now calculate whether it is time to update the forcing arrays
       CALL GET_PERIODIC_INTERVAL(
     O                   intimeP, intime0, intime1, bWght, aWght,
     I                   DIC_forcingCycle, DIC_forcingPeriod,
     I                   deltaTClock, myTime, myThid )

       bi = myBxLo(myThid)
       bj = myByLo(myThid)
#ifdef ALLOW_DEBUG
       IF ( debugLevel.GE.debLevB ) THEN
        _BEGIN_MASTER(myThid)
        WRITE(standardMessageUnit,'(A,I10,A,4I5,A,2F14.10)')
     &   ' DIC_FIELDS_LOAD,', myIter,
     &   ' : iP,iLd,i0,i1=', intimeP,DIC_ldRec(bi,bj), intime0,intime1,
     &   ' ; Wght=', bWght, aWght
        _END_MASTER(myThid)
       ENDIF
#endif /* ALLOW_DEBUG */

#ifdef ALLOW_AUTODIFF
C-    assuming that we call S/R DIC_FIELDS_LOAD at each time-step and
C     with increasing time, this will catch when we need to load new records;
C     But with Adjoint run, this is not always the case => might end-up using
C     the wrong time-records
       IF ( intime0.NE.intimeP .OR. myIter.EQ.nIter0 ) THEN
#else /* ALLOW_AUTODIFF */
C-    Make no assumption on sequence of calls to DIC_FIELDS_LOAD ;
C     This is the correct formulation (works in Adjoint run).
C     Unfortunatly, produces many recomputations <== not used until it is fixed
       IF ( intime1.NE.DIC_ldRec(bi,bj) ) THEN
#endif /* ALLOW_AUTODIFF */
 
C--   If the above condition is met then we need to read in
C     data for the period ahead and the period behind myTime.
        IF ( debugLevel.GE.debLevZero ) THEN
         _BEGIN_MASTER(myThid)
         WRITE(standardMessageUnit,'(A,I10,A,2(2I5,A))')
     &    ' DIC_FIELDS_LOAD, it=', myIter,
     &    ' : Reading new data, i0,i1=', intime0, intime1,
     &    ' (prev=', intimeP, DIC_ldRec(bi,bj), ' )'
         _END_MASTER(myThid)
        ENDIF

        _BARRIER

        IF ( DIC_windFile .NE. ' '  ) THEN
         CALL READ_REC_XY_RS( DIC_windFile,dicwind0,intime0,
     &        myIter,myThid )
         CALL READ_REC_XY_RS( DIC_windFile,dicwind1,intime1,
     &        myIter,myThid )
        ENDIF
        IF ( DIC_atmospFile .NE. ' '  ) THEN
         CALL READ_REC_XY_RS( DIC_atmospFile,atmosp0,intime0,
     &        myIter,myThid )
         CALL READ_REC_XY_RS( DIC_atmospFile,atmosp1,intime1,
     &        myIter,myThid )
        ENDIF
        IF ( DIC_silicaFile .NE. ' '  ) THEN
         CALL READ_REC_XY_RS( DIC_silicaFile,silicaSurf0,intime0,
     &        myIter,myThid )
         CALL READ_REC_XY_RS( DIC_silicaFile,silicaSurf1,intime1,
     &        myIter,myThid )
        ENDIF
#ifdef DIC_CALCITE_SAT
        IF ( DIC_deepSilicaFile .NE. ' '  ) THEN
         CALL READ_REC_XYZ_RS( DIC_deepSilicaFile,silicaDeep0,intime0,
     &        myIter,myThid )
         CALL READ_REC_XYZ_RS( DIC_deepSilicaFile,silicaDeep1,intime1,
     &        myIter,myThid )
        ENDIF
#endif
        IF ( DIC_iceFile .NE. ' '  ) THEN
         CALL READ_REC_XY_RS( DIC_iceFile,ice0,intime0,
     &       myIter,myThid )
         CALL READ_REC_XY_RS( DIC_iceFile,ice1,intime1,
     &       myIter,myThid )
        ENDIF
#ifdef READ_PAR
        IF ( DIC_parFile .NE. ' '  ) THEN
         CALL READ_REC_XY_RS( DIC_parFile,par0,intime0,
     &       myIter,myThid )
         CALL READ_REC_XY_RS( DIC_parFile,par1,intime1,
     &       myIter,myThid )
        ENDIF
#endif
#ifdef LIGHT_CHL
C--   Load chlorophyll climatology data, unit for chlorophyll : mg/m3
        IF ( DIC_chlaFile .NE. ' '  ) THEN
         CALL READ_REC_XY_RS( DIC_chlaFile,chlinput,1,
     &       myIter,myThid )
        ENDIF
#endif
#ifdef ALLOW_FE
        IF ( DIC_ironFile .NE. ' '  ) THEN
         CALL READ_REC_XY_RS( DIC_ironFile,feinput0,intime0,
     &       myIter,myThid )
         CALL READ_REC_XY_RS( DIC_ironFile,feinput1,intime1,
     &       myIter,myThid )
        ENDIF
#endif
 
C--   fill-in overlap after loading temp arrays:
         _EXCH_XY_RS(dicwind0, myThid )
         _EXCH_XY_RS(dicwind1, myThid )
         _EXCH_XY_RS(atmosp0, myThid )
         _EXCH_XY_RS(atmosp1, myThid )
         _EXCH_XY_RS(silicaSurf0, myThid )
         _EXCH_XY_RS(silicaSurf1, myThid )
#ifdef DIC_CALCITE_SAT
      IF ( DIC_deepSilicaFile .NE. ' '  ) THEN
        _EXCH_XYZ_RS(silicaDeep0, myThid )
        _EXCH_XYZ_RS(silicaDeep1, myThid )
      ENDIF
#endif
         _EXCH_XY_RS(ice0, myThid )
         _EXCH_XY_RS(ice1, myThid )
#ifdef READ_PAR
         _EXCH_XY_RS(par0, myThid )
         _EXCH_XY_RS(par1, myThid )
#endif
#ifdef LIGHT_CHL
         _EXCH_XY_RS(chlinput, myThid )
#endif
#ifdef ALLOW_FE
         _EXCH_XY_RS(feinput0, myThid )
         _EXCH_XY_RS(feinput1, myThid )
#endif
 
C-    save newly loaded time-record
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
           DIC_ldRec(bi,bj) = intime1
         ENDDO
        ENDDO

C-     end if-bloc (time to load new fields)
       ENDIF
 
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         IF ( DIC_windFile .NE. ' '  ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             WIND(i,j,bi,bj) = bWght*dicwind0(i,j,bi,bj)
     &                       + aWght*dicwind1(i,j,bi,bj)
            ENDDO
           ENDDO
C calculate piston velocity
C QQ: note - we should have wind speed variance in here
C QQ         also need to check units, and conversion factors
c          pisvel(i,j,bi,bj)  =0.337*wind(i,j,bi,bj)**2/3.6d5    QQQQ
          ENDIF
#ifndef USE_PLOAD
         IF ( DIC_atmospFile .NE. ' '  ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             AtmosP(i,j,bi,bj) = bWght*atmosp0(i,j,bi,bj)
     &                         + aWght*atmosp1(i,j,bi,bj)
            ENDDO
           ENDDO
         ENDIF
#endif
#ifdef DIC_CALCITE_SAT
         IF ( useCalciteSaturation .AND.
     &        DIC_deepSilicaFile .NE. ' '  ) THEN
          DO k=1,Nr
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             silicaDeep(i,j,k,bi,bj) = bWght*silicaDeep0(i,j,k,bi,bj)
     &                               + aWght*silicaDeep1(i,j,k,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ENDIF
#endif
         IF ( DIC_silicaFile .NE. ' '  ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
C If file provided for surface silicate, read it in
             silicaSurf(i,j,bi,bj) = bWght*silicaSurf0(i,j,bi,bj)
     &                             + aWght*silicaSurf1(i,j,bi,bj)
            ENDDO
           ENDDO
#ifndef ALLOW_AUTODIFF
#ifdef DIC_CALCITE_SAT
          ELSEIF ( DIC_deepSilicaFile .NE. ' '  ) THEN
            DO j=1-OLy,sNy+OLy
             DO i=1-OLx,sNx+OLx
C If no surface silicate file but deep (3d) silicate provided, use top level
             silicaSurf(i,j,bi,bj) = bWght*silicaDeep0(i,j,1,bi,bj)
     &                             + aWght*silicaDeep1(i,j,1,bi,bj)
            ENDDO
           ENDDO
#endif /* DIC_CALCITE_SAT */
#endif /* not ALLOW_AUTODIFF */
         ENDIF
 
         IF ( DIC_iceFile .NE. ' '  ) THEN
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
             fIce(i,j,bi,bj) = bWght*ice0(i,j,bi,bj)
     &                       + aWght*ice1(i,j,bi,bj)
           ENDDO
          ENDDO
         ENDIF
 
#ifdef READ_PAR
         IF ( DIC_parFile .NE. ' '  ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             PAR(i,j,bi,bj) = bWght*par0(i,j,bi,bj)
     &                      + aWght*par1(i,j,bi,bj)
            ENDDO
           ENDDO
         ENDIF
#endif
#ifdef LIGHT_CHL
          IF ( DIC_chlaFile .NE. ' '  ) THEN
            DO j=1-OLy,sNy+OLy
             DO i=1-OLx,sNx+OLx
              CHL(i,j,bi,bj) = chlinput(i,j,bi,bj)
             ENDDO
            ENDDO
          ENDIF
#endif
#ifdef ALLOW_FE
         IF ( DIC_ironFile .NE. ' '  ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             InputFe(i,j,bi,bj) = bWght*feinput0(i,j,bi,bj)
     &                          + aWght*feinput1(i,j,bi,bj)
            ENDDO
           ENDDO
         ENDIF
#endif
        ENDDO
       ENDDO

C endif for DIC_forcingCycle
      ENDIF
#endif /* GCHEM_ALLOW_FFIELDS */
#endif /* ALLOW_DIC */
      RETURN
      END
