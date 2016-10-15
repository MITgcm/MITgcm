C $Header: /u/gcmpack/MITgcm/pkg/bling/bling_fields_load.F,v 1.2 2016/10/15 22:57:35 mmazloff Exp $
C $Name:  $

#include "BLING_OPTIONS.h"

CBOP
      subroutine BLING_FIELDS_LOAD (
     I           myIter,myTime,myThid)

C     ==========================================================
C     | subroutine bling_fields_load
C     | o Read in fields needed for CO2, O2 flux terms, silica 
C     |   for pH calculation
C     ==========================================================

      implicit none
      
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#ifdef ALLOW_EXF
# include "EXF_OPTIONS.h"
# include "EXF_PARAM.h"
# include "EXF_FIELDS.h"
#endif
#include "BLING_VARS.h"
#include "BLING_LOAD.h"
#ifdef ALLOW_THSICE
# include "THSICE_VARS.h"
#endif
#ifdef ALLOW_SEAICE
# include "SEAICE_SIZE.h"
# include "SEAICE.h"
#endif

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
C  myIter               :: current timestep
C  myTime               :: current time
      INTEGER myIter
      _RL myTime
      INTEGER myThid

#ifdef ALLOW_BLING

c !LOCAL VARIABLES: ===================================================
      INTEGER bi, bj, i, j
      INTEGER intimeP, intime0, intime1
      _RL aWght,bWght
CEOP

      IF (  BLING_forcingCycle.gt.0. _d 0 ) THEN

C--   Now calculate whether it is time to update the forcing arrays
       CALL GET_PERIODIC_INTERVAL(
     O                   intimeP, intime0, intime1, bWght, aWght,
     I                   BLING_forcingCycle, BLING_forcingPeriod,
     I                   deltaTclock, myTime, myThid )

       bi = myBxLo(myThid)
       bj = myByLo(myThid)
#ifdef ALLOW_DEBUG
       IF ( debugLevel.GE.debLevB ) THEN
        _BEGIN_MASTER(myThid)
        WRITE(standardMessageUnit,'(A,I10,A,4I5,A,2F14.10)')
     &   ' BLING_FIELDS_LOAD,', myIter,
     &   ' : iP,iLd,i0,i1=', intimeP,BLING_ldRec(bi,bj), intime0,
     &                       intime1,
     &   ' ; Wght=', bWght, aWght
        _END_MASTER(myThid)
       ENDIF
#endif /* ALLOW_DEBUG */

#ifdef ALLOW_AUTODIFF
C-    assuming that we call S/R BLING_FIELDS_LOAD at each time-step and
C     with increasing time, this will catch when we need to load new records;
C     But with Adjoint run, this is not always the case => might end-up using
C     the wrong time-records
       IF ( intime0.NE.intimeP .OR. myIter.EQ.nIter0 ) THEN
#else /* ALLOW_AUTODIFF */
C-    Make no assumption on sequence of calls to BLING_FIELDS_LOAD ;
C     This is the correct formulation (works in Adjoint run).
C     Unfortunatly, produces many recomputations <== not used until it is fixed
       IF ( intime1.NE.BLING_ldRec(bi,bj) ) THEN
#endif /* ALLOW_AUTODIFF */

C--   If the above condition is met then we need to read in
C     data for the period ahead and the period behind myTime.
        IF ( debugLevel.GE.debLevZero ) THEN
         _BEGIN_MASTER(myThid)
         WRITE(standardMessageUnit,'(A,I10,A,2(2I5,A))')
     &    ' BLING_FIELDS_LOAD, it=', myIter,
     &    ' : Reading new data, i0,i1=', intime0, intime1,
     &    ' (prev=', intimeP, BLING_ldRec(bi,bj), ' )'
         _END_MASTER(myThid)
        ENDIF

        _BARRIER

        IF ( BLING_windFile .NE. ' '  ) THEN
         CALL READ_REC_XY_RS( BLING_windFile,dicwind0,intime0,
     &        myIter,myThid )
         CALL READ_REC_XY_RS( BLING_windFile,dicwind1,intime1,
     &        myIter,myThid )
        ENDIF
        IF ( BLING_atmospFile .NE. ' '  ) THEN
         CALL READ_REC_XY_RS( BLING_atmospFile,atmosp0,intime0,
     &        myIter,myThid )
         CALL READ_REC_XY_RS( BLING_atmospFile,atmosp1,intime1,
     &        myIter,myThid )
        ENDIF
        IF ( BLING_silicaFile .NE. ' '  ) THEN
         CALL READ_REC_XY_RS( BLING_silicaFile,silica0,intime0,
     &        myIter,myThid )
         CALL READ_REC_XY_RS( BLING_silicaFile,silica1,intime1,
     &        myIter,myThid )
        ENDIF
        IF ( BLING_iceFile .NE. ' '  ) THEN
         CALL READ_REC_XY_RS( BLING_iceFile,ice0,intime0,
     &       myIter,myThid )
         CALL READ_REC_XY_RS( BLING_iceFile,ice1,intime1,
     &       myIter,myThid )
        ENDIF
        IF ( BLING_ironFile .NE. ' '  ) THEN
         CALL READ_REC_XY_RS( BLING_ironFile,feinput0,intime0,
     &       myIter,myThid )
         CALL READ_REC_XY_RS( BLING_ironFile,feinput1,intime1,
     &       myIter,myThid )
        ENDIF

C GET CORRECT FIRST RECORD
c-----------------------------------------------------------
c Get Atmospheric carbon dioxide concentration from EXF 
c if decide to use exf for multiple input fields should put this
c in subroutine
#ifdef USE_EXFCO2
c     Atmospheric carbon dioxide concentration
      call EXF_SET_GEN  (
     &     apco2file, apco2startdate, apco2period,
     &     exf_inscal_apco2,
     &     apco2_exfremo_intercept, apco2_exfremo_slope,
     &     apco2, apco20, apco21, apco2mask,
# ifdef USE_EXF_INTERPOLATION
     &     apco2_lon0, apco2_lon_inc, apco2_lat0, apco2_lat_inc,
     &     apco2_nlon, apco2_nlat, xC, yC, apco2_interpMethod,
# endif
     &     mytime, myiter, mythid )
#endif

C--   fill-in overlap after loading temp arrays:
        _EXCH_XY_RS(dicwind0, myThid )
        _EXCH_XY_RS(dicwind1, myThid )
        _EXCH_XY_RS(atmosp0, myThid )
        _EXCH_XY_RS(atmosp1, myThid )
        _EXCH_XY_RS(silica0, myThid )
        _EXCH_XY_RS(silica1, myThid )
        _EXCH_XY_RS(ice0, myThid )
        _EXCH_XY_RS(ice1, myThid )
        _EXCH_XY_RS(feinput0, myThid )
        _EXCH_XY_RS(feinput1, myThid )

C-    save newly loaded time-record
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
           BLING_ldRec(bi,bj) = intime1
         ENDDO
        ENDDO

C-     end if-bloc (time to load new fields)
       ENDIF

       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         IF ( BLING_windFile .NE. ' '  ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             WIND(i,j,bi,bj) = bWght*dicwind0(i,j,bi,bj)
     &                       + aWght*dicwind1(i,j,bi,bj)
            ENDDO
           ENDDO
         ENDIF
#ifndef USE_PLOAD
         IF ( BLING_atmospFile .NE. ' '  ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             AtmosP(i,j,bi,bj) = bWght*atmosp0(i,j,bi,bj)
     &                         + aWght*atmosp1(i,j,bi,bj)
            ENDDO
           ENDDO
         ENDIF
#endif
         IF ( BLING_silicaFile .NE. ' '  ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             SILICA(i,j,bi,bj) = bWght*silica0(i,j,bi,bj)
     &                         + aWght*silica1(i,j,bi,bj)
            ENDDO
           ENDDO
         ENDIF
         IF ( BLING_iceFile .NE. ' '  ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             FIce(i,j,bi,bj) = bWght*ice0(i,j,bi,bj)
     &                       + aWght*ice1(i,j,bi,bj)
            ENDDO
           ENDDO
         ENDIF

         IF ( BLING_ironFile .NE. ' '  ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             InputFe(i,j,bi,bj) = bWght*feinput0(i,j,bi,bj)
     &                          + aWght*feinput1(i,j,bi,bj)
            ENDDO
           ENDDO
         ENDIF
        ENDDO
       ENDDO

C endif for BLING_forcingCycle
      ENDIF

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
#ifdef ALLOW_SEAICE
         IF ( useSEAICE ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             FIce(i,j,bi,bj) = AREA(i,j,bi,bj)
            ENDDO
           ENDDO
         ENDIF
#endif
#ifdef ALLOW_THSICE
         IF ( useThSIce ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             FIce(i,j,bi,bj) = iceMask(i,j,bi,bj)
            ENDDO
           ENDDO
         ENDIF
#endif
       ENDDO
      ENDDO

#endif /* ALLOW_BLING */
      RETURN
      END
