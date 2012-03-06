C $Header: /u/gcmpack/MITgcm/pkg/dic/dic_fields_load.F,v 1.32 2012/03/06 15:37:24 jmc Exp $
C $Name:  $

#include "DIC_OPTIONS.h"

CBOP
C !ROUTINE: DIC_FIELDS_LOAD

C !INTERFACE: ==========================================================
      SUBROUTINE DIC_FIELDS_LOAD (
     I           myIter,myTime,myThid)

C !DESCRIPTION:
C  Read in fields needed for CO2,O2 fluxterms, silica for pH calculation

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DIC_VARS.h"
#include "DIC_LOAD.h"
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

#ifdef ALLOW_DIC

c !LOCAL VARIABLES: ===================================================
      INTEGER bi, bj, i, j
      INTEGER intimeP, intime0, intime1
      _RL aWght,bWght
#ifdef READ_PAR
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#endif
CEOP

      IF (  DIC_forcingCycle.gt.0. _d 0 ) THEN

C--   Now calculate whether it is time to update the forcing arrays
       CALL GET_PERIODIC_INTERVAL(
     O                   intimeP, intime0, intime1, bWght, aWght,
     I                   DIC_forcingCycle, DIC_forcingPeriod,
     I                   deltaTclock, myTime, myThid )

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

#ifdef ALLOW_AUTODIFF_TAMC
C-    assuming that we call S/R DIC_FIELDS_LOAD at each time-step and
C     with increasing time, this will catch when we need to load new records;
C     But with Adjoint run, this is not always the case => might end-up using
C     the wrong time-records
       IF ( intime0.NE.intimeP .OR. myIter.EQ.nIter0 ) THEN
#else /* ALLOW_AUTODIFF_TAMC */
C-    Make no assumption on sequence of calls to DIC_FIELDS_LOAD ;
C     This is the correct formulation (works in Adjoint run).
C     Unfortunatly, produces many recomputations <== not used until it is fixed
       IF ( intime1.NE.DIC_ldRec(bi,bj) ) THEN
#endif /* ALLOW_AUTODIFF_TAMC */

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
         CALL READ_REC_XY_RS( DIC_silicaFile,silica0,intime0,
     &        myIter,myThid )
         CALL READ_REC_XY_RS( DIC_silicaFile,silica1,intime1,
     &        myIter,myThid )
        ENDIF
        IF ( DIC_iceFile .NE. ' '  ) THEN
         CALL READ_REC_XY_RS( DIC_iceFile,ice0,intime0,
     &       myIter,myThid )
         CALL READ_REC_XY_RS( DIC_iceFile,ice1,intime1,
     &       myIter,myThid )
        ENDIF
#ifdef READ_PAR
        IF ( Filename1 .NE. ' '  ) THEN
         CALL READ_REC_XY_RS( Filename1,par0,intime0,
     &       myIter,myThid )
         CALL READ_REC_XY_RS( Filename1,par1,intime1,
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
        _EXCH_XY_RS(silica0, myThid )
        _EXCH_XY_RS(silica1, myThid )
        _EXCH_XY_RS(ice0, myThid )
        _EXCH_XY_RS(ice1, myThid )
#ifdef READ_PAR
        _EXCH_XY_RS(par0, myThid )
        _EXCH_XY_RS(par1, myThid )
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
c          pisvel(i,j,bi,bj)  =0.337*wind(i,j,bi,bj)**2/3.6d5    !QQQQ
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
         IF ( DIC_silicaFile .NE. ' '  ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             SILICA(i,j,bi,bj) = bWght*silica0(i,j,bi,bj)
     &                         + aWght*silica1(i,j,bi,bj)
            ENDDO
           ENDDO
         ENDIF
         IF ( DIC_iceFile .NE. ' '  ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             FIce(i,j,bi,bj) = bWght*ice0(i,j,bi,bj)
     &                       + aWght*ice1(i,j,bi,bj)
            ENDDO
           ENDDO
         ENDIF

#ifdef READ_PAR
         IF ( Filename1 .NE. ' '  ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             PAR(i,j,bi,bj) = bWght*par0(i,j,bi,bj)
     &                      + aWght*par1(i,j,bi,bj)
            ENDDO
           ENDDO
         ELSE
            WRITE(msgBuf,'(2A)')
     &       ' DIC_FIELDS_LOAD: You need to provide ',
     &       ' a file if you want to use READ_PAR'
            CALL PRINT_ERROR( msgBuf, myThid )
            STOP 'ABNORMAL END: S/R DIC_FIELDS_LOAD'
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

#endif /* ALLOW_DIC */
      RETURN
      END
