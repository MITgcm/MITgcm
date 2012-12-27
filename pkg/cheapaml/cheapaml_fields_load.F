C $Header: /u/gcmpack/MITgcm/pkg/cheapaml/cheapaml_fields_load.F,v 1.18 2012/12/27 18:41:03 jmc Exp $
C $Name:  $

#include "CHEAPAML_OPTIONS.h"


C     !ROUTINE: CHEAPAML_FIELDS_LOAD
C     !INTERFACE:
      SUBROUTINE CHEAPAML_FIELDS_LOAD( myTime, myIter, myThid )
C     *==========================================================*
C     | SUBROUTINE CHEAPAML_FIELDS_LOAD
C     | o Control reading of fields from external source.
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "CHEAPAML.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myIter :: Simulation timestep number
C     myTime :: Simulation time
C     myThid :: Thread no. that called this routine.
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
C     === in common block ===
C     trair[01]   :: Relaxation temp. profile for air temperature
C     qrair[01]   :: Relaxation specific humidity profile for air
C     solar[01]   :: short wave flux
C     uwind[01]   :: zonal wind
C     vwind[01]   :: meridional wind
C     ustress[01] ::
C     vstress[01] ::
C     wavesh[01]  ::
C     wavesp[01]  ::
C     rair[01]    ::
C     CheaptracerR[01] :: Relaxation profile for passive tracer

      COMMON /BULKFFIELDS/
     &         trair0,   trair1,
     &         qrair0,   qrair1,
     &         Solar0,   Solar1,
     &         uwind0,   uwind1,
     &         vwind0,   vwind1,
     &         ustress0, ustress1,
     &         vstress0, vstress1,
     &         wavesh0,  wavesh1,
     &         wavesp0,  wavesp1,
     &         rair0,    rair1,
     &         CheaptracerR0, CheaptracerR1

      _RL  trair0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  trair1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  qrair0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  qrair1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  Solar0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  Solar1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  uwind0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  uwind1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  vwind0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  vwind1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  ustress0  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  ustress1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  vstress0  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  vstress1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  wavesh0  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  wavesh1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  wavesp0  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  wavesp1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  rair0  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  rair1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  CheaptracerR0 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  CheaptracerR1 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

C     === Local arrays ===
C     dsolms - Solar variation at Southern boundary
C     dsolmn - Solar variation at Northern boundary
C     xphaseinit - user input initial phase of year relative to mid winter.
C                  e.g. xphaseinit = pi implies time zero is mid summer.
C     cheaph[01]  ::
C     cheapcl[01] ::
C     cheaplw[01] ::
C     aWght,bWght :: Interpolation weights

c     _RL     dsolms,dsolmn
c     _RL     xphaseinit
      _RL  cheaph0   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  cheaph1   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  cheapcl0   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  cheapcl1   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  cheaplw0   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  cheaplw1   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      INTEGER bi, bj
      INTEGER i, j, iG, jG
      INTEGER intimeP, intime0, intime1
      _RL     aWght, bWght

      _RL recipNym1, local, u
      _RL ssq0,ssq1,ssq2,ssqa
C xsolph - phase of year, assuming time zero is mid winter
C xinxx - cos ( xsolph )
      _RL xsolph,xinxx
C coefficients used to compute saturation specific humidity
      DATA   ssq0,           ssq1,           ssq2
     &     / 3.797915 _d 0 , 7.93252 _d -6 , 2.166847 _d -3 /

      recipNym1 = Ny - 1
      IF ( Ny.GT.1 ) recipNym1 = 1. _d 0 / recipNym1

      IF ( periodicExternalForcing_cheap ) THEN

C the objective here is to give cheapaml a default periodic forcing
C consisting only of annually varying solar forcing, and thus Trelaxation
C variation.  everything else, relative humidity, wind, are fixed.  This
C keys off of solardata.  if a solar data file exists, the model will
C assume there are files to be read and interpolated between, as is standard
C for the MITgcm.

C!BD bug a corriger:
       IF ( SolarFile .EQ. ' '  ) THEN
        IF ( useStressOption ) THEN
          WRITE(*,*)' stress option is turned on.  this is not ',
     &    'consistent with the default time dependent forcing option'
          STOP
        ENDIF
        IF ( myIter .EQ. nIter0 ) THEN
         WRITE(*,*)
     &  'S/R  Assuming Standard Annually Varying Solar Forcing'
        ENDIF
         xsolph=myTime*2. _d 0*3.14159 _d 0/365. _d 0/86400. _d 0
         xinxx=COS(xsolph+xphaseinit+3.14159 _d 0)
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j=1,sNy
            DO i=1,sNx
              jG = myYGlobalLo-1+(bj-1)*sNy+j
              local = 225. _d 0 + dsolms*xinxx
     &              - (jG-1)*recipNym1*(37.5 _d 0-dsolmn*xinxx)
              Solar(i,j,bi,bj) = local
            ENDDO
           ENDDO
          ENDDO
         ENDDO
         _EXCH_XY_RL( solar, myThid )
C relaxation temperature in radiative equilibrium
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j=1,sNy
            DO i=1,sNx
              local = solar(i,j,bi,bj)
              local = (2. _d 0*local/stefan)**(0.25 _d 0)-celsius2K
              TR(i,j,bi,bj) = local
            ENDDO
           ENDDO
          ENDDO
         ENDDO
         _EXCH_XY_RL( TR, myThid )
C default specific humidity profile to 80% relative humidity
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j=1,sNy
            DO i=1,sNx
              local = Tr(i,j,bi,bj)+celsius2K
              ssqa = ssq0*exp( lath*(ssq1-ssq2/local)) / p0
              qr(i,j,bi,bj) = 0.8 _d 0*ssqa
            ENDDO
           ENDDO
          ENDDO
         ENDDO
         _EXCH_XY_RL( qr, myThid )
C u wind field
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j=1,sNy
            DO i=1,sNx
              jG = myYGlobalLo-1+(bj-1)*sNy+j
              local = -5. _d 0*COS( 2. _d 0*PI*(jG-1)*recipNym1 )
              uwind(i,j,bi,bj) = local
            ENDDO
           ENDDO
          ENDDO
         ENDDO
         _EXCH_XY_RL( uwind, myThid )
C v wind field
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j=1,sNy
            DO i=1,sNx
              vwind(i,j,bi,bj) = 0. _d 0
            ENDDO
           ENDDO
          ENDDO
         ENDDO
         _EXCH_XY_RL( vwind, myThid )
C Tracer field
         IF ( useCheapTracer ) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j=1,sNy
            DO i=1,sNx
              CheaptracerR(i,j,bi,bj) = 290. _d 0
            ENDDO
           ENDDO
          ENDDO
         ENDDO
         ENDIF
          _EXCH_XY_RL( CheaptracerR, myThid )

C else (case solarFile is not empty)
       ELSE

C here for usual interpolative forcings
C First call requires that we initialize everything to zero for safety
        IF ( myIter .EQ. nIter0 ) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             trair0  (i,j,bi,bj) = 0.
             trair1  (i,j,bi,bj) = 0.
             qrair0  (i,j,bi,bj) = 0.
             qrair1  (i,j,bi,bj) = 0.
             solar0  (i,j,bi,bj) = 0.
             solar1  (i,j,bi,bj) = 0.
             uwind0  (i,j,bi,bj) = 0.
             uwind1  (i,j,bi,bj) = 0.
             vwind0  (i,j,bi,bj) = 0.
             vwind1  (i,j,bi,bj) = 0.
             cheaph0 (i,j,bi,bj) = 0.
             cheaph1 (i,j,bi,bj) = 0.
             cheapcl0(i,j,bi,bj) = 0.5
             cheapcl1(i,j,bi,bj) = 0.5
             cheaplw0(i,j,bi,bj) = 0.
             cheaplw1(i,j,bi,bj) = 0.
             ustress0(i,j,bi,bj) = 0.
             ustress1(i,j,bi,bj) = 0.
             vstress0(i,j,bi,bj) = 0.
             vstress1(i,j,bi,bj) = 0.
             wavesh0 (i,j,bi,bj) = 0.
             wavesh1 (i,j,bi,bj) = 0.
             wavesp0 (i,j,bi,bj) = 0.
             wavesp1 (i,j,bi,bj) = 0.
             rair0   (i,j,bi,bj) = 0.
             rair1   (i,j,bi,bj) = 0.
             CheaptracerR0 (i,j,bi,bj) = 0.
             CheaptracerR1 (i,j,bi,bj) = 0.
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDIF

C--   Now calculate whether it is time to update the forcing arrays
        CALL GET_PERIODIC_INTERVAL(
     O           intimeP, intime0, intime1, bWght, aWght,
     I           externForcingCycle_cheap, externForcingPeriod_cheap,
     I           deltaTclock, myTime, myThid )

        IF ( intime0.NE.intimeP .OR. myIter.EQ.nIter0 ) THEN

C      If the above condition is met then we need to read in
C      data for the period ahead and the period behind myTime.
         WRITE(*,*) 'S/R CHEAPAML_FIELDS_LOAD'
         IF ( SolarFile .NE. ' ' ) THEN
          CALL READ_REC_XY_RL( SolarFile,solar0,intime0,
     &                         myIter, myThid )
          CALL READ_REC_XY_RL( SolarFile,solar1,intime1,
     &                         myIter, myThid )
         ENDIF
         IF ( TrFile .NE. ' ' ) THEN
          CALL READ_REC_XY_RL( TRFile,trair0,intime0,
     &                         myIter, myThid )
          CALL READ_REC_XY_RL( TRFile,trair1,intime1,
     &                         myIter, myThid )
         ENDIF
         IF ( QrFile .NE. ' ' ) THEN
          CALL READ_REC_XY_RL( QrFile,qrair0,intime0,
     &                         myIter, myThid )
          CALL READ_REC_XY_RL( QrFile,qrair1,intime1,
     &                         myIter, myThid )
         ENDIF
         IF ( UWindFile .NE. ' ' ) THEN
          CALL READ_REC_XY_RL( UWindFile,uwind0,intime0,
     &                         myIter, myThid )
          CALL READ_REC_XY_RL( UWindFile,uwind1,intime1,
     &                         myIter, myThid )
         ENDIF
         IF ( VWindFile .NE. ' ' ) THEN
          CALL READ_REC_XY_RL( VWindFile,vwind0,intime0,
     &                         myIter, myThid )
          CALL READ_REC_XY_RL( VWindFile,vwind1,intime1,
     &                         myIter, myThid )
         ENDIF
         IF ( usetimevarblh .AND. cheap_hfile .NE. ' ' ) THEN
          CALL READ_REC_XY_RL( cheap_hfile,cheaph0,intime0,
     &                         myIter, myThid )
          CALL READ_REC_XY_RL( cheap_hfile,cheaph1,intime1,
     &                         myIter, myThid )
         ENDIF
         IF ( useclouds .AND. cheap_clfile .NE. ' ' ) THEN
          CALL READ_REC_XY_RL( cheap_clfile,cheapcl0,intime0,
     &                         myIter, myThid )
          CALL READ_REC_XY_RL( cheap_clfile,cheapcl1,intime1,
     &                         myIter, myThid )
         ENDIF
         IF ( usedlongwave .AND. cheap_dlwfile .NE. ' ' ) THEN
          CALL READ_REC_XY_RL( cheap_dlwfile,cheaplw0,intime0,
     &                         myIter, myThid )
          CALL READ_REC_XY_RL( cheap_dlwfile,cheaplw1,intime1,
     &                         myIter, myThid )
         ENDIF
         IF ( useStressOption .AND. UStressFile .NE. ' ' ) THEN
          CALL READ_REC_XY_RL( UStressFile,ustress0,intime0,
     &                         myIter, myThid )
          CALL READ_REC_XY_RL( UStressFile,ustress1,intime1,
     &                         myIter, myThid )
         ENDIF
         IF ( useStressOption .AND. VStressFile .NE. ' ' ) THEN
          CALL READ_REC_XY_RL( VStressFile,vstress0,intime0,
     &                         myIter, myThid )
          CALL READ_REC_XY_RL( VStressFile,vstress1,intime1,
     &                         myIter, myThid )
         ENDIF
         IF ( FluxFormula.EQ.'COARE3' .AND. WaveHFile.NE.' ' ) THEN
          CALL READ_REC_XY_RL( WaveHFile,wavesh0,intime0,
     &                         myIter, myThid )
          CALL READ_REC_XY_RL( WaveHFile,wavesh1,intime1,
     &                         myIter, myThid )
         ENDIF
         IF ( FluxFormula.EQ.'COARE3' .AND. WavePFile.NE.' ' ) THEN
          CALL READ_REC_XY_RL( WavePFile,wavesp0,intime0,
     &                         myIter, myThid )
          CALL READ_REC_XY_RL( WavePFile,wavesp1,intime1,
     &                         myIter, myThid )
         ENDIF
         IF ( useCheapTracer .AND. TracerRFile .NE. ' ' ) THEN
           CALL READ_REC_XY_RL( TracerRFile,CheaptracerR0,intime0,
     &                         myIter, myThid )
           CALL READ_REC_XY_RL( TracerRFile,CheaptracerR1,intime1,
     &                         myIter, myThid )
         ENDIF
         _EXCH_XY_RL( trair0 , myThid )
         _EXCH_XY_RL( qrair0 , myThid )
         _EXCH_XY_RL( solar0 , myThid )
         _EXCH_XY_RL( uwind0 , myThid )
         _EXCH_XY_RL( vwind0 , myThid )
         _EXCH_XY_RL( trair1 , myThid )
         _EXCH_XY_RL( qrair1 , myThid )
         _EXCH_XY_RL( solar1 , myThid )
         _EXCH_XY_RL( uwind1 , myThid )
         _EXCH_XY_RL( vwind1 , myThid )
         IF ( usetimevarblh ) THEN
         _EXCH_XY_RL( cheaph0, myThid )
         _EXCH_XY_RL( cheaph1 , myThid )
         ENDIF
         IF ( useclouds ) THEN
         _EXCH_XY_RL( cheapcl0, myThid )
         _EXCH_XY_RL( cheapcl1 , myThid )
         ENDIF
         IF ( usedlongwave ) THEN
         _EXCH_XY_RL( cheaplw0, myThid )
         _EXCH_XY_RL( cheaplw1 , myThid )
         ENDIF
         IF ( useStressOption ) THEN
         _EXCH_XY_RL( uwind0 , myThid )
         _EXCH_XY_RL( vwind0 , myThid )
         _EXCH_XY_RL( uwind1 , myThid )
         _EXCH_XY_RL( vwind1 , myThid )
          ENDIF
          IF ( FluxFormula.EQ.'COARE3' ) THEN
         _EXCH_XY_RL( wavesp0 , myThid )
         _EXCH_XY_RL( wavesp1 , myThid )
         _EXCH_XY_RL( wavesh0 , myThid )
         _EXCH_XY_RL( wavesh1 , myThid )
          ENDIF
         IF ( useCheapTracer ) THEN
         _EXCH_XY_RL( CheaptracerR0 , myThid )
         _EXCH_XY_RL( CheaptracerR1, myThid )
         ENDIF

C     end of loading new fields block
        ENDIF

C--   Interpolate TR, QR, SOLAR
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            TR(i,j,bi,bj)           = bWght*trair0(i,j,bi,bj)
     &                              + aWght*trair1(i,j,bi,bj)   !+273.15
            qr(i,j,bi,bj)           = bWght*qrair0(i,j,bi,bj)
     &                              + aWght*qrair1(i,j,bi,bj)
            uwind(i,j,bi,bj)        = bWght*uwind0(i,j,bi,bj)
     &                              + aWght*uwind1(i,j,bi,bj)
            vwind(i,j,bi,bj)        = bWght*vwind0(i,j,bi,bj)
     &                              + aWght*vwind1(i,j,bi,bj)
            solar(i,j,bi,bj)        = bWght*solar0(i,j,bi,bj)
     &                              + aWght*solar1(i,j,bi,bj)
            IF ( useStressOption ) THEN
             ustress(i,j,bi,bj)     = bWght*ustress0(i,j,bi,bj)
     &                              + aWght*ustress1(i,j,bi,bj)
             vstress(i,j,bi,bj)     = bWght*vstress0(i,j,bi,bj)
     &                              + aWght*vstress1(i,j,bi,bj)
            ENDIF
            IF ( usetimevarblh ) THEN
             cheaphgrid(i,j,bi,bj)  = bWght*cheaph0(i,j,bi,bj)
     &                              + aWght*cheaph1(i,j,bi,bj)
            ENDIF
            IF ( useclouds ) THEN
             cheapclouds(i,j,bi,bj) = bWght*cheapcl0(i,j,bi,bj)
     &                              + aWght*cheapcl1(i,j,bi,bj)
            ENDIF
            IF ( usedlongwave ) THEN
             cheapdlongwave(i,j,bi,bj) = bWght*cheaplw0(i,j,bi,bj)
     &                                 + aWght*cheaplw1(i,j,bi,bj)
            ENDIF
            IF ( useCheapTracer ) THEN
             CheaptracerR(i,j,bi,bj) = bWght*CheaptracerR0(i,j,bi,bj)
     &                               + aWght*CheaptracerR1(i,j,bi,bj)
            ENDIF
            IF ( FluxFormula.EQ.'COARE3' ) THEN
             IF ( WaveHFile.NE.' ' ) THEN
              wavesh(i,j,bi,bj)     = bWght*wavesh0(i,j,bi,bj)
     &                              + aWght*wavesh1(i,j,bi,bj)
             ENDIF
             IF ( WavePFile.NE.' ' ) THEN
              wavesp(i,j,bi,bj)     = bWght*wavesp0(i,j,bi,bj)
     &                              + aWght*wavesp1(i,j,bi,bj)
             ENDIF
            ELSE
             u = uwind(i,j,bi,bj)**2 + vwind(i,j,bi,bj)**2
             u = SQRT(u)
             wavesp(i,j,bi,bj) = 0.729 _d 0 * u
             wavesh(i,j,bi,bj) = 0.018 _d 0 * u*u*(1. + .015 _d 0 *u)
            ENDIF
           ENDDO
          ENDDO
         ENDDO
        ENDDO

C end if/else solarFile is empty
       ENDIF

C end of periodic forcing options, on to steady option
      ELSE

       IF ( myIter .EQ. nIter0 ) THEN
        IF ( SolarFile .NE. ' ' ) THEN
         CALL READ_FLD_XY_RS( SolarFile,' ',solar,0,myThid )
        ELSE
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j=1,sNy
            DO i=1,sNx
              jG = myYGlobalLo-1+(bj-1)*sNy+j
              local = 225. _d 0 - (jG-1)*recipNym1*37.5 _d 0
              Solar(i,j,bi,bj) = local
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDIF
        _EXCH_XY_RL( solar, myThid )

        IF ( TrFile .NE. ' '  ) THEN
         CALL READ_FLD_XY_RL( TrFile,' ',tr,0,myThid )
        ELSE
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j=1,sNy
            DO i=1,sNx
              local = solar(i,j,bi,bj)
              local = (2. _d 0*local/stefan)**(0.25 _d 0)-273.16 _d 0
              TR(i,j,bi,bj) = local
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDIF
        _EXCH_XY_RL( TR, myThid )

C do specific humidity
        IF ( QrFile .NE. ' ') THEN
         CALL READ_FLD_XY_RL( QrFile,' ',qr,0,myThid )
        ELSE
C default specific humidity profile to 80% relative humidity
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j=1,sNy
            DO i=1,sNx
              local = Tr(i,j,bi,bj)+273.16 _d 0
              ssqa = ssq0*EXP( lath*(ssq1-ssq2/local)) / p0
              qr(i,j,bi,bj) = 0.8 _d 0*ssqa
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDIF
        _EXCH_XY_RL( qr, myThid )

        IF ( UWindFile .NE. ' '  ) THEN
         CALL READ_FLD_XY_RL( UWindFile,' ',uwind,0,myThid )
        ELSE
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j=1,sNy
            DO i=1,sNx
              jG = myYGlobalLo-1+(bj-1)*sNy+j
C mod for debug
C to return  to original code, uncomment following line
C comment out 2nd line
              local = -5. _d 0*COS( 2. _d 0*PI*(jG-1)*recipNym1 )
c             local =  0. _d 0*COS( 2. _d 0*PI*(jG-1)*recipNym1 )
              uwind(i,j,bi,bj) = local
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDIF
         _EXCH_XY_RL( uwind, myThid )

        IF ( VWindFile .NE. ' '  ) THEN
         CALL READ_FLD_XY_RL( VWindFile,' ',vwind,0,myThid )
        ELSE
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j=1,sNy
            DO i=1,sNx
              vwind(i,j,bi,bj) = 0. _d 0
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDIF
         _EXCH_XY_RL( vwind, myThid )

        IF ( useStressOption ) THEN
         IF ( UStressFile .NE. ' '  ) THEN
          CALL READ_FLD_XY_RL( UStressFile,' ',ustress,0,myThid )
         ELSE
          WRITE(*,*)' U Stress File absent with stress option'
          STOP
         ENDIF
         IF ( VStressFile .NE. ' '  ) THEN
          CALL READ_FLD_XY_RL( VStressFile,' ',vstress,0,myThid )
         ELSE
          WRITE(*,*)' V Stress File absent with stress option'
          STOP
         ENDIF
         _EXCH_XY_RL( ustress, myThid )
         _EXCH_XY_RL( vstress, myThid )
        ENDIF
        IF ( useCheapTracer ) THEN
         IF ( TracerRFile .NE. ' ' ) THEN
          CALL READ_FLD_XY_RL( TracerRFile,' ',CheaptracerR,0,myThid )
         ELSE
           DO bj = myByLo(myThid), myByHi(myThid)
            DO bi = myBxLo(myThid), myBxHi(myThid)
             DO j=1,sNy
              DO i=1,sNx
               CheaptracerR(i,j,bi,bj)=290. _d 0
              ENDDO
             ENDDO
            ENDDO
           ENDDO
         ENDIF
         _EXCH_XY_RL( CheaptracerR, myThid )
        ENDIF
        IF ( FluxFormula.EQ.'COARE3' ) THEN
         IF ( WaveHFile.NE.' ' ) THEN
          CALL READ_FLD_XY_RL( WaveHFile,' ',wavesh,0,myThid )
         ENDIF
         IF ( WavePFile.NE.' ' ) THEN
          CALL READ_FLD_XY_RL( WavePFile,' ',wavesp,0,myThid )
         ELSE
          DO bj = myByLo(myThid), myByHi(myThid)
           DO bi = myBxLo(myThid), myBxHi(myThid)
            DO j=1,sNy
             DO i=1,sNx
              u = uwind(i,j,bi,bj)**2 + vwind(i,j,bi,bj)**2
              u = SQRT(u)
              wavesp(i,j,bi,bj)=0.729 _d 0 * u
              wavesh(i,j,bi,bj)=0.018 _d 0 * u*u*(1. + .015 _d 0 *u)
             ENDDO
            ENDDO
           ENDDO
          ENDDO
         ENDIF
         _EXCH_XY_RL( wavesp, myThid )
         _EXCH_XY_RL( wavesh, myThid )
        ENDIF

C--    endif myIter = nIter0
       ENDIF

C     BL height is done in cheapaml_ini_varia

       IF ( useclouds ) THEN
         IF ( cheap_clfile .NE. ' ' ) THEN
          CALL READ_FLD_XY_RL( cheap_clfile,' ',Cheapclouds,0,myThid )
         ELSE
          DO bj = myByLo(myThid), myByHi(myThid)
           DO bi = myBxLo(myThid), myBxHi(myThid)
            DO j=1,sNy
             DO i=1,sNx
               Cheapclouds(i,j,bi,bj)=0.5
             ENDDO
            ENDDO
           ENDDO
          ENDDO
         ENDIF
         _EXCH_XY_RL( Cheapclouds, myThid )
       ENDIF

       IF ( usedlongwave ) THEN
         IF ( cheap_dlwfile .NE. ' ' ) THEN
          CALL READ_FLD_XY_RL(cheap_dlwfile,' ',Cheapdlongwave,0,myThid)
         ELSE
          WRITE(*,*) 'with usedlongwave = true,  you must provide',
     $               ' a downward longwave file'
          STOP
         ENDIF
         _EXCH_XY_RL( Cheapdlongwave, myThid )
       ENDIF

C endif for Steady Option
      ENDIF

C fill in outer edges

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          jG = myYGlobalLo-1+(bj-1)*sNy+j
          DO i=1-OLx,sNx+OLx
           iG=myXGlobalLo-1+(bi-1)*sNx+i

           IF (iG.LT.1) THEN

            Tr(i,j,bi,bj)=Tr(1,j,bi,bj)
            qr(i,j,bi,bj)=qr(1,j,bi,bj)
            uwind(i,j,bi,bj)=uwind(1,j,bi,bj)
            vwind(i,j,bi,bj)=vwind(1,j,bi,bj)
            Solar(i,j,bi,bj)=Solar(1,j,bi,bj)
            IF ( useStressOption ) THEN
              ustress(i,j,bi,bj)=ustress(1,j,bi,bj)
              vstress(i,j,bi,bj)=vstress(1,j,bi,bj)
            ENDIF
            IF ( useCheapTracer ) THEN
              CheaptracerR(i,j,bi,bj)=CheaptracerR(1,j,bi,bj)
            ENDIF
            IF ( FluxFormula.EQ.'COARE3' ) THEN
              wavesp(i,j,bi,bj)=wavesp(1,j,bi,bj)
              wavesh(i,j,bi,bj)=wavesh(1,j,bi,bj)
            ENDIF
            IF ( useclouds ) THEN
              Cheapclouds(i,j,bi,bj)=Cheapclouds(1,j,bi,bj)
            ENDIF
            IF ( usedlongwave ) THEN
              Cheapdlongwave(i,j,bi,bj)=Cheapdlongwave(1,j,bi,bj)
            ENDIF

           ELSEIF (iG.GT.Nx) THEN

            Tr(i,j,bi,bj)=Tr(sNx,j,bi,bj)
            qr(i,j,bi,bj)=qr(sNx,j,bi,bj)
            uwind(i,j,bi,bj)=uwind(sNx,j,bi,bj)
            vwind(i,j,bi,bj)=vwind(sNx,j,bi,bj)
            Solar(i,j,bi,bj)=Solar(sNx,j,bi,bj)
            IF ( useStressOption ) THEN
              ustress(i,j,bi,bj)=ustress(sNx,j,bi,bj)
              vstress(i,j,bi,bj)=vstress(sNx,j,bi,bj)
            ENDIF
            IF ( useCheapTracer ) THEN
              CheaptracerR(i,j,bi,bj)=CheaptracerR(sNx,j,bi,bj)
            ENDIF
            IF ( FluxFormula.EQ.'COARE3' ) THEN
              wavesp(i,j,bi,bj)=wavesp(sNx,j,bi,bj)
              wavesh(i,j,bi,bj)=wavesh(sNx,j,bi,bj)
            ENDIF
            IF ( useclouds ) THEN
              Cheapclouds(i,j,bi,bj)=Cheapclouds(sNx,j,bi,bj)
            ENDIF
            IF ( usedlongwave ) THEN
              Cheapdlongwave(i,j,bi,bj)=Cheapdlongwave(sNx,j,bi,bj)
            ENDIF

           ELSEIF (jG.LT.1) THEN

            Tr(i,j,bi,bj)=Tr(i,1,bi,bj)
            qr(i,j,bi,bj)=qr(i,1,bi,bj)
            uwind(i,j,bi,bj)=uwind(i,1,bi,bj)
            vwind(i,j,bi,bj)=vwind(i,1,bi,bj)
            Solar(i,j,bi,bj)=Solar(i,1,bi,bj)
            IF ( useStressOption ) THEN
              ustress(i,j,bi,bj)=ustress(i,1,bi,bj)
              vstress(i,j,bi,bj)=vstress(i,1,bi,bj)
            ENDIF
            IF ( useCheapTracer ) THEN
              CheaptracerR(i,j,bi,bj)=CheaptracerR(i,1,bi,bj)
            ENDIF
            IF ( useclouds ) THEN
              Cheapclouds(i,j,bi,bj)=Cheapclouds(i,1,bi,bj)
            ENDIF
            IF ( usedlongwave ) THEN
              Cheapdlongwave(i,j,bi,bj)=Cheapdlongwave(i,1,bi,bj)
            ENDIF
            IF ( FluxFormula.EQ.'COARE3' ) THEN
              wavesp(i,j,bi,bj)=wavesp(i,1,bi,bj)
              wavesh(i,j,bi,bj)=wavesh(i,1,bi,bj)
            ENDIF

           ELSEIF (jG.GT.Ny) THEN

            Tr(i,j,bi,bj)=Tr(i,sNy,bi,bj)
            qr(i,j,bi,bj)=qr(i,sNy,bi,bj)
            uwind(i,j,bi,bj)=uwind(i,sNy,bi,bj)
            vwind(i,j,bi,bj)=vwind(i,sNy,bi,bj)
            Solar(i,j,bi,bj)=Solar(i,sNy,bi,bj)
            IF ( useStressOption ) THEN
              ustress(i,j,bi,bj)=ustress(i,sNy,bi,bj)
              vstress(i,j,bi,bj)=vstress(i,sNy,bi,bj)
            ENDIF
            IF ( useCheapTracer ) THEN
              CheaptracerR(i,j,bi,bj)=CheaptracerR(i,sNy,bi,bj)
            ENDIF
            IF ( FluxFormula.EQ.'COARE3' ) THEN
              wavesp(i,j,bi,bj)=wavesp(i,sNy,bi,bj)
              wavesh(i,j,bi,bj)=wavesh(i,sNy,bi,bj)
            ENDIF
            IF ( useclouds ) THEN
              Cheapclouds(i,j,bi,bj)=Cheapclouds(i,sNy,bi,bj)
            ENDIF
            IF ( usedlongwave ) THEN
              Cheapdlongwave(i,j,bi,bj)=Cheapdlongwave(i,sNy,bi,bj)
            ENDIF

           ENDIF
          ENDDO
         ENDDO
       ENDDO
      ENDDO

      RETURN
      END
