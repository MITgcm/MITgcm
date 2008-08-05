C $Header: /u/gcmpack/MITgcm/pkg/cheapaml/cheapaml_fields_load.F,v 1.1 2008/08/05 21:49:31 jmc Exp $
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
c #include "GRID.h"
c #include "DYNVARS.h"
C #include "BULKF.h"
#ifdef ALLOW_THSICE
#include "THSICE_VARS.h"
#endif
#include "CHEAPAML.h"
 
C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid - Thread no. that called this routine.
C     myTime - Simulation time
C     myIter - Simulation timestep number
C     dsolms - Solar variation at Southern boundary
C     dsolmn - Solar variation at Northern boundary
c     xphaseinit - user input initial phase of year relative
c     to mid winter.  E.G. xphaseinit = pi implies time zero
c     is mid summer.  
      INTEGER myThid
      _RL     myTime
      _RL     local ,bump
c      _RL     dsolms,dsolmn
c      _RL     xphaseinit
      INTEGER myIter
	INTEGER jg
 
C     !LOCAL VARIABLES:
C     === Local arrays ===
C     trair[01]  :: Relaxation temp. profile for air temperature
C     qrair[01]  :: Relaxation specific humidity profile for air
C     solar[01]  :: short wave flux
C     uwind[01]  :: zonal wind
C     vwind[01]  :: meridional wind

C     aWght, bWght :: Interpolation weights
      COMMON /BULKFFIELDS/
     &                 trair0,
     &                 trair1,
     &                 qrair0,
     &                 qrair1
 
      _RS  trair0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  trair1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  qrair0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  qrair1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  Solar0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  Solar1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  uwind0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  uwind1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  vwind0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  vwind1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      INTEGER bi,bj,i,j,intime0,intime1
      _RL aWght,bWght,rdt
      _RL ssq0,ssq1,ssq2,lath,p0,ssqa,q
c xsolph - phase of year, assuming time zero is mid winter 
c xinxx - cos ( xsolph )
      _RL xsolph,xinxx
      INTEGER nForcingPeriods,Imytm,Ifprd,Ifcyc,Iftm
c coefficients used to compute saturation specific humidity
      DATA   ssq0,           ssq1,           ssq2 
     &     / 3.797915 _d 0 , 7.93252 _d -6 , 2.166847 _d -3 /

c latent heat (J/kg)
      lath=2.5d6
c sea level pressure
      p0=1000.d0

      IF ( periodicExternalForcing ) THEN

      write(*,*) 'TEST 1 ========================='

c the objective here is to give cheapaml a default periodic forcing
c consisting only of annually varying solar forcing, and thus Trelaxation
c variation.  everything else, relative humidity, wind, are fixed.  This
c keys off of solardata.  if a solar data file exists, the model will 
c assume there are files to be read and interpolated between, as is standard
c for the MITGCM.

      IF ( SolarFile .EQ. ' '  ) THEN
         if ( myIter .EQ. nIter0 )then
         WRITE(*,*)
     &  'S/R  Assuming Standard Annually Varying Solar Forcing'
         endif	
	     xsolph=myTime*2.d0*3.14159 _d 0/365. _d 0/86400. _d 0
	     xinxx=cos(xsolph+xphaseinit+3.14159 _d 0)
      DO bj=1,nSy
         DO bi=1,nSx
            DO j=1,sNy
               DO i=1,sNx
                  jG = myYGlobalLo-1+(bj-1)*sNy+j
            local=225.d0+dsolms*xinxx-float((jg-1))/float((ny-1))*
     &        (37.5d0-dsolmn*xinxx)
            if ( jG .le. 3 ) local = local + 200
                  Solar(i,j,bi,bj) = local
               ENDDO
            ENDDO
         ENDDO
      ENDDO
       _EXCH_XY_RS(solar, mythid)
c relaxation temperature in radiative equilibrium
      DO bj=1,nSy
         DO bi=1,nSx
            DO j=1,sNy
               DO i=1,sNx
          jG = myYGlobalLo-1+(bj-1)*sNy+j
          local=solar(i,j,bi,bj)
          local=(2.d0*local/stefan)**(0.25d0)-273.16
          bump=-5.d0*EXP(-(float(jg-127)*float(jg-127))/1920.0)
          local=local+bump
          TR(i,j,bi,bj) = local
               ENDDO
            ENDDO
         ENDDO
      ENDDO
	_EXCH_XY_RS(TR, mythid)
c default specific humidity profile to 80% relative humidity
      DO bj=1,nSy
         DO bi=1,nSx
            DO j=1,sNy
               DO i=1,sNx
c                  jG = myYGlobalLo-1+(bj-1)*sNy+j
                  local = Tr(i,j,bi,bj)+273.16d0
              ssqa = ssq0*exp( lath*(ssq1-ssq2/local)) / p0
                  qr(i,j,bi,bj) = 0.8d0*ssqa
               ENDDO
            ENDDO
         ENDDO
      ENDDO
	_EXCH_XY_RS(qr, mythid)
c u wind field
      DO bj=1,nSy
         DO bi=1,nSx
            DO j=1,sNy
               DO i=1,sNx
                  jG = myYGlobalLo-1+(bj-1)*sNy+j
	local=-5.d0*cos(2.d0*pi*float(jg-1)/(float(ny-1)))
                  uwind(i,j,bi,bj) = local
               ENDDO
            ENDDO
         ENDDO
      ENDDO
	_EXCH_XY_RS(uwind, mythid)
c v wind field
      DO bj=1,nSy
         DO bi=1,nSx
            DO j=1,sNy
               DO i=1,sNx
                  jG = myYGlobalLo-1+(bj-1)*sNy+j
                  vwind(i,j,bi,bj) = 0.d0
               ENDDO
            ENDDO
         ENDDO
      ENDDO
	_EXCH_XY_RS(vwind, mythid)
	else

c here for usual interpolative forcings
C First call requires that we initialize everything to zero for safety
      IF ( myIter .EQ. nIter0 ) THEN
       CALL LEF_ZERO( trair0 ,myThid )
       CALL LEF_ZERO( trair1 ,myThid )
       CALL LEF_ZERO( qrair0 ,myThid )
       CALL LEF_ZERO( qrair1 ,myThid )
       CALL LEF_ZERO( solar0 ,myThid )
       CALL LEF_ZERO( solar1 ,myThid )
       CALL LEF_ZERO( uwind0 ,myThid )
       CALL LEF_ZERO( uwind1 ,myThid )
       CALL LEF_ZERO( vwind0 ,myThid )
       CALL LEF_ZERO( vwind1 ,myThid )
      ENDIF

C Now calculate whether it is time to update the forcing arrays
      rdt=1. _d 0 / deltaTclock
      nForcingPeriods=
     &  int(externForcingCycle/externForcingPeriod+0.5)
      Imytm=int(myTime*rdt+0.5)
      Ifprd=int(externForcingPeriod*rdt+0.5)
      Ifcyc=int(externForcingCycle*rdt+0.5)
      Iftm=mod( Imytm+Ifcyc-Ifprd/2 ,Ifcyc)

      intime0=int(Iftm/Ifprd)
      intime1=mod(intime0+1,nForcingPeriods)
c     aWght=float( Iftm-Ifprd*intime0 )/float( Ifprd )
      aWght=dfloat( Iftm-Ifprd*intime0 )/dfloat( Ifprd )
      bWght=1.-aWght

      intime0=intime0+1
      intime1=intime1+1

      IF (
     &  Iftm-Ifprd*(intime0-1) .EQ. 0
     &  .OR. myIter .EQ. nIter0
     & ) THEN

       _BEGIN_MASTER(myThid)

C      If the above condition is met then we need to read in
C      data for the period ahead and the period behind myTime.
       WRITE(*,*)
     &  'S/R CHEAPAML_FIELDS_LOAD'
      IF ( SolarFile .NE. ' '  ) THEN
       CALL READ_REC_XY_RS( SolarFile,solar0,intime0,
     &       myIter,myThid )
       CALL READ_REC_XY_RS( SolarFile,solar1,intime1,
     &       myIter,myThid )
      ENDIF
      IF ( TrFile .NE. ' '  ) THEN
       CALL READ_REC_XY_RS( TRFile,trair0,intime0,
     &       myIter,myThid )
       CALL READ_REC_XY_RS( TRFile,trair1,intime1,
     &       myIter,myThid )
	ENDIF
      IF ( QrFile .NE. ' '  ) THEN
       CALL READ_REC_XY_RS( QrFile,qrair0,intime0,
     &       myIter,myThid )
       CALL READ_REC_XY_RS( QrFile,qrair1,intime1,
     &       myIter,myThid )
	ENDIF
      IF ( UWindFile .NE. ' '  ) THEN
       CALL READ_REC_XY_RS( UWindFile,uwind0,intime0,
     &       myIter,myThid )
       CALL READ_REC_XY_RS( UWindFile,uwind1,intime1,
     &       myIter,myThid )
	ENDIF
      IF ( VWindFile .NE. ' '  ) THEN
       CALL READ_REC_XY_RS( VWindFile,vwind0,intime0,
     &       myIter,myThid )
       CALL READ_REC_XY_RS( VWindFile,vwind1,intime1,
     &       myIter,myThid )
	ENDIF

       _END_MASTER(myThid)
C
       _EXCH_XY_R4(trair0 , myThid )
       _EXCH_XY_R4(qrair0 , myThid )
       _EXCH_XY_R4(solar0 , myThid )
       _EXCH_XY_R4(uwind0 , myThid )
       _EXCH_XY_R4(vwind0 , myThid )
       _EXCH_XY_R4(trair1 , myThid )
       _EXCH_XY_R4(qrair1 , myThid )
       _EXCH_XY_R4(solar1 , myThid )
       _EXCH_XY_R4(uwind1 , myThid )
       _EXCH_XY_R4(vwind1 , myThid )
C
      ENDIF

C--   Interpolate TR, QR, SOLAR
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          TR(i,j,bi,bj)   = bWght*trair0(i,j,bi,bj)  
     &                     +aWght*trair1(i,j,bi,bj)   !+273.15
          qr(i,j,bi,bj)   = bWght*qrair0(i,j,bi,bj)  
     &                     +aWght*qrair1(i,j,bi,bj)  
          uwind(i,j,bi,bj)   = bWght*uwind0(i,j,bi,bj)  
     &                     +aWght*uwind1(i,j,bi,bj)   
          vwind(i,j,bi,bj)   = bWght*vwind0(i,j,bi,bj)  
     &                     +aWght*vwind1(i,j,bi,bj)   
          solar(i,j,bi,bj)   = bWght*solar0(i,j,bi,bj)  
     &                     +aWght*solar1(i,j,bi,bj)   
         ENDDO
        ENDDO
       ENDDO
      ENDDO
	endif
c end of periodic forcing options, on to steady option

      ELSE
       IF ( myIter .EQ. nIter0 ) THEN
        IF ( SolarFile .NE. ' '  ) THEN
         CALL READ_FLD_XY_RS( SolarFile,solar,' ',0,myThid )
	ELSE
      DO bj=1,nSy
         DO bi=1,nSx
            DO j=1,sNy
               DO i=1,sNx
                  jG = myYGlobalLo-1+(bj-1)*sNy+j
           local=225.d0-float((jg-1))/float((ny-1))*37.5d0
           IF ( jG .le. 3 ) local =local + 200
                  Solar(i,j,bi,bj) = local
               ENDDO
            ENDDO
         ENDDO
      ENDDO
	_EXCH_XY_RS(solar, mythid)
        ENDIF
        IF ( TrFile .NE. ' '  ) THEN
         CALL READ_FLD_XY_RS( TrFile,tr,' ',0,myThid )
	ELSE
      DO bj=1,nSy
         DO bi=1,nSx
            DO j=1,sNy
               DO i=1,sNx
        jG = myYGlobalLo-1+(bj-1)*sNy+j
	    local=solar(i,j,bi,bj)
	    local=(2.d0*local/stefan)**(0.25d0)-273.16
        bump=-5.d0*EXP(-(float(jg-127)*float(jg-127))/1920.0)
        local=local+bump
                  TR(i,j,bi,bj) = local
               ENDDO
            ENDDO
         ENDDO
      ENDDO
	_EXCH_XY_RS(TR, mythid)
        ENDIF
c do specific humidity
        IF ( QrFile .NE. ' '  ) THEN
         CALL READ_FLD_XY_RS( QrFile,qr,' ',0,myThid )
	ELSE
c default specific humidity profile to 80% relative humidity
      DO bj=1,nSy
         DO bi=1,nSx
            DO j=1,sNy
               DO i=1,sNx
c                  jG = myYGlobalLo-1+(bj-1)*sNy+j
                  local = Tr(i,j,bi,bj)+273.16d0
              ssqa = ssq0*exp( lath*(ssq1-ssq2/local)) / p0
                  qr(i,j,bi,bj) = 0.8d0*ssqa
               ENDDO
            ENDDO
         ENDDO
      ENDDO
	_EXCH_XY_RS(qr, mythid)
        ENDIF
        IF ( UWindFile .NE. ' '  ) THEN
         CALL READ_FLD_XY_RS( UWindFile,uwind,' ',0,myThid )
	ELSE
      DO bj=1,nSy
         DO bi=1,nSx
            DO j=1,sNy
               DO i=1,sNx
                  jG = myYGlobalLo-1+(bj-1)*sNy+j
c mod for debug
c to return to original code, uncomment following line
c comment out 2nd line
	local=-5.d0*cos(2.d0*pi*float(jg-1)/(float(ny-1)))
c	local=0.d0*cos(2.d0*pi*float(jg-1)/(float(ny-1)))
                  uwind(i,j,bi,bj) = local
               ENDDO
            ENDDO
         ENDDO
      ENDDO
	_EXCH_XY_RS(uwind, mythid)
        ENDIF
        IF ( VWindFile .NE. ' '  ) THEN
         CALL READ_FLD_XY_RS( VWindFile,vwind,' ',0,myThid )
	ELSE
      DO bj=1,nSy
         DO bi=1,nSx
            DO j=1,sNy
               DO i=1,sNx
                  jG = myYGlobalLo-1+(bj-1)*sNy+j
                  vwind(i,j,bi,bj) = 0.d0
               ENDDO
            ENDDO
         ENDDO
      ENDDO
	_EXCH_XY_RS(vwind, mythid)
        ENDIF
       ENDIF
C endif for periodicForcing
      ENDIF

      RETURN
      END
