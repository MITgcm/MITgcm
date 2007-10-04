#include "ctrparam.h"
#include "ATM2D_OPTIONS.h"

C     !INTERFACE:
      SUBROUTINE RELAX_ADD( wght0, wght1,
     &               intime0, intime1, iftime, myIter, myThid)
C     *==========================================================*
C     | Adds restoring terms to surface forcing. Note that:      |
C     |    - restoring is phased out as restor (or act.) SST <2C |
C     |    - if nsTypeRelax NE 0, salt rest. phased out nr poles |
C     |    - if ntTypeRelax NE 0, temp rest. phased out nr poles |
C     *==========================================================*
        IMPLICIT NONE

#include "ATMSIZE.h"
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "THSICE_VARS.h"
#include "ATM2D_VARS.h"

c include ocean and seaice vars

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     wght0, wght1   - weight of first and second month, respectively
C     intime0,intime1- month id # for first and second months
C     iftime - true -> prompts a reloading of data from disk
C     myIter - Ocean iteration number
C     myThid - Thread no. that called this routine.
      _RL  wght0
      _RL  wght1
      INTEGER intime0
      INTEGER intime1
      LOGICAL iftime
      INTEGER myIter
      INTEGER myThid

C     LOCAL VARIABLES:
C     Save below so that continual file reloads aren't necessary
      COMMON /OCEANRELAX/
     &                 sst0, sst1, sss0, sss1

      _RS  sst0(1-Olx:sNx+Olx,1-Oly:sNy+Oly,1,1)
      _RS  sst1(1-Olx:sNx+Olx,1-Oly:sNy+Oly,1,1)     
      _RS  sss0(1-Olx:sNx+Olx,1-Oly:sNy+Oly,1,1)    
      _RS  sss1(1-Olx:sNx+Olx,1-Oly:sNy+Oly,1,1)    
      _RL lambdaTheta,lambdaSalt
      _RS nearIce    ! constant used to phase out rest near frz point
      _RL qrelflux, frelflux
      _RL sstRelax(1:sNx,1:sNy) ! relaxation sst as computed from file
      _RL sssRelax(1:sNx,1:sNy) ! relaxation sss as computed from file
      INTEGER i,j

      IF (ifTime) THEN

C      If the above condition is met then we need to read in
C      data for the period ahead and the period behind current time.

        WRITE(*,*) 'S/R RELAX_ADD: Reading new data'
        IF ( thetaRelaxFile .NE. ' '  ) THEN
          CALL READ_REC_XY_RS( thetaRelaxFile,sst0,intime0,
     &                      myIter,myThid )
          CALL READ_REC_XY_RS( thetaRelaxFile,sst1,intime1,
     &                      myIter,myThid )
        ENDIF
        IF ( saltRelaxFile .NE. ' '  ) THEN
          CALL READ_REC_XY_RS( saltRelaxFile,sss0,intime0,
     &                      myIter,myThid )
          CALL READ_REC_XY_RS( saltRelaxFile,sss1,intime1,
     &                      myIter,myThid )
        ENDIF

      ENDIF

      IF ((thetaRelaxFile.NE.' ').OR.(saltRelaxFile.NE.' ')) THEN

C--   Interpolate and add to anomaly
      DO j=1,sNy

        IF (ntTypeRelax .EQ. 0) THEN
          lambdaTheta =  r_tauThetaRelax
        ELSE
          lambdaTheta = r_tauThetaRelax *
     &                 max(cos(1.5 _d 0*yC(1,j,1,1)*deg2rad),0. _d 0)
        ENDIF
        IF (nsTypeRelax .EQ. 0) THEN
          lambdaSalt = r_tauSaltRelax
        ELSE
          lambdaSalt = r_tauSaltRelax *
     &                max(cos(1.5 _d 0*yC(1,j,1,1)*deg2rad),0. _d 0)
        ENDIF

        DO i=1,sNx

          IF (maskC(i,j,1,1,1) .EQ. 1.) THEN

          IF (thetaRelaxFile.NE.' ') THEN
            sstRelax(i,j)= (wght0*sst0(i,j,1,1) + wght1*sst1(i,j,1,1))
          ELSE  !no T restoring; use actual SST to determine if nr freezing
            sstRelax(i,j)= sstFromOcn(i,j)
          ENDIF

          IF (saltRelaxFile.NE.' ') THEN
            sssRelax(i,j)= (wght0*sss0(i,j,1,1) + wght1*sss1(i,j,1,1))
          ELSE  ! no S restoring; this ensures frelflux=0
            sssRelax(i,j)= sssFromOcn(i,j)
          ENDIF


C         Next lines: linearly phase out SST restoring between 2C and -1C
C         ONLY if seaice is present
          IF ((sstRelax(i,j).GT.2. _d 0).OR.
     &        (iceMask(i,j,1,1) .EQ. 0. _d 0)) THEN
              nearIce=1.0
          ELSEIF (sstRelax(i,j) .LE. -1. _d 0) THEN
              nearIce=0.0
          ELSE
              nearIce=(sstRelax(i,j)+1.0)/3.0
          endif
        
          qrelflux= lambdaTheta*(sstFromOcn(i,j)-sstRelax(i,j))/
     &              (recip_Cp*recip_rhoNil*recip_drF(1))*nearIce

          qneto_2D(i,j)= qneto_2D(i,j) + qrelflux
          qneti_2D(i,j)= qneti_2D(i,j) + qrelflux

          frelflux= -lambdaSalt*(sssFromOcn(i,j)-sssRelax(i,j))/
     &                  (convertFW2Salt *recip_drF(1))*nearIce

C         or use actual salt instead of convertFW2salt above?

          IF (frelflux .GT. 0. _d 0) THEN
            evapo_2D(i,j)= evapo_2D(i,j) - frelflux
C           note most of the time, evapi=0 when iceMask>0 anyway
C           (i.e., only when relaxing SST >2 but ocn still ice-covered)
            IF (iceMask(i,j,1,1).GT.0. _d 0)
     &            evapi_2D(i,j)= evapi_2D(i,j) - frelflux
          ELSE
            precipo_2D(i,j)= precipo_2D(i,j) + frelflux
            IF (iceMask(i,j,1,1).GT.0. _d 0)
     &            precipi_2D(i,j)= precipi_2D(i,j) + frelflux
          ENDIF

C          IF (iceMask(i,j,1,1) .GT. 0. _d 0) THEN
C          PRINT *,'Frelflux',frelflux,precipi_2D(i,j),atm_precip(j+1)
C          ENDIF

C         Diagnostics
          sum_qrel(i,j)= sum_qrel(i,j) + qrelflux*dtatmo
          sum_frel(i,j)= sum_frel(i,j) + frelflux*dtatmo

          ENDIF
        ENDDO
      ENDDO
      ENDIF

C      PRINT *,'***bottom of relaxadd',wght0,wght1,intime0,intime1
C      PRINT *,'evapo_2d: ',evapo_2D(JBUGI,JBUGJ)
C      PRINT *,'precipo_2d: ',precipo_2D(JBUGI,JBUGJ)
C      PRINT *,'qneto_2d: ',qneto_2D(JBUGI,JBUGJ)
C      PRINT *,'SStfrom Ocn: ',sstfromocn(JBUGI,JBUGJ)
C      PRINT *,'SSSfrom Ocn: ',sssfromocn(JBUGI,JBUGJ)

      RETURN
      END
