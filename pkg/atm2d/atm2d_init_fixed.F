C $Header: /u/gcmpack/MITgcm/pkg/atm2d/atm2d_init_fixed.F,v 1.5 2013/12/02 23:54:43 jmc Exp $
C $Name:  $

#include "ctrparam.h"
#ifdef OCEAN_3D
#  include "ATM2D_OPTIONS.h"
#endif
C
#ifndef OCEAN_3D
      SUBROUTINE ATM2D_INIT_FIXED(nCouplePer, myThid)
#else
      SUBROUTINE ATM2D_INIT_FIXED(myThid)
#endif
C     |==========================================================|
C     | Initialization steps prior to any pickup info loaded.    |
C     \==========================================================/
      IMPLICIT NONE

#include "ATMSIZE.h"
#include "DRIVER.h"
#ifdef OCEAN_3D
#  include "SIZE.h"
#  include "EEPARAMS.h"
#  include "PARAMS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     nCouplePer - total number of coupled periods for this run
C     myThid - thread number for this instance of the routine.
#ifndef OCEAN_3D
      INTEGER nCouplePer    !not a subroutine parm for OCEAN_3D
#endif
      INTEGER myThid

      NAMELIST /COUPLE_PARM/
     &        dtcouple, dtatm, dtocn, startYear, endYear, taveDump

C     === Local variables ===
#ifdef OCEAN_3D
      INTEGER nCouplePer    !not a subroutine parm for OCEAN_3D
# ifdef ATM2D_MPI_ON
      INTEGER msgUnit
# endif
#endif

      CALL CHECK_SETTINGS

C     set default values for these parms in couple.nml
      dtatm = 1
      dtocn = 8
      dtcouple = 8
      startYear = 1
      endYear = 100
      taveDump=100

C     Next lines done in stand-alone ML model, so don't use any
C     MITGCM helper routines; hopefully no unit conflict...
      OPEN(514,file='couple.nml',status='old')
      READ(514,COUPLE_PARM)
      CLOSE(514)

#ifdef OCEAN_3D
      CALL ATM2D_READPARMS(myThid)
#endif

      ncall_atm=dtcouple/dtatm
      ncall_ocean=dtcouple/dtocn
      nCouplePer = (endYear-startYear+1)*24*365/dtcouple

      PRINT *,'Model run years:',startYear,' to ', endYear
      PRINT *,'dcouple=',dtcouple
      PRINT *,'dtatm=',dtatm
      PRINT *,'dtocn=',dtocn
      PRINT *,'Total number of coupled periods:',nCouplePer

#ifdef OCEAN_3D
      nTimeSteps = nCouplePer    !overwrite whatever MITGCM gets from data
#endif

#ifdef CPL_OCEANCO2
      ocupt=0.0
      temuptann=0.0
#endif

#ifdef OCEAN_3D
#  ifdef ATM2D_MPI_ON

      CALL INITIALISE

C     Perform registration with other components
      CALL ACCEPT_COMPONENT_REGISTRATIONS

C     Coordinate the transfer configuration information
C     between components
      msgUnit = standardMessageUnit
      CALL EXCH_COMPONENT_CONFIGS(
     I                             msgUnit )
#  endif

      CALL INIT_ATM2D(dtatm, dtocn, dtcouple, myThid)
#endif

      RETURN
      END
