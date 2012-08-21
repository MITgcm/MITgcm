C $Header: /u/gcmpack/MITgcm/pkg/atm2d/atm2d_init_vars.F,v 1.10 2012/08/21 21:02:22 jscott Exp $
C $Name:  $

#include "ctrparam.h"
C
      SUBROUTINE ATM2D_INIT_VARS(myThid)
C     |==========================================================|
C     | Initialization steps which depend on pickups loading.    |
C     \==========================================================/
      IMPLICIT NONE

#include "ATMSIZE.h"
#include "DRIVER.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid - thread number for this instance of the routine.
      INTEGER myThid

C     === Local variables ===

#ifdef PREDICTED_AEROSOL
      PRINT *,' Before eppaemission'
      CALL eppaemission (0)
#endif

#ifdef CLM
#  ifndef CLM35 
      CALL atmosphere(0,0)     ! initialize the atmospheric model
#  endif
      CALL clm4mit2d           ! initialize CLM
#  ifdef CLM35 
      PRINT *,'After clm4mit2d  initialization'
      PRINT *,'orbfix=',orbfix,' dyn_pft=',dyn_pft
      PRINT *,'orbyr=',orbyr,' rampyr_dynpft=',rampyr_dynpft
#    ifndef ORBITAL_FOR 
      IF (orbyr.ne.2000) THEN
         PRINT *,'Wrong orbyr for CLM3.5'
         STOP
      ENDIF
#    endif
      CALL atmosphere(0,0)
#  endif
#else
      CALL atmosphere(0,0)
#endif

#ifdef ML_2D
C    2D mixed layer model  initialization
      CALL ocean_ml(dtocn*3600.,dtatm*3600.)
#  ifdef CPL_OCEANCO2
      CALL zerogary
#  endif
#endif

#ifdef CPL_TEM
      PRINT *,' Start of tem_init'
      aocuav=0.0
      nepav=0.0
#  ifdef DATA4TEM
      nfile=1
#  endif
c      call tem_init(inyear,lyear)
      CALL climate2tem(0,0)
      CALL temclm_init
      CALL lcluc_init
      CALL updatelcluc(0)
      CALL tem_init
      adupt=0.0
      PRINT *,' TEM initialization finished'
#endif

#ifdef OCEAN_3D
      CALL ATM2D_READ_PICKUP(myThid)
#endif

      RETURN
      END
