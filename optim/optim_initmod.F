
      subroutine optim_initmod(
     I                    nn,
     O                    xx
     &                  )

c     ==================================================================
c     SUBROUTINE optim_initmod
c     ==================================================================
c
c     o Optim_Initmod is called before the optimization. It sets a first guess
c       of the parameter vector. It may also contain the initialization
c       of the model.
c
c     started: Christian Eckert eckert@mit.edu 15-Feb-2000
c
c     changed: Christian Eckert eckert@mit.edu 10-Mar-2000
c
c              - Added ECCO layout.
c
c     changed:  Patrick Heimbach heimbach@mit.edu 19-Jun-2000
c               - finished, revised and debugged
c
c     ==================================================================
c     SUBROUTINE optim_initmod
c     ==================================================================

      IMPLICIT NONE

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "ctrl.h"
#include "optim.h"

c     == routine arguments ==

      integer nn
      _RL   xx(nn)

c     == local variables ==

      character*(80) fname

c     == end of interface ==

c--   Generate file name for the scaling of the control vector
c--   contributions.
      write(fname(1:80),'(80a)') ' '
      write(fname(1:80),'(4a,i10.10,a)')
     &  scalname(1:9),'_',yctrlid,'.',optimcycle,'.data'

c--   Read the scaling for the control vector.

c     --> Presently not implemented, since the control vector and the
c         gradient of the cost function can already be non-dimensionalized
c         by *ecco_ControlPack* and *ecco_ControlUnPack*.
c 
c         To do this set:
c 
c           #define  USE_NONDIMENSIONAL_CONTROL_IO
c 
c         in CPP_OPTIONS.h. The subroutine *ecco_ControlPack* will then
c         non-dimensionalize its output and *ecco_ControlUnPack* will
c         dimensionalize its input.

      return
      end
