#include "ECCO_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

      SUBROUTINE ECCO_SUMMARY( mythid )

c     ==================================================================
c     SUBROUTINE ecco_summary
c     ==================================================================
c
c     o Summarize the setup of the ECCO release.
c
#ifdef ECCO_CTRL_DEPRECATED
c     started: Christian Eckert eckert@mit.edu 29-Feb-1999
c
c              - Introduced to create a package for the MITgcmUV.
c
c     changed: Christian Eckert eckert@mit.edu
#endif
c
c     ==================================================================
c     SUBROUTINE ecco_summary
c     ==================================================================

      implicit none

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "ECCO_SIZE.h"
#include "ECCO.h"

#ifdef ALLOW_CTRL
# include "optim.h"
#endif

c     == routine arguments ==
c     integer mythid - thread number for this instance of the routine.
      integer mythid

c     == local variables ==
      character*(max_len_mbuf) msgbuf
#ifdef ALLOW_GENCOST_CONTRIBUTION
      integer k, k2, il, msgunit
#endif

c     == external ==
      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'// ======================================================='
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'// ECCO configuration >>> START <<<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'// ======================================================='
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

#ifdef ECCO_CTRL_DEPRECATED
      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a)')
     &'ECCO version:              ',eccoversion
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'Packages used: '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a)')
     &'Calendar version:          ',eccoUsesCalVersion
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a)')
     &'External Forcing version:  ',eccoUsesExfVersion
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a)')
     &'Adjoint support version:   ',eccoUsesAdsuppVersion
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a)')
     &'Optimization version:      ',eccoUsesOptimVersion
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

c--   For each CPP option state if it is enabled or disabled.
c--   =======================================================

#ifdef ALLOW_ECCO_FORWARD_RUN
      write(msgbuf,'(a)')
     &'// ALLOW_ECCO_FORWARD_RUN:               defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// ALLOW_ECCO_FORWARD_RUN:           NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ALLOW_ECCO_DIAGNOSTIC_RUN
      write(msgbuf,'(a)')
     &'// ALLOW_ECCO_DIAGNOSTIC_RUN:            defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// ALLOW_ECCO_DIAGNOSTIC_RUN:        NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ALLOW_ADJOINT_RUN
      write(msgbuf,'(a)')
     &'// ALLOW_ADJOINT_RUN:                    defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// ALLOW_ADJOINT_RUN:                NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ALLOW_GRADIENT_CHECK
      write(msgbuf,'(a)')
     &'// ALLOW_GRADIENT_CHECK:                 defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// ALLOW_GRADIENT_CHECK:             NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ALLOW_NO_DYNAMICS
c--   Just do a "dry" run ( useful for testing ).
      write(msgbuf,'(a)')
     &'// ALLOW_NO_DYNAMICS:                    defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// ALLOW_NO_DYNAMICS:                NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ALLOW_YMDS_TREE
c--   Use the Yearly-Monthly-Daily-Stepping call tree.
      write(msgbuf,'(a)')
     &'// ALLOW_YMDS_TREE:                      defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// ALLOW_YMDS_TREE:                  NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ALLOW_STEPPING_CALL
c--   Do not call stepping.
      write(msgbuf,'(a)')
     &'// ALLOW_STEPPING_CALL:                  defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// ALLOW_STEPPING_CALL:              NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ALLOW_NONDIMENSIONAL_CONTROL_IO
c--   Use non-dimensional vector of controls as input.
      write(msgbuf,'(a)')
     &'// ALLOW_NONDIMENSIONAL_CONTROL_IO:      defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// ALLOW_NONDIMENSIONAL_CONTROL_IO:  NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ALLOW_EGM96_ERROR_COV
c--   Read the EGM error covariance from file.
      write(msgbuf,'(a)')
     &'// ALLOW_EGM96_ERROR_COV:                defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// ALLOW_EGM96_ERROR_COV:            NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ALLOW_READ_EGM_DATA
c--   Read the EGM data from file.
      write(msgbuf,'(a)')
     &'// ALLOW_READ_EGM_DATA:                  defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// ALLOW_READ_EGM_DATA:              NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#if (defined (ALLOW_SCAT_COST_CONTRIBUTION) || \
     defined (ALLOW_DAILYSCAT_COST_CONTRIBUTION) )
c--   Use NSCAT data for getting the weights.
      write(msgbuf,'(a)')
     &'// ALLOW_SCAT_COST_CONTRIBUTION:         defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// ALLOW_SCAT_COST_CONTRIBUTION:     NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ALLOW_HFLUX_COST_CONTRIBUTION
c--   Include code to use the heat flux contribution.
      write(msgbuf,'(a)')
     &'// ALLOW_HFLUX_COST_CONTRIBUTION:        defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// ALLOW_HFLUX_COST_CONTRIBUTION:    NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ALLOW_SFLUX_COST_CONTRIBUTION
c--   Include code to use the salt flux contribution.
      write(msgbuf,'(a)')
     &'// ALLOW_SFLUX_COST_CONTRIBUTION:        defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// ALLOW_SFLUX_COST_CONTRIBUTION:    NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ALLOW_USTRESS_COST_CONTRIBUTION
c--   Include code to use the zonal wind stress contribution.
      write(msgbuf,'(a)')
     &'// ALLOW_USTRESS_COST_CONTRIBUTION:      defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// ALLOW_USTRESS_COST_CONTRIBUTION:  NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ALLOW_VSTRESS_COST_CONTRIBUTION
c--   Include code to use the meridional wind stress contribution.
      write(msgbuf,'(a)')
     &'// ALLOW_VSTRESS_COST_CONTRIBUTION:      defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// ALLOW_VSTRESS_COST_CONTRIBUTION:  NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ALLOW_SIGMAR_COST_CONTRIBUTION
c--   Enable testing of the 3d sigmaR field contribution
c--   to the cost function.
      write(msgbuf,'(a)')
     &'// ALLOW_SIGMAR_COST_CONTRIBUTION:       defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// ALLOW_SIGMAR_COST_CONTRIBUTION:   NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ALLOW_THETA_COST_CONTRIBUTION
c--   Enable testing of the 3d temperature field contribution
c--   to the cost function.
      write(msgbuf,'(a)')
     &'// ALLOW_THETA_COST_CONTRIBUTION:        defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// ALLOW_THETA_COST_CONTRIBUTION:    NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ALLOW_SST_COST_CONTRIBUTION
c--   Enable testing of the sea surface temperature contribution
c--   to the cost function.
      write(msgbuf,'(a)')
     &'// ALLOW_SST_COST_CONTRIBUTION:          defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// ALLOW_SST_COST_CONTRIBUTION:      NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ALLOW_SALT_COST_CONTRIBUTION
c--   Enable testing of the 3d salinity field contribution
c--   to the cost function.
      write(msgbuf,'(a)')
     &'// ALLOW_SALT_COST_CONTRIBUTION:         defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// ALLOW_SALT_COST_CONTRIBUTION:     NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ALLOW_SSH_COST_CONTRIBUTION
c--   Enable testing of the sea surface height contribution
c--   to the cost function.
      write(msgbuf,'(a)')
     &'// ALLOW_SSH_COST_CONTRIBUTION:          defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// ALLOW_SSH_COST_CONTRIBUTION:      NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef APPLY_HFLUX_COST_CONTRIBUTION
c--   Enable testing of the heat flux contribution to the
c--   cost function.
      write(msgbuf,'(a)')
     &'// APPLY_HFLUX_COST_CONTRIBUTION:        defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// APPLY_HFLUX_COST_CONTRIBUTION:    NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef APPLY_SFLUX_COST_CONTRIBUTION
c--   Enable testing of the salt flux contribution to the
c--   cost function.
      write(msgbuf,'(a)')
     &'// APPLY_SFLUX_COST_CONTRIBUTION:        defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// APPLY_SFLUX_COST_CONTRIBUTION:    NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef APPLY_USTRESS_COST_CONTRIBUTION
c--   Enable testing of the zonal wind stress contribution
c--   to the cost function.
      write(msgbuf,'(a)')
     &'// APPLY_USTRESS_COST_CONTRIBUTION:      defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// APPLY_USTRESS_COST_CONTRIBUTION:  NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef APPLY_VSTRESS_COST_CONTRIBUTION
c--   Enable testing of the meridional wind stress contribution
c--   to the cost function.
      write(msgbuf,'(a)')
     &'// APPLY_VSTRESS_COST_CONTRIBUTION:      defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// APPLY_VSTRESS_COST_CONTRIBUTION:  NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef APPLY_THETA_COST_CONTRIBUTION
c--   Enable testing of the initial state temperature contribution
c--   to the cost function.
      write(msgbuf,'(a)')
     &'// APPLY_THETA_COST_CONTRIBUTION:        defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// APPLY_THETA_COST_CONTRIBUTION:    NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef APPLY_SALT_COST_CONTRIBUTION
c--   Enable testing of the initial state salinity contribution
c--   to the cost function.
      write(msgbuf,'(a)')
     &'// APPLY_SALT_COST_CONTRIBUTION:         defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// APPLY_SALT_COST_CONTRIBUTION:     NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef APPLY_SST_COST_CONTRIBUTION
c--   Enable testing of the sea surface temperature contribution
c--   to the cost function.
      write(msgbuf,'(a)')
     &'// APPLY_SST_COST_CONTRIBUTION:          defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// APPLY_SST_COST_CONTRIBUTION:      NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef APPLY_SSH_COST_CONTRIBUTION
c--   Enable testing of the sea surface height contribution
c--   to the cost function.
      write(msgbuf,'(a)')
     &'// APPLY_SSH_COST_CONTRIBUTION:          defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// APPLY_SSH_COST_CONTRIBUTION:      NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ALLOW_SPH_PROJECTION
c--   Use projection onto spherical harmonics for the evaluation of
c--   the cost function contribution of the mean sea surface elevation.
      write(msgbuf,'(a)')
     &'// ALLOW_SPH_PROJECTION:                 defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// ALLOW_SPH_PROJECTION:             NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ALLOW_THETA0_CONTROL
c--   Initial state temperature used as part of the control vector.
      write(msgbuf,'(a)')
     &'// ALLOW_THETA0_CONTROL:                 defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// ALLOW_THETA0_CONTROL:             NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ALLOW_SALT0_CONTROL
c--   Initial state salinity used as part of the control vector.
      write(msgbuf,'(a)')
     &'// ALLOW_SALT0_CONTROL:                  defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// ALLOW_SALT0_CONTROL:              NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ALLOW_ETAN0_CONTROL
c--   Initial state sealevel used as part of the control vector.
      write(msgbuf,'(a)')
     &'// ALLOW_ETAN0_CONTROL:                  defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// ALLOW_ETAN0_CONTROL:              NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif
#ifdef ALLOW_UVEL0_CONTROL
c--   Initial state velocity used as part of the control vector.
      write(msgbuf,'(a)')
     &'// ALLOW_UVEL0_CONTROL:                  defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// ALLOW_UVEL0_CONTROL:              NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif
#ifdef ALLOW_VVEL0_CONTROL
c--   Initial state velocity used as part of the control vector.
      write(msgbuf,'(a)')
     &'// ALLOW_VVEL0_CONTROL:                  defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// ALLOW_VVEL0_CONTROL:              NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ALLOW_HFLUX_CONTROL
c--   Surface heat flux used as part of the control vector.
      write(msgbuf,'(a)')
     &'// ALLOW_HFLUX_CONTROL:                  defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// ALLOW_HFLUX_CONTROL:              NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ALLOW_SFLUX_CONTROL
c--   Short wave radiative flux used as part of the control vector.
      write(msgbuf,'(a)')
     &'// ALLOW_SFLUX_CONTROL:                  defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// ALLOW_SFLUX_CONTROL:              NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ALLOW_USTRESS_CONTROL
c--   Zonal wind stress used as part of the control vector.
      write(msgbuf,'(a)')
     &'// ALLOW_USTRESS_CONTROL:                defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// ALLOW_USTRESS_CONTROL:            NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ALLOW_VSTRESS_CONTROL
c--   Meridional wind stress used as part of the control vector.
      write(msgbuf,'(a)')
     &'// ALLOW_VSTRESS_CONTROL:                defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// ALLOW_VSTRESS_CONTROL:            NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ALLOW_SWFLUX_CONTROL
c--   Short wave radiative flux used as part of the control vector.
      write(msgbuf,'(a)')
     &'// ALLOW_SWFLUX_CONTROL:                 defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// ALLOW_SWFLUX_CONTROL:             NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ALLOW_SWDOWN_CONTROL
c--   Short wave radiative flux used as part of the control vector.
      write(msgbuf,'(a)')
     &'// ALLOW_SWDOWN_CONTROL:                 defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// ALLOW_SWDOWN_CONTROL:             NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ALLOW_ATEMP_CONTROL
c--   Atmospheric Temperature used as part of the control vector.
      write(msgbuf,'(a)')
     &'// ALLOW_ATEMP_CONTROL:                  defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// ALLOW_ATEMP_CONTROL:              NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ALLOW_AQH_CONTROL
c--   Specific humidity used as part of the control vector.
      write(msgbuf,'(a)')
     &'// ALLOW_AQH_CONTROL:                    defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// ALLOW_AQH_CONTROL:                NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ALLOW_UWIND_CONTROL
c--   Zonal winds used as part of the control vector.
      write(msgbuf,'(a)')
     &'// ALLOW_UWIND_CONTROL:                  defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// ALLOW_UWIND_CONTROL:              NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ALLOW_VWIND_CONTROL
c--   Meridional winds used as part of the control vector.
      write(msgbuf,'(a)')
     &'// ALLOW_VWIND_CONTROL:                  defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// ALLOW_VWIND_CONTROL:              NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ALLOW_PRECIP_CONTROL
c--   Precipitation used as part of the control vector.
      write(msgbuf,'(a)')
     &'// ALLOW_PRECIP_CONTROL:                 defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// ALLOW_PRECIP_CONTROL:             NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ALLOW_AUTODIFF_TAMC
c--   Enable automatic differentiation of the ECCO code.
      write(msgbuf,'(a)')
     &'// ALLOW_AUTODIFF_TAMC:                  defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// ALLOW_AUTODIFF_TAMC:              NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

#ifdef ALLOW_TAMC_CHECKPOINTING
c--   Checkpointing as handled by TAMC.
      write(msgbuf,'(a)')
     &'// ALLOW_TAMC_CHECKPOINTING:             defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#else
      write(msgbuf,'(a)')
     &'// ALLOW_TAMC_CHECKPOINTING:         NOT defined'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

c--   General summary.
c     write(msgbuf,'(a)')
c    &' '
c     call print_message( msgbuf, standardmessageunit,
c    &                    SQUEEZE_RIGHT , mythid)
c     write(msgbuf,'(a,2x,a10)')
c    &' Experiment name:',expId
c     call print_message( msgbuf, standardmessageunit,
c    &                    SQUEEZE_RIGHT , mythid)

#ifdef ALLOW_AUTODIFF_TAMC
c--   Generation of adjoint code.
      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &' Generation of adjoint code for the ECCO setup is enabled'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
#else
c--   Generation of adjoint code not enabled.
      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &' Generation of adjoint code for the ECCO setup is not enabled'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif
#endif /* ECCO_CTRL_DEPRECATED */

#ifdef ALLOW_GENCOST_CONTRIBUTION
      msgunit=standardmessageunit
c--
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, msgunit, SQUEEZE_RIGHT , mythid)
c--
      do k = 1, NGENCOST
       if (using_gencost(k)) then
c
        il = ilnblnk(gencost_name(k))
        write(msgbuf,'(a,i2,a,a)') 'gencost(',k,
     &        ') = ',gencost_name(k)(1:il)
        call print_message( msgbuf, msgunit, SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a)') '-------------'
        call print_message( msgbuf, msgunit, SQUEEZE_RIGHT , mythid)
c
        if ( gencost_datafile(k) .ne. ' ' ) then
         il = ilnblnk(gencost_datafile(k))
         write(msgbuf,'(a,a)') ' data file = ',gencost_datafile(k)(1:il)
         call print_message( msgbuf, msgunit, SQUEEZE_RIGHT , mythid)
        endif
c
         if (gencost_nrec(k).GT.1) then
          write(msgbuf,'(a)') '  starts and ends at : '
          call print_message( msgbuf, msgunit, SQUEEZE_RIGHT , mythid)
          CALL CAL_PRINTDATE(gencost_startdate(1,k),mythid)
          CALL CAL_PRINTDATE(gencost_enddate(1,k),mythid)
          write(msgbuf,'(a,i5)') '  number of records = ',
     &                            gencost_nrec(k)
          call print_message( msgbuf, msgunit, SQUEEZE_RIGHT , mythid)
         endif
c
         il = ilnblnk(gencost_barfile(k))
         write(msgbuf,'(a,a)') ' model file = ',gencost_barfile(k)(1:il)
         call print_message( msgbuf, msgunit, SQUEEZE_RIGHT , mythid)
         if ((gencost_barfile(k)(1:9).EQ.'m_ptracer').OR.
     &       (gencost_barfile(k)(1:17).EQ.'m_boxmean_ptracer')) then
           write(msgbuf,'(a,i2)') ' gencost_itracer = ',
     &           gencost_itracer(k)
           call print_message( msgbuf, msgunit, SQUEEZE_RIGHT , mythid)
         endif
         il = ilnblnk(gencost_errfile(k))
         write(msgbuf,'(a,a)') ' error file = ',gencost_errfile(k)(1:il)
         call print_message( msgbuf, msgunit, SQUEEZE_RIGHT , mythid)
         if (gencost_scalefile(k).NE.' ') then
         il = ilnblnk(gencost_scalefile(k))
         write(msgbuf,'(a,a)') ' scale file = ',
     &        gencost_scalefile(k)(1:il)
         call print_message( msgbuf, msgunit, SQUEEZE_RIGHT , mythid)
         endif
         do k2 = 1, NGENPPROC
          if (gencost_preproc(k2,k).NE.' ') then
            il = ilnblnk(gencost_preproc(k2,k))
            write(msgbuf,'(a,a)') ' preprocess = ',
     &                            gencost_preproc(k2,k)(1:il)
            call print_message( msgbuf, msgunit, SQUEEZE_RIGHT , mythid)
          endif
          if (gencost_posproc(k2,k).NE.' ') then
            il = ilnblnk(gencost_posproc(k2,k))
            write(msgbuf,'(a,a)') ' posprocess = ',
     &                            gencost_posproc(k2,k)(1:il)
            call print_message( msgbuf, msgunit, SQUEEZE_RIGHT , mythid)
          endif
         enddo
c
         write(msgbuf,'(a,i2)') ' gencost_flag = ',gencost_flag(k)
         call print_message( msgbuf, msgunit, SQUEEZE_RIGHT , mythid)
         write(msgbuf,'(a,i2)') ' gencost_outputlevel = ',
     &         gencost_outputlevel(k)
         call print_message( msgbuf, msgunit, SQUEEZE_RIGHT , mythid)
         write(msgbuf,'(a,i2)') ' gencost_kLev_select = ',
     &         gencost_kLev_select(k)
         call print_message( msgbuf, msgunit, SQUEEZE_RIGHT , mythid)
c
         if (gencost_is3d(k)) then
         write(msgbuf,'(a,i2)') ' gencost_pointer3d = ',
     &         gencost_pointer3d(k)
         call print_message( msgbuf, msgunit, SQUEEZE_RIGHT , mythid)
         endif
         if (gencost_timevaryweight(k)) then
         write(msgbuf,'(a,L2)') ' time varia. weight = ',
     &         gencost_timevaryweight(k)
         call print_message( msgbuf, msgunit, SQUEEZE_RIGHT , mythid)
         endif
         if (gencost_barskip(k)) then
         write(msgbuf,'(a,L2)') ' skip barfile write = ',
     &         gencost_barskip(k)
         call print_message( msgbuf, msgunit, SQUEEZE_RIGHT , mythid)
         endif

        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, msgunit, SQUEEZE_RIGHT , mythid)
c
       endif
      enddo
c--
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, msgunit, SQUEEZE_RIGHT , mythid)
#endif /* ALLOW_GENCOST_CONTRIBUTION */

      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'// ======================================================='
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'// ECCO configuration  >>> END <<<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'// ======================================================='
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

      RETURN
      END
