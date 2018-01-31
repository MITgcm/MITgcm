#     include "GAD_OPTIONS.h"

C--  File gad_plm_fun.F: Routines for monotone piecewise linear method.
C--   Contents
C--   o GAD_PLM_FUN_U
C--   o GAD_PLM_FUN_V

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE GAD_PLM_FUN_U(
     I           ffll,ff00,ffrr,
     O           dfds)
C     |================================================================|
C     | PLM_FUN_U: monotone piecewise linear method.                   |
C     |     - uniform grid-spacing variant.                            |
C     |================================================================|

          implicit none

C     ====================================================== arguments
          _RL ffll,ff00,ffrr
          _RL dfds(-1:+1)

C     ====================================================== variables
          _RL fell,ferr,scal
          _RL epsil
          PARAMETER( epsil = 1. _d -16 )

          dfds(-1) = ff00 - ffll
          dfds(+1) = ffrr - ff00

          if (dfds(-1) * dfds(+1) .gt. 0.0 _d 0) then

C     ======================================= calc. ll//rr edge values
              fell = 0.5 _d 0 * (ffll + ff00)
              ferr = 0.5 _d 0 * (ff00 + ffrr)

C     ======================================= calc. centred derivative
              dfds(+0) =
     &               0.5 _d 0 * (ferr - fell)

C     ======================================= monotonic slope-limiting
              scal = min(abs(dfds(-1)),
     &                   abs(dfds(+1)))
     &             / max(abs(dfds(+0)), epsil)
c    &             / max(abs(dfds(+0)), epsilon(ff00))
              scal = min(scal, 1.0 _d 0)

              dfds(+0) = scal * dfds(+0)

          else

C     ======================================= flatten if local extrema
              dfds(+0) = 0.0 _d 0

          end if

          dfds(-1) = 0.5 _d 0 * dfds(-1)
          dfds(+1) = 0.5 _d 0 * dfds(+1)

          return

c     end subroutine GAD_PLM_FUN_U
      end

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE GAD_PLM_FUN_V(
     I           ffll,ff00,ffrr,
     I           ddll,dd00,ddrr,
     O           dfds)
C     |================================================================|
C     | PLM_FUN_V: monotone piecewise linear method.                   |
C     |     - variable grid-spacing variant.                           |
C     |================================================================|

          implicit none

C     ====================================================== arguments
          _RL ffll,ff00,ffrr
          _RL ddll,dd00,ddrr
          _RL dfds(-1:+1)

C     ====================================================== variables
          _RL fell,ferr,scal
          _RL epsil
          PARAMETER( epsil = 1. _d -16 )

          dfds(-1) = ff00 - ffll
          dfds(+1) = ffrr - ff00

          if (dfds(-1) * dfds(+1) .gt. 0.0 _d 0) then

C     ======================================= calc. ll//rr edge values
              fell = (dd00 * ffll + ddll * ff00)
     &             / (ddll + dd00)
              ferr = (ddrr * ff00 + dd00 * ffrr)
     &             / (dd00 + ddrr)

C     ======================================= calc. centred derivative
              dfds(+0) =
     &               0.5 _d 0 * (ferr - fell)

C     ======================================= monotonic slope-limiting
              scal = min(abs(dfds(-1)),
     &                   abs(dfds(+1)))
     &             / max(abs(dfds(+0)), epsil)
c    &             / max(abs(dfds(+0)), epsilon(ff00))
              scal = min(scal, 1.0 _d 0)

              dfds(+0) = scal * dfds(+0)

          else

C     ======================================= flatten if local extrema
              dfds(+0) = 0.0 _d 0

          end if

C     == !! check this
          dfds(-1) = dfds(-1) / (ddll + dd00) * dd00
          dfds(+1) = dfds(+1) / (dd00 + ddrr) * dd00

          return

c     end subroutine GAD_PLM_FUN_V
      end
