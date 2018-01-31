#     include "GAD_OPTIONS.h"

C--  File gad_ppm_fun.F: Routines to form PPM grid-cell polynomial.
C--   Contents
C--   o GAD_PPM_FUN_NULL
C--   o GAD_PPM_FUN_MONO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE GAD_PPM_FUN_NULL(
     I           ff00,
     I           fell,ferr,
     O           fhat,mono)
C     |================================================================|
C     | PPM_FUN_NULL: form PPM grid-cell polynomial.                   |
C     | Piecewise Parabolic Method (PPM), unlimited variant.           |
C     |================================================================|

          implicit none

C     ====================================================== arguments
          _RL ff00
          _RL fell,ferr
          _RL fhat(+1:+3)
          integer  mono

          mono = +0

C     ============================================== unlimited profile
          fhat( 1 ) =
     &  +(3. _d 0 / 2. _d 0) * ff00
     &  -(1. _d 0 / 4. _d 0) *(ferr+fell)
          fhat( 2 ) =
     &  +(1. _d 0 / 2. _d 0) *(ferr-fell)
          fhat( 3 ) =
     &  -(3. _d 0 / 2. _d 0) * ff00
     &  +(3. _d 0 / 4. _d 0) *(ferr+fell)

          return

c     end subroutine GAD_PPM_FUN_NULL
      end

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE GAD_PPM_FUN_MONO(
     I           ff00,
     I           ffll,ffrr,
     I           fell,ferr,
     I           dfds,
     O           fhat,mono)
C     |================================================================|
C     | PPM_FUN_MONO: form PPM grid-cell polynomial.                   |
C     | Piecewise Parabolic Method (PPM) - monotonic variant.          |
C     |================================================================|

          implicit none

C     ====================================================== arguments
          _RL ff00
          _RL ffll,ffrr
          _RL fell,ferr
          _RL dfds(-1:+1)
          _RL fhat(+1:+3)
          integer mono

C     ====================================================== variables
          _RL turn

          mono = 0

C     ============================================== "flatten" extrema
          if((ffrr-ff00) *
     &       (ff00-ffll) .le. 0. _d 0) then

              mono = +1

              fhat(1) = ff00

              fhat(2) = 0. _d 0
              fhat(3) = 0. _d 0

              return

          end if

C     ============================================== limit edge values
          if((ffll-fell) *
     &       (fell-ff00) .le. 0. _d 0) then

              mono = +1

              fell = ff00 - dfds(0)

          end if

          if((ffrr-ferr) *
     &       (ferr-ff00) .le. 0. _d 0) then

              mono = +1

              ferr = ff00 + dfds(0)

          end if

C     ============================================== limit cell values
          fhat( 1 ) =
     &  +(3. _d 0 / 2. _d 0) * ff00
     &  -(1. _d 0 / 4. _d 0) *(ferr+fell)
          fhat( 2 ) =
     &  +(1. _d 0 / 2. _d 0) *(ferr-fell)
          fhat( 3 ) =
     &  -(3. _d 0 / 2. _d 0) * ff00
     &  +(3. _d 0 / 4. _d 0) *(ferr+fell)

          if (abs(fhat(3)) .gt.
     &        abs(fhat(2))*.5 _d 0) then

          turn = -0.5 _d 0 * fhat(2)
     &                     / fhat(3)

          if ((turn .ge. -1. _d 0)
     &   .and.(turn .le. +0. _d 0)) then

              mono = +2

C     ====================================== push TURN onto lower edge
              ferr = +3. _d 0 * ff00
     &               -2. _d 0 * fell

          end if

          if ((turn .gt. +0. _d 0)
     &   .and.(turn .le. +1. _d 0)) then

              mono = +2

C     ====================================== push TURN onto upper edge
              fell = +3. _d 0 * ff00
     &               -2. _d 0 * ferr

          end if

          end if

          if (mono .gt. +1) then

C     ====================================== re-calc. coeff. on demand
          fhat( 1 ) =
     &  +(3. _d 0 / 2. _d 0) * ff00
     &  -(1. _d 0 / 4. _d 0) *(ferr+fell)
          fhat( 2 ) =
     &  +(1. _d 0 / 2. _d 0) *(ferr-fell)
          fhat( 3 ) =
     &  -(3. _d 0 / 2. _d 0) * ff00
     &  +(3. _d 0 / 4. _d 0) *(ferr+fell)

          end if

          return

c     end subroutine GAD_PPM_FUN_MONO
      end
