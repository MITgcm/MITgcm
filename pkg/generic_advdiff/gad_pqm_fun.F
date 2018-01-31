#     include "GAD_OPTIONS.h"

C--  File gad_pqm_fun.F: Routines to form PQM grid-cell polynomial.
C--   Contents
C--   o QUADROOT
C--   o GAD_PPM_FUN_NULL
C--   o GAD_PPM_FUN_MONO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      LOGICAL FUNCTION QUADROOT(aa,bb,cc,xx)
C     |================================================================|
C     | QUADROOT: find roots of quadratic ax**2 + bx + c = 0.          |
C     |================================================================|

          implicit none

C     ====================================================== arguments
          _RL aa,bb,cc
          _RL xx(1:2)

C     ====================================================== variables
          _RL sq,a0,b0

          a0 = abs(aa)
          b0 = abs(bb)

          sq = bb * bb - 4. _d 0 * aa * cc

          if (a0 .gt. 0. _d 0) then

          if (sq .ge. 0. _d 0) then

              QUADROOT =  .TRUE.

              sq = sqrt(sq)

              xx(1) =  - bb + sq
              xx(2) =  - bb - sq

              aa = 0.5 _d 0 / aa

              xx(1) = xx(1) * aa
              xx(2) = xx(2) * aa

          else

              QUADROOT = .FALSE.

          end if

          else

          if (b0 .gt. 0. _d 0) then

              QUADROOT =  .TRUE.

              xx(1) =  - cc / bb
              xx(2) =  - cc / bb

          else

              QUADROOT = .FALSE.

          end if

          end if

          return

c     end function QUADROOT
      end

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE GAD_PQM_FUN_NULL(
     I           ff00,
     I           fell,ferr,
     I           dell,derr,
     O           fhat,mono)
C     |================================================================|
C     | PQM_FUN_NULL: form PQM grid-cell polynomial.                   |
C     | Piecewise Quartic Method (PQM) - unlimited variant.            |
C     |================================================================|

          implicit none

C     ====================================================== arguments
          _RL ff00
          _RL fell,ferr
          _RL dell,derr
          _RL fhat(+1:+5)
          integer mono

          mono = +0

C     ============================================== unlimited profile
          fhat(1) =
     & + (30. _d 0 / 16. _d 0) * ff00
     & - ( 7. _d 0 / 16. _d 0) *(ferr+fell)
     & + ( 1. _d 0 / 16. _d 0) *(derr-dell)
          fhat(2) =
     & + ( 3. _d 0 /  4. _d 0) *(ferr-fell)
     & - ( 1. _d 0 /  4. _d 0) *(derr+dell)
          fhat(3) =
     & - (30. _d 0 /  8. _d 0) * ff00
     & + (15. _d 0 /  8. _d 0) *(ferr+fell)
     & - ( 3. _d 0 /  8. _d 0) *(derr-dell)
          fhat(4) =
     & - ( 1. _d 0 /  4. _d 0) *(ferr-fell-derr-dell)
          fhat(5) =
     & + (30. _d 0 / 16. _d 0) * ff00
     & - (15. _d 0 / 16. _d 0) *(ferr+fell)
     & + ( 5. _d 0 / 16. _d 0) *(derr-dell)

          return

c     end subroutine GAD_PQM_FUN_NULL
      end

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE GAD_PQM_FUN_MONO(
     I           ff00,
     I           ffll,ffrr,
     I           fell,ferr,
     I           dell,derr,
     I           dfds,
     O           fhat,mono)
C     |================================================================|
C     | PQM_FUN_MONO: form PQM grid-cell polynomial.                   |
C     | Piecewise Quartic Method (PQM) - monotonic variant.            |
C     |================================================================|

          implicit none

C     ====================================================== arguments
          _RL ff00,ffll,ffrr
          _RL fell,ferr
          _RL dell,derr
          _RL dfds(-1:+1)
          _RL fhat(+1:+5)
          integer mono

C     ====================================================== functions
          logical QUADROOT

C     ====================================================== variables
          integer bind
          _RL aval,bval,cval
          _RL iflx(+1:+2)
          _RL dflx(+1:+2)

          mono = +0

C     ============================================== "flatten" extrema
          if((ffrr-ff00) *
     &       (ff00-ffll) .le. 0. _d 0) then

              mono = +1

              fhat(1) = ff00

              fhat(2) = 0. _d 0
              fhat(3) = 0. _d 0
              fhat(4) = 0. _d 0
              fhat(5) = 0. _d 0

              return

          end if

C     ============================================== limit edge values
          if((ffll - fell) *
     &       (fell - ff00) .le. 0. _d 0) then

              mono = +1

              fell = ff00 - dfds(0)

          end if

          if((ffrr - ferr) *
     &       (ferr - ff00) .le. 0. _d 0) then

              mono = +1

              ferr = ff00 + dfds(0)

          end if

C     ============================================== limit edge slopes
          if((dell * dfds(-1)) .lt. 0. _d 0) then

              mono = +1

              dell = dfds(-1)

          end if

          if((derr * dfds(+1)) .lt. 0. _d 0) then

              mono = +1

              derr = dfds(+1)

          end if

C     ============================================== limit cell values
          fhat(1) =
     & + (30. _d 0 / 16. _d 0) * ff00
     & - ( 7. _d 0 / 16. _d 0) *(ferr+fell)
     & + ( 1. _d 0 / 16. _d 0) *(derr-dell)
          fhat(2) =
     & + ( 3. _d 0 /  4. _d 0) *(ferr-fell)
     & - ( 1. _d 0 /  4. _d 0) *(derr+dell)
          fhat(3) =
     & - (30. _d 0 /  8. _d 0) * ff00
     & + (15. _d 0 /  8. _d 0) *(ferr+fell)
     & - ( 3. _d 0 /  8. _d 0) *(derr-dell)
          fhat(4) =
     & - ( 1. _d 0 /  4. _d 0) *(ferr-fell-derr-dell)
          fhat(5) =
     & + (30. _d 0 / 16. _d 0) * ff00
     & - (15. _d 0 / 16. _d 0) *(ferr+fell)
     & + ( 5. _d 0 / 16. _d 0) *(derr-dell)

C     ============================= calc. inflexion via 2nd-derivative
          aval = 12. _d 0 * fhat(5)
          bval =  6. _d 0 * fhat(4)
          cval =  2. _d 0 * fhat(3)

          if ( QUADROOT(aval,bval,cval,iflx) ) then

              bind = +0

              if ( ( iflx(1) .gt. -1. _d 0 )
     &       .and. ( iflx(1) .lt. +1. _d 0 ) ) then

C     ============================= check for non-monotonic inflection
              dflx(1) =       fhat(2)
     &      + iflx(1)       * fhat(3) * 2. _d 0
     &      +(iflx(1) ** 2) * fhat(4) * 3. _d 0
     &      +(iflx(1) ** 3) * fhat(5) * 4. _d 0

              if (dflx(1)*dfds(+0) .lt. 0. _d 0) then

                  if (abs(dell)
     &           .lt. abs(derr) ) then

                      bind = -1

                  else

                      bind = +1

                  end if

              end if

              end if

              if ( ( iflx(2) .gt. -1. _d 0 )
     &       .and. ( iflx(2) .lt. +1. _d 0 ) ) then

C     ============================= check for non-monotonic inflection
              dflx(2) =       fhat(2)
     &      + iflx(2)       * fhat(3) * 2. _d 0
     &      +(iflx(2) ** 2) * fhat(4) * 3. _d 0
     &      +(iflx(2) ** 3) * fhat(5) * 4. _d 0

              if (dflx(2)*dfds(+0) .lt. 0. _d 0) then

                  if (abs(dell)
     &           .lt. abs(derr) ) then

                      bind = -1

                  else

                      bind = +1

                  end if

              end if

              end if

C     ============================= pop non-monotone inflexion to edge

              if (bind .eq. -1) then

C     ============================= pop inflection points onto -1 edge
                  mono = +2

                  derr =
     &          -( 5. _d 0 / 1. _d 0) * ff00
     &          +( 3. _d 0 / 1. _d 0) * ferr
     &          +( 2. _d 0 / 1. _d 0) * fell
                  dell =
     &          +( 5. _d 0 / 3. _d 0) * ff00
     &          -( 1. _d 0 / 3. _d 0) * ferr
     &          -( 4. _d 0 / 3. _d 0) * fell

                  if (dell*dfds(-1) .lt. 0. _d 0) then

                      dell = 0. _d 0

                      ferr =
     &              +( 5. _d 0 / 1. _d 0) * ff00
     &              -( 4. _d 0 / 1. _d 0) * fell
                      derr =
     &              +(10. _d 0 / 1. _d 0) * ff00
     &              -(10. _d 0 / 1. _d 0) * fell

                  end if

                  if (derr*dfds(+1) .lt. 0. _d 0) then

                      derr = 0. _d 0

                      fell =
     &              +( 5. _d 0 / 2. _d 0) * ff00
     &              -( 3. _d 0 / 2. _d 0) * ferr
                      dell =
     &              -( 5. _d 0 / 3. _d 0) * ff00
     &              +( 5. _d 0 / 3. _d 0) * ferr

                  end if

              end if

              if (bind .eq. +1) then

C     ============================= pop inflection points onto +1 edge
                  mono = +2

                  derr =
     &          -( 5. _d 0 / 3. _d 0) * ff00
     &          +( 4. _d 0 / 3. _d 0) * ferr
     &          +( 1. _d 0 / 3. _d 0) * fell
                  dell =
     &          +( 5. _d 0 / 1. _d 0) * ff00
     &          -( 2. _d 0 / 1. _d 0) * ferr
     &          -( 3. _d 0 / 1. _d 0) * fell

                  if (dell*dfds(-1) .lt. 0. _d 0) then

                      dell = 0. _d 0

                      ferr =
     &              +( 5. _d 0 / 2. _d 0) * ff00
     &              -( 3. _d 0 / 2. _d 0) * fell
                      derr =
     &              +( 5. _d 0 / 3. _d 0) * ff00
     &              -( 5. _d 0 / 3. _d 0) * fell

                  end if

                  if (derr*dfds(+1) .lt. 0. _d 0) then

                      derr = 0. _d 0

                      fell =
     &              +( 5. _d 0 / 1. _d 0) * ff00
     &              -( 4. _d 0 / 1. _d 0) * ferr
                      dell =
     &              -(10. _d 0 / 1. _d 0) * ff00
     &              +(10. _d 0 / 1. _d 0) * ferr

                  end if

              end if

          end if

C     ============================= re-assemble coefficients on demand
          if (mono .eq. +2) then

          fhat(1) =
     & + (30. _d 0 / 16. _d 0) * ff00
     & - ( 7. _d 0 / 16. _d 0) *(ferr+fell)
     & + ( 1. _d 0 / 16. _d 0) *(derr-dell)
          fhat(2) =
     & + ( 3. _d 0 /  4. _d 0) *(ferr-fell)
     & - ( 1. _d 0 /  4. _d 0) *(derr+dell)
          fhat(3) =
     & - (30. _d 0 /  8. _d 0) * ff00
     & + (15. _d 0 /  8. _d 0) *(ferr+fell)
     & - ( 3. _d 0 /  8. _d 0) *(derr-dell)
          fhat(4) =
     & - ( 1. _d 0 /  4. _d 0) *(ferr-fell-derr-dell)
          fhat(5) =
     & + (30. _d 0 / 16. _d 0) * ff00
     & - (15. _d 0 / 16. _d 0) *(ferr+fell)
     & + ( 5. _d 0 / 16. _d 0) *(derr-dell)

          end if

          return

c     end subroutine GAD_PQM_FUN_MONO
      end
