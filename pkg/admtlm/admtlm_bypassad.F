
#include "CPP_OPTIONS.h"

      subroutine admtlm_bypassad( myThid )
C     /==========================================================\
C     | subroutine admtlm_bypassad                               |
C     | o This routine assigns final T,S to cost function        |
C     \==========================================================/
       implicit none

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_ADMTLM
# include "ctrl.h"
# include "ctrl_dummy.h"
# include "optim.h"
# include "adcost.h"
# include "g_cost.h"
# include "adcommon.h"
#endif

C     ======== Routine arguments ======================
C     myThid - Thread number for this instance of the routine.
      integer myThid

#ifdef ALLOW_ADMTLM

C     ========= Local variables =========================
      integer i, j, k
      integer bi, bj
      integer imin, imax
      integer jmin, jmax
      integer itlo, ithi
      integer jtlo, jthi
      integer il

      logical ladinit
      logical doglobalread
      logical equal
      double precision fac
      character*(80) fnamegeneric

C     ==============================================
C     declare external procedures and functions
C     ==============================================
      integer ilnblnk
      external ilnblnk

C     ===================================================

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
      jmin = 1
      jmax = sny
      imin = 1
      imax = snx
      doglobalread =  .false. 
      ladinit =  .false. 
      equal =  .true. 
      if (equal) then
        fac = 1.d0
      else
        fac = 0.d0
      endif

      DO bj = jtlo, jthi
       DO bi = itlo, ithi
        DO j = jmin, jmax
         DO i = imin, imax
          DO k=1,Nr
            adtheta(i,j,k,bi,bj) = 
     &       g_objf_state_final(i,j,bi,bj,k)
            adsalt(i,j,k,bi,bj) = 
     &       g_objf_state_final(i,j,bi,bj,1*Nr+k)
            aduvel(i,j,k,bi,bj) = 
     &       g_objf_state_final(i,j,bi,bj,2*Nr+k)
            advvel(i,j,k,bi,bj) = 
     &       g_objf_state_final(i,j,bi,bj,3*Nr+k)
          END DO
          adetan(i,j,bi,bj) = 
     &       g_objf_state_final(i,j,bi,bj,4*Nr+1)
         END DO
        END DO
       END DO
      END DO

c---------------------------------------------------------------------

      do bj = jtlo, jthi
        do bi = itlo, ithi
          do j = jmin, jmax
            do i = imin, imax
              tmpfld2d(i,j,bi,bj) = tmpfld2d(i,j,bi,bj)
     &              + adetan(i,j,bi,bj)
            end do
          end do
        end do
      end do
      il = ilnblnk(xx_etan_file)
      write(unit=fnamegeneric(1:80),fmt='(2a,i10.10)') 
     &     xx_etan_file(1:il),'.',optimcycle
      call adactive_read_xy_loc( fnamegeneric,1,doglobalread,ladinit,
     &     optimcycle,mythid,tmpfld2d )
c--
      do bj = jtlo, jthi
        do bi = itlo, ithi
          do k = 1, nr
            do j = jmin, jmax
              do i = imin, imax
                tmpfld3d(i,j,k,bi,bj) = tmpfld3d(i,j,k,bi,bj)
     &                + advvel(i,j,k,bi,bj)
              end do
            end do
          end do
        end do
      end do
      il = ilnblnk(xx_vvel_file)
      write(unit=fnamegeneric(1:80),fmt='(2a,i10.10)') 
     &     xx_vvel_file(1:il),'.',optimcycle
      call adactive_read_xyz( fnamegeneric,1,doglobalread,ladinit,
     &     optimcycle,mythid,tmpfld3d )
c--
      do bj = jtlo, jthi
        do bi = itlo, ithi
          do k = 1, nr
            do j = jmin, jmax
              do i = imin, imax
                tmpfld3d(i,j,k,bi,bj) = tmpfld3d(i,j,k,bi,bj)
     &                + aduvel(i,j,k,bi,bj)
              end do
            end do
          end do
        end do
      end do
      il = ilnblnk(xx_uvel_file)
      write(unit=fnamegeneric(1:80),fmt='(2a,i10.10)') 
     &     xx_uvel_file(1:il),'.',optimcycle
      call adactive_read_xyz( fnamegeneric,1,doglobalread,ladinit,
     &     optimcycle,mythid,tmpfld3d )
c--
      do bj = jtlo, jthi
        do bi = itlo, ithi
          do k = 1, nr
            do j = jmin, jmax
              do i = imin, imax
                tmpfld3d(i,j,k,bi,bj) = tmpfld3d(i,j,k,bi,bj)
     &                + adsalt(i,j,k,bi,bj)*fac
              end do
            end do
          end do
        end do
      end do
      il = ilnblnk(xx_salt_file)
      write(unit=fnamegeneric(1:80),fmt='(2a,i10.10)') 
     &     xx_salt_file(1:il),'.',optimcycle
      call adactive_read_xyz_loc( fnamegeneric,1,doglobalread,ladinit,
     &     optimcycle,mythid,tmpfld3d )
c--
      do bj = jtlo, jthi
        do bi = itlo, ithi
          do k = 1, nr
            do j = jmin, jmax
              do i = imin, imax
                tmpfld3d(i,j,k,bi,bj) = tmpfld3d(i,j,k,bi,bj)
     &                + adtheta(i,j,k,bi,bj)*fac
              end do
            end do
          end do
        end do
      end do
      il = ilnblnk(xx_theta_file)
      write(unit=fnamegeneric(1:80),fmt='(2a,i10.10)') 
     &     xx_theta_file(1:il),'.',optimcycle
      call adactive_read_xyz_loc( fnamegeneric,1,doglobalread,ladinit,
     &     optimcycle,mythid,tmpfld3d )

#endif
      
      end
