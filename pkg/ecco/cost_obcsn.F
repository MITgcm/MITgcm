
#include "COST_CPPOPTIONS.h"
#ifdef ALLOW_OBCS
# include "OBCS_OPTIONS.h"
#endif

      subroutine cost_obcsn(
     I                       myiter,
     I                       mytime,
     I                       mythid
     &                     )

c     ==================================================================
c     SUBROUTINE cost_obcsn
c     ==================================================================
c
c     o cost function contribution obc
c
c     ==================================================================
c     SUBROUTINE cost_obcsn
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#ifdef ALLOW_OBCS
# include "OBCS.h"
#endif

#include "cal.h"
#include "ecco_cost.h"
#include "ctrl.h"
#include "ctrl_dummy.h"
#include "optim.h"

c     == routine arguments ==

      integer myiter
      _RL     mytime
      integer mythid

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer irec
      integer il
      integer iobcs
      integer jp1

      _RL fctile
      _RL fcthread
      _RL dummy

      character*(80) fnametheta
      character*(80) fnamesalt
      character*(80) fnameuvel
      character*(80) fnamevvel

      logical doglobalread
      logical ladinit

#ifdef ECCO_VERBOSE
      character*(MAX_LEN_MBUF) msgbuf
#endif

c     == external functions ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
      jmin = 1
      jmax = sny
      imin = 1
      imax = snx

c--   Read tiled data.
      doglobalread = .false.
      ladinit      = .false.

#ifdef ALLOW_OBCSN_COST_CONTRIBUTION

      jp1 = 0
      fcthread = 0. _d 0

c--   Loop over records.
      do irec = 1,nmonsrec

c--     temperature
        iobcs = 1
c--     Read time averages and the monthly mean data.
        il = ilnblnk( tbarfile )
        write(fnametheta(1:80),'(2a,i10.10)')
     &       tbarfile(1:il),'.',optimcycle
        call active_read_xyz( fnametheta, tbar, irec,
     &                        doglobalread, ladinit,
     &                        optimcycle, mythid,
     &                        xx_tbar_mean_dummy )

        call mdsreadfieldxz( OBNtFile, readBinaryPrec, 'RS', 
     &                       nr, OBNt, irec, mythid)

        do bj = jtlo,jthi
          do bi = itlo,ithi
c--         Loop over the model layers
            fctile = 0. _d 0
            do k = 1,nr
c--           Compute model data misfit and cost function term for
c             the temperature field.
               do i = imin,imax
                  j = OB_Jn(I,bi,bj)
                  if (maskS(i,j+jp1,k,bi,bj) .ne. 0.) then
                     fctile = fctile +
     &                    wobcsn(k,iobcs)*cosphi(i,j,bi,bj)*
     &                    (tbar(i,j,k,bi,bj) - OBNt(i,k,bi,bj))*
     &                    (tbar(i,j,k,bi,bj) - OBNt(i,k,bi,bj))
                  endif
               enddo
            enddo
c--         End of loop over layers.
            fcthread         = fcthread           + fctile
            objf_obcsn(bi,bj) = objf_obcsn(bi,bj) + fctile
          enddo
        enddo

c--     salt
        iobcs = 2
c--     Read time averages and the monthly mean data.
        il = ilnblnk( sbarfile )
        write(fnamesalt(1:80),'(2a,i10.10)')
     &       sbarfile(1:il),'.',optimcycle
        call active_read_xyz( fnamesalt, sbar, irec,
     &                        doglobalread, ladinit,
     &                        optimcycle, mythid,
     &                        xx_sbar_mean_dummy )

        call mdsreadfieldxz( OBNsFile, readBinaryPrec, 'RS', 
     &                       nr, OBNs, irec, mythid)

        do bj = jtlo,jthi
          do bi = itlo,ithi
c--         Loop over the model layers
            fctile = 0. _d 0
            do k = 1,nr
c--           Compute model data misfit and cost function term for
c             the temperature field.
               do i = imin,imax
                  j = OB_Jn(I,bi,bj)
                  if (maskS(i,j+jp1,k,bi,bj) .ne. 0.) then
                     fctile = fctile +
     &                    wobcsn(k,iobcs)*cosphi(i,j,bi,bj)*
     &                    (sbar(i,j,k,bi,bj) - OBNs(i,k,bi,bj))*
     &                    (sbar(i,j,k,bi,bj) - OBNs(i,k,bi,bj))
                  endif
               enddo
            enddo
c--         End of loop over layers.
            fcthread         = fcthread           + fctile
            objf_obcsn(bi,bj) = objf_obcsn(bi,bj) + fctile
          enddo
        enddo

c--     uvel
        iobcs = 3
c--     Read time averages and the monthly mean data.
        il = ilnblnk( ubarfile )
        write(fnameuvel(1:80),'(2a,i10.10)')
     &       ubarfile(1:il),'.',optimcycle
        call active_read_xyz( fnameuvel, ubar, irec,
     &                        doglobalread, ladinit,
     &                        optimcycle, mythid,
     &                        dummy )

        call mdsreadfieldxz( OBNuFile, readBinaryPrec, 'RS', 
     &                       nr, OBNu, irec, mythid)
        do bj = jtlo,jthi
          do bi = itlo,ithi
c--         Loop over the model layers
            fctile = 0. _d 0
            do k = 1,nr
c--           Compute model data misfit and cost function term for
c             the temperature field.
               do i = imin,imax
                  j = OB_Jn(I,bi,bj)
                  if (maskW(i,j,k,bi,bj) .ne. 0.) then
                     fctile = fctile +
     &                    wobcsn(k,iobcs)*cosphi(i,j,bi,bj)*
     &                    (ubar(i,j,k,bi,bj) - OBNu(i,k,bi,bj))*
     &                    (ubar(i,j,k,bi,bj) - OBNu(i,k,bi,bj))
                  endif
               enddo
            enddo
c--         End of loop over layers.
            fcthread         = fcthread           + fctile
            objf_obcsn(bi,bj) = objf_obcsn(bi,bj) + fctile
          enddo
        enddo

c--     vvel
        iobcs = 4
c--     Read time averages and the monthly mean data.
        il = ilnblnk( vbarfile )
        write(fnamevvel(1:80),'(2a,i10.10)')
     &       vbarfile(1:il),'.',optimcycle
        call active_read_xyz( fnamevvel, vbar, irec,
     &                        doglobalread, ladinit,
     &                        optimcycle, mythid,
     &                        dummy )

        call mdsreadfieldxz( OBNvFile, readBinaryPrec, 'RS', 
     &                       nr, OBNv, irec, mythid)

        do bj = jtlo,jthi
          do bi = itlo,ithi
c--         Loop over the model layers
            fctile = 0. _d 0
            do k = 1,nr
c--           Compute model data misfit and cost function term for
c             the temperature field.
               do i = imin,imax
                  j = OB_Jn(I,bi,bj)
                  if (maskS(i,j+jp1,k,bi,bj) .ne. 0.) then
                     fctile = fctile +
     &                    wobcsn(k,iobcs)*cosphi(i,j,bi,bj)*
     &                    (vbar(i,j,k,bi,bj) - OBNv(i,k,bi,bj))*
     &                    (vbar(i,j,k,bi,bj) - OBNv(i,k,bi,bj))
                  endif
               enddo
            enddo
c--         End of loop over layers.
            fcthread         = fcthread           + fctile
            objf_obcsn(bi,bj) = objf_obcsn(bi,bj) + fctile
          enddo
        enddo

#ifdef ECCO_VERBOSE
c--     Print cost function for all tiles.
        _GLOBAL_SUM_R8( fcthread , myThid )
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,i8.8)')
     &    ' cost_obcsn: irec = ',irec
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,a,d22.15)')
     &    ' global cost function value',
     &    ' (obcsn) = ',fcthread
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
#endif

      enddo
c--   End of loop over records.

#endif

      return
      end







