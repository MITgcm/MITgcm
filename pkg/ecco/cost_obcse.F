
#include "COST_CPPOPTIONS.h"
#ifdef ALLOW_OBCS
# include "OBCS_OPTIONS.h"
#endif

      subroutine cost_obcse(
     I                       myiter,
     I                       mytime,
     I                       mythid
     &                     )

c     ==================================================================
c     SUBROUTINE cost_obcse
c     ==================================================================
c
c     o cost function contribution obc
c
c     ==================================================================
c     SUBROUTINE cost_obcse
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
      integer ip1

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

#ifdef ALLOW_OBCSE_COST_CONTRIBUTION

      ip1 = 0
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

        call mdsreadfieldyz( OBEtFile, ReadBinaryPrec, 'RS', 
     &                       nr, OBEt, irec, mythid)

        do bj = jtlo,jthi
          do bi = itlo,ithi
c--         Loop over the model layers
            fctile = 0. _d 0
            do k = 1,nr
c--           Compute model data misfit and cost function term for
c             the temperature field.
               do j = jmin,jmax
                  i = OB_Ie(J,bi,bj)
                  if (maskS(i+ip1,j,k,bi,bj) .ne. 0.) then
                     fctile = fctile +
     &                    wobcse(k,iobcs)*cosphi(i,j,bi,bj)*
     &                    (tbar(i,j,k,bi,bj) - OBEt(j,k,bi,bj))*
     &                    (tbar(i,j,k,bi,bj) - OBEt(j,k,bi,bj))
                  endif
               enddo
            enddo
c--         End of loop over layers.
            fcthread         = fcthread           + fctile
            objf_obcse(bi,bj) = objf_obcse(bi,bj) + fctile
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

        call mdsreadfieldyz( OBEsFile, readBinaryPrec, 'RS', 
     &                       nr, OBEs, irec, mythid)

        do bj = jtlo,jthi
          do bi = itlo,ithi
c--         Loop over the model layers
            fctile = 0. _d 0
            do k = 1,nr
c--           Compute model data misfit and cost function term for
c             the temperature field.
               do j = jmin,jmax
                  i = OB_Ie(J,bi,bj)
                  if (maskS(i+ip1,j,k,bi,bj) .ne. 0.) then
                     fctile = fctile +
     &                    wobcse(k,iobcs)*cosphi(i,j,bi,bj)*
     &                    (sbar(i,j,k,bi,bj) - OBEs(j,k,bi,bj))*
     &                    (sbar(i,j,k,bi,bj) - OBEs(j,k,bi,bj))
                  endif
               enddo
            enddo
c--         End of loop over layers.
            fcthread         = fcthread           + fctile
            objf_obcse(bi,bj) = objf_obcse(bi,bj) + fctile
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

        call mdsreadfieldyz( OBEuFile, readBinaryPrec, 'RS', 
     &                       nr, OBEu, irec, mythid)
        do bj = jtlo,jthi
          do bi = itlo,ithi
c--         Loop over the model layers
            fctile = 0. _d 0
            do k = 1,nr
c--           Compute model data misfit and cost function term for
c             the temperature field.
               do j = jmin,jmax
                  i = OB_Ie(J,bi,bj)
                  if (maskW(i+ip1,j,k,bi,bj) .ne. 0.) then
                     fctile = fctile +
     &                    wobcse(k,iobcs)*cosphi(i,j,bi,bj)*
     &                    (ubar(i,j,k,bi,bj) - OBEu(j,k,bi,bj))*
     &                    (ubar(i,j,k,bi,bj) - OBEu(j,k,bi,bj))
                  endif
               enddo
            enddo
c--         End of loop over layers.
            fcthread         = fcthread           + fctile
            objf_obcse(bi,bj) = objf_obcse(bi,bj) + fctile
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

        call mdsreadfieldyz( OBEvFile, readBinaryPrec, 'RS', 
     &                       nr, OBEv, irec, mythid)

        do bj = jtlo,jthi
          do bi = itlo,ithi
c--         Loop over the model layers
            fctile = 0. _d 0
            do k = 1,nr
c--           Compute model data misfit and cost function term for
c             the temperature field.
               do j = jmin,jmax
                  i = OB_Ie(J,bi,bj)
                  if (maskS(i,j,k,bi,bj) .ne. 0.) then
                     fctile = fctile +
     &                    wobcse(k,iobcs)*cosphi(i,j,bi,bj)*
     &                    (vbar(i,j,k,bi,bj) - OBEv(j,k,bi,bj))*
     &                    (vbar(i,j,k,bi,bj) - OBEv(j,k,bi,bj))
                  endif
               enddo
            enddo
c--         End of loop over layers.
            fcthread         = fcthread           + fctile
            objf_obcse(bi,bj) = objf_obcse(bi,bj) + fctile
          enddo
        enddo

#ifdef ECCO_VERBOSE
c--     Print cost function for all tiles.
        _GLOBAL_SUM_R8( fcthread , myThid )
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,i8.8)')
     &    ' cost_obcse: irec = ',irec
        call print_message( msgbuf, standardmessageunit,
     &                      SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,a,d22.15)')
     &    ' global cost function value',
     &    ' (obcse) = ',fcthread
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







