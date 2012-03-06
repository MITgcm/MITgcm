C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_cost_export.F,v 1.5 2012/03/06 16:45:20 jmc Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

      subroutine seaice_cost_export( myThid )
C     *==========================================================*
C     | subroutine seaice_cost_export
C     | o Poor man ice export through Fram Strait
C     |   Parameters currently hard-wired for arctic210x192x50
C     *==========================================================*
       implicit none

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SEAICE_SIZE.h"
#include "SEAICE.h"
#include "SEAICE_COST.h"

C     ======== Routine arguments ======================
C     myThid - Thread number for this instance of the routine.
      integer myThid

#ifdef ALLOW_SEAICE_COST_EXPORT
C     ========= Local variables =========================
      integer    isecbeg      , isecend      , jsec
      integer    jsecbeg      , jsecend      , isec
      integer    kmaxdepth
      integer i, j, k
      integer ig, jg
      integer bi, bj

#if (defined (SEAICE_COST_EXPORT_FRAM))
      parameter ( isec = 74, jsecbeg = 60, jsecend = 89 )
#elif (defined (SEAICE_COST_EXPORT_CANARCH))
      parameter ( isec = 202, jsecbeg = 236, jsecend = 242 )
#else
      parameter ( isec = 1, jsecbeg = 1, jsecend = 1)
#endif
      parameter ( kmaxdepth = 1 )

      do bj=myByLo(myThid),myByHi(myThid)
       do bi=myBxLo(myThid),myBxHi(myThid)
        do i=1,sNx
         ig = myXGlobalLo-1+(bi-1)*sNx+i
         if (ig .eq. isec) then
          do k = 1, kmaxdepth
           do j=1,sNy
            jg = myYGlobalLo-1+(bj-1)*sNy+j
            if ((jg .ge. jsecbeg) .and. (jg .le. jsecend)) then
             objf_ice_export(bi,bj) = objf_ice_export(bi,bj)
     &              - uHeffExportCell(i,j,bi,bj)
cph(
cph             print *, 'ph-canarch pos i, ig, j, jg, X, Y ',
cph     &        i, ig, j, jg, xC(i,j,bi,bj), yC(i,j,bi,bj)
cph)
            end if
           enddo
          enddo
         end if
        end do
c--   end of bi,bj loop
       end do
      end do

#endif

      return
      end
