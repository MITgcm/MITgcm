
#include "SEAICE_OPTIONS.h"

      subroutine seaice_cost_export( myThid )
C     /==========================================================\
C     | subroutine seaice_cost_export
C     | o Poor man's ice export through Fram Strait
C     |   Parameters currently hard-wired for arctic210x192x50
C     \==========================================================/
       implicit none

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
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

      parameter ( isec = 79, jsecbeg = 57, jsecend = 83 )
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
     &              + vHeffExportCell(i,j,bi,bj)
            end if
           enddo
          enddo
         end if
        end do
c--   end of bi,bj loop
       end do
      end do

#endif
      
      end
