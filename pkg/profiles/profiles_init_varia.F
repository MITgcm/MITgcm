#include "PROFILES_OPTIONS.h"

      subroutine profiles_init_varia( mythid )

c     ==================================================================
c     SUBROUTINE profiles_init_varia
c     ==================================================================
c
c     o Initialise the variable cost function part.
c
c     started: Christian Eckert eckert@mit.edu 30-Jun-1999
c     changed: Christian Eckert eckert@mit.edu 18-Apr-2000
c              - Restructured the code in order to create a package
c                for the MITgcmUV.
c     heimbach@mit.edu 05-Nov-2003 Now ecco part of cost
c
c     ==================================================================
c     SUBROUTINE profiles_init_varia
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"

#ifdef ALLOW_PROFILES
# include "PROFILES_SIZE.h"
# include "profiles.h"
#endif

#ifdef ALLOW_GRDCHK
# include "netcdf.inc"
#endif

c     == routine arguments ==

      integer mythid

c     == local variables ==

      integer bi,bj
      integer itlo,ithi
      integer jtlo,jthi
      integer num_file,num_var
#ifdef ALLOW_GRDCHK
      integer err, iG, jG, IL, JL
      character*(max_len_fnam) profilesfile
      character*(max_len_fnam) fnameequinc
#endif /* ALLOW_GRDCHK */

c     == external functions ==
#ifdef ALLOW_GRDCHK
      INTEGER ILNBLNK
      EXTERNAL ILNBLNK
#endif /* ALLOW_GRDCHK */

c     == end of interface ==
      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)

c--   Initialize the tiled cost function contributions.
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do num_file=1,NFILESPROFMAX
           do num_var=1,NVARMAX
            objf_profiles(num_file,num_var,bi,bj)= 0. _d 0
            num_profiles(num_file,num_var,bi,bj) = 0. _d 0
            profiles_dummy(num_file,num_var,bi,bj)=0. _d 0
           enddo
          enddo
          do num_var=1,NVARMAX
           objf_profiles_mean(num_var,bi,bj)= 0. _d 0
           num_profiles_mean(num_var,bi,bj) = 0. _d 0
          enddo
#ifdef ALLOW_GRDCHK
          if (profilesDoNcOutput) then
           do num_file=1,NFILESPROFMAX
            if (ProfNo(num_file,bi,bj).GT.0) then
C Close .equi file so data in buffer is not lost when run finishes
             err = NF_CLOSE(fidforward(num_file,bi,bj))
             CALL PROFILES_NF_ERROR( 'INIT_VARIA: NF_CLOSE',
     &            err,bi,bj,myThid )

C determine model equivalent file name
             iG=bi+(myXGlobalLo-1)/sNx ! Kludge until unstructered tiles
             jG=bj+(myYGlobalLo-1)/sNy ! Kludge until unstructered tiles
             IL  = ILNBLNK( profilesfiles(num_file) )
             write(profilesfile,'(a)')
     &       profilesfiles(num_file)(1:IL)
             IL  = ILNBLNK( profilesfile )
             JL  = ILNBLNK( profilesDir )
             write(fnameequinc,'(3a,2(i3.3,a))')
     &       profilesDir(1:JL),profilesfile(1:IL),'.',
     &       iG,'.',jG,'.equi.nc'

c re-open to reset write permissions
             JL  = ILNBLNK( fnameequinc )
             err = NF_OPEN( fnameequinc(1:JL),NF_WRITE,
     &       fidforward(num_file,bi,bj))
             CALL PROFILES_NF_ERROR(
     &            'INIT_VARIA: NF_OPEN('//fnameequinc(1:JL)//')',
     &            err,bi,bj,myThid )
            endif
           enddo
          endif
#endif /* ALLOW_GRDCHK */
        enddo
      enddo

      _BARRIER

      return
      end
