C $Header: /u/gcmpack/MITgcm/verification/aim.5l_cs/code/DIAGNOSTICS_SIZE.h,v 1.1 2005/02/02 03:27:34 jmc Exp $
C $Name:  $


C     Diagnostics Array Dimension
C     ---------------------------
C     ndiagMax   :: maximum total number of available diagnostics
C     numlists   :: maximum number of diagnostics list (in data.diagnostics)
C     numperlist :: maximum number of active diagnostics per list (data.diagnostics)
C     numLevels  :: maximum number of levels to write    (data.diagnostics)
C     numdiags   :: maximum size of the storage array for active diagnostics
      INTEGER    ndiagMax
      INTEGER    numlists
      INTEGER    numperlist
      INTEGER    numLevels
      INTEGER    numdiags
      PARAMETER( ndiagMax = 500 )
      PARAMETER( numlists = 10, numperlist = 50, numLevels=2*Nr )
      PARAMETER( numdiags = 200 )


CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
