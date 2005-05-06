C $Header: /u/gcmpack/MITgcm/verification/global_with_exf/code/DIAGNOSTICS_SIZE.h,v 1.2 2005/05/06 03:46:40 dimitri Exp $
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
      PARAMETER( numlists = 8, numperlist = 1, numLevels=1 )
      PARAMETER( numdiags = 8 )


CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
