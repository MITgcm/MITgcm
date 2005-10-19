%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%       GraphixRun default parameters (also used in GraphixLoad)          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% All of these defaults can be overriden in the GraphixRun call or in the
% panel cell arrays themselves (if you do not want them overriden for
% everything on the page).

DumpData     = 0;
GraphixDebug = 0;
LoadGridData = 1;
LoadData     = 1;
OutputDir    = '';
SavePlots    = 1;

Grads      = 0;
GridSuffix = '';
ZcordFile  = '';
Vector     = 0;
uFld       = 'uVeltave';
vFld       = 'vVeltave';
gmfile     = 'gm_tave';
KwxFld     = 'Kwx';
KwyFld     = 'Kwy';
Mate       = 0;
Index      = {};
Dim        = 0;
Year0Iter  = 0;
SecPerYear = 31104000;
Months     = [];
PlotFld    = '';
DataIn     = [];
XL         = -180:2.5:177.5;
YL         =  -90:  2:   88;
ThetaToActT    = 0;
ThetaToThetaEc = 0;