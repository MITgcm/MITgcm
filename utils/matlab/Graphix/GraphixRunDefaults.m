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
PlotResults  = 1;

Grads      = 0;
GridSuffix = '';
ZcordFile  = '';
Vector     = 0;
SecMom     = '';
uFld       = 'uVeltave';
u2Fld      = 'UUtave';
vFld       = 'vVeltave';
v2Fld      = 'VVtave';
TFld       = 'Ttave';
T2Fld      = 'TTtave';
EtaFld     = 'Etatave';
Eta2Fld    = 'Eta2tave';
WFld       = 'WVEL';
W2Fld      = 'WVELSQ';
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
XL         = -180:2:178;%-180:2:178;
YL         =  -90:2: 90;% -90:2: 90;
ThetaToActT    = 0;
ThetaToThetaEc = 0;
DevFromMean = 0;