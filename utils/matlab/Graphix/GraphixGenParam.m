%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                  Experiment and configuration indecies                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Current experiment order with indecies defined:
%   { FieldName  (fln), Experiment (trl),  DataType   (dat),...
%     DataDir    (dad), GridDir    (grd),  Iterations (itr),...
%     TimeStep   (tst), Fluid      (flu),  DataFormat (drm),...
%     Averaging  (avg),  SliceType (slc),  PlotStyle  (pst),...
%     Optional parameters}
%
% Field descriptions/options:
%   FieldName  (fln):   Variable name.
%   FileName   (fil):   File name.
%   DataDir    (dad):   Data directory.
%   GridDir    (grd):   Grid data directory.
%   Experiment (trl):   Experiment name.
%   DataType   (dat):   Data type, supports 'Tav', 'Ins'.
%   Iterations (itr):   Iterations for analysis.
%   TimeStep   (tst):   Time step.
%   Fluid      (flu):   Fluid, supports 'A', 'O'.
%   DataFormat (dfm):   Data format, supports 'MDS', 'MNC'.
%   Averaging  (avg):  'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
%                      'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec',
%                      'DJF', 'MAM', 'JJA', 'SON', 'Ann', 'Ins',
%   SliceType  (slc):  'Sur', 'Zon', 'i=#', 'j=#', 'k=#'
%   PlotStyle  (pst):  'Grd', 'Int', 'Con', 'Cnf', 'Lin'
ifln = 1;
ifil = 2;
idad = 3;
igrd = 4;
itrl = 5;
idat = 6;
iitr = 7;
itst = 8;
iflu = 9;
iddf = 10;
igdf = 11;
iavg = 12;
islc = 13;
ipst = 14;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                             Field Dimensions                            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fields2D = {'USTR'  ,'VSTR'  ,'TSR'   ,'OLR'   ,'SSR'   ,...
            'SLR'   ,'SHF'   ,'EVAP'  ,'PRECNV','PRECLS',...
            'CLOUDC','CLTOP' ,'CBMF'  ,'DRAG'  ,'aimV0' ,...
            'aimT0' ,'aimQ0' ,'EnFxPr','albedo','dTsurf',...
            'fract' ,'iceH'  ,'snowH' ,'Tsrf'  ,'Tice1' ,...
            'Tice2' ,'snowPr','albedo','flx2oc','frw2oc',...
            'SLP'   ,'HF'    ,'QSW'   ,'TX'    ,'TY'    ,...
            'FW'    ,'SFx'   ,'SIC'   ,'MXL'   ,'SST'   ,...
            'SSS'   ,'vSq'   ,'Eta'   ,'ETAstd'};
    
fields3D = {'S','T','U','V','W','RH','Tstd','KEpri','phiHyd',...
            'Psi','Bol','Res','Conv','ActT','ThetaEc','ActTstd'};


% Zonal averaging parameters.
ny = 64;

% Constants
g        = 9.81;        % Acceleration due to gravity [m/s2].
presrefA = 100000;      % Atmospheric reference pressure [Pa].
RdA      = 287.05;      % Gas constant for dry air [J/kg/K].
RvA      = 461;         % Gas constant for ?moist? air [J/kg/K].
cpA      = 1005;        % Atmos. spec. heat at constant pres. [J/kg/K].
LHvapA   = 2501000;     % Atmos. latent heat of vaporization [J/kg].
A_CC     = 611;         % Clausius-Clapeyron A coefficient [Pa].
Beta_CC  = 0.067;       % Clausius-Clapeyron Beta coefficient [1/C].

% Conversions
K2C      = 273.15;      % Kelvin = K2C + Celsius [K or C].