%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                           Plotting parameters                           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Tick label information.
lontick = [-180:60:180];
lattick = [ -60:30:60 ];
lonticklabel = {'180W','120W','60W','0','60E','120E','180E'};
latticklabel = {        '60S','30S','0','30N', '60N'       };
vertickA = [0:10000:100000];    % Top down.
vertickO = [-5000:1000:0];      % Bottom up.
verticklabelA = vertickA./100;  % Show in hPa rather than Pa.
verticklabelO = abs(vertickO);  % Have depth as positive quantity.
timtick = [0:1:12];
timticklabel = {0:1:12};

clabelv = [];

% Title parameters.
FigureTitle      = 1;   % Put on figure title.
SubplotTitle     = 1;   % Put on subplot title.
SubplotExp       = 1;   % Put on subplot experiment name.
SubplotFld       = 1;   % Put on subplot experiment name.
SubplotSlc       = 0;   % Put on subplot sliceType information.
SubplotAvg       = 0;   % Put on subplot averaging information.
SubplotPst       = 0;   % Put on subplot plotStyle information.
SubplotMinMax    = 0;   % Put on subplot minimum and maximum.
SubplotModelTime = 0;   % Put on subplot model time.
SubplotTitleOverride = '';

XLabel = '';
YLabel = '';
Edges  = 0;

UseColorbar       = 0;  % Throw on a colorbar.
UseNiceTickLabels = 1;  % Use nice tick labels (otherwise default)

Orientation = 'tall';      % Options: 'landscape', 'tall'
Box         = 'on';        % Options: 'on', 'off'
Grid        = 'on';        % Options: 'on', 'off'
Coast       = 0;           % Earth coast line:  0 (off), 1 (on)
TickDir     = 'in';        % Options: 'in','out'
Shading     = '';

CaxisFixed  = 1;   % Use fixed color axis.
CaxisMinMax = 1;   % Use minimum and maximum for color axis.
AxesTrimX   = 1;   % Reset axis, trimming in x.
AxesTrimY   = 1;   % Reset axis, trimming in y.
ContFixed   = 1;   % Use fixed contour intervals.
UseConLabel = 1;   % Use 'contour' contour labels.
UseCnfLabel = 0;   % Use 'contourf' contour labels.
UseLegend   = 1;   % Put legend on (some) line plots.

LegendPlacement = 1;  % See help for legend legend placement for options.
TseXaxMod = .5;       % Time averaging sequential tick label mod factor.

% Page layout parameters.
dxl   = .07;       % Left side margin.
dxr   = .07;       % Right side margin.
dxm   = .075;      % Gap between subplots horizontally.
dxcb  = .025;      % DX for colorbar.
dxcbg = .005;      % DX for gap between plot and colorbar.
dyb   = .075;      % Bottom margin.
dyt   = .1;        % Top margin.
dym   = .08;       % Gap between subplots vertically.
titlefac = 0.5;    % Fractional distance from first subplot to top.

% Fontsizes.
fs_title     = 14;   % Font size:  Title.
fs_sptitle   = 14;   % Font size:  Subplot title.
fs_colorbar  = 10;   % Font size:  Colorbar.
fs_axis      = 12;   % Font size:  Axes.
fs_axislabel = 12;   % Font size:  Axes label.
fs_tick      = 12;   % Font size:  Tick label.
fs_clabel    = 10;   % Font size:  Contour label.
fs_textbox   = 10;

% Line details.
linecolors = {'b','r','k','g','c','m','y','b--','r--','k--','g--','c--'};
linewidth = .5;
cmap = 'jet'; % 'jet','solid-dashed','BW'
cmapcenter0 = 0;
labelspacing = 300;

% Axis ranges.
xlim = [];
ylim = [];
ylimO = [-5200,0];
ylimA = [0,100000];

offset = 0;

% Text box
TextBox = '';
TextPos = [0,0];