function ifig = GraphixPlot(plotparam,plotparamvalue,pagename,page,data,...
                            xax,yax,XG,YG,time,pltslc,outputdir,...
                            LoadGridData,SavePlots,GraphixDebug);

% Function: GraphixPlot
% Author:   Daniel Enderton
%
% Input Fields:
%
%   Field        Type        (Brief) Description
%   -----------------------------------------------------------------------
%   ifig         integer     Figure counter
%   field        cell array  Experiment and plot configuration information
%   data         cell array  Plotting data
%   datatime     array       Iterations ('Int') or months ('Tav') of data
%   outputdir    string      Output directory name
%   LoadGridData 0/1         Optionally load grid data
%   SavePlots    0/1         Optionally save plot to .eps file
%
% Descripton:
%   This is the plotting function for the diagnostics package.  This
%   function should allow you to plot all sorts of things, and is
%   reasonably versitile.  Unfortunately, this also means that things get
%   quite comlicated.  The code is broken up into little scripts to make it
%   more managable, though unfortunately this makes it rather difficult to
%   track variables and make changes throughout the function.

% Load diagnostics parameters:  Plot, general, and field information.
GraphixGenParam;
GraphixPlotDefaults;
GraphixFieldParamA;
GraphixFieldParamO;
GraphixFieldParamC;
GraphixFieldParamI;
diagrunparam  = ReadVariables('GraphixRunDefaults.m');
nrow = length(page);

% Initiate figure, with number 'ifig', set oreiention.
figure; clf; set(gca,'fontsize',fs_tick);

% Loop over subplots and make plots.
for inrow = 1:nrow
    
    ntrl = length(page{inrow}); if ntrl ~= 1, ntrl = ntrl - 1; end
    if ntrl == 1, cmp = 'Sep'; else, cmp = page{inrow}{end}; end
    if ntrl == 1, ncol = 1; elseif isequal(cmp,'Sbs'), ncol = ntrl; else ncol = 1; end
    
    for incol = 1:ncol
        
        if ~isequal(data{inrow}{incol},'Empty')
        
            % Set panel settings to default values, override first with
            % page overrides and finally with panel overrides.
            GraphixPlotDefaults;
            for iarg = 1:length(plotparam)
                incoming = plotparamvalue{iarg};
                eval([plotparam{iarg},'=incoming;']);
            end
			ExpInfo = page{inrow}{incol};
            for iarg = 15:2:length(ExpInfo)
                incoming = ExpInfo{iarg+1};
                eval([ExpInfo{iarg},'=incoming;']);
            end
            
            eval(['orient ',Orientation,';']);
            
            % Very crude test to see if cube sphere, must be fixed!
            test = size(data{inrow}{incol});
            if test(1)./test(2) == 6, isCS = 1; else, isCS = 0; end
            fln = page{inrow}{incol}{ifln};
            try fln = PlotFld; catch, end
            pst = page{inrow}{incol}{ipst};
            flu = page{inrow}{incol}{iflu};
            
            disp(['  Row: ',num2str(inrow),'/',num2str(nrow),...
                  '; Col: ',num2str(incol),'/',num2str(ncol),...
                  '; Cmp: ',cmp]);
                  
            % Load contour intervals, units (loaded in
            % 'GraphixFieldParam[A,O,I,C]').
            if ~isequal(cmp,'Dif')
                try, contint; catch
                    try, eval(['contint = ',fln,'contour',flu,';']);
                    catch
                    	disp(['***Warning***  No contour information for ',fln]);
                        disp(['               Using 10 generic contour levels.']);
                        contint = 10;
                    end
                end
            else, try, contint; catch, contint = 10; end, end
            try, units; catch
                try, eval(['units = ',fln,'units',flu,';']);
                catch,
                	disp(['***Warning***  No unit information found for ',fln]);
                    disp(['               Using question mark.']);
                    units = '?';
                end
            end
                
            dx = (1-dxl-dxr-(ncol-1)*dxm)/ncol;
            dy = (1-dyb-dyt-(nrow-1)*dym)/nrow;
            xi = dxl + (incol-1)*(dx+dxm);
            yi = 1-dyt-inrow*dy-(inrow-1)*dym;
            
            isp = (inrow-1)*ncol+incol;
            if GraphixDebug, disp(['  GraphixDebug:  Subplot:  ',mat2str([nrow,ncol,isp])]); end
            if GraphixDebug, disp(['  GraphixDebug:  SP Range: ',mat2str([xi,yi,dx,dy])]); end
            subplot(nrow,ncol,isp); hold on;
            set(gca,'position',[xi,yi,dx,dy],'fontsize',fs_axis,...
                'TickDir',TickDir);
            
            if Coast
                fac = pi./180;
                xax{inrow}{incol} = xax{inrow}{incol}.*fac;
                yax{inrow}{incol} = yax{inrow}{incol}.*fac;
            else, fac = 1; end
            
            if isempty(clabelv)
                clabelv = contint;
            end
            
            % (Re)set axes and color axis -- Accounts for things like a possible
            % colorbar, or trimming the axis in certain ways.
            % Apply desired colorbar, contour label, tick labels, box, grid, and
            % other such odds and ends.
            GraphixPlotMakePlot;
            GraphixPlotResetAxes;
            GraphixPlotMisc;
            GraphixPlotTitles;
            
            clear contint units crange clabelv
        end
    end
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                   Save plot, update figure counter                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Save plot as desired.
if SavePlots
    if isequal(outputdir,'')
        outputfile = [pagename,'.eps'];
    else
        outputfile = [outputdir,'/',pagename,'.eps'];
    end
    print('-depsc2',outputfile);
end
