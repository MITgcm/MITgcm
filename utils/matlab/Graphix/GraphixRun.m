function GraphixRun(varargin);

% Function: GraphixRun
% Author:   Daniel Enderton
%
% Input Fields:
%
%   Field        Type        (Brief) Description
%   -----------------------------------------------------------------------
%   page         cell array  Experiment and plot information     (Required)
%   pagename     string      Page name (title and output file)   (Required)
%
%   OutputDir    string      Output directory name               (Optional)
%   LoadData     0/1         Load data                           (Optional)
%   LoadGridData 0/1         Load grid data                      (Optional)
%   DumpData     0/1         Save plotting data                  (Optional)
%   SavePlots    0/1         Save plot                           (Optional)
%   GraphixDebug 0/1         Print debug flags                   (Optional)
%
%   (More optional parameters seen in GraphixRunDefaults)
%
% Descripton:
%   This is the top level function of the cubed-sphere global model
%   data manipulation and plotting package.
%
%   Sample function call is as follows (page and pagename appropriately
%   defined):
%
%   >> GraphixRun(page,pagename,'LoadData',1);
%   >> GraphixRun(page,pagename,'LoadData',1,'OutputDir','~/');


% Load GraphixRun default parameters and general diagnostics parameters. 
GraphixGenParam;
GraphixRunDefaults;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                      Read in function arguements                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Read in list of possible run parameters.
grphxrunparam  = ReadVariables('GraphixRunDefaults.m');
grphxplotparam = ReadVariables('GraphixPlotDefaults.m');
grphxplotparam{end+1} = 'contint';
grphxplotparam{end+1} = 'units';
grphxplotparam{end+1} = 'range';

% Rudimentary checks to make sure that the page and pagemane fields are
% there and that they are the correct classes.
if length(varargin) < 2
    error('There must be at least two input arguements.')
else
    if isequal(class(varargin{1}),'cell')
        page = varargin{1};
    else
        error('''page'' (1st argument) must be cell array.');
    end
    if isequal(class(varargin{2}),'char')
        pagename = varargin{2};
    else
        error('''pagename'' (2nd arguement) must be a string.');
    end
end

% Read in optional parameters, overriding the GraphixRun defaults.
plotparam = {}; plotparamvalue = {};
for iarg = 3:2:length(varargin)
    incoming = varargin{iarg+1};
    if ismember(varargin{iarg},grphxrunparam)
        eval([varargin{iarg},'=incoming;']);
    elseif ismember(varargin{iarg},grphxplotparam)
        plotparam{end+1}      = varargin{iarg};
        plotparamvalue{end+1} = incoming;
    else
        error(['Unrecognized GraphixRun setting:  ',lower(varargin{iarg})]);
    end
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                             Function body                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Determine number of plots for this page.
nplot = length(page);
    
% Here would be a nice place to implement a function to verify that the
% experiment field, slicing, averaging, and plot configuration information
% is all self-consistent.


% If the loaddata options is set to 0, the loading, averaging and slicing
% of the data will be bypassed and the data to be plotted will be pulled
% from the 'dump.mat' file.  This options is here almost entirely for the
% purpose of fine-tuning your plots with the 'GraphixPlot' function (also good
% for testing modifications to GraphixPlot!).  If the setting is set to 1,
% then after the loading, averaging, and slicing the data to be plotted is
% automatically saved to 'GraphixDump.mat' so you so not have to load the
% exact same data next time is you so desire.
if LoadData
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %              Load data (calls GraphixAverage and GraphixSlice             %
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    for inplot = 1:nplot
        ntrl = length(page{inplot}); if ntrl ~= 1, ntrl = ntrl - 1; end
        for intrl = 1:ntrl
            ExpInfo = page{inplot}{intrl};
            
            % Put 'Empty' for data holders if a blank space is desired.
            if isequal(ExpInfo,'Empty')
                data{inplot}{intrl}   = 'Empty';
                xax{inplot}{intrl}    = 'Empty';
                yax{inplot}{intrl}    = 'Empty';
                time{inplot}{intrl}   = 'Empty';
                pltslc{inplot}{intrl} = 'Empty';
                
            else
                fil = ExpInfo{ifil};  % FileName     (Required)
				fln = ExpInfo{ifln};  % FieldName    (Required)
				trl = ExpInfo{itrl};  % Experiment   (Required)
				dat = ExpInfo{idat};  % DataType     (Required)
				dad = ExpInfo{idad};  % DataDir      (Required)
				grd = ExpInfo{igrd};  % GridDir      (Required)
				itr = ExpInfo{iitr};  % Iterations   (Required)
				tst = ExpInfo{itst};  % TimeStep     (Required)
				flu = ExpInfo{iflu};  % Fluid        (Required)
				ddf = ExpInfo{iddf};  % DataFormat   (Required)
                gdf = ExpInfo{igdf};  % GridFormat   (Required)     
				avg = ExpInfo{iavg};  % Averaging    (Required)
				slc = ExpInfo{islc};  % SliceType    (Required)
				pst = ExpInfo{ipst};  % PlotStyle    (Required)
                
                % Set panel settings to default values or this that are
                % overriden for the entire page.  Override within panel.
                for param = grphxrunparam
                    eval([param{1},'Temp=',param{1},';']);
                end
				for iarg = 15:2:length(ExpInfo)
                    incoming = ExpInfo{iarg+1};
                    eval([ExpInfo{iarg},'Temp=incoming;']);
                end
                
                % Make GraphixLoad function call.
                disp(['Loading - Experiment:  ',trl,';  Field:  ',fln]);
                [data{inplot}{intrl},xax{inplot}{intrl},...
                 yax{inplot}{intrl},time{inplot}{intrl},...
                 pltslc{inplot}{intrl},XG{inplot}{intrl},...
                 YG{inplot}{intrl}] = GraphixLoad(...
                    fil,fln,trl,dat,dad,grd,itr,tst,flu,ddf,gdf,avg,slc,...
                    pst,LoadGridData,GraphixDebug,GridSuffixTemp,...
                    ZcordFileTemp,IndexTemp,DimTemp,VectorTemp,MateTemp,...
                    uFldTemp,vFldTemp,gmfileTemp,KwxFldTemp,KwyFldTemp,...
                    GradsTemp,Year0IterTemp,SecPerYearTemp,MonthsTemp,...
                    PlotFldTemp,XLTemp,YLTemp,ThetaToActTTemp,...
                    ThetaToThetaEcTemp,DataInTemp,SecMomTemp,TFldTemp,...
                    T2FldTemp,EtaFldTemp,Eta2FldTemp,u2FldTemp,...
                    v2FldTemp,DevFromMeanTemp,WFldTemp,W2FldTemp);
                
                % Save panel data for outside use.
                if DumpData
                    datadump   = data{inplot}{intrl};
                    xaxdump    = xax{inplot}{intrl};
                    yaxdump    = yax{inplot}{intrl};
                    timedump   = time{inplot}{intrl};
                    pltslcdump = pltslc{inplot}{intrl};
                    save(['Data',pagename,fln,flu,'.mat'],'datadump',...
                         'xaxdump','yaxdump','timedump','pltslcdump');
                end
            end
        end
    end
    save('GraphixDump.mat','data','xax','yax','time','pltslc','XG','YG');
else
    load('GraphixDump.mat');
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                             Plot Data                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Now to make the actual plots.  Basically, all of the information is
% passed to the 'GraphixPlot' functions, and things get a bit hairy in
% there.  Hopefully the comments in the function, and its supporting
% scripts will help you to make sense of this as needed.
if PlotResults
    disp(['Plotting results:']);
    GraphixPlot(plotparam,plotparamvalue,pagename,page,data,xax,yax,...
                XG,YG,time,pltslc,OutputDir,LoadGridData,SavePlots,...
                GraphixDebug);
end
