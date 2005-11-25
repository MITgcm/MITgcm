function [nc,dim,XC,XG,YC,YG,ZC,ZF,RAC,drC,drF,HFacC,...
          HFacW,HFacS,dxG,dyG,dxC,dyC,AngleCS,AngleSN] = ...
    DiagLoadGridData(LoadGridData,grd,gdf,flu,GridSuffix,ZcordFile)

if ismember(flu,{'C','L'}), flu = 'A'; end
if ismember(flu,{'I'}    ), flu = 'O'; end
grdfile = [grd,'/GridData',flu,GridSuffix,'.mat'];

% If LoadGridData is turned on, load grid data from the model output files.
% If it is turned off, load it from the data file where saved from a
% previous loading of the raw data.  The file name (and path) of this file
% is set in the DiagGenParam file.
if LoadGridData

	if isequal(gdf,'MDS')
		XC  = rdmds([grd,'/','XC' ]);
		YC  = rdmds([grd,'/','YC' ]);
		XG  = rdmds([grd,'/','XG' ]);
		YG  = rdmds([grd,'/','YG' ]);
		dxC = rdmds([grd,'/','DXC']);
		dyC = rdmds([grd,'/','DYC']);
		dxG = rdmds([grd,'/','DXG']);
		dyG = rdmds([grd,'/','DYG']);
		RAC = rdmds([grd,'/','RAC']);
 		HFacC = rdmds([grd,'/','hFacC']);
		HFacS = rdmds([grd,'/','hFacS']);
		HFacW = rdmds([grd,'/','hFacW']);
        dim = size(XC); nc  = dim(2);
        
        % Information for z-axis.
        if isequal(ZcordFile,'')
    		load(['Zcord',flu,GridSuffix,'.mat']);
        else
            load(ZcordFile);
        end	
        
	elseif isequal(gdf,'MNC')
        if isempty(dir([grd,'/grid.*'])), ls([grd,'/grid.*']); end
        data = rdmnc([grd,'/grid.*']);
        dim = size(data.XC); nc  = dim(2);
        RAC = data.rA;
		XC  = data.XC(1:6.*nc,1:nc); dxC = data.dxC(1:6.*nc,1:nc);
        XG  = data.XG(1:6.*nc,1:nc); dxG = data.dxG(1:6.*nc,1:nc);
		YC  = data.YC(1:6.*nc,1:nc); dyC = data.dyC(1:6.*nc,1:nc);
        YG  = data.YG(1:6.*nc,1:nc); dyG = data.dyG(1:6.*nc,1:nc);
        ZC = data.RC;                drC = data.drC;
		ZF = data.RF;                drF = data.drF;
 		HFacC = data.HFacC(1:6.*nc,1:nc,:);
		HFacW = data.HFacW(1:6.*nc,1:nc,:);
		HFacS = data.HFacS(1:6.*nc,1:nc,:);
        %AngleCS = data.AngleCS;
        %AngleSN = data.AngleSN;
    end
    
    [AngleCS,AngleSN] = cubeCalcAngle(YG,RAC,dxG,dyG);
    
    save(grdfile,'XC','XG','YC','YG','ZC','ZF','RAC','drC','drF',...
         'HFacC','HFacW','HFacS','dxG','dyG','dxC','dyC','AngleCS',...
         'AngleSN','dim','nc');
     
% If LoadGridData is turned off, load it from the data file where saved
% from a previous loading of the raw data.
else
    load(grdfile)
end