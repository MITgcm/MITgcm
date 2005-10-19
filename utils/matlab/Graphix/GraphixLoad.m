function [data,xax,yax,time,pltslc] = ...
    GraphixLoad(fil,fln,trl,dat,dad,grd,itr,tst,flu,ddf,gdf,avg,slc,pst,...
                LoadGridData,GraphixDebug,GridSuffix,ZcordFile,Index,...
                Dim,Vector,Mate,uFld,vFld,gmfile,KwxFld,KwyFld,Grads,...
                Year0Iter,SecPerYear,Months,PlotFld,XL,YL,ThetaToActT,...
                ThetaToThetaEc,DataIn);

% Load parameters.
GraphixGenParam;
GraphixFieldParamA;
GraphixFieldParamO;
GraphixFieldParamI;
GraphixFieldParamC;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%             Prepare for the various data types, set time                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Time stuff (itr = 0 is start of new 360 day year).
if isempty(Months)
    months = (itr-Year0Iter)./(30.*24.*3600./tst);
else
    months = Months;
end
time = months/12;

if Dim == 0
    if ismember(PlotFld,fields2D) | ismember(fln,fields2D), Dim = 2; end
    if ismember(PlotFld,fields3D) | ismember(fln,fields3D), Dim = 3; end
    if Dim == 0
        error('Unknown field dimension!');
    end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                             Read in data                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Load grid information.
[nc,dim,XC,XG,YC,YG,Ylat,ZC,ZF,RAC,drC,drF,HFacC,...
 HFacW,HFacS,dxG,dyG,dxC,dyC,AngleCS,AngleSN] = ...
    GraphixLoadGridData(LoadGridData,grd,gdf,flu,GridSuffix,ZcordFile);

% Load GRADS data
if ~isequal(Grads,0)
    if GraphixDebug, disp(['  Debug -- Loading GRADS field.']); end
    [data,xax,yax,zax,months,time,dim] = ...
        GraphixLoadGradsData(Grads,dad,fln,GraphixDebug);
    GraphixDebug_Local(GraphixDebug,'data','load',data);
    data = GraphixAverage(data,fln,avg,months,ddf,dim);
    GraphixDebug_Local(GraphixDebug,'data','average',data);
    [data,xax,yax,pltslc] = ...
        GraphixSliceGradsData(fln,flu,slc,data,xax,yax,zax);
    GraphixDebug_Local(GraphixDebug,'data','slice',data);

% Load monitor data -- can be cleaned up.
elseif isequal(dat(1:2),'Mn') | isequal(dat,'Mon')
    [data,time] = ...
        GraphixLoadMonitor(fln,fil,dad,itr,tst,SecPerYear,GraphixDebug);
    xax = time; yax = NaN; pltslc = 'timfld';

% Load meridional overturning.
elseif ismember(fln,{'Bol','Psi','Res'})
    if isequal(flu,'A'),     delM = - 1000*drF ./ g;
    elseif isequal(flu,'O'), delM = drF;             end
    if ismember(fln,{'Psi','Res'})
        if isequal(ddf,'MDS')
            U = LocalLoad([dad,uFld,'.*'],uFld,itr,ddf,nc,DataIn);
            V = LocalLoad([dad,vFld,'.*'],vFld,itr,ddf,nc,DataIn);
        elseif isequal(ddf,'MNC')
            U = LocalLoad([dad,fil,'.*'],uFld,itr,ddf,nc,DataIn);
            V = LocalLoad([dad,fil,'.*'],vFld,itr,ddf,nc,DataIn);
        end
        U = GraphixAverage(U,fln,avg,months,ddf,Dim);
        V = GraphixAverage(V,fln,avg,months,ddf,Dim);
        [psiavg,mask,ylat]=calc_PsiCube(delM,U.*HFacW,V.*HFacS,dxG,dyG);
    end
    if ismember(fln,{'Bol','Res'})
        try
            kwx = LocalLoad([dad,gmfile,'.*'],KwxFld,itr,ddf,nc,DataIn);
            kwy = LocalLoad([dad,gmfile,'.*'],KwyFld,itr,ddf,nc,DataIn);
        catch
            kwx = LocalLoad([dad,'GM_Kwx-T'],'',itr,'MDS',nc,DataIn);
            kwy = LocalLoad([dad,'GM_Kwy-T'],'',itr,'MDS',nc,DataIn);
        end
        kwx = GraphixAverage(kwx,fln,avg,months,ddf,Dim);
        kwy = GraphixAverage(kwy,fln,avg,months,ddf,Dim);
        [ub,vb]=calc_Bolus_CS(RAC,dxC,dyC,dxG,dyG,drC,kwx,kwy,HFacW,HFacS);
        [psibol,mask,ylat]=calc_PsiCube(delM,ub.*HFacW,vb.*HFacS,dxG,dyG);
    end
    if isequal(fln,'Psi')
        data = psiavg(2:end-1,:)';
    elseif isequal(fln,'Bol')
        data = psibol(2:end-1,:)';
    elseif isequal(fln,'Res')
        data = psibol(2:end-1,:)' + psiavg(2:end-1,:)';
    end
    if isequal(flu,'A'), data = data./1e6; end
    xax = ylat; yax = ZF; pltslc = 'lathgt';
              
% Second moments -- This can eventually be made more elegant.
elseif isequal(fln,'ETAstd')
    ETA  = LocalLoad(dad,'ETA' ,itr,ddf,nc,DataIn)./100  ;
    ETA2 = LocalLoad(dad,'Eta2',itr,ddf,nc,DataIn)./10000;
    data = sqrt(ETA2-ETA.*ETA);
    data = GraphixAverage(data,fln,avg,months,ddf,Dim);
    [data,xax,yax,pltslc] = ...
        GraphixSlice(data,fln,trl,dat,dad,grd,itr,tst,flu,ddf,gdf,avg,slc,pst,...
                  Dim,LoadGridData,GridSuffix,ZcordFile,Vector,PlotFld);
elseif ismember(fln,{'Tstd','ActTstd'})
    T  = LocalLoad(dad,'T' ,itr,ddf,nc,DataIn);
    TT = LocalLoad(dad,'TT',itr,ddf,nc,DataIn);
    T  = GraphixAverage(T ,fln,avg,months,ddf,Dim);
    TT = GraphixAverage(TT,fln,avg,months,ddf,Dim);
    data = TT-T.^2;
    if isequal(fln,'ActTstd')
        pres = NaN.*zeros(size(data));
        for iz=1:length(ZC), pres(:,:,iz)=ZC(iz); end
        Exner = (pres./presrefA).^(RdA/cpA);
        data=data.*Exner.^2;
    end
    [data,xax,yax,pltslc] = ...
        GraphixSlice(data,fln,trl,dat,dad,grd,itr,tst,flu,ddf,gdf,avg,slc,pst,...
                  Dim,LoadGridData,GridSuffix,ZcordFile,Vector,PlotFld);
elseif isequal(fln,'KEpri')
    U  = LocalLoad(dad,'uVel',itr,ddf,nc,DataIn);
    V  = LocalLoad(dad,'vVel',itr,ddf,nc,DataIn);
    UU = LocalLoad(dad,'UU'  ,itr,ddf,nc,DataIn);
    VV = LocalLoad(dad,'VV'  ,itr,ddf,nc,DataIn);
    U = U(1:dim(1),1:dim(2),:,:); UU = UU(1:dim(1),1:dim(2),:,:);
    V = V(1:dim(1),1:dim(2),:,:); VV = VV(1:dim(1),1:dim(2),:,:);
    U  = GraphixAverage(U ,fln,avg,months,ddf,Dim);
    V  = GraphixAverage(V ,fln,avg,months,ddf,Dim);
    UU = GraphixAverage(UU,fln,avg,months,ddf,Dim);
    VV = GraphixAverage(VV,fln,avg,months,ddf,Dim);
	u_dim = size(U); nz=prod(u_dim(3:end));
	U  = reshape(U ,[6*nc nc nz]);
	V  = reshape(V ,[6*nc nc nz]);
    UU = reshape(UU,[6*nc nc nz]);
	VV = reshape(VV,[6*nc nc nz]);
    [U_spl ,V_spl ] = split_UV_cub(U ,V );
    [UU_spl,VV_spl] = split_UV_cub(UU,VV);
	U_spl  = reshape(U_spl ,[nc+1 nc nz 6]);
	V_spl  = reshape(V_spl ,[nc nc+1 nz 6]);
	UU_spl = reshape(UU_spl,[nc+1 nc nz 6]);
	VV_spl = reshape(VV_spl,[nc nc+1 nz 6]);
	U_spl  = (U_spl(1:nc,:,:,:)  +  U_spl(2:nc+1,:,:,:))./2;
	V_spl  = (V_spl(:,1:nc,:,:)  +  V_spl(:,2:nc+1,:,:))./2;   
	UU_spl = (UU_spl(1:nc,:,:,:) + UU_spl(2:nc+1,:,:,:))./2;
	VV_spl = (VV_spl(:,1:nc,:,:) + VV_spl(:,2:nc+1,:,:))./2;
	U  = reshape(permute(U_spl ,[1 4 2 3]),[6*nc nc nz]);
	V  = reshape(permute(V_spl ,[1 4 2 3]),[6*nc nc nz]);
	UU = reshape(permute(UU_spl,[1 4 2 3]),[6*nc nc nz]);
	VV = reshape(permute(VV_spl,[1 4 2 3]),[6*nc nc nz]);
    data = sqrt((UU + VV)-(U.*U + V.*V));
    [data,xax,yax,pltslc] = ...
        GraphixSlice(data,fln,trl,dat,dad,grd,itr,tst,flu,ddf,gdf,avg,slc,pst,...
                  Dim,LoadGridData,GridSuffix,ZcordFile,Vector,PlotFld);

           
% General reader.
else
    if isequal(Vector,0)
        if isempty(Index)
            if     isequal(ddf,'MNC'), Index = {'+',fln};
            elseif isequal(ddf,'MDS'), Index = {'+', ''}; end
        end
        for ii = 1:2:length(Index)
            if isequal(ddf,'MNC')
                d = LocalLoad([dad,fil,'.*'],Index{ii+1},itr,ddf,nc,DataIn);
            elseif isequal(ddf,'MDS')
                d = LocalLoad([dad,fil],fln,itr,ddf,nc,DataIn);
                if ~isequal(Index{ii+1},'')
                    if     isequal(Dim,2)
                        d=squeeze(d(:,:,Index{ii+1},:));            
                    elseif isequal(Dim,3)
                        d=squeeze(d(:,:,:,Index{ii+1},:));
                    end
                end
            end
            if ii == 1, eval(['data = '     ,Index{ii},' d;']);
            else,       eval(['data = data ',Index{ii},' d;']); end
        end
        GraphixDebug_Local(GraphixDebug,'data','load',data);
        data = GraphixAverage(data,fln,avg,months,ddf,Dim);
        GraphixDebug_Local(GraphixDebug,'data','average',data);
        if ThetaToActT | ThetaToThetaEc
            pres = NaN.*zeros(size(data));
            for iz=1:length(ZC), pres(:,:,iz)=ZC(iz); end
            temp=data.*(pres./presrefA).^(RdA/cpA);
            data=temp;
        end
        if ThetaToThetaEc
        	es=A_CC.*exp(Beta_CC.*(temp-K2C));
            qstar=(RdA/RvA).*(es./pres);
            data=theta.*exp(LHvapA.*qstar./cpA./temp);
        end
    elseif ~isempty(find([1,2]==Vector))
        if isequal(ddf,'MDS')
            data = LocalLoad([dad,fil,'.*'],fln ,itr,ddf,nc,DataIn);
            if     isequal(Dim,2), data1 = squeeze(data(:,:,Index,:));
                                   data2 = squeeze(data(:,:, Mate,:));
            elseif isequal(Dim,3), data1 = squeeze(data(:,:,:,Index,:));
                                   data2 = squeeze(data(:,:,:, Mate,:)); end
        elseif isequal(ddf,'MNC')
            data1 = LocalLoad([dad,fil,'.*'],fln ,itr,ddf,nc,DataIn);
            data2 = LocalLoad([dad,fil,'.*'],Mate,itr,ddf,nc,DataIn);
        end
        if     isequal(Vector,1), U = data1; V = data2;
        elseif isequal(Vector,2), U = data2; V = data1; end
        GraphixDebug_Local(GraphixDebug,'U-dir vector','load',U);
        GraphixDebug_Local(GraphixDebug,'V-dir vector','load',V);
        U = GraphixAverage(U,fln,avg,months,ddf,Dim);
        V = GraphixAverage(V,fln,avg,months,ddf,Dim);
        GraphixDebug_Local(GraphixDebug,'U-dir vector','average',U);
        GraphixDebug_Local(GraphixDebug,'V-dir vector','average',V);
        [uE,vN] = cubeuv2uvEN_C(U,V,AngleCS,AngleSN,XG,YG);
        %[U,V]=uvcube2latlon(XC,YC,U,V,XL,YL);
        if     isequal(Vector,1), data = uE;
        elseif isequal(Vector,2), data = vN; end
        GraphixDebug_Local(GraphixDebug,'data','vector manipulation',data);
    end
    
    [data,xax,yax,pltslc] = ...
        GraphixSlice(data,fln,trl,dat,dad,grd,itr,tst,flu,ddf,gdf,avg,...
                     slc,pst,Dim,LoadGridData,GridSuffix,ZcordFile,...
                     Vector,PlotFld);
    GraphixDebug_Local(GraphixDebug,'data','slice',data);
    GraphixDebug_Local(GraphixDebug,'xax' ,'slice',xax);
    GraphixDebug_Local(GraphixDebug,'yax' ,'slice',yax);
end

%-------------------------------------------------------------------------%
%                            Local functions                              %
%-------------------------------------------------------------------------%

% Thus is merely a local function that calls the load data functions
% according to the DataFormat for the data specified by dfm.  The (some-
% times length) load calls can be avoided by directly passing the desired
% data matrix with the 'DataIn' agruement.
function data = LocalLoad(fil,fln,itr,dfm,nc,DataIn)
    if isempty(DataIn)
        if isempty(dir([fil,'*'])), ls([fil,'*']); end
        if isequal(dfm,'MDS'),     data = rdmds(fil,itr);
        elseif isequal(dfm,'MNC'), data = rdmnc(fil,fln,itr);
        else error(['Unrecognized data type:  ',dfm]); end
    else
        data = DataIn;
    end
    if isequal(dfm,'MNC')
        eval(['data = data.',fln,';']);
        data = data(1:6.*nc,1:nc,:,:);
    end

function GraphixDebug_Local(GraphixDebug,arrayname,operation,data)
	if GraphixDebug
        disp(['  Debug -- ''',arrayname,''' size after',...
              ' ',operation,':  ',mat2str(size(data))]);
    end
