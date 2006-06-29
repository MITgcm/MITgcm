function [data,xax,yax,pltslc] = ...
    GraphixSlice(data,fln,trl,dat,dad,grd,itr,tst,flu,ddf,gdf,avg,slc,...
                 pst,Dim,LoadGridData,GridSuffix,ZcordFile,Vector,...
                 FieldName,XL,YL);

% Function: GraphixSlice
% Author:   Daniel Enderton
%
% Input Fields:
%
%   Field        Type        (Brief) Description
%   -----------------------------------------------------------------------
%   data         array       Field name
%   fln          string      Field name
%   dad          string      Data directory.
%   grd          string      Grid data directory.
%   avg          string      Averaging scheme ('Ann','DJF', 'JJA',...)
%   slc          string      Slice type ('Zon','k=#',...)
%   pst          string      Plot style ('Con','Sur',...)
%   flu          string      Fluid ('A','O')
%   dfm          string      Data format ('MDS' or 'MNC')
%   LoadGridData 0/1         Optionally load grid data
%
% Output Fields:
%
%   Field       Type        (Brief) Description
%   -----------------------------------------------------------------------
%   data        array       Sliced data.
%
% Descripton:
%   This function slices the data according to SliceType 'slc'.  Depending
%   on the SliceType and PlotStyle, the data may be converted to a lat-lon
%   grid.  At the end there is also a range check.  If the value is out of
%   the user specified range given in 'DaigFieldParamA'.

% Load diagnostics parameters, grid data, mark data size
GraphixGenParam;
GraphixFieldParamA;
GraphixFieldParamO;
GraphixFieldParamC;
GraphixFieldParamI;
[nc,dim,XC,XG,YC,YG,ZC,ZF,RAC,drC,drF,HFacC,...
 HFacW,HFacS,dxG,dyG,dxC,dyC,AngleCS,AngleSN] = ...
    GraphixLoadGridData(LoadGridData,grd,gdf,flu,GridSuffix,ZcordFile);
datasize = size(data);


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                               Surface plot                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% All surface plots are direct cube sphere outputs, and therefore must have
% the dimensions of the cube grid data.  If the 'pst' is 'Grd' or 'Int',
% everything is already all set since those handle the cubed sphere data.
% If the 'pst' is 'Con' or 'Cnf', convert data to a lat-lon grid so that it
% can easily be contoured.
if isequal(slc,'Sur')
    if ismember(fln,{'TX','TY','USTR','VSTR'}) | ~isequal(Vector,0)
        data = data'; xax = XL; yax = YL;
    elseif ~isequal(datasize,size(XC)) 
        error('Incorrect dimensions for slc:  ',slc);
    else
        if ismember(pst,{'Grd','Int'})
            if     isequal(pst,'Grd'),
                xax = XG;
                yax = YG;
            elseif isequal(pst,'Int'),
                xax = XC;
                yax = YC;
            end
        elseif ismember(pst,{'Con','Cnf'})
            data = cube2latlon(XC,YC,data,XL,YL)';
            xax = XL; yax = YL;
        else
            error(['slc = ''Sur'' can not use pst:  ',pst]);
        end
    end
    pltslc='lonlat';
    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                            Zonal average plot                           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% When computing a zonal average, there are many different options for the
% initial data.  If the data is cube-sphere, it could be the shape of XC,
% possibly also with a vertical axis.  In either case, 'calc_ZonAv_CS' (a
% nifty zonally averaging script from JMC) is called to compute the zonal
% average for raw cube sphere data.  For horizontal velocities, the data
% could be of the size [length(XL),length(YL),z-axis length], in which case
% you just need to take the mean over the longitude axis (always 1).
elseif isequal(slc,'Zon')
    if isequal(datasize(1:2),size(XC))
        if isequal(flu,'O'),                nBas = 0; end
        if ismember(flu,{'A','I','C','L'}), nBas = 0; end
        if Dim == 2 & length(size(data)) == 3
            data = reshape(data,[size(data,1),size(data,2),1,size(data,3)]);
        end
        [data,maskzon,Ylat,areazon] = ...
            calcZonalAvgCube(data,ny,YC,RAC,HFacC);
        if isequal(avg,'Tse')
            data=data(:,:,1); xax=itr.*tst./31104000;
            yax=Ylat;         pltslc='timlat';
        elseif isequal(avg,'Tyr')
            data=data(:,:,1);  xax=[0:12];   yax=Ylat; pltslc='timlat';
        elseif isequal(pst,'Lin')
            data=data(:,:,1)'; xax=Ylat;     yax=NaN;  pltslc='latfld';
        else
            data=data(:,:,1)'; xax=Ylat;     yax=ZC;   pltslc='lathgt';
        end
    elseif isequal(datasize(1:2),[length(XL),length(YL)])
        if ~isequal(pst,'Lin')
            data = squeeze(mean(data,1))'; xax=YL; yax=ZC;  pltslc='lathgt';
        else
            data = squeeze(mean(data,1))'; xax=YL; yax=NaN; pltslc='latfld';
        end
    else
        error('Incorrect dimensions for slc = ''Zon''');
    end
    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                   i,j,k=#                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% These slicing settings just take a slice on one of the axes.  If the
% slice option is 'i=' or 'j=' and the data is cube-sphere, it is converted
% to lat-lon first and indexed then (thus the index number should be based
% off of the XL and YL interpolated grids defined in 'GraphixGenParam'.  It
% would be nice to add functions that can slice by longitude, latitude, and
% height values directly.  Note that there is transposing of all the data
% to prepare it for contour plots (harmless is line plots).
elseif isequal(slc(1:2),'i=')
    ii = str2num(slc(3:end));
    data = cube2latlon(XC,YC,data,XL,YL);
    if ismember(fln,fields2D)
        data=squeeze(data(ii,:));
        xax=YL; yax=NaN; pltslc='latfld';
    elseif ismember(fln,fields3D)
        data=squeeze(data(ii,:,:))';
        xax=YL; yax=ZC;  pltslc='lathgt';
    end
    
elseif isequal(slc(1:2),'j=')
    jj = str2num(slc(3:end));
    data = cube2latlon(XC,YC,data,XL,YL);
    if ismember(fln,fields2D)
        data = squeeze(data(:,jj));
        xax=XL; yax=NaN; pltslc='lonfld';
    elseif ismember(fln,fields3D)
        data = squeeze(data(:,jj,:))';
        xax=XL; yax=ZC;  pltslc='lonhgt';
    end
    
elseif isequal(slc(1:2),'k=')
    kk = str2num(slc(3:end));
    data = squeeze(data(:,:,kk));
    if isequal(pst,'Grd'),
        xax = XG;
        yax = YG;
    elseif isequal(pst,'Int'),
        xax = XC;
        yax = YC;
    elseif ismember(pst,{'Con','Cnf'})
        data = cube2latlon(XC,YC,data,XL,YL)';
        xax = XL; yax = YL;
    end
    pltslc='lonlat';
    
else
    error(['Unrecognized SliceType:  ',slc]);
end
