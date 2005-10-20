function [data,xax,yax,pltslc] = ...
    GraphixSliceGradsData(fln,flu,slc,data,xaxin,yaxin,zaxin);

% Function: GraphixSliceGradsData
% Author:   Daniel Enderton
%
% Input Fields:
%
%   Field        Type        (Brief) Description
%   -----------------------------------------------------------------------
%   slc          string      Slice type ('Zon','k=#',...)
%
% Output Fields:
%
%   Field       Type        (Brief) Description
%   -----------------------------------------------------------------------
%   data        array       Sliced data.

GraphixFieldParamA;
GraphixFieldParamO;
GraphixFieldParamC;
GraphixFieldParamI;

if isequal(slc,'Sur')
    data = data';
    xax = xaxin;
    yax = yaxin;
    pltslc='lonlat';

elseif isequal(slc,'Zon')
    data = squeeze(mean(data,1))';
    xax=yaxin;
    if isequal(length(zaxin),1)
        yax=NaN;
        pltslc='latfld';
    else
        yax=zaxin;
        pltslc='lathgt';
    end

elseif isequal(slc(1:2),'i=')
    ii = str2num(slc(3:end));
    data=squeeze(data(ii,:,:,:));
    xax=yaxin;
    if isequal(length(zaxin),1)
        yax=NaN;
        pltslc='latfld';
    else
        yax=ZC;
        pltslc='lathgt';
    end
    
elseif isequal(slc(1:2),'j=')
    jj = str2num(slc(3:end));
    data = squeeze(data(:,jj,:,:));
    if isequal(length(zaxin),1)
        yax=NaN;
        pltslc='lonfld';
    else
        yax=ZC;
        pltslc='lonhgt';
    end
    
elseif isequal(slc(1:2),'k=')
    kk = str2num(slc(3:end));
    data = squeeze(data(:,:,kk,:))';
    xax = xaxin;
    yax = yaxin;
    pltslc='lonlat';
    
else
    error(['Unrecognized SliceType:  ',slc]);
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                               Check range                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Load fixed and data ranges, throw a warning is data out of range.
datarange = [min(data(:)),max(data(:))];
try
    eval(['fixedrange = ',fln,'range',flu,';']);
    if datarange(1) < fixedrange(1) | ...
       datarange(2) > fixedrange(2)
        disp(['***Warning***  Value out of range for ',fln]);
        disp(['               Data range:  ',mat2str(datarange)]);
        disp(['               Fixed range:  ',mat2str(fixedrange)]);
    end
catch
    disp(['***Warning***  No range information found for ',fln]);
    disp(['               Data range:  ',mat2str(datarange)]);
end
