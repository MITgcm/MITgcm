function data = GraphixAverage(data,fln,avg,absmonths,ddf,Dim);

% Function: GraphixAverage
% Author:   Daniel Enderton
%
% Input Fields:
%
%   Field       Type        (Brief) Description
%   -----------------------------------------------------------------------
%   data        array       Data
%   fln         string      Field name
%   avg         string      Averaging scheme ('Ann','DJF', 'JJA',...)
%   absmonths   array       Absolute months (time axis of data)
%
% Output Fields:
%
%   Field       Type        (Brief) Description
%   -----------------------------------------------------------------------
%   data        array       Averaged data.
%
% Descripton:
%   This function averages the data.  The field to average is defined by
%   'fln', and the loaded, pre-averaged data is 'data'.  The field
%   'absmonths' is the time axis given in months (1,13,25,... are assumed
%   to be Janurary), and is returned in 'datatime' so that there can be a
%   time axis for every experiment / configuration setting.  It is
%   important to remember that this function assumed monthly data!

% Load parameters (here only general parameters).
GraphixGenParam;

% Calcuate months of year.
months = mod(absmonths-1,12)+1;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                       Take monthly average of data                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Take monthly average of data.  The result has the same dimensions of the
% time-independent field (2D or 3D), and the last dimension is the time,
% which has a length of 12, running from 1 to 12, representing January
% through December respectively.  If there is no data for a months, that
% months values are set to NaN.
if isequal(avg,'Non')
    temp = data;
elseif isequal(avg,'Avg')
    if     isequal(Dim,2), temp = meanovernan(data,3);
    elseif isequal(Dim,3), temp = meanovernan(data,4); end
elseif isequal(avg,'Tse')
    temp = data;
else
	for imon = 1:12
        if isequal(Dim,2)
            if isempty(find(months == imon))
                dataMonAvg(:,:,imon) = NaN * zeros(size(data(:,:,1)));
            else
                inds=find(months==imon);
                dataMonAvg(:,:,imon) = meanovernan(data(:,:,absmonths(inds)),3);
            end
        elseif isequal(Dim,3)
            if isempty(find(months == imon))
                dataMonAvg(:,:,:,imon) = NaN * zeros(size(data(:,:,:,1)));
            else
                inds=find(months==imon);
                dataMonAvg(:,:,:,imon) = meanovernan(data(:,:,:,absmonths(inds)),4);
            end
        else
            error('Field not accounted for in ''fields2D'' of ''fields3D''');
        end
	end
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                               Average data                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% If 'avg' (the averaging scheme) is time-sequential, merely return the
% data, if it is time-year, merely return the monthly averaged data.  Note
% that these options are for a contour plot of a property which is only a
% function of latitude, that is you can only use  this for a zonally
% averaged to i=# slice of a 2D field.
if ismember(avg,{'Non','Avg','Tse'})
    dummy = 1;
elseif isequal(avg,'Tyr')
    temp = dataMonAvg;
    temp(:,:,13) = temp(:,:,1);



% Average along certain month combinations, or just select a month itself.
% Here we pick the month index number(s) based on the averaging parameter,
% and then select those indecies of the monthly averaged data and take a
% mean over the time axis.
else
    if     isequal(avg,'Jan'), index = [1];
    elseif isequal(avg,'Feb'), index = [2];
    elseif isequal(avg,'Mar'), index = [3];
    elseif isequal(avg,'Apr'), index = [4];
    elseif isequal(avg,'May'), index = [5];
    elseif isequal(avg,'Jun'), index = [6];
    elseif isequal(avg,'Jul'), index = [7];
    elseif isequal(avg,'Aug'), index = [8];
    elseif isequal(avg,'Sep'), index = [9];
    elseif isequal(avg,'Oct'), index = [10];
    elseif isequal(avg,'Nov'), index = [11];
    elseif isequal(avg,'Dec'), index = [12];
    elseif isequal(avg,'DJF'), index = [12,1,2];
    elseif isequal(avg,'MAM'), index = [3,4,5];
    elseif isequal(avg,'JJA'), index = [6,7,8];
    elseif isequal(avg,'SON'), index = [9,10,11];
    elseif isequal(avg,'Ann'), index = [1:12];
    else
        error(['Unaccounted for averaging scheme:  ',avg]);
    end
    
    % Make an array 'newindex' which has all the values as in 'index', but
    % with the emission those months with no data (all NaNs).  Note that
    % sometimes the edges of the U and V velocity fields can be NaN from
    % the interpolation, so check for NaN months a few horizontal indecies
    % into the array.
    umonths = unique(months);
    newindex = umonths(ismember(umonths,index));
    
    % Cast a warning or error for all or some missing data, respectively.
    if isempty(newindex)
        error(['No data for the averaging sceheme:  ',avg,]);
    elseif length(newindex) ~= length(index)
        disp(['***Warning***  Missing data for averaging scheme:  ',avg]);
        disp(['               Month(s) with data:      ',mat2str(newindex)]);
        disp(['               Month(s) for averaging:  ',mat2str(index)]);
    end
    
    % Perform time averaging.
    if isequal(Dim,2)
        temp = meanovernan(dataMonAvg(:,:,newindex),3);
    elseif isequal(Dim,3)
        temp = meanovernan(dataMonAvg(:,:,:,newindex),4);
    end
    
end

data = temp;