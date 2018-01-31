function [] = nc_add(nc,shortName,longName,units,grid,var,varargin)
% nc_add(nc,shortName,longName,units,grid,var)
% nc_add(nc,shortName,longName,units,grid,var,record_number)
%
% Adds variables (with data) to a NETCDF file with handle "nc"
%  nc  is the netcdf file handle
%  shortName     is the name used to refer to the variable 'theta'
%  longName      is a more descriptive name e.g. 'Potential temperature'
%  units         is the units of the variable e.g. 'Degrees Celsius'
%  grid          is a collection cell array of dimensino names e.g. {'Y' 'X'}
%  var           is the scalar/vector/array of data to be written
%  record_number is the record slot to write when there is a "recordable"
%                dimension in the grid specification
%  
% If "shortName" is identical to "grid" then the variable is written to the
% netcdf file as dimension data and can subsequently be used in other grid
% specifications. The size of the dimension is the length of the vector of
% data. To create a recordable dimension, supply an empty vector, [].
%
% e.g.
% >> nc=netcdf('test.nc','clobber');
%
% Add a dimension (use grid==shortName)
% >> nc_add(nc,'lon','Longitude','Degrees East','lon',xc)
% >> nc_add(nc,'lat','Latitude','Degrees North','lat',yc)
%
% Add a record dimension (use grid==shortName and no values)
% >> nc_add(nc,'time','Time','Years','time',[])
%
% Add a variable on a grid
% >> nc_add(nc,'H','Orography','m',{'lat' 'lon'},H)
%
% Add a variable on a grid with recordable dimension
% >> nc_add(nc,'time','Time','Years','time',1978,1)
% >> nc_add(nc,'time','Time','Years','time',1978,2)
% >> nc_add(nc,'theta','Pot. Temp','Degrees K',{'time' 'lat' 'lon'},theta,1)
% >> nc_add(nc,'theta','Pot. Temp','Degrees K',{'time' 'lat' 'lon'},theta,2)
%
% >> close(nc);
%
% Written by adcroft@mit.edu, 2004.
if strcmp(shortName,grid{1})
 nc(shortName) = length(var);
end

nc{shortName} = grid;
%nc{shortName}.uniquename = longName;
nc{shortName}.long_name = longName;
nc{shortName}.units = units;
nc{shortName}.missing_value = ncdouble(NaN);

nvargs=nargin-6;
rcn=0;
if nvargs==1
 rcn=varargin{1};
end

if ~isempty(var)
 if prod(size(var))==1
  nc{shortName}(rcn) = var;
 else
  if rcn ~= 0
   nc{shortName}(rcn,:) = permute(var,ndims(var):-1:1);
  else
   nc{shortName}(:) = permute(var,ndims(var):-1:1);
  end
 end
end

