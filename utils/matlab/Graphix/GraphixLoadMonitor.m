function [data,time] = ...
    GraphixLoadMonitor(fln,mnchandle,dad,itr,tst,SecPerYear,DiagDebug);

% Read in files names.
%files = dir([dad,'/',mnchandle,'*']);
filesin=ls([dad,'/',mnchandle,'*']);
index=1;
while ~isempty(filesin)
    [token,filesin] = strtok(filesin);
    if ~isempty(filesin)
        files(index).name=token; index=index+1;
    end
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                             Read in data                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Initialize result arrays.
if isempty(itr),
    time=[];
    data=[];
else
    time=NaN.*zeros(size(itr));
    data=NaN.*zeros(size(itr));
end

for ifile = 1:length(files)
    
    % Open monitor file.
    if DiagDebug, disp(['Entering file:  ',files(ifile).name]); end
    nc=netcdf(files(ifile).name,'read');
    
    % Read time and data information.
    nciter=nc{'T'}(:);
    
    ncdata=nc{fln};
    if isempty(nciter), error('Monitor time axis not found!'); end
    if isempty(ncdata), error(['Monitor field not found:  ',fln]); end
    
    % Load desired indecies.
    if isempty(itr)
        if isempty(time),
            time=nciter(:);
            data=ncdata(:);
        else
            index=find(~ismember(nciter(:),time));
	        if ~isempty(index)
	            nciter=nciter(index); time=[time;NaN;nciter];
        	    ncdata=ncdata(index); data=[data;NaN;ncdata];
            end
        end
    else
        [test,loc]=ismember(itr,nciter(:));
        index=find(test);
        if ~isempty(index)
            time(index)=nciter(loc(index));
            data(index)=ncdata(loc(index));
        end
    end
    
    close(nc);
end

% Check for missing indecies.
% if ~isempty(find(isnan(time))) || ~isempty(find(isnan(data)))
%     error('Missing monitor data for specified indecies!');
% end

time=time./SecPerYear;
