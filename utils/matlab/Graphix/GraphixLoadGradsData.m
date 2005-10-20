function [data,xax,yax,zax,months,time,dim] = ...
    DiagLoadGradsData(Grads,dad,fln,DiagDebug)


% Initial assumptions about data.
ShiftData = 0;
MultFiles = 0;
format = 'NONSEQUENTIAL';


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                          Parse table file                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tablfile = [dad,'/',Grads];
file = textread(tablfile,'%s','delimiter','\n','whitespace','');

% Cycle though lines of the GRADS table file, pasring the file line-by-line
% to properly read in and format the data.  The first line of every file
% has an identifier denoting the information on that line.  The following
% blocks of code parse the information according to different identifiers.
for iline = 1:length(file)
    rem = file{iline}; tokens = {}; itoken = 0;
    while ~isempty(rem)
        [token,rem] = strtok(rem); itoken = itoken + 1;
        if ~isempty(token), tokens{itoken} = token; end
    end
    
    % Determine file(s) to read in.
    if isequal(tokens{1},'DSET')
        if isempty(findstr(tokens{2},'%'))
            datafile = [dad,tokens{2}(2:end)];
        else
            str = tokens{2}(2:end);
            alldatafile = strrep(str,'%y2','*');
            alldatafile = strrep(alldatafile,'%m2','*');
            files = ls([dad,alldatafile]);
            breaks = [0,find(isspace(files))];
            for ibreak = 1:length(breaks)-1
                datafile{ibreak} = files([breaks(ibreak)+1:breaks(ibreak+1)-1]);
            end
            MultFiles = 1;
        end
    end  
    
    % Read in x-axis information.
    if isequal(tokens{1},'XDEF')
        if ~isequal(tokens{3},'LINEAR')
            error('Unable to deal with nonlinear grads x-axis data!');
        end
        num = str2num(tokens{2});
        ini = str2num(tokens{4});
        inc = str2num(tokens{5});
        xax = [ini:inc:ini+(num-1)*inc];
        % If the x-axis is from 0 to 360, the axis information and the data
        % will later have to be shifted to a -180 to 180 axis.
        if min(xax) >= 0 && max(xax) > 180
            ShiftData = 1;
        end
        nx = length(xax);
    end
    
    % Read in y-axis information.
    if isequal(tokens{1},'YDEF')
        if ~isequal(tokens{3},'LINEAR')
            error('Unable to deal with nonlinear grads y-axis data!');
        end
        num = str2num(tokens{2});
        ini = str2num(tokens{4});
        inc = str2num(tokens{5});
        yax = [ini:inc:ini+(num-1)*inc]; ny = length(yax);
    end
    
    % Read in z-axis information.
    if isequal(tokens{1},'ZDEF')
        if isequal(tokens{2},'1')
            zax = [0]; zax_found = 1; dim = 2;
        else
            dim = 3;
            if isequal(tokens{3},'LINEAR')
                num = str2num(tokens{2});
                ini = str2num(tokens{4});
                inc = str2num(tokens{5});
                zax = [ini:inc:ini+(num-1)*inc];
            elseif isequal(tokens{3},'LEVELS')
                num = str2num(tokens{2});
                for inum = 1:num
                    zax(inum) = 100.*str2num(tokens{inum+3});
                end
            else
                error('Unknown information in z-axis data!');
            end
        end
        nz = length(zax);
    end
    
    % Read in t-axis (time) information.
    if isequal(tokens{1},'TDEF'), ini=tokens{4};
        if ~isequal(tokens{3},'LINEAR')
            error('Currently unable to deal with nonlinear grads t-axis data!');
        end
        if ~isequal(tokens{5},'1mo') & ~isequal(tokens{5},'01mo')
            % Note that monthly mean assumptions are also made when data
            % spanning multiple files are read in later on and that this
            % part must be updated if non-monthly mean data is allowed.
            error('Currently unable to deal with non monthly mean grads data!');
        end
        % Determine initial month and initial year.  This is done in an ad
        % hoc manner in that depending on the length start time string, the
        % month is pluck out.  There would be problems is there are start
        % time strings of different lengths have months in different
        % locations.  In this case the month will not be found and an error
        % will be thrown.  The year is only required for data spanning
        % multiple files, which thus fas has a start time string length of
        % 12.  If it is not found, and error of undefined yrchar will be
        % cast and the code will need to be appropriately updated.
        if     length(ini) == 13, monchar=ini(9:11);
        elseif length(ini) == 12, monchar=ini(6:8);  yrchar = ini(9:12);
        elseif length(ini) == 10, monchar=ini(6:8);
        elseif length(ini) == 7,  monchar=ini(1:3);
        else, error('Cannot parse TDEF correctly'); end
        monchar = upper(monchar);
        if     isequal(monchar,'JAN'), inimonth = 1;
        elseif isequal(monchar,'FEB'), inimonth = 2;
        elseif isequal(monchar,'MAR'), inimonth = 3;
        elseif isequal(monchar,'APR'), inimonth = 4;
        elseif isequal(monchar,'MAY'), inimonth = 5;
        elseif isequal(monchar,'JUN'), inimonth = 6;
        elseif isequal(monchar,'JUL'), inimonth = 7;
        elseif isequal(monchar,'AUG'), inimonth = 8;
        elseif isequal(monchar,'SEP'), inimonth = 9;
        elseif isequal(monchar,'OCT'), inimonth = 10;
        elseif isequal(monchar,'NOV'), inimonth = 11;
        elseif isequal(monchar,'DEC'), inimonth = 12;
        else, error('Can''t determine initial month in GRADS data!'); end
        num = str2num(tokens{2});
        % Determine num and inimonth parameters accounting for the unknown
        % number of data files present when dealing with multiple data
        % files.  Assumption of monthly mean data is made.
        if MultFiles;
            initFileFound = 0;
            for ifile = 1:length(datafile)
                initFileFound = ~isempty(findstr(yrchar,datafile{ifile})) ...
                                | initFileFound;
            end
            if ~initFileFound, inimonth = 1; end
            num = 12.*length(datafile);
        end
        % Assumption of monthly mean data is made.
        months=[inimonth:num+inimonth-1];
        time=months/12; nt = length(months);
    end
    
    % Find where the desired variable is located.
    if isequal(tokens{1},'VARS')
        nv = str2num(tokens{2});
        VARSline = iline;
    end
    if isequal(tokens{1},fln)
        FIELDline = iline;
        ivar = iline - VARSline;
    end
    
    % Find the values defonting undefined data.
    if isequal(tokens{1},'UNDEF')
        undef = str2num(tokens{2});
    end
    
    % Determine data format type.
    if isequal(tokens{1},'FORMAT')
        if isequal(tokens{2},'SEQUENTIAL') | isequal(tokens{2},'sequential')
            format = 'SEQUENTIAL';
        else
            disp(['Unrecognized grads FORMAT:  ',tokens{2}]);
        end
    end 
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                       Verification and debugging                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Verify that everything we need from the table file has been read in.
if DiagDebug, disp(['  Debug -- grads data format:  ',format]); end
parameters = {'xax' ,'yax' ,'zax' ,'time','nv'  ,'undef','ivar'};
GRADSnames = {'XDEF','YDEF','ZDEF','TDEF','VARS','UNDEF',fln   };
for ii = 1:length(parameters)
    try
        eval([parameters{ii},';']);
    catch
        error(['GRADS information not found:  ',GRADSnames{ii}]);
    end
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                              Read data file                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Read in data from a single file.
if ~MultFiles
    fid=fopen(datafile,'r','b');
    data = fread(fid,'real*4');
    fclose(fid);
	if isequal(format,'SEQUENTIAL') | isequal(format,'sequential')
        index=true([nx*ny*nz*nv*nt+2*nv*nt,1]);
        index([1:nx*ny*nz+2:end])=false;
        index([2:nx*ny*nz+2:end])=false;
        data = data(index);
	end
    data = reshape(data,[nx,ny,nz,nv,nt]);
    data = data(:,:,:,ivar,:);
    datatmp(:,:,:,:,inimonth:num+inimonth-1) = data;
    datatmp(:,:,:,:,1:inimonth-1) = NaN;
    data=datatmp;
% Read in data from multiple files.
else
    for ifile = 1:length(datafile)
        if DiagDebug, disp(['  Debug -- Reading grads ',...
                            'data file:  ',datafile{ifile}]); end
        fid=fopen(datafile{ifile},'r','b');
        datatemp = fread(fid,'real*4');
        fclose(fid);
        % Assumption of monthly mean data is made.
        if MultFiles, nt = 12; end
        if initFileFound & (inimonth ~= 1)
            error(['Currently not able to handle Grads data spanning ',...
                    'multiple files not starting in January']);
        end
        datatemp = reshape(datatemp,[nx,ny,nz,nv,nt]);
        datatemp = squeeze(datatemp(:,:,:,ivar,:));
        data(1:nx,1:ny,1:nz,nt.*(ifile-1)+1:nt.*ifile) = datatemp;
    end
end
data = squeeze(data);

% Turn undefined values into NaN.
tol = 1e-5;
data( abs((data-undef)/undef) < tol ) = NaN;

% Shift data such that it is on a -180 to 180 longitude axis.
if ShiftData
    indexWestHemi = xax>=180;
    indexEastHemi = xax<180;
    data = cat(1,data(indexWestHemi,:,:,:),data(indexEastHemi,:,:,:));
    xax = cat(2,xax(indexWestHemi)-360,xax(indexEastHemi));
end
