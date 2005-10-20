function varcell = ReadVariables(filename)

% Function: ReadVariables
% Author:   Daniel Enderton
%
% Input Fields:
%
%   Field        Type        (Brief) Description
%   -----------------------------------------------------------------------
%   filename     string      File name to parse for variables.
%
% Output Fields:
%
%   Field        Type        (Brief) Description
%   -----------------------------------------------------------------------
%   varcell      cell array  List of variables in file.
%
% Descripton:
%   This function takes a text file (likely Matlab .m file) and parses it,
%   returning a cell array of all variables in the file where variables are
%   defined by <variable_name> = <variable_data>.  If a line has no equal
%   signs in it, it will be ignored.

varcell = {};
ivar = 1;
fid = fopen(filename,'r');
EndFile = 0;
while ~EndFile
    line = fgetl(fid);
    if ~ischar(line)
        EndFile = 1;
    else
        line = strrep(line,' ','');
        if length(line) ~= 0
            if ~isequal(line(1),'%')
                variablename = line(1:find(line == '=')-1);
                if length(variablename) > 0
                    varcell{ivar} = variablename;
                    ivar = ivar+1;
                end
            end    
        end
    end
end
fclose(fid);