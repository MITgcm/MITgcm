function y = mit_getparm(fname,pname);
%function y = mit_getparm(fname,pname);
  
  y = [];
  [fp, msg] = fopen(fname,'r');
  if fp > 0
    str{1} = [];
    notfound = 0;
    while isempty(mystrfind(str{1},pname))
      str{1} = fgetl(fp); 
      if ~ischar(str{1}); % this catches the end of file (not very clean)
	notfound = 1;
	break; 
      end
      % clear again if commented
      if ~isempty(mystrfind(str{1},'#')) 
	str{1} = [];
      end
    end
    if notfound
      disp(['Warning: ' pname ' not found in ' fname])
      y=[];
%      disp(['         setting ' pname ' to zero'])
%      y = 0;
    else
      teststr = [];
      % find the termination of parameter pname 
      % (next line with an '=' sign
      % or with a namelist termination character '&' or '/')
      n = 1;
      while ( isempty(mystrfind(teststr,'=')) & ...
	      isempty(mystrfind(teststr,'&')) & ...
	      isempty(mystrfind(teststr,'/')) )
	n = n + 1;
	teststr = fgetl(fp);
	str{n}  = teststr;
	if ~ischar(teststr); break; end
      end
      eqind = findstr(str{1},'=');
      y = str{1}(eqind+1:end-1);
      % check whether it is a string in quotes or something else
      quotes = findstr(y,'''');
      if ~isempty(quotes)
	y(quotes(2):end) = [];
	y(1:quotes(1)) = [];
      else
	if ~( strcmpi(y,'.TRUE.') | strcmpi(y,'.FALSE.') )
	  y = convert_str2num(str{1}(eqind+1:end-1));
	  for k=2:n-1
	    y = [y; convert_str2num(str{k}(eqind+1:end-1))];
	  end
% $$$ 	  y = str2num(str{1}(eqind+1:end-1))';
% $$$ 	  for k=2:n-1
% $$$ 	    y = [y; str2num(str{k}(eqind+1:end-1))'];
% $$$ 	  end
	end
      end
    end
    fclose(fp);
  else
    error([fname ' could not be opened: ' msg])
  end

  return

function y = convert_str2num(str)
  
% take care of the n*value format
  stars = findstr(str,'*');
  if isempty(stars)
    y = str2num(str)';
  else
    if length(stars)==1
      y=repmat(str2num(str(stars+1:end)),[str2num(str(1:stars-1)) 1]);
    else
      warning('The format n*x1,m*x2, ... cannot be handled')
% $$$       for k=1:length(stars)
% $$$ 	% find the beginnin and termination of the set
% $$$       end
    end
  end
  
  return

function [ind] = mystrfind(str,pattern)
  
  if length(pattern) > length(str)
    ind = [];
    return
  else
    ind = findstr(str,pattern);
  end
