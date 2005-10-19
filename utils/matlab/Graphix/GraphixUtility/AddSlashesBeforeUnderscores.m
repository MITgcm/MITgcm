function string = AddSlashesBeforeUnderscores(str)

index = find(str == '_');

if ~isempty(index)
	string = '';
	for ichar = 1:length(index)
        
        if ichar == 1
            i = 1;
        else
            i = index(ichar-1);
        end
        
        string = [string,str(i:index(ichar)-1),'\'];
	end
    
	string = [string,str(index(ichar):end)];
    
else
    string = str;
    
end