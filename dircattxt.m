function results=dircattxt(fileind)

%%%%�@�J�����g�f�B���N�g������fileind�Ō������C�Y���e�L�X�g�t�@�C���𐂒������ɘA����GUI�ŕۑ� %%%%

list=dir(['' fileind '.txt']);

for n = 1 : size(list, 1) ;
    fileName = list(n).name;
    results_block{n, 1} = dlmread(fileName);
end
results = [results_block{1}];
for n = 2 : size(list, 1);
    results =vertcat(results, [results_block{n}]);
end

% GUI input%
%Prompt='Save as';
%Title='Input';
%fileName =inputdlg(Prompt, Title);
%dlmwrite(['' fileName{1} '.txt'], results, 'precision', 6);
end