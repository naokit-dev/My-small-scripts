function results=dircattxt(fileind)

%%%%　カレントディレクトリ内をfileindで検索し，該当テキストファイルを垂直方向に連結しGUIで保存 %%%%

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