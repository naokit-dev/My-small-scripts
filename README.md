# My small scripts
独学で作成した実験、研究サポート用のコードを公開するrepository  実験機器制御や、データの形成、基礎統計、解析、レポート作成を自動化するプログラム
  
自身の研究課題のみでなく  チーム内で共有される処理に注目し  プロセスのに要する時間短縮、ヒューマンエラーの軽減に取り組んだ

## clip2ttest_p.R
クリップボード上のデータ（ヘッダー付き、2群の列データ）に対してT検定を実施、統計値を返すプログラム  混沌とした実験データの即時可視化、解釈-修正のプロセスを効率化するために作成  (R言語、Rスクリプト)

## randpaired_TMS.m
実験に必要な機器をTTL信号を使用して制御するプログラム  実験に必要なマンパワーを減らし、生産性の向上に貢献  (MATLAB, Mファイル: スクリプト)

## dircattxt.m
カレントディレクトリ内を任意の単語（入力引数）でワイルドカード検索し、  該当ファイル内のデータをすべて列方向に連結、別のファイルに保存する関数  従来コピーアンドペーストで繰り返されていた作業を自動化  複数の実験データをまとめて解析を行う際に生じる、ヒューマンエラーの抑制、マンパワー軽減に貢献  (MATLAB, Mファイル: 関数)

## EMGcheck.m
連続データの中にハズレ値を含む試行を検出し、そのインデックスを返す関数  膨大なデータに対して事後解析にて行っていた処理を自動化  大幅なマンパワー軽減だけでなく、データ処理の客観性、透明性向上に貢献  (MATLAB, Mファイル: 関数)

## CRT_MEP_SICI_anlyzer_sub.R
自身の実験専用のデータ解析、可視化プログラム  生データから即時に基礎統計値を含む図表を作成しPDFファイルで出力可能  混沌とした数値データを、他者と共有、コミュニケーション可能な材料に瞬時に変換可能  結果の即時解釈、小規模な修正の円滑化に貢献  (R言語、Rスクリプト)
