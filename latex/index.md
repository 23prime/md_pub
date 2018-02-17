# LaTeX

LaTeX 関連のツールに関してのメモ．

## Latexmk

LaTeX 関連のコマンドをいい感じにまとめてくれてるやつです．

BiBTeX とか，何回もタイプセットしなきゃいけないのをまとめて自動的にやってくれたり，勝手に PDF を生成してくれたりします．

```shell
latexmk hoge.tex`
```

オプションについて．
- `-pv`  : PDF を生成して，それをビューワで開く．
- `-pvc` : `-pv` してさらに `hoge.tex` が更新されたら勝手に再コンパイル．
- `-c`   : 中間ファイル（`.dvi` `.pdf` 除く）の削除を行う．
- `-C`   : 中間ファイル（`.dvi` `.pdf` 含む）の削除を行う．

設定は次みたいな感じで書きます．

`~./latexmkrc`
```perl
#!/usr/bin/env perl
$latex         = 'platex -halt-on-error';
$latex_silent  = 'platex -halt-on-error -interaction=batchmode';
$bibtex        = 'pbibtex';
$dvipdf        = 'dvipdfmx %O -o %D %S';
$makeindex     = 'mendex %O -o %D %S';
$max_repeat    = 5;
$pdf_mode      = 3;
$pvc_view_file_via_temporary = 0;
$pdf_previewer = "evince";
```

基本的には使うコマンドを指定するだけなので，好きなものを書きます．

`-halt-on-error` `-interaction=batchmode`により，途中のエラーを無視したりしてます．


## YaTeX + RefTeX

LaTeX 執筆に不可欠な Emacs のモードです．

設定だけ．

`init.el`
```lisp
;; YaTeX + RefTeX
(setq auto-mode-alist
      (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons (cons "\\.sty$" 'yatex-mode) auto-mode-alist))
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq YaTeX-open-lines 0)
(setq YaTeX-kanji-code nil)
(setq tex-command "latexmk -pvc")
(setq dviprint-from-format "-p %b")
(setq dviprint-to-format "-l %e")
(setq dviprint-command-format "dvips %f %t %s | lpr")
(add-hook 'yatex-mode-hook'(lambda ()(setq auto-fill-function nil)))
(add-hook 'latex-mode-hook 'turn-on-reftex)
(add-hook 'yatex-mode-hook 'turn-on-reftex)
```

`(setq tex-command "latexmk -pvc")` で `C-c C-t j` から `latexmk -pvc` を呼べるようにしてます．

## TeXrm

LaTeX の中間ファイルたちをまとめて片付けるために書いたスクリプトです．

`latexmk -c` の方がちゃんとした人が作ってるのでいいかも．

`texrm`
```shell
#!/bin/bash

ARG=$1

# 削除対象ファイルの指定
targets=(
    *.dvi
    *.aux
    *.log
    *.out
    *.nav
    *.toc
    *.snm
    *.vrb
    *.loa
    texput*
    *.fdb_latexmk
    *.fls
    *.gz
)

# 対象ファイルの探索して配列化
files=()
for i in ${targets[@]}; do
    files+=(`find -maxdepth 1 -name "${i}"`)
done

# 実行部
if [ "${#files[@]}" -eq 0 ]; then # ファイルの有無を確認
    echo "No such files."
    exit 1
elif [ $# -eq 0 ]; then  # 確認してから実行
    for j in ${files[@]}; do # 対象ファイルの表示
        echo "${j}"
    done
    echo "Remove these files? [Y/n]" # 削除実行の確認
    read ANS
    case $ANS in
        "" | "Y" | "y" | "yes" | "Yes" | "YES" )
            for i in ${files[@]}; do
                rm -rf "${i}"
            done
            echo "Removed!" ;;
        * )
            echo "Canceled." ;;
    esac
    exit 0
elif [ $# -eq 1 -a $ARG = "-y" ]; then # 確認無しで実行
    for j in ${files[@]}; do
        echo "${j}"
    done
    for i in ${files[@]}; do
        rm -rf "${i}"
    done
    echo "These files have been Removed!"
    exit 0
else
    echo "Error: Invalid arguments." 1>&2
    exit 1
fi
```

一応 Emacs から使えるようにしておきます．

`init.el`
```lisp
(defun texrm ()
  (interactive)
  (shell-command-to-string "texrm -y"))
(define-key global-map (kbd "C-c d") 'texrm)
```
