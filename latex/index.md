# LaTeX

LaTeX 関連のツールに関してのメモ．

## <a name="mk"> Latexmk </a>

LaTeX 関連のコマンドをいい感じにまとめてくれてるやつです．

BiBTeX とか，何回もタイプセットしなきゃいけないのをまとめて自動的にやってくれたり，勝手に PDF を生成してくれたりします．

TeXLive なんかには既に入ってると思います．

参考：[Latexmk - TeX Wiki](https://texwiki.texjp.org/?Latexmk)

参考：[latexmk で楽々 TeX タイプセットの薦め（＆ biblatex+biberで先進的な参考文献処理） - konn-san.com](https://konn-san.com/prog/why-not-latexmk.html)

### Usage
```
latexmk [<options>] hoge.tex
```

### Options
- `-pv`  : PDF を生成して，それをビューワで開く．
- `-pvc` : `-pv` してさらに `hoge.tex` が更新されたら勝手に再コンパイル．
- `-c`   : 中間ファイル（`.dvi` `.pdf` 除く）の削除を行う．
- `-C`   : 中間ファイル（`.dvi` `.pdf` 含む）の削除を行う．


### Settings

`~./latexmkrc`
```perl
#!/usr/bin/env perl
$latex         = 'uplatex -synctex=1 -halt-on-error';
$latex_silent  = 'uplatex -synctex=1 -halt-on-error -interaction=batchmode';
$bibtex        = 'upbibtex';
$dvipdf        = 'dvipdfmx %O -o %D %S';
$makeindex     = 'mendex %O -o %D %S';
$max_repeat    = 5;
$pdf_mode      = 3;
$pdf_previewer = "evince";
$pvc_view_file_via_temporary = 0;
```

基本的には使うコマンドを指定するだけなので，好きなものを書きます．

なお，  `uplatex` は，`platex` の Unicode 化です．
```tex
\documentclass[uplatex]{jsarticle}
```
とか書くと使えます．[こちら](https://qiita.com/zr_tex8r/items/5c14042078b20edbfb07) が参考になります．

また， `-halt-on-error` `-interaction=batchmode` により，途中のエラーを無視したり，`-synctex=1` で後述の [SyncTeX](#sync) を有効にしたりしてます．

## <a name="yr"> YaTeX + RefTeX </a>

LaTeX 執筆に不可欠な Emacs のモードです．

色々と便利なコマンドを提供してくれたり，括弧の自動補完とかしてくれます．

#### Settings

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

#### （個人的に）よく使うコマンド

- `C-c C-t j` : コンパイル（`tex-command` で，デフォルトでは `platex` してくれる）
- `C-c C-t d` : PDF の生成（デフォルトでは `platex` + `dvipdfmx` してくれる）
  - ↑これらは実行時に `.tex` の Save も行う．
- `C-c C-b SPC` : `\begin{hoge}` `\end{hoge}` を作ってくれる


## <a name="rm"> TeXrm </a>

LaTeX の中間ファイルたちをまとめて片付けるために書いたスクリプトです．

ちゃんと誰かが作ってる `latexmk -c` の方がいいかも．

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

# help で呼び出すやつ
function usage {
    cat <<EOF
Usage:
    texrm [<options>]

Options:
    -y, --yes    : Not ask whether to remove.
    -e, --except : Exclude some files by extension.
    -v, --version: TeXrm version.
    -h, --help   : Help for TeXrm.
EOF
}

function fsort {
org_ifs=$IFS # デリミタを一時的に定義しなおす
IFS=$'\n'
files=($(echo "${files[*]}" | sort -n)) # 配列を辞書順ソート
IFS=$org_ifs
}

# 削除を確認してから消す関数
function askremove {
    for j in ${files[@]}; do # 対象ファイルの表示
        echo "${j}"
    done
    echo "Remove these files? [Y/n]" # 削除実行の確認
    read ANS
    case $ANS in
        "" | Y | y | yes | Yes | YES )
            for i in ${files[@]}; do
                rm -rf "${i}"
            done
            echo "Removed!" ;;
        * )
            echo "Canceled." ;;
    esac
    exit 0
}

# 対象ファイルを探して配列化
files=()
for i in ${targets[@]}; do
    files+=(`find -maxdepth 1 -name "${i}"`) # カレントディレクトリ直下のファイルのみ対象
done

# 実行部
if [ "${#files[@]}" -eq 0 ]; then # ファイルの有無を確認
    echo "No such files."
    exit 0
else
    case $ARG in
        "" )
            fsort
            askremove 
            exit 0 ;;
        -Y | -y | -yes | -Yes | -YES ) # 削除の確認をしないやつ
            fsort
            for j in ${files[@]}; do
                echo "${j}"
            done
            for i in ${files[@]}; do
                rm -rf "${i}"
            done
            echo "These files have been Removed!"
            exit 0 ;;
        -e | --except ) # 削除しない拡張子を指定できる
            echo "Please enter the extension to exclude."
            read EXT
            files=()
            for i in ${targets[@]}; do
                if [[ $i != *.$EXT ]]; then
                    files+=(`find -maxdepth 1 -name "${i}"`)
                else
                    :
                fi
            done
            fsort
            askremove
            exit 0 ;;
        -h | --help )
            usage
            exit 0 ;;
        -v | --version )
            echo "TeXrm: Version 1.0.0"
            exit 0 ;;
        * )
            echo "TeXrm: Error; Bad Option." 1>&2
            echo ""
            echo "Use:"
            echo "    texrm -h"
            exit 1
    esac
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

## <a name="sync"> SyncTeX </a>

TeX のソースファイルと PDF を相互にジャンプできるようにするやつです．

#### Usage

タイプセットコマンドに `-synctex=1` とか書くだけで使えます（[Latexmk](#mk) の設定に書いてあげてます）．

PDF から飛ぶときは，`Ctrl + 左クリック` でいけます．

詳しくは [SyncTeX - TeX Wiki](https://texwiki.texjp.org/?SyncTeX) で．

#### Settings

とはいえ，エディタやビューワの設定が必要です．

Emacs と Evince の設定を載せます．

##### Settings for Evince
`path の通った場所/fwdevince`

参考：[Evince/fwdevince/Python - TeX Wiki](https://texwiki.texjp.org/?Evince%2Ffwdevince%2FPython)

```python
#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from gi.repository import GLib, Gio
import dbus
import argparse
import pathlib
import traceback
import urllib.parse

class EvinceForwardSearch:
    def parse_args(self):
        parser = argparse.ArgumentParser(description='Forward search with Evince')
        parser.add_argument('pdf', nargs=1, help='PDF file')
        parser.add_argument('line', nargs=1, type=int, help='Line')
        parser.add_argument('tex', nargs=1, help='TeX file')
        return parser.parse_args()

    def run(self):
        args = self.parse_args()
        pdf = str(pathlib.Path(args.pdf[0]).resolve()).replace(" ", "%20")
        line = int(args.line[0])
        tex = str(pathlib.Path(args.tex[0]).resolve().parent / pathlib.Path('./') / pathlib.Path(args.tex[0]).resolve().name)

        try:
            bus = Gio.bus_get_sync(Gio.BusType.SESSION, None)
            daemon = Gio.DBusProxy.new_sync(bus, 0, None, 'org.gnome.evince.Daemon', '/org/gnome/evince/Daemon', 'org.gnome.evince.Daemon', None)
            dbus_name = daemon.call_sync('FindDocument', GLib.Variant('(sb)', ('file://' + urllib.parse.quote(pdf, safe="%/:=&?~#+!$,;'@()*[]"), True)), Gio.DBusSignalFlags.NONE, -1, None)
            dbus_name = dbus_name.unpack()[0]
            window = Gio.DBusProxy.new_sync(bus, 0, None, dbus_name, '/org/gnome/evince/Window/0', 'org.gnome.evince.Window', None)
            window.call_sync('SyncView', GLib.Variant('(s(ii)u)', (tex, (line, 1), 0)), Gio.DBusSignalFlags.NONE, -1, None)
        except Exception:
            traceback.print_exc()

class EvinceInverseSearch:
    def parse_args(self):
        parser = argparse.ArgumentParser(description='Inverse search with Evince')
        parser.add_argument('pdf', nargs=1, help='PDF file')
        parser.add_argument('editor', nargs=1, help='Editor command')
        return parser.parse_args()

    def run(self):
        args = self.parse_args()
        pdf = str(pathlib.Path(args.pdf[0]).resolve())
        editor = args.editor[0]
        import dbus.mainloop.glib
        dbus.mainloop.glib.DBusGMainLoop(set_as_default=True)
        a = EvinceWindowProxy('file://' + pdf, editor, True)
        GLib.MainLoop().run()

class EvinceWindowProxy:
    """A Dbus proxy for an Evince Window."""
    daemon = None
    bus = None

    RUNNING = range(2)
    CLOSED = range(2)
    EV_DAEMON_PATH = '/org/gnome/evince/Daemon'
    EV_DAEMON_NAME = 'org.gnome.evince.Daemon'
    EV_DAEMON_IFACE = 'org.gnome.evince.Daemon'

    EVINCE_PATH = '/org/gnome/evince/Evince'
    EVINCE_IFACE = 'org.gnome.evince.Application'

    EV_WINDOW_IFACE = 'org.gnome.evince.Window'

    def __init__(self, uri, editor, spawn=False, logger=None):
        self._log = logger
        self.uri = uri.replace(" ", "%20")
        self.editor = editor
        self.status = self.CLOSED
        self.source_handler = None
        self.dbus_name = ''
        self._handler = None
        try:
            if EvinceWindowProxy.bus is None:
                EvinceWindowProxy.bus = dbus.SessionBus()

            if EvinceWindowProxy.daemon is None:
                EvinceWindowProxy.daemon = EvinceWindowProxy.bus.get_object(self.EV_DAEMON_NAME,
                    self.EV_DAEMON_PATH,
                    follow_name_owner_changes=True)
            EvinceWindowProxy.bus.add_signal_receiver(self._on_doc_loaded,
                signal_name='DocumentLoaded',
                dbus_interface=self.EV_WINDOW_IFACE,
                sender_keyword='sender')
            self._get_dbus_name(False)
        except dbus.DBusException:
            traceback.print_exc()
            if self._log:
                self._log.debug('Could not connect to the Evince Daemon')

    def _on_doc_loaded(self, uri, **keyargs):
        if uri == self.uri and self._handler is None:
            self.handle_find_document_reply(keyargs['sender'])

    def _get_dbus_name(self, spawn):
        EvinceWindowProxy.daemon.FindDocument(self.uri, spawn,
                     reply_handler=self.handle_find_document_reply,
                     error_handler=self.handle_find_document_error,
                     dbus_interface = self.EV_DAEMON_IFACE)

    def handle_find_document_error(self, error):
        if self._log:
            self._log.debug('FindDocument DBus call has failed')

    def handle_find_document_reply(self, evince_name):
        if self._handler is not None:
            handler = self._handler
        else:
            handler = self.handle_get_window_list_reply
        if evince_name != '':
            self.dbus_name = evince_name
            self.status = self.RUNNING
            self.evince = EvinceWindowProxy.bus.get_object(self.dbus_name, self.EVINCE_PATH)
            self.evince.GetWindowList(dbus_interface = self.EVINCE_IFACE,
                          reply_handler = handler,
                          error_handler = self.handle_get_window_list_error)

    def handle_get_window_list_error (self, e):
        if self._log:
            self._log.debug("GetWindowList DBus call has failed")

    def handle_get_window_list_reply (self, window_list):
        if len(window_list) > 0:
            window_obj = EvinceWindowProxy.bus.get_object(self.dbus_name, window_list[0])
            self.window = dbus.Interface(window_obj,self.EV_WINDOW_IFACE)
            self.window.connect_to_signal("SyncSource", self.on_sync_source)
        else:
            #That should never happen.
            if self._log:
                self._log.debug("GetWindowList returned empty list")

    def on_sync_source(self, input_file, source_link, timestamp):
        import subprocess
        import re
        print(input_file + ':' + str(source_link[0]))
        input_file = input_file.replace("%20", " ")
        input_file = urllib.parse.unquote(input_file)
        print(type(input_file), input_file)
        cmd = re.sub("%f", input_file.replace('file://', ''), self.editor)
        cmd = re.sub("%l", str(source_link[0]), cmd)
        print(cmd)
        subprocess.run(cmd, shell=True)
        if self.source_handler is not None:
            self.source_handler(input_file, source_link, timestamp)

if __name__ == '__main__':
    import sys
    cmd = pathlib.Path(sys.argv[0]).name
    if cmd == 'fwdevince' or cmd == 'evince_forward_search':
        EvinceForwardSearch().run()
    elif cmd == 'invevince' or cmd == 'bwdevince' or cmd == 'evince_inverse_search' or cmd == 'evince_backward_search':
        EvinceInverseSearch().run()
    else:
        sys.stderr.write("rename 'fwdevince' or 'invevince'\n")
        sys.exit(1)
```

##### Settings for Emacs
`init.el`

参考：[SyncTeXの設定。 Emacs(YaTeX) + Evince - うぶつん](https://ubutun.blogspot.jp/2012/05/synctex-emacsyatex-evince.html)

```lisp
;; SyncTeX
(defun evince-forward-search ()
  (interactive)
  (let* ((ctf (buffer-name))
         (mtf)
         (pf)
         (ln (format "%d" (line-number-at-pos)))
         (cmd "fwdevince")
         (args))
    (if (YaTeX-main-file-p)
        (setq mtf (buffer-name))
      (progn
        (if (equal YaTeX-parent-file nil)
            (save-excursion
              (YaTeX-visit-main t)))
        (setq mtf YaTeX-parent-file)))
    (setq pf (concat (car (split-string mtf "\\.")) ".pdf"))
    (setq args (concat pf " " ln " " ctf))
    (message (concat cmd " " args))
    (process-kill-without-query
     (start-process-shell-command "fwdevince" nil cmd args))))

(add-hook 'yatex-mode-hook
          '(lambda ()
             (define-key YaTeX-mode-map (kbd "C-c e") 'evince-forward-search)))

(require 'dbus)

(defun un-urlify (fname-or-url)
  "A trivial function that replaces a prefix of file:/// with just /."
  (if (string= (substring fname-or-url 0 8) "file:///")
      (substring fname-or-url 7)
    fname-or-url))

(defun th-evince-sync (file linecol &rest ignored)
  (let* ((fname (un-urlify file))
 ;        (buf (find-buffer-visiting fname))
         (buf (find-file fname))
         (line (car linecol))
         (col (cadr linecol)))
    (if (null buf)
        (message "[Synctex]: %s is not opened..." fname)
      (switch-to-buffer buf)
      (goto-line (car linecol))
      (unless (= col -1)
        (move-to-column col)))))

(defvar *dbus-evince-signal* nil)

(defun enable-evince-sync ()
  (require 'dbus)
  (when (and
         (eq window-system 'x)
         (fboundp 'dbus-register-signal))
    (unless *dbus-evince-signal*
      (setf *dbus-evince-signal*
            (dbus-register-signal
             :session nil "/org/gnome/evince/Window/0"
             "org.gnome.evince.Window" "SyncSource"
             'th-evince-sync)))))

(add-hook 'yatex-mode-hook 'enable-evince-sync)
```