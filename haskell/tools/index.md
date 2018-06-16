# Haskell の開発環境についてのメモ

## Stack

[Stack](https://docs.haskellstack.org/) は，簡単に言うと Haskell のプロジェクト用ビルドツールです．

Stack では，従来 Haskell での開発の際にあった（らしい）パッケージの依存関係がすぐぶっ壊れる問題を解決しているため，現在こいつをって開発を行うのが一般的っぽいです．


### Installing

Unix 系 OS であれば次のどちらかを打ちます．

```
$ curl -sSL https://get.haskellstack.org/ | sh
```
```
$ wget -qO- https://get.haskellstack.org/ | sh
```

Windows の場合は [インストーラー](https://get.haskellstack.org/stable/windows-x86_64-installer.exe) が用意されています．


### よく使うコマンド

- プロジェクトの新規作成（雛形を使う）
```
$ stack new <PROJECT NAME>
```
この雛形には `package.yaml` と `stack.yaml` という設定ファイルが含まれています．`package.yaml` には依存パッケージやプロジェクト全体の設定やバージョン等の情報，`stack.yaml` には GHC のバージョン等を記述します．

- プロジェクトの新規作成（雛形を使わない）
```
$ stack init <PROJECT NAME>
```

- プロジェクト指定のパッケージやコンパイラをインストール
```
$ stack setup
```

- ツールのインストール（後述の HLint や stylish-haskell とか）
```
$ stack install <TOOL NAME>
```
インストールされたコンパイラやパッケージ等は，`~/.stack` 以下に格納されます．いざとなればこれを丸ごとふっ飛ばせば，リセットできます．

- プロジェクトのビルド
```
$ stack build
```

- プロジェクトの実行
```
$ stack exec <EXECUTABLE>
```

- REPL の起動
```
$ stack ghci
```

- Stack の更新
```
$ stack update
```

[](https://haskell.e-bigmoon.com/stack/)

## Emacs

### Intero for Emacs

Flycheck や補完や REPL 等，最低限必要な機能がこれ一つで一通り揃います．Stack との相性も良いです．詳しくは [公式](https://commercialhaskell.github.io/intero/) を参照．

#### Installing

[公式](https://commercialhaskell.github.io/intero/) に従います．

MELPA を登録していない場合は `init.el` 等に次のように記述します．

```lisp
(require 'package)
(add-to-list
  'package-archives
  '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)
```

`init.el` とかに以下を記述しておくか，或いは普通にパッケージをインストールできます．

```lisp
(package-install 'intero)
```

`haskell-mode` に hook しておきます．
```lisp
(add-hook 'haskell-mode-hook 'intero-mode)
```

#### Using

上のようにしておけば，`.hs` ファイル等を開いた時に勝手に Intero が起動してくれます．

よく使うやつ：
 - `intero-repl-load` で，REPL で現在開いている module を load してくれます．デフォルトでは `C-c C-l` に当てられてます．

これに加えて，Flycheck 表示用の window を同時に開くコマンドを定義しています．

```lisp
(defun intero-repl-and-flycheck ()
  (interactive)
  (delete-other-windows)
  (flycheck-list-errors)
  (intero-repl-load)
  (split-window-below)
  (other-window 1)
  (switch-to-buffer flycheck-error-list-buffer)
  (other-window 1)
  (windmove-right)
  )
```

`C-c C-p` に当てときます．

```lisp
(add-hook 'haskell-mode-hook
          '(lambda ()
             (define-key intero-mode-map (kbd "C-c C-p") 'intero-repl-and-flycheck)))
```


### HLint

[HLint](https://github.com/ndmitchell/hlint) は Haskell 用の lint ツール（ソースコード検査ツール）です．

不要な記述や推奨されていない関数や言語拡張を指摘してくれたり，「アンタこう書いてるけど，こんな関数があるから使うとええでｗ」みたいなことを教えてくれます．

#### Installing

```
$ stack install hlint
```
でおｋ．

#### Using

基本的な使い方に関しては [こちら](https://haskell.e-bigmoon.com/posts/2018-01-29-awesome-hlint.html) がとても参考になります．

Emacs 上では [flycheck](http://www.flycheck.org/) で利用するので，まずはこれをインストールしておきます．

その上で，Intero と共存する形で HLint を使用するため，次のように記述します．

```lisp
(flycheck-add-next-checker 'intero '(warning . haskell-hlint))
```

あとは勝手に色々教えてくれるので，とても便利．


### stylish-haskell

[stylish-haskell](https://github.com/jaspervdj/stylish-haskell) は，ソースコードを自動的にいい感じに整形してくれる（`import` や `case` 文を揃えてくれる）ツールです．

#### Intalling

```
$ stack install stylish-haskell
```

#### Using

save 時に勝手に整形してくれるようにします．次のように記述します．

```lisp
(custom-set-variables '(haskell-stylish-on-save t))
```

