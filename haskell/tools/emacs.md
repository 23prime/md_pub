# Emacs の設定について

Eamcs の Haskell に関する私の設定は[こちら](https://github.com/23prime/.emacs.d/blob/master/inits/12-haskell.el)．

## Intero for Emacs

Flycheck や補完や REPL 等，最低限必要な機能がこれ一つで一通り揃う．Stack との相性も良く，使い勝手も非常に良い．詳しくは [公式](https://commercialhaskell.github.io/intero/) を参照．

### Installing

[公式](https://commercialhaskell.github.io/intero/) に従う．

MELPA を登録していない場合は，パッケージアーカイブスのリストに追加する．

例えば，`init.el` 等に次のように記述する．

※ 私の設定は[こんな感じ](https://github.com/23prime/.emacs.d/blob/master/inits/20-package.el)．

```lisp
(require 'package)
(add-to-list
  'package-archives
  '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)
```

`init.el` とかに以下を記述しておくか，或いは普通にパッケージをインストールできる．

```lisp
(package-install 'intero)
```

`haskell-mode` に hook しておきます．

```lisp
(add-hook 'haskell-mode-hook 'intero-mode)
```

#### Usage

上のようにしておけば，`.hs` ファイル等を開いた時に勝手に Intero が起動してくれます．

よく使うやつ：

- `intero-repl-load` で，REPL で現在開いている module を load してくれる．デフォルトでは `C-c C-l` に当てられている．

これに加えて，Flycheck 表示用の window を同時に開くコマンドを定義している．

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

これを `C-c C-p` に当てておく．

```lisp
(add-hook 'haskell-mode-hook
          '(lambda ()
             (define-key intero-mode-map (kbd "C-c C-p") 'intero-repl-and-flycheck)))
```

### HLint

[HLint](https://github.com/ndmitchell/hlint) は Haskell 用の lint ツール（ソースコード検査ツール）．

不要な記述や推奨されていない関数や言語拡張を指摘してくれたり，「アンタこう書いてるけど，こんな関数があるから使うとええでｗ」みたいなことを教えてくれる．

#### Installing

```
$ stack install hlint
```

でおｋ．

#### Usage

基本的な使い方に関しては [こちら](https://haskell.e-bigmoon.com/posts/2018-01-29-awesome-hlint.html) がとても参考になる．

Emacs 上では [flycheck](http://www.flycheck.org/) で利用するため，まずはこれをインストールしておく．

その上で，Intero と共存する形で HLint を使用するため，次のように記述する．

```lisp
(flycheck-add-next-checker 'intero '(warning . haskell-hlint))
```

あとは勝手に色々教えてくれるので，とても便利．

### stylish-haskell

[stylish-haskell](https://github.com/jaspervdj/stylish-haskell) は，ソースコードを自動的にいい感じに整形してくれる（`import` や `case` 文を揃えてくれる）ツール．

#### Intalling

```
$ stack install stylish-haskell
```

#### Usage

save 時に勝手に整形してくれるよう，次のように記述する．

```lisp
(custom-set-variables '(haskell-stylish-on-save t))
```