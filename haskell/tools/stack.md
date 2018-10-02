# Stack

[Stack](https://docs.haskellstack.org/) は，簡単に言うと Haskell のプロジェクト用ビルドツール．

Stack では，従来 Haskell での開発の際にあった（らしい）パッケージの依存関係がすぐぶっ壊れる問題を解決しているため，現在こいつをって開発を行うのが一般的．

## Installing

Unix 系 OS であれば次のどちらかを打ちます．

```
$ curl -sSL https://get.haskellstack.org/ | sh
```

```
$ wget -qO- https://get.haskellstack.org/ | sh
```

Windows の場合は [インストーラー](https://get.haskellstack.org/stable/windows-x86_64-installer.exe) が用意されている．


### よく使うコマンド

- プロジェクトの新規作成（雛形を使う）

```
$ stack new <PROJECT NAME>
```

この雛形には `package.yaml` と `stack.yaml` という設定ファイルが含まれている．`package.yaml` には依存パッケージやプロジェクト全体の設定やバージョン等の情報，`stack.yaml` には GHC のバージョン等を記述する（[後述](#settings)）．

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

  インストールされたコンパイラやパッケージ等は，`~/.stack` 以下に格納される．いざとなればこれを丸ごとふっ飛ばせば，リセットできる．

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
  $ stack upgrade
  ```


## <a name="settings">設定ファイルの書き方とか</a>

設定ファイルは

- `hoge.cabal`
- `stack.yaml`

から成ります．

最近のバージョンではデフォルトで [hpack](http://hackage.haskell.org/package/hpack) に対応しているため， cabal ファイルを直接編集する必要はあまりない．
代わりに `package.yaml` を編集する．


### グローバルな設定

個々のプロジェクトではなく，どこからでも Stack 経由で GHC を使う感じのヤツ．

- グローバルな GHC バージョンの指定
  - `~/.stack/global-project/stack.yaml` に記述．
  - 記述しないと，最新版が入る．

### 外部ライブラリを使うための記述

[標準ライブラリ](https://hackage.haskell.org/package/base) 以外のライブラリを使いたいときの設定の記述方法．

基本的に，次にあるライブラリを使うことができる．

- [Stackage](https://www.stackage.org/)

  `package.yaml` の `dependencies` に，

  ```yaml
  dependencies:
  - containers
  - text
  ```

  などと書き加えていくだけでおｋ．

- [Hackage](https://hackage.haskell.org/)

  `stack.yaml` の `extra-deps` に，使いたいライブラリを書き加える．

  ```yaml
  extra-deps:
  - hoge-3.1.4
  ```
  
  その上で，`package.yaml` の `dependencies` にも書き加える．

- [GitHub](https://github.com/)

  `stack.yaml` にソースリポジトリの情報を書きます．
  
  ```yaml
  extra-deps:
  - github: user/repository
    commit: commitID
  ```

  その上で，やはり `package.yaml` の `dependencies` にも書き加える．