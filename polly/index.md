# Polly

## What
Slack で簡単に投票等を行える BOT です．

## Usage

### 開始

次のコマンドを打ちます．
```
/poll "ここにタイトルを入れます"
```

もしくは，次のように打つと，後述の `Multiple Choice` が選ばれ，その選択肢が自動的に生成されます．
```
/poll "タイトル" "選択肢1" "選択肢2" "選択肢3"
```

### 設定
#### Poll Types
5種類の投票方式が用意されています．

- `0-to-10` : 0から10までの数字のどれかをプルダウンから選択する．

- `1-to-5` : 1から5の数字のどれかを選択する．

- `Agree/DisAgree` : 賛成/反対のいずれかを選択する．

- `Multiple Choice` : 複数選択肢から1つを選択する．
  - `+ Add Options` で選択肢を設定します．


- `Open Ended` : 自由記述式．

投票のタイミングを設定します．

- `One-time` : すぐに投票開始．

- `Scheduled one-time` : 投票期間を設定できる．

- `Every hoge fuga (recurring)` : 定期的に投票を行う．
 
#### Submit

- `Submit to Channel` : 現在のチャンネルへ投票を投稿する．

- `Submit to DM` : DM へ投票を投稿する．

### Options

#### Poll Options

- `One Vote Max` : 複数選択を禁止する．

- `Hide Results` : 他の人が結果を見られないようにする．

- `Anonymous` : 匿名投票にする．

- `Private Comments` : 投票者が入力するコメントを非公開にする．

- `Allow Adding Options` : 選択肢の追加を許可する．


#### Poll Audience  

投票の対象者を選択できます．