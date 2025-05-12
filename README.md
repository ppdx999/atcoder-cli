# atcli

## Install

このレポジトリをクローンしてビルドしてください

## Usage

```
atcli <サブコマンド>
```

## サブコマンド

| cmd | 説明 | Usage |
| -- | -- | -- |
| init | コンテストを初期化 | `atcli init abc100` |
| login | ログイン | `atcli login` |
| download | 問題をダウンロード | `atcli download` |
| test | テストケースを実行 | `atcli test` |
| submit | コードを提出 | `atcli submit` |

※ WSL環境でブラウザを起動するためには、`wsl-open`をインストールする必要があります

## チュートリアル

こんにちは！ atcli は、AtCoder のコンテストへの参加をよりスムーズにするためのコマンドラインツールです。 このチュートリアルでは、atcli の基本的な使い方をステップバイステップで説明します。

### 1. AtCoder へのログイン (login)

まずはじめに、AtCoder アカウントにログインします。 ターミナルで以下のコマンドを実行してください。

```terminal
atcli login
```

このコマンドを実行すると、認証プロセスが開始されます。

認証が開始されたらブラウザを手動で起動して、`https://atcoder.jp/login`からAtCoderにログインしてください。その後開発者ツールを起動して、Cookieに保存されているREVEL_SESSIONの値を取得して、ターミナルに張り付けてください。

正しい認証IDを渡した場合、`ログインに成功しました`というメッセージが表示されます。


### 2. コンテストの初期化 (init)

次に、参加したいコンテストの準備を行います。例えば、abc100 というコンテストに参加する場合、以下のコマンドを実行します。

```terminal
$ atcli init abc100
```

このコマンドを実行すると、カレントディレクトリに abc100 という名前のフォルダ（またはコンテスト名に基づいたフォルダ）が作成され、その中に問題ごとのサブフォルダや解答用テンプレートファイルなどが準備されることが一般的です。

### 3. 問題のダウンロード (download)
コンテストの初期化が完了したら、問題文やサンプルテストケースをダウンロードします。

まずは、initコマンドで作成した問題ディレクトリに移動してください

```terminal
cd abc100/a
```

その次に以下のコマンドを実行して、Atcoderからテストケースをダウンロードします。

```terminal
$ atcli download
```

ここで１つ注意事項があります。`atcli`は、コンテストと問題をカレントディレクトリのパスから推測します。今あなたは`/path/to/atcoder/abc100/a`というディレクトリにいます。そのため`atcli`はコンテストが`abc100`で問題が`a`という前提で動作します。

### 4. 解答コードの作成

お気に入りのエディタで解答コードを作成します。 もしあなたの使用言語がcppなら`main.cpp`というファイルを作成してください。

そのほかの言語を利用する場合は以下の表を参照ください。

#### 図(言語別Mainファイル名)

| 言語 | 解答コードのファイル名 |
| -- | -- |
| c | main.c |
| cpp | main.cpp |
| python | main.py |
| java | Main.java |
| haskell | Main.hs |
| go | main.go |
| rust | main.rs |
| zig | main.zig |


### 5. テスト (test)

コードが完成したら、提供されているサンプルケースで正しく動作するかをテストしましょう。テストを実行するには、以下のコマンドを使用します。

```terminal
atcli test
```

testコマンドは `図(言語別Mainファイル名)` の通りに、メインファイルから言語を自動で選択して、ビルド及び実行します。

言語ごとのビルド/実行コマンド配下のようになっています。

| 言語 | ビルドコマンド | 実行コマンド |
| -- | -- | -- |
| c | gcc main.c -o main | ./main |
| cpp | g++ main.cpp -o main | ./main |
| python | - | python main.py |
| java | javac Main.java | java Main |
| haskell | ghc --make --no-keep-hi-files -no-keep-o-files Main.hs | ./Main |
| go | go build main.go | ./main |
| rust | rustc -o main main.rs | ./main |
| zig | zig build main.zig | ./main |


### 6. コードの提出 (submit)

サンプルテストケースで全て正解するようになったら、いよいよ AtCoder にコードを提出します。 以下のコマンドを実行してください。

```terminal
atcli submit
```

このコマンドを実行すると、 `atcli`はソースコードをクリップボードにコピーして、ブラウザを開きます。

そのあと手動でブラウザを操作して提出作業を行ってください。

※ WSL環境でブラウザを起動するためには、`wsl-open`をインストールする必要があります
