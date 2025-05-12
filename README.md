注意: このプロジェクトは未完成で、鋭意開発中です


# atcli

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

## Example

```terminal
$ atcli init abc100
ディレクトリ ./abc100/aを作成しました
ディレクトリ ./abc100/bを作成しました
ディレクトリ ./abc100/cを作成しました
.
.
.

$ cd abc100/a

$ atcli download
コンテスト: abc100 問題: a
ディレクトリ./testを作成しました。
テストサンプル1を./test/1.in ./test/1.outに保存しました。
テストサンプル2を./test/2.in ./test/2.outに保存しました。

$ vim main.cpp
コードを記述する

$ atcli test
cpp言語が検出されました
テストケースが2件見つかりました
テストを実行します
.
.


$ atcli submit
cpp言語が検出されました
コードをクリップボードにコピーします
ブラウザで提出ページを開きます
```


# testサブコマンド

カレントディレクトリのファイルを main.cpp,main.c,main.py,main.go...の順に探し、見つかったファイルをメインの言語として検出します
その後、その言語をビルドしてテストを実行します

## 言語の種類と検出方法


| 言語 | メインファイル | ビルドコマンド | テストコマンド |
| -- | -- | -- | -- |
| cpp | main.cpp | ... | ... |
| c | main.c | ... | ... |
| py | main.py | ... | ... |



# submit

ソースコードをクリップボードにコピーした後、ブラウザで提出ページを開きます

※ WSL環境でブラウザを起動するためには、`wsl-open`をインストールする必要があります