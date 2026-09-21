# Emacs セットアップメモ
<small>最終更新: 2026-07-10</small>

新規に Emacs を自分の環境ファイルを使ってセットアップする機会があったので、その時のメモを備忘録として残しておく

## 前準備
### MSYS2 のインストール
<small>なくても動くけど、結局grepとかがないと不便なので…</small>

[本家のページ](https://www.msys2.org) よりダウンロードしてインストール

## emacs をインストール
### 本体
[本家のページ](https://www.gnu.org/software/emacs) よりダウンロードしてインストール

### 環境ファイル
[My GitHub](https://github.com/toastedlaver/.emacs.d) より取得
- ssh 経由で clone する
  ```
  $ git clone git@github.com:toastedlaver/.emacs.d.git
  ```
- または ZIP ダウンロード

### 追加パッケージをインストール
emacs 起動して、エラー出る箇所を見ながら下記をインストールしていく

#### `package-install` で導入
| パッケージ   | 概要                                                         | 補足                                                                       |
|--------------|--------------------------------------------------------------|----------------------------------------------------------------------------|
| migemo       | ローマ字日本語検索                                           |                                                                            |
| ddskk        | 日本語入力                                                   |                                                                            |
| bf-mode      | dired でファイルの中身を表示                                 |                                                                            |
| color-moccur | 検索                                                         | grep/ripgrep などが使えない環境 (Windows native とか) のため               |
| recentf-ext  | recentf でディレクトリも保存                                 | helm を動かすために必要                                                    |
| helm 本体    | 検索                                                         | `(require 'helm-config)` ではなく `(require 'helm)` になったようだ |
| helm-gtags   |                                                              |                                                                            |
| helm-swoop   | 検索画面を編集して元ファイルに反映                           |                                                                            |
| helm-ag      | ag で helm インターフェースを使う                            | 設定すれば grep/ripgrep も使える                                           |
| shell-toggle |                                                              |                                                                            |
| session      | kill-ring やミニバッファ入力、開いたファイルなどの履歴を保存 |                                                                            |
| magit        | git I/F                                                      |                                                                            |

#### パッケージ管理外
| elisp                | 概要                                | DL                                                                             | メモ                                                                                               |
|----------------------|-------------------------------------|--------------------------------------------------------------------------------|----------------------------------------------------------------------------------------------------|
| sorter.el            | dired でファイルのソートを拡張      | オリジナルのファイルが所在不明                                                 |                                                                                                    |
| w32-symlinks.el      | dired で Win のショートカットを処理 | [EmacsWiki](https://www.emacswiki.org/emacs/w32-symlinks.el)                   |                                                                                                    |
| dabbrev-highlight.el | dabbrev 展開時に色つけ              | [オリジナルの改良版](http://www.namazu.org/~tsuchiya/elisp/#dabbrev-highlight) |                                                                                                    |
| dabbrev-ja.el        | dabbrev を日本語に対応              | [コード](http://www.namazu.org/~tsuchiya/elisp/dabbrev-ja.el)                  | [dabbrev-highlight のページ](http://www.namazu.org/~tsuchiya/elisp/#dabbrev-highlight) → 追加設定 |
| cygwin-mount.el      | cygwin形式のパスを扱えるようにする  | [EmacsWiki](https://www.emacswiki.org/emacs/cygwin-mount.el)                   | Cygwin や MSYS2 等の一部コマンドは UNIX 形式パスしか処理できないので必要                           |
| psvn                 | Subversion I/F                      | [EmacsWiki](https://www.emacswiki.org/emacs/psvn.el)                           | もうコードがメンテされてなさそうなので、 `package-install` できる dsvn に変えた方が良いかも    |


#### その他
##### for migemo
- [実行ファイル cmigemo](https://github.com/koron/cmigemo)
  + Windows では他のアプリでも使うので `%USERPROFILE%\AppData\Local\cmigemo` に入れている
  + Linux なら普通に `make` → `make install`
  + migemo-dictionary 等の値を環境に合わせ設定
    - 辞書の文字コードは UTF-8 (もう SJIS とか使わんでいいやろw)

##### for ddskk
- L 辞書は [github](https://github.com/skk-dev/dict) からダウンロードして `.emacs.d/etc/skk` に入れる
  + Win 環境では [SKKFEP](http://coexe.web.fc2.com/skkfep.html) の L 辞書と共有しようと画策したが、文字コードが違うので諦める
