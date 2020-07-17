# icfpc2020

## for cabal user

icfpc2020.cabal は `hpack` コマンドを使って
package.yaml から生成しています。
特にオプションも必要無いはず。

```
hpack
```

## submission

submission というブランチ名と submissions/* というパターンのブランチ名を push すると、
コンテストのシステムへの submit として扱われます。

executable の solution ターゲットを用意して、
submissions/solution-exec ブランチで starterkit と同様のものを動くようにしてあります。

solution ターゲットは static link しているので、開発環境のバージョンの多少にズレは吸収できているはずです。

`./solution/solution` を置き換えることで、 submit するプログラムを更新できます。

- stack ユーザー用 `./stack-update-submission.sh`
- cabal V1 ユーザー用 `./v1-update-submission.sh`
