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

現状、submissions/solution-exec ブランチで starterkit と同様のものを動くようにしてあります。

`./solution/solution` を置き換えることで、 submit するプログラムを更新できます。

cabal V1 ユーザーなら `./update-submission.sh` をそのまま使えると思います。
