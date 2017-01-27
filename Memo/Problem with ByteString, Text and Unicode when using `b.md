Problem with ByteString, Text and Unicode when using `bytestring-trie`
====

# 問題
bytestring-trieを使ってみようと思ったんだけど、Unicodeのせいで中々やりにくい。

## キーはChar8で収まる文字種のみ使う。

ちゃんと数字などのobject IDで管理できれば良いかもしれないが、それだとTrieを使う意味すら無い。
もちろん、グルーピングして取り出せる機能があるから、"HumanNNN"の様にするのはおかしくはない。
逆に名前がかぶったりしそうだから、実際の名前よりIDの方が正しいのかもしれない。
こうすれば当面の問題はない。普通のプログラミングのようだ。
ただ次の問題は残る。

# 中身はText? それともByteString

同じく中身にTextを入れるかByteStringを入れるかが問題である。
予定として、Trieの中身をまたキーとして使うことが有る。
もちろん、普通に別の処理にも使う。

ここで一般的なByteStringとTextの処理方法を考えよう。
別の処理では当たり前のようにUnicodeを使う。
その為、ByteStringはやや効率が悪いだろう。Textに比べ文字数の数え方とかの効率がおそらく悪いはずだ。
よって、少なくとも取り出す・検索する際には変換が必要になり、やはりTextで保存する必要があるだろう。

ただ、DatabaseのキーとしてはTextが扱いにくくなる。
Textで問い合わせる時はInterfaceでText->ByteString変換を行う。
幸いにもByteStringからTextへの変換が必要になるケースは多くないだろう。

要は使い勝手と、処理速度である。

とにかく、TrieのキーとしてUnicodeを使うことはあると考える。

## Lazy or Strict?

### Saveを考える。

まあ、LazyであったとしてもSave/Loadの際にはおそらく途中で作られた複数のchunkは失われ一つのchunkになるはずだ。
それでも、文字列の操作が良くある場合はLazyで保存すべきである。
