---
title: リンクとリンクウォーキング
project: riak
version: 0.10.0+
document: tutorial
audience: beginner
keywords: [tutorial, fast-track]
prev: "[[データのロードと MapReduce クエリ]]"
up:   "[[The Riak Fast Track]]"
next: "[[CAP コントロールを調整する]]"
versions: false
interest: [
"[[Links]]",
"<a href='http://blog.basho.com/2010/02/24/link-walking-by-example'>Link Walking By Example</a>",
"<a href='http://blog.inagist.com/link-map-reduce-in-riak-an-example-from-inagi'>Link-Map-Reduce Example</a>",
"<a href='http://tools.ietf.org/html/draft-nottingham-http-link-header-10'>IETF 'Web Linking' Draft</a>"
]
---

Map と Reduce が終わったので、次は "リンク"について説明しましょう。

Fast Track のこのセクションでは、リンクがどのように働くのか、そして "リンクウォーキング" クエリの動作、最後に易しい、でも詳しいスクリーンキャストで基礎チュートリアルを補います。さあ、心してかかってください。

## リンクとは？

キー/バリューストアが提供するかなり制限されたデータモデルを拡張する方法の一つが、"リンク"という概念で、"リンクウォーキング"として知られるクエリタイプです。 

リンクとは、オブジェクトに一方通行的な関連を持つメタデータのことです。リンクしてアタッチされたら、あるオブジェクトから別のオブジェクトへクエリを "ウォーク" することができるようになります。リンクによって、軽い、データへのポインタを作ることができます。たとえば、'projects'から'milestones'へ、さらに'tasks'へといった。そしてこれを、簡単なクライアント API コマンドによって階層を追ってデータを選択できるのです。(いざというときには、キーにアタッチされたリンクの数が少なければ、これは簡単な画像データベースといえます)。リンクは Riak にパワフルな能力を追加し、開発者が適切に調理すればアプリケーションを多機能化することができます。

## リンクをたどる

リンクはオブジェクトのメタデータの中で、"Link" ヘッダとしてアタッチされています。リンクヘッダはこのようになります:

```bash
Link: </riak/people/dhh>; riaktag="friend"
```

実際、これはどういうことでしょうか？　カギ括弧の中に、リンクを示す URL があります。プレフィクスの "riak" に続いて、バケット名 "people" と、キー "dhh" があります。次に "riaktag" が来ます。riaktag フィールドの中には、リンク先の説明となる識別子が含まれています。ここでは riaktag は "friend" です。

CURL で、リンクヘッダ付きの完全な PUT リクエストをすると次のようになります:

```bash
$ curl -v -XPUT -H 'Link: </riak/people/dhh>; riaktag="friend"' \
  -H "content-type: text/plain" http://127.0.0.1:8091/riak/people/timoreilly \
  -d 'I am an excellent public speaker.'
```

このリクエストでは 'Link:</riak/people/dhh>; riaktag="friend"' を、"people" バケットの中にある "timoreilly" というキーにアタッチしています。

試してみてください。簡単ですよね？　Riak にオブジェクトへのリンクをアタッチできました。

<div class="info">オブジェクトからリンクを除去するのも簡単です。オブジェクトを読み出し(GET)、リンク情報を remove して、Riak に書き戻します。</div>

アタッチされたリンクを見るためにオブジェクトを取得するには、次のように行います:

```bash
$ curl -v http://127.0.0.1:8091/riak/people/timoreilly
```

レスポンスヘッダの "Link" フィールドを見ます。リンク情報が見えるはずです。

よろしいですか。私たちは "dhh" オブジェクトを指す "friend" タグを、"timoreilly" オブジェクトに格納しました。今度は "timoreilly" がリンクされている "dhh" オブジェクトを格納する必要があります。

```bash
$ curl -v -XPUT -H "content-type: text/plain" http://127.0.0.1:8091/riak/people/dhh \
  -d 'I drive a Zonda.'
```

よろしい。これで "people" バケットに "timoreilly"を格納しました。これは同じ "people" バケットにある "dhh" オブジェクトにリンクしています。

点と点をどのように結びましょうか？　それがリンクウォーキングです。

## リンクウォーキング

オブジェクトをリンクしたら、"Link Walking" と呼ばれる操作でそれをたどることができます。1 度のリクエストでリンクをいくつでもたどることができます。さらに、一致したすべてのオブジェクトを 1 回のリザルトとして返すようにもできます。

上記のサンプルを続けましょう。現在 "timoreilly" オブジェクトは "people" バケットの "dhh" オブジェクトを指し示しています。リンクウォーキング クエリで、"timoreilly" から "dhh" までをたどってみましょう。クエリはこのようになります:

```bash
$ curl -v http://127.0.0.1:8091/riak/people/timoreilly/people,friend,1
```

リクエストに "/people,friend,1" を付けたことに着目してください。これはリンクを指定しています。これは3つのパートに分かれています。

* Bucket - リンクの範囲を制限するためのバケット名 (上記のリクエストでは 'people')
* Tag - リンクの範囲を制限するための "riaktag" (上記のリクエストでは 'friend')
* Keep - 0 または 1、このステップまたはフェーズからリザルトを返すかどうか

すべてがうまく行けば、上記のリクエストでは "dhh" オブジェクトを含んだレコードを、レスポンスボディに返します。

"bucket" と "tag" フィールドはいずれも、アンダースコアで置き換えることができます。これは全てのバケットやタグ名とマッチさせるという意味です。たとえば、下記のリクエストは、すべてをきちんと指定した上記のリクエストと同じデータを返すはずです。

```bash
$ curl -v http://127.0.0.1:8091/riak/people/timoreilly/_,friend,1
```

ウォークの各ステップはフェーズとして扱われます。内部では、リンクウォーキングのリクエストは MapReduce と同じメカニズムを使い、URL で指定された各ステップは1つの MapReduce のフェーズに変換されます。複数のステップをウォークしたいときは、Keep パラメータでどのステップを実行するのかを指定します。

デフォルトでは、最後のステップで見つけたオブジェクトだけを含みます。たとえば、オリジナルオブジェクト(ここでは "timoreilly")からリンクをたどって、見つけたものを図示したいといったときに、役に立ちます。例題として、"timoreilly" の友人である "davethomas" という別のオブジェクトを追加しましょう。

```bash
$ curl -v -XPUT http://127.0.0.1:8091/riak/people/davethomas \
  -H 'Link: </riak/people/timoreilly>; riaktag="friend"' \
  -H "content-type: text/plain" \
  -d 'I publish books'
```

"davethomas" から "dhh" までをたどります。最後のパラメータに着目してください。
Now we can walk from "davethomas" to "dhh" in one go, and you'll see the last parameter in action:

```bash
$ curl -v localhost:8091/riak/people/davethomas/_,friend,_/_,friend,_/
```

リザルトとして "dhh"オブジェクトだけが取得できます。途中のステップでは最後のパラメータを "_" にします。デフォルト(0) は何も返しません。最後のステップでは 1 にします。つまり、やりたいことを実現するには、最後のパラメータを 1 にするだけです。

```bash
$ curl -v localhost:8091/riak/people/davethomas/_,friend,1/_,friend,_/
```

実際にこれを行うとき、出力はもっとややこしくなります。というのは、今は 2つの部分しかありませんが、実際にはそれぞれがもっとたくさんのオブジェクトをふくんでいるのですから。

いよいよ最後に、"davethomas" を直接 "dhh" の友人にします。これで本当の図が描かれましたが、このやり方は1つではありません。

```bash
$ curl -v -XPUT http://127.0.0.1:8091/riak/people/dhh
  -H 'Link: </riak/people/davethomas>; riaktag="friend"' \
  -H "content-type: text/plain" \
  -d 'I drive a Zonda.'
```

リクエストによってはもっとリンクを追加することができます。"dhh" から "davethomas" を通って "timoreilly" へ行く、あるいは "davethomas" から "davethomas" へだって。リンクウォーキングのステップを指定するだけです。

```bash
$ curl -v localhost:8091/riak/people/davethomas/_,friend,_/_,friend,_/_,friend,_/
```

これまでやったことを再確認しましょう。

1. 格納されたオブジェクトのリンクをアタッチする
2. リンクが指しているオブジェクトを格納する
3. 1つのオブジェクトから別のオブジェクトへ、リンクウォーキング クエリでオブジェクトの中をたどって行く

これはかなり強力な機能です。これまで見てきたことは、リンクが何を行えるか、どのようなことに使えるのかという表面的なことだけです。

## リンクウォーキングの素晴らしさについてのスクリーンキャスト

Basho のハッカーである Sean Cribbs はスクリーンキャストで、リンクウォーキングの基本と、もっと複雑なこと、さらにリンクの拡張的使用法についてご案内します。

<div style="display:none" class="iframe-video" id="http://player.vimeo.com/video/14563219"></div>

<p><a href="http://vimeo.com/14563219">Riak でのリンクとリンクウォーキングについて</a> <a href="http://vimeo.com">Vimeo</a> の<a href="http://vimeo.com/bashotech">Basho Technologies</a> 上で公開</p>

## リンクウォーキングのスクリプト

上述のスクリーンキャストにおいて、Sean はいくつかのスクリプトを使って Riak のリンクに関して、より突っ込んだ説明を行なっています。これがそのスクリプトです:

<dl>
<dt>[[load_people.sh|https://github.com/basho/basho_docs/raw/master/source/data/load_people.sh]]</dt>
<dt>[[people_queries.sh|https://github.com/basho/basho_docs/raw/master/source/data/people_queries.sh]]</dt>
</dl>

ビデオをご覧いただけば、これらのスクリプトがどのようにリンクウォーキングで使われるのかは一目瞭然です。ビデオを見られない、あるいはスクリプトを改造したい方は、次の図を参照ください。
![Circle of Friends](/images/circle-of-friends.png)

"load_people.sh" は動作中の 3つのノードの Riak クラスタへ、上図の関連したデータおよび、アタッチされた必要なリンクといったデータを、自動的にロードします。

"people_queries.sh" は、一連のリンクウォーキング クエリによって、load_people.sh スクリプトでプレロードしたデータの関係を明らかにします。

"load_people.sh" を使うには、"dev" ディレクトリにダウンロードし、実行します。

```bash
$ chmod +x load_people.sh
```

次に、

```bash
$ ./load_people.sh
```

何行か表示され、完了します。"people_queries.sh" も同じように実行してください。

```bash
$ chmod +x people_queries.sh
```

次に、


```bash
$ ./people_queries.sh
```


このようになるはずです:


```bash
Press [[Enter]] after each query description to execute.
Q: Get Sean's friends (A:Mark, Kevin)
```

さあ、お楽しみください。

参考

* [[リンクについて|Links]]
* [リンクウォーキングのサンプル](http://blog.basho.com/2010/02/24/link-walking-by-example)
* [Link-Map-Reduce with Riak at inagist.com](http://blog.inagist.com/link-map-reduce-in-riak-an-example-from-inagi)
* [IETF "Web Linking" Draft](http://tools.ietf.org/html/draft-nottingham-http-link-header-10)
