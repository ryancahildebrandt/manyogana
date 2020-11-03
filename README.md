# A Modern (and much simplified) Implementation of Manyōgana

---

---

![Custom Transliteration Algorithm](https://github.com/ryancahildebrandt/manyogana/blob/master/Manyogana Algorithm.png)

---

---
[*Open*](https://gitpod.io/#https://github.com/ryancahildebrandt/manyogana) *in gitpod*

## *Purpose*
An interpretation and application of some of the usage patterns of Japanese manyōgana script, applied to modern Japanese text via a transliteration algorithm. More of a fun tool to play around with than anything designed to have specific practical uses.

---

## *Introduction*
At its simplest, manyōgana（万葉仮名）was a writing system used beginning in the Asuka/Nara periods. It consisted of Chinese characters used to represent the Japanese language phonetically. This system would later be simplified into hiragana and katana syllabaries, but was notable as the first written system to represent Japanese phonetically. Beyond this surface definition, all sorts of factors complicate the usage and application of manyōgana. For example, any given kanji in a written sentence could be a manyōgana used solely for its phonetic value (of which many kanji have several), a kanji used for its meaning alone, or a combination of these two. Manyōgana was heavily used to record poetry, and as a result many artistic choices were made in the usage of manyōgana. One kanji may represent a given sound in one place, with a completely different kanji for the same sound only a few lines later. Some kanji corresponded closely to one mora at a time, while some kanji routlinely represented 2 or 3 morae, again depending on context and authorial choice. These factors resulted in huge variation in what characters could be or were used for different sounds.Some morae had nearly 30 corresponding kanji which could be used as manyōgana, each with their own meaning and usage outside of this writing system.

---

## *Data*
The data sources used for the current project are listed below:
+ [Wikipedia Manyōgana Page](https://en.wikipedia.org/wiki/Man%27y%C5%8Dgana), used for easily accessible tables of which kanji correspond to which kana, as well as basic mechanics of how manyōgana were used
+ [Wikipedia2Vec Japanese Pretrained Embedding](https://wikipedia2vec.github.io/wikipedia2vec/pretrained/), used for comparing semantic similarity between manyōgana candidates
+ [Udpipe in R](https://bnosac.github.io/udpipe/en/index.html), for tokenization of Japanese text input

---

## *Approach*
tokenizer
1:1
cosine
numeric




---

## *Outputs*

+ The main [report](https://datapane.com/ryancahildebrandt/reports/The_Numbers_on_Particles/?accesstoken=88050a78fe9e93296933b540aba600969cd63b84), compiled with datapane and also in [html](outputs/particles_rprt.html) format
+ The [png](https://github.com/ryancahildebrandt/particles/blob/master/outputs/particle_cloud.png) for the wordcloud used at the top of the page
+ Interactive [sankey](http://htmlpreview.github.io/?https://github.com/ryancahildebrandt/particles/blob/master/outputs/sankey.html) plot for the particles and their attributes
+ Another [sankey](http://htmlpreview.github.io/?https://github.com/ryancahildebrandt/particles/blob/master/outputs/sankey_Head.html), this time for the syntactic heads
+ A comparison of the [top 10](outputs/top10_stacked.html) most commonly used particles
+ Another comparison [chart](outputs/top10_head_stacked.html), this time for syntactic heads
+ The [notebook](NLP.ipynb) for the NLP analyses (NOTE: this takes a very time long to run, I'd avoid it if possible as the remainder of the code runs just fine without having to run this every time)
+ The [notebook](particles_nb.ipynb) for the analyses and viz generated *after* the NLP