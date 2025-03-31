# A Modern (and much simplified) Implementation of Manyōgana

---

---

![Custom Transliteration Algorithm](https://github.com/ryancahildebrandt/manyogana/blob/master/Manyogana_Algorithm.png)

---

---
[![Open in gitpod](https://gitpod.io/button/open-in-gitpod.svg)](https://gitpod.io/#https://github.com/ryancahildebrandt/manyogana)
[![This project contains 0% LLM-generated content](https://brainmade.org/88x31-dark.png)](https://brainmade.org/)

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
+ [Japanese Word Frequency List](https://namakajiri.net/nikki/a-2015-count-of-japanese-word-frequency/), to keep the word2vec model a bit smaller while covering the most frequent Japanese words.

---

## *Approach*
The transliteration algorithm is outlined in the flowchart at the top of the page, and below are some more detailed notes of different parts of the process.
+ *Tokenization:* Tokenization is a process by which text data is split into constituent pieces called tokens. In the case of this data, input text is split into words and sub-words. For example, the string "これはペンです。" would be split into   "これ | は | ペン | です| 。"
+ *Tagging:* After tokenization, each token is labeled with different semantic and other linguistic information, such as part of speech and its syntactic head. The previous sentence would be tagged with part of speech information as   "これ(**PRONOUN**) | は(**PARTICLE**) | ペン(**NOUN**) | です(**COPULA**)| 。(**PUNCTUATION**)"
+ *1:1 Substitution:* To handle kana making up particles, conjugations, and other helper words, 1:1 a  substitution approach was used in which each kana is swapped for one corresponding manyōgana. These manyogana were selected based on the kanji from which the hiragana was originally derived. For kana which did not have immediate candidates, kanji were manually selected from the text of the 万葉集 based on frequent usage across different words and authors. 
+ *Cosine Similarity:* For words written in kana, semantic similarity was used to choose between candidate manyōgana. Cosine similarity is one way of calculating semantic similarity between two words, and was calculated between each word and each possible manyōgana for each kana in the parent word. As a short example,"りんご" would be split into "り | ん | ご", and each kana would return the following possible manyōgana candidates:
    + り | 里理利梨隣入煎  
    + ん | 无  
    + ご | 吾呉胡娯後籠児悟誤其期碁語御馭凝  
  
    Cosine similarity is then calculated between the parent word りんご and each manyōgana candidate, and the most similar manyōgana is selected for each constituent kana.  
  
    + り | 利  
    + ん | 无  
    + ご | 其  

---

## *Outputs*

+ The main [app](https://rhildebrandt.shinyapps.io/manyogana/), compiled with R's Shiny 
+ The [png](https://github.com/ryancahildebrandt/manyogana/blob/master/Manyogana_Algorithm.png) for the flowchart used at the top of the page
