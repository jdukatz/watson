# watson

Project I completed last year in an information retrieval class. Our problem was to build a rudimentary version of IBM's Watson and have it answer questions from Jeopardy by querying wikipedia articles with Lucene. The code flow starts in Watson.scala and has some branching if/else statements based on what you want it to do. Rather than a simple "questions in, answers out" system, we were required to use and compare several different preprocessing and indexing strategies. The first option when the code runs is to build an index or answer questions. When building an index, the code can use three different strategies.

- Use Lucene's `StandardAnalyzer()` to build the index, which does some automatic preprocessing, then indexes the documents (wikipedia articles) you feed it, using the Okapi BM25 formula (https://en.wikipedia.org/wiki/Okapi_BM25)
- Use Lucene's `StandardAnalyzer()` to build the index, but with a `ClassicSimilarity()` scoring algorithm, which implements a vector space scoring model (https://lucene.apache.org/core/5_4_0/core/org/apache/lucene/search/similarities/TFIDFSimilarity.html)
- Use Lucene's `WhitespaceAnalyzer()` to split each document on white space (bypassing any Lucene preprocessing), then feed the result to Stanford's CoreNLP system to perform all preprocessing.

Then at the play step, the user can choose which index to query. The `JeopardyPlayer` class uses Lucene to find the best wiki article as an answer to each clue. The top ten answers for each clue are printed out in `answers.txt`. It will also return the mean reciprocal rank and precision at 1 of the responses. The `answerQuestionsImproved` method attempts to improve results by filtering on part of speech tags. This ended up not improving the results, but the requirement for the assignment was just to try something. To truly improve the results, you would need to do a lot of much more in depth parsing of both the clue and the category, since often the correct answer is not as simple as searching for the most related wikipedia article (this is what the real Watson does, although its foundation is actually based on just querying Wikipedia).

Note: as is you can't run this because I didn't include the index or the wikipedia data in the repo, since they're over a gigabyte each.
