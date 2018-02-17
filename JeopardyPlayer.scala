import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.analysis.core.WhitespaceAnalyzer
import org.apache.lucene.analysis.core.StopAnalyzer
import org.apache.lucene.store.FSDirectory
import org.apache.lucene.search.IndexSearcher
import org.apache.lucene.search.similarities.ClassicSimilarity
import org.apache.lucene.index.DirectoryReader
import org.apache.lucene.search.Query
import org.apache.lucene.queryparser.classic.QueryParser
import org.apache.lucene.queryparser.flexible.standard.QueryParserUtil
import edu.stanford.nlp.pipeline._
import edu.stanford.nlp.ling.CoreAnnotations._
import edu.stanford.nlp.trees.TreeCoreAnnotations.TreeAnnotation

import java.util.Set
import java.nio.file.Paths
import java.util.Properties
import java.io._

import scala.io.Source
import scala.collection.mutable.ArrayBuffer

class JeopardyPlayer() {

	val questionKey = scala.collection.mutable.Map[String, String]()
	val STOP_WORDS = StopAnalyzer.ENGLISH_STOP_WORDS_SET
	var responses = scala.collection.mutable.Map[String, ArrayBuffer[String]]() //correct answer -> Watson's answer

	def gatherQuestions(qFile:String) {
		val qText = Source.fromFile(qFile).getLines.toList
		var i = 0
		while (i < qText.length) {
			val cat = qText(i)
			i += 1
			val q = qText(i)
			i += 1
			val answer = qText(i)
			i += 2
			questionKey(cat + " " + q) = answer
		}
	}

	def answerQuestionsLucene(classicSim:Boolean=false) {
		//clear out answers from previous runs
		responses = scala.collection.mutable.Map[String, ArrayBuffer[String]]() //correct answer -> Watson's answer
		//set up index to be searched
		var indexPath = ""
		if (classicSim == true) {
			indexPath = "index/LuceneClassicIndex/StandardIndex.lucene"
		} else {
			indexPath = "index/LuceneIndex/StandardIndex.lucene"
		}
		val reader = DirectoryReader.open(FSDirectory.open(Paths.get(indexPath)))
		val searcher = new IndexSearcher(reader)
		//set up analyzer to read queries
		val analyzer = new StandardAnalyzer()
		var outFileString = ""
		if (classicSim == true) {
			//analyzer.setSimilarity(new ClassicSimilarity())
			outFileString = "answersLuceneClassic.txt"
		} else {
			outFileString = "answersLuceneDefault.txt"
		}
		val outWriter = new BufferedWriter(new FileWriter(new File(outFileString)))
		for ((q, a) <- questionKey) {
			val sanitizedQuery = QueryParserUtil.escape(q)
			val parsedQ = new QueryParser("body", analyzer).parse(sanitizedQuery)
			val docs = searcher.search(parsedQ, 10)
			val hits = docs.scoreDocs
			outWriter.write("Clue: " + q + "\n")
			outWriter.write("\tcorrect answer: " + a + "\n")
			outWriter.write("\tMy answers\n")
			val watsonAnswers = ArrayBuffer[String]()
			//println("Clue: " + q)
			//println("\tMy answers: ")
			for (page <- hits) {
				val ans = reader.document(page.doc).getField("title").stringValue
				outWriter.write("\t\tWhat is " + ans + "\n")
				watsonAnswers += ans
				//println("\t\tWhat is " + reader.document(page.doc).getField("title").stringValue)
			}
			responses(a) = watsonAnswers
		}
		outWriter.close()
	}

	def answerQuestionsStanford() {
		//clear out answers from previous runs
		responses = scala.collection.mutable.Map[String, ArrayBuffer[String]]() //correct answer -> Watson's answer
		//set up processor to process queries
		val props = new Properties()
		props.setProperty("annotators", "tokenize, ssplit, pos, lemma")
		val pipeline = new StanfordCoreNLP(props)
		//set up index to be searched
		//val indexLoc = new File("index/StanfordIndex")
		val reader = DirectoryReader.open(FSDirectory.open(Paths.get("index/StanfordIndex/StandardIndex.lucene")))
		val searcher = new IndexSearcher(reader)
		//set up analyzer to read queries
		val analyzer = new WhitespaceAnalyzer()
		val outWriter = new BufferedWriter(new FileWriter(new File("answersStanford.txt")))
		for ((q, a) <- questionKey) {
			//first process query with coreNLP in the same way the index was processed
			var doc = new Annotation(q)
			pipeline.annotate(doc)
			val sentences = doc.get(classOf[SentencesAnnotation])
			var processedQ = ""
			for (i <- 0 until sentences.size) {
				val s = sentences.get(i)
				val tokens = s.get(classOf[TokensAnnotation])
				for (j <- 0 until tokens.size) {
					val token = tokens.get(j)
					val lemma = token.get(classOf[LemmaAnnotation])
					if (!(STOP_WORDS contains lemma)) {
						processedQ += lemma + " "
					}
				}
			}
			val sanitizedQuery = QueryParserUtil.escape(processedQ)
			val parsedQ = new QueryParser("body", analyzer).parse(sanitizedQuery)
			val docs = searcher.search(parsedQ, 10)
			val hits = docs.scoreDocs
			outWriter.write("Clue: " + q + "\n")
			outWriter.write("\tcorrect answer: " + a + "\n")
			outWriter.write("\tMy answers\n")
			val watsonAnswers = ArrayBuffer[String]()
			//println("Clue: " + q)
			//println("\tMy answers: ")
			for (page <- hits) {
				val ans = reader.document(page.doc).getField("title").stringValue
				outWriter.write("\t\tWhat is " + ans + "\n")
				//println("\t\tWhat is " + reader.document(page.doc).getField("title").stringValue)
				watsonAnswers += ans
			}
			responses(a) = watsonAnswers
		}
		outWriter.close()

	}

	def analyzeResponses() {
		var correctAnswers = 0
		var recipRank = 0.0
		for ((correct, attempts) <- responses) {
			val possibleCorrect = correct.split('|')
			if (possibleCorrect contains attempts(0)) {
				correctAnswers += 1
			}
			for ((a, i) <- attempts.zipWithIndex) {
				if (possibleCorrect contains a) {
					recipRank += (1.0 / (i + 1))
				}
			}
		}
		println("Precision@1 = " + (correctAnswers.toFloat / responses.size))
		println("MRR = " + (recipRank / responses.size))
	}

	def answerQuestionsImproved() {
		//initialize Stanford parser to get NER tags from clue
		val props = new Properties()
		props.setProperty("annotators", "tokenize, ssplit, pos, lemma, ner, parse")
		val pipeline = new StanfordCoreNLP(props)
		//clear out answers from previous runs
		responses = scala.collection.mutable.Map[String, ArrayBuffer[String]]() //correct answer -> Watson's answer
		//set up index to be searched
		val reader = DirectoryReader.open(FSDirectory.open(Paths.get("index/LuceneIndex/StandardIndex.lucene")))
		val searcher = new IndexSearcher(reader)
		//set up analyzer to read queries
		val analyzer = new StandardAnalyzer()
		val outWriter = new BufferedWriter(new FileWriter(new File("answersImproved.txt")))
		val posToKeep = List("NNP", "NNPS", "NN", "VB", "VBN", "VBP", "VBD")
		for ((q, a) <- questionKey) {
			
			//println(q)
			//val parsedCat = new Annotation(q.split('|')(0))
			val parsedCat = new Annotation(q)
			pipeline.annotate(parsedCat)
			val sentences = parsedCat.get(classOf[SentencesAnnotation])
			var newQ = ""
			for (i <- 0 until sentences.size) {
				val s = sentences.get(i)
				val tokens = s.get(classOf[TokensAnnotation])
				//val tree = s.get(classOf[TreeAnnotation])
				for (j <- 0 until tokens.size) {
					val token = tokens.get(j)
					val pos = token.get(classOf[PartOfSpeechAnnotation])
					//println(pos + " " + token.get(classOf[TextAnnotation]))
					val ner = token.get(classOf[NamedEntityTagAnnotation])
					if (ner.equals("O") == false || (posToKeep contains pos)) {
						newQ += (token.get(classOf[TextAnnotation]) + " ")
					}
				}
			}
			//if we didn't find anything, just use the original clue
			if (newQ == "") {
				newQ = q
			}
			//val newQuery = newCat + " " + q.split('|')(1)

			//println(newQ)
			val sanitizedQuery = QueryParserUtil.escape(newQ)
			//println(sanitizedQuery)
			val parsedQ = new QueryParser("body", analyzer).parse(sanitizedQuery)
			val docs = searcher.search(parsedQ, 20)
			val hits = docs.scoreDocs
			outWriter.write("Clue: " + q + "\n")
			outWriter.write("\tcorrect answer: " + a + "\n")
			outWriter.write("\tMy answers\n")
			val watsonAnswers = ArrayBuffer[String]()
			//println("Clue: " + q)
			//println("\tcorrect answer: " + a)
			//println("\tMy answers: ")
			for (page <- hits) {
				val ans = reader.document(page.doc).getField("title").stringValue
				var badAns = false
				if (q.toLowerCase contains ans) {badAns == true}
				if (badAns == false) {watsonAnswers += ans}
				//println("\t\tWhat is " + reader.document(page.doc).getField("title").stringValue)
			}

			val finalAnswers = watsonAnswers.slice(0, 10)
			for (answer <- finalAnswers) {
				//println("\t\tWhat is " + answer)
				outWriter.write("\t\tWhat is " + answer + "\n")
			}











			/*
			for (clue <- questionKey.keys) {
				println(clue)
				var parsedClue = new Annotation(clue)
				pipeline.annotate(parsedClue)
				val sentences = parsedClue.get(classOf[SentencesAnnotation])
				for (i <- 0 until sentences.size) {
					val s = sentences.get(i)
					val tree = s.get(classOf[TreeAnnotation])
					println(tree)
					println()
					/*val tokens = s.get(classOf[TokensAnnotation])
					for (j <- 0 until tokens.size) {
						val token = tokens.get(j)
						val ner = token.get(classOf[NamedEntityTagAnnotation])
						println(ner)
					}*/
				}
			}*/

			responses(a) = watsonAnswers
		}
		outWriter.close()
	}
}