import org.apache.lucene.store.FSDirectory
import org.apache.lucene.analysis.core.WhitespaceAnalyzer
import org.apache.lucene.analysis.core.StopAnalyzer
import org.apache.lucene.index.IndexWriter
import org.apache.lucene.index.IndexWriterConfig
import org.apache.lucene.document.Document
import org.apache.lucene.document.Field
import org.apache.lucene.document.TextField
import org.apache.lucene.document.StringField
import edu.stanford.nlp.pipeline._
import edu.stanford.nlp.ling.CoreAnnotations._

import java.io.File
import java.nio.file.Paths
import java.util.Properties

import scala.io.Source
import scala.util.matching.Regex

class StanfordIndexer(folder:String) {
	/*
	Builds an index using Stanford's CoreNLP preprocessing
	Includes:
	tokenization
	lemmatization
	stop-word removal (Lucene defaults)
	*/

	val STOP_WORDS = StopAnalyzer.ENGLISH_STOP_WORDS_SET
	val props = new Properties()
	props.setProperty("annotators", "tokenize, ssplit, pos, lemma")
	val pipeline = new StanfordCoreNLP(props)
	val index = FSDirectory.open(Paths.get("index/StanfordIndex/StandardIndex.lucene"))
	val analyzer = new WhitespaceAnalyzer()
	val config = new IndexWriterConfig(analyzer)
	val w = new IndexWriter(index, config)
	
	def indexData() {

		val wikiPages = new File(folder)
		val t0 = System.nanoTime()

		for (f <- wikiPages.listFiles) {
			//-> read pages line by line
			//-> gather a document
			//-> preprocess it
			//-> save to index
			println("indexing file " + f + "...")
			val rawLines = Source.fromFile(f).getLines.toList
			val titlePattern = "^\\[\\[(.*?)\\]\\]$".r
			var title = ""
			var docString = ""
			var i = 0
			var disamb = false
			while (i < rawLines.length) {
				if (i % 10000 == 0) {println("\tline " + i + " " + title)}
				val l = rawLines(i)
				if (titlePattern.findFirstIn(l) != None) {
					//start of a new document
					if (i != 0) {
						//excluding the first line, add the previous doc to the index
						if (disamb == false) {
							addToIndex(title, docString)
						} else {
							disamb = false //unless it was a disambiguation page
						}
					}
					//new title and document content
					title = l.replace("[","").replace("]","")
					docString = ""
					val twoLinesDown = rawLines(i+2)
					if ((twoLinesDown contains title) && (twoLinesDown contains "may refer to")) {
						disamb = true
					}
					i += 1
				} else if (l.slice(0, 9).equals("#REDIRECT")) {
					disamb = true //also exclude redirection pages
					i += 3
				} else {
					docString += l
					i += 1
				}

			}
			if (disamb == false) {
				addToIndex(title, docString) //index last entry in file
			}
		}
		println("Index successfully built in " + (System.nanoTime() - t0) / 1e9d + "seconds")
		w.close()
	}

	def addToIndex(title:String, docString:String) {
		val processedString = preProcess(docString)
		val docObj = new Document()
		docObj.add(new StringField("title", title, Field.Store.YES))
		docObj.add(new TextField("body", docString, Field.Store.YES))
		w.addDocument(docObj)
	}

	def preProcess(docString:String):String = {
		var document = new Annotation(docString)
		pipeline.annotate(document)
		val sentences = document.get(classOf[SentencesAnnotation])
		var outString = ""
		for (i <- 0 until sentences.size) {
			val s = sentences.get(i)
			val tokens = s.get(classOf[TokensAnnotation])
			for (j <- 0 until tokens.size) {
				val token = tokens.get(j)
				val lemma = token.get(classOf[LemmaAnnotation])
				if (!(STOP_WORDS contains lemma)) {
					outString += lemma + " "
				}
			}
		}
		//println(outString.slice(0,50))
		outString
	}
}