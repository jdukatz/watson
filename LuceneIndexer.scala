import org.apache.lucene.store.FSDirectory
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.index.IndexWriter
import org.apache.lucene.index.IndexWriterConfig
import org.apache.lucene.search.similarities.ClassicSimilarity
import org.apache.lucene.document.Document
import org.apache.lucene.document.Field
import org.apache.lucene.document.TextField
import org.apache.lucene.document.StringField

import java.io.File
import java.nio.file.Paths

import scala.io.Source
import scala.util.matching.Regex

class LuceneIndexer(folder:String, classic:Boolean=false) {
	/*
	Builds an index using Lucene's built-in preprocessing
	Includes:
	tokenization
	stemming
	stop-word removal
	*/
	var indexPath = ""
	if (classic == true) {
		indexPath = "index/LuceneClassicIndex/StandardIndex.lucene"
	} else {
		indexPath = "index/LuceneIndex/StandardIndex.lucene"
	}
	var index = FSDirectory.open(Paths.get(indexPath))
	val analyzer = new StandardAnalyzer()
	val config = new IndexWriterConfig(analyzer)
	if (classic == true) {
		config.setSimilarity(new ClassicSimilarity())
	}
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
				if (i % 10000 == 0) {println("\tline " + i)}
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
					disamb = true //don't index redirects
					i += 3
				} else {
					docString += l
					i += 1
				}

			}
			if (disamb == false) {
				addToIndex(title, docString) //don't forget to add the last entry
			}
		}
		println("Index successfully built in " + (System.nanoTime() - t0) / 1e9d + " seconds")
		w.close()
	}

	def addToIndex(title:String, docString:String) {
		val docObj = new Document()
		docObj.add(new StringField("title", title, Field.Store.YES))
		docObj.add(new TextField("body", docString, Field.Store.YES))
		w.addDocument(docObj)
	}
}