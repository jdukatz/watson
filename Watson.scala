import scala.io.Source
import scala.io.StdIn

object Watson {
	def main(args:Array[String]) {

		val task = StdIn.readLine("Enter 1 to build an index, 2 to play Jeopardy\n").toInt

		if (task == 1) {
			val folderName = StdIn.readLine("Enter folder containing data to be indexed\n")

			val indexType = StdIn.readLine("Enter 1 for Lucene preprocessing, 2 for CoreNLP preprocessing, 3 for classic similarity scoring\n").toInt
			
			if (indexType == 1) {
				//Lucene processing
				println("Initializing indexing using Lucene preprocessing")
				val luceneIdxr = new LuceneIndexer(folderName)
				luceneIdxr.indexData()
			} else if (indexType == 2) {
				//coreNLP processing
				println("Initializing indexing using Stanford preprocessing")
				val stanfIdxr = new StanfordIndexer(folderName)
				stanfIdxr.indexData()
			} else if (indexType == 3) {
				//classic similarity indexing (Lucene)
				println("Initializing indexing using Lucene preprocessing (classic similarity)")
				val classicIdxr = new LuceneIndexer(folderName, true)
				classicIdxr.indexData()
			} else {
				Console.err.println("Invalid choice")
				sys.exit(0)
			}
		} else if (task == 2) {
			println("YOU'LL RUE THE DAY YOU CROSSED ME TREBEK")
			val indexToUse = StdIn.readLine("Enter 1 for Lucene index, 2 for CoreNLP\n").toInt
			val questionsFile = StdIn.readLine("Enter file containing questions\n")
			val player = new JeopardyPlayer()
			player.gatherQuestions(questionsFile)
			if (indexToUse == 1) {
				//query Lucene index
				println("Generating answers to questions via Lucene Index...")
				player.answerQuestionsLucene()
				println("Response analysis with default scoring:")
				player.analyzeResponses()
				//answer questions with classic similarity
				println("Generating answers to questions via Lucene Index (classic similarity)")
				player.answerQuestionsLucene(true)
				println("Response analysis with classic similarity scoring")
				player.analyzeResponses()
				
			} else if (indexToUse == 2) {
				//query CoreNLP Index
				println("Generating answers to questions via Stanford Index")
				player.answerQuestionsStanford()
				println("Response analysis:")
				player.analyzeResponses()
			} else {
				Console.err.println("Invalid choice")
				sys.exit(0)
			}
			
			println("Initializing improved retrieval system:")
			val improvedPlayer = new JeopardyPlayer()
			improvedPlayer.gatherQuestions(questionsFile)
			improvedPlayer.answerQuestionsImproved()
			improvedPlayer.analyzeResponses()
			
		} else {
			Console.err.println("Invalid choice")
			sys.exit(0)
		}
	}
}