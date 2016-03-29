
import scala.collection.mutable.ListBuffer

/**
  * Created by adminhsu on 2016/3/29.
  */

object ngram extends App {
    val path = "D:\\elasticsearchtry\\test.txt"
    val cutlist = "\uFEFF<>/:：;；,、＂’，.）（。！？「」｢\"\'\\\n\r《》[]“”!@#$%^&*()"
    val cut = cutlist.split("")
    def cleanString(path:String,keywords:ListBuffer[String]):List[String]={
        val lines = scala.io.Source.fromFile(path).getLines.filter(!_.isEmpty())
        var textList = new ListBuffer[String]()
        var sentence = ""
        for (line <- lines){
            var newLine = line
            println(keywords)
            if (keywords.length>0){
                for (keyword <- keywords){
                    newLine = newLine.replaceAll(keyword,"")
                }
            }
            for (word <- newLine) {
                if (!cutlist.contains(word)) {
                    sentence += word
                }
                else {
                    textList += sentence
                    sentence = ""
                }
            }
            textList += sentence
        }
        return textList.toList
    }


    def ngram(textLists:List[String],n:Int,minFreq:Int):ListBuffer[(String,Int)]={
        var words = new ListBuffer[String]()
        var words_freq = collection.mutable.Map[String, Int]()
        var result = new ListBuffer[(String,Int)]
        for (textList <- textLists){
//            if(!textList.isEmpty){
//                textList.split("").sliding(2).foreach(
//                    word => words += word.mkString
//                )
//            }
            val max = textList.length
            for (senIndex <- 0 to max-n){
                words += textList.toString.slice(senIndex,senIndex+n)
            }
        }
        for (word<-words){
            if (!words_freq.contains(word)){
                words_freq += (word -> 1)
            }
            else{
                words_freq(word) += 1
            }
        }
        val words_freqArray  = words_freq.toSeq.sortWith(_._2 > _._2)
        for (word <- words_freqArray){
            if (word._2 >= minFreq){
                 result += word
            }
        }
        return result
    }
    def longTermPriority(path:String,maxTermLength:Int,minFreq:Int):ListBuffer[(String,Int)]={
        val longTerms = new ListBuffer[String]()
        val longTermsFreq = new ListBuffer[(String,Int)]()
        for (a <- maxTermLength to 2 by -1) {
            println(a)
            val text_list = cleanString(path, longTerms)
            val words_freq = ngram(text_list, a, minFreq)
            for (word_freq <- words_freq) {
                longTerms.append(word_freq._1)
                longTermsFreq.append(word_freq)
            }
        }
        return longTermsFreq
    }
    val longTermsFreq = longTermPriority(path,5,3)
    print(longTermsFreq.toSeq.sortWith(_._2 > _._2))
}
