// questions 1, 2, 3, 4
import scala.collection.mutable.ListBuffer

object list2 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  //1
  def avg(scores: List[Double]): Double = {
  		var total = 0.0
  		for (f <- scores) total += f
  		total/scores.length
  }                                               //> avg: (scores#4908032: List#4519004[Double#1218])Double#1218
  
  
  def avgAvg(scores: List[List[Double]]): List[Double] = {	//list of avgs for each student
  		//use map
  		scores.map(avg)
  }                                               //> avgAvg: (scores#4908062: List#4519004[List#4519004[Double#1218]])List#451900
                                                  //| 4[Double#1218]
 
  var cs152 = List(List(93.0, 89.0, 90.0), List(75.0, 76.0, 68.0), List(88.0, 82.0, 78.0), List(20.0, 38.0, 73.0))
                                                  //> cs152  : List#6684[List#6684[Double#1218]] = List(List(93.0, 89.0, 90.0), Li
                                                  //| st(75.0, 76.0, 68.0), List(88.0, 82.0, 78.0), List(20.0, 38.0, 73.0))
  avgAvg(cs152)                                   //> res0: List#4519004[Double#1218] = List(90.66666666666667, 73.0, 82.666666666
                                                  //| 66667, 43.666666666666664)
                                           
 
  def passing(scores: List[List[Double]]): List[Int] = {
  		val averages = avgAvg(scores)
  		var positions = new ListBuffer[Int]
  		for (i <- 0 until averages.length){
  			if (averages(i) > 70) positions += i
  		}
  		positions.toList
  }                                               //> passing: (scores#4908147: List#4519004[List#4519004[Double#1218]])List#45190
                                                  //| 04[Int#921]
  def overSeventy(x: Double) = x >= 70            //> overSeventy: (x#4908204: Double#1218)Boolean#1740
 
  	
  passing(cs152)                                  //> res1: List#4519004[Int#921] = List(0, 1, 2)
  
  def sumSums(scores: List[List[Double]]):Double = {
  		scores.map(sum).reduce(_ + _)		//use map to get list of sums of the lists. use reduce to get the sum of the list
  }                                               //> sumSums: (scores#4908206: List#4519004[List#4519004[Double#1218]])Double#12
                                                  //| 18
  
  
  def sum(list: List[Double]): Double = {
  		var total = 0.0
  		for (f <- list) total += f
  		total
  }                                               //> sum: (list#4908226: List#4519004[Double#1218])Double#1218
  
  sumSums(cs152)                                  //> res2: Double#1218 = 870.0
    
    
  //2
  def spellCheck(doc: List[String], dictionary: List[String]): List[String] = {
  		var notInDict = new ListBuffer[String]
  		for (f <- doc) {
  			if (!dictionary.contains(f)) notInDict += f	//not in dictionary, add to list
  		}
  		notInDict.toList
  }                                               //> spellCheck: (doc#4908389: List#4519004[String#4579186], dictionary#4908390:
                                                  //|  List#4519004[String#4579186])List#4519004[String#4579186]
    
  spellCheck(List("hmm", "is", "this", "in", "it"), List("in", "it"))
                                                  //> res3: List#4519004[String#4579186] = List(hmm, is, this)
    
    
  //3
  def spellCheckM(doc: List[String], dictionary: List[String]): List[String] = {
  		//use a filter to figure out what's not in dictionary
  		doc.filter(x =>{var isIn = true; if(dictionary.contains(x)) isIn = false; isIn} )
  }                                               //> spellCheckM: (doc#4908461: List#4519004[String#4579186], dictionary#4908462
                                                  //| : List#4519004[String#4579186])List#4519004[String#4579186]
  
  spellCheckM(List("hmm", "is", "this", "in", "it"), List("in", "it"))
                                                  //> res4: List#4519004[String#4579186] = List(hmm, is, this)
  
  
  //4
 	def evalMono(mono: (Double, Double), x: Double): Double = {
 		val power = mono._2
 		val coeff = mono._1
 		scala.math.pow(x, power)*coeff
 	}                                         //> evalMono: (mono#4908490: (Double#1218, Double#1218), x#4908491: Double#1218
                                                  //| )Double#1218
 	
 	evalMono((3,2),2)                         //> res5: Double#1218 = 12.0
 	evalMono((2,3),2)                         //> res6: Double#1218 = 16.0
 	evalMono((4,1),2)                         //> res7: Double#1218 = 8.0
 	
 	
 	def evalPoly(poly: List[(Double, Double)], x: Double):Double = {
 		var total = 0.0;
 		for (f <- poly) {
 			total += evalMono(f, x)
 		}
 		total
 	}                                         //> evalPoly: (poly#4908513: List#4519004[(Double#1218, Double#1218)], x#490851
                                                  //| 4: Double#1218)Double#1218
 	evalPoly(List((3,2),(2,3),(4,1)),2)       //> res8: Double#1218 = 36.0
 	//12 + 16 + 8
  
}