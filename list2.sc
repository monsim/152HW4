// questions 1, 2, 3, 4

object list2 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  //1
  def avg(scores: List[Double]): Double = {
  		var total = 0.0
  		for (f <- scores) total += f
  		total/scores.length
  }                                               //> avg: (scores#912418: List#726408[Double#1218])Double#1218
  
  
  def avgAvg(scores: List[List[Double]]): List[Double] = {	//list of avgs for each student
  		//use map
  		scores.map(avg)
  }                                               //> avgAvg: (scores#912448: List#726408[List#726408[Double#1218]])List#726408[Do
                                                  //| uble#1218]
 
 	//is there a way to make this 93, 89, 90 but still work as a list of doubles? without generics in the function header
  var cs152 = List(List(93.0, 89.0, 90.0), List(75.0, 76.0, 68.0), List(88.0, 82.0, 78.0))
                                                  //> cs152  : List#6684[List#6684[Double#1218]] = List(List(93.0, 89.0, 90.0), Li
                                                  //| st(75.0, 76.0, 68.0), List(88.0, 82.0, 78.0))
  avgAvg(cs152)                                   //> res0: List#726408[Double#1218] = List(90.66666666666667, 73.0, 82.6666666666
                                                  //| 6667)
                                           
  def passing(scores: List[List[Double]]): List[Double] = {		//should be List[Int]
  		//list of positions in the list with avg >= 70
  		//use filter
  		//first find average of everything, then find positions >= 70
  		scores.map(avg).filter(overSeventy)
  }                                               //> passing: (scores#912526: List#726408[List#726408[Double#1218]])List#726408[D
                                                  //| ouble#1218]
  def overSeventy(x: Double) = x >= 70            //> overSeventy: (x#912668: Double#1218)Boolean#1740
 
  
  passing(cs152)                                  //> res1: List#726408[Double#1218] = List(90.66666666666667, 73.0, 82.6666666666
                                                  //| 6667)
  
  def sumSums(scores: List[List[Double]]):Double = {
  		scores.map(sum).reduce(_ + _)		//use map to get list of sums of the lists. use reduce to get the sum of the list
  }                                               //> sumSums: (scores#912672: List#726408[List#726408[Double#1218]])Double#1218
  
  //is this ok that it's iterative?
  def sum(list: List[Double]): Double = {
  		var total = 0.0
  		for (f <- list) total += f
  		total
  }                                               //> sum: (list#912692: List#726408[Double#1218])Double#1218
  
  sumSums(cs152)                                  //> res2: Double#1218 = 739.0
                                      
}