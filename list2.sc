// questions 1, 2, 3, 4

object list2 {
  println("Welcome to the Scala worksheet")
  
  //1
  def avg(scores: List[Double]): Double = {
  		var total = 0.0
  		for (f <- scores) total += f
  		total/scores.length
  }
  
  
  def avgAvg(scores: List[List[Double]]): List[Double] = {	//list of avgs for each student
  		//use map
  		scores.map(avg)
  }
 
 	//is there a way to make this 93, 89, 90 but still work as a list of doubles? without generics in the function header
  var cs152 = List(List(93.0, 89.0, 90.0), List(75.0, 76.0, 68.0), List(88.0, 82.0, 78.0))
  avgAvg(cs152)
                                           
  def passing(scores: List[List[Double]]): List[Double] = {		//should be List[Int], not working
  		//list of positions in the list with avg >= 70
  		//use filter
  		//first find average of everything, then find positions >= 70
  		scores.map(avg).filter(overSeventy)
  }
  def overSeventy(x: Double) = x >= 70
 
  
  passing(cs152)
  
  def sumSums(scores: List[List[Double]]):Double = {
  		scores.map(sum).reduce(_ + _)		//use map to get list of sums of the lists. use reduce to get the sum of the list
  }
  
  //is this ok that it's iterative?
  def sum(list: List[Double]): Double = {
  		var total = 0.0
  		for (f <- list) total += f
  		total
  }
  
  sumSums(cs152)
    
    
  //2
  def spellCheck(doc: List[String], dictionary: List[String]): List[String] = {
  		//use a filter to figure out what's not in dictionary
  		doc.filter(isInDict _)
  }
  
  def isInDict(doc: List[String], dictionary: List[String]): List
}