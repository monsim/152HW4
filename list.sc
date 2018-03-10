//questions 1, 2, 3, 6, 7, 8, 10, 13

object list {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
	
	
	//1
	
	//iterative
	def sumCubesI(list: List[Int]) = {
		var total = 0;
		for (f <- list) {
			if (f % 2 ==1) { //odd
				val cube = f*f*f
				total += cube
			}
		}
		total
	}                                         //> sumCubesI: (list: List[Int])Int
	
	//recursive
	def sumCubesR(list: List[Int]):Int = {
		if (list == Nil) 0
		else {
			if (list.head % 2 == 1) list.head*list.head*list.head + sumCubesR(list.tail)
			else sumCubesR(list.tail)
		}
	}                                         //> sumCubesR: (list: List[Int])Int

	//tail-recursive
	def sumCubesT(list: List[Int]):Int = {
		def helper(count: Int, result: Int): Int =
			if (list.length-1 < count) result
			else {
				val value = list(count)
				if (value % 2 == 1) {		//odd
					val cube = value*value*value
					helper(count+1, result+cube)
				}
				else {
					helper(count+1, result)
				}
			}
		helper(0,0)
	}                                         //> sumCubesT: (list: List[Int])Int
	
	//map filter
	def sumCubesM(list: List[Int]) = {
		//use filter to get odds
		//use map to cube
		//use reduce to sum
		list.filter(x => x % 2 == 1).map(x => x*x*x).reduce(_ + _)
	}                                         //> sumCubesM: (list: List[Int])Int
	
	
	sumCubesI(List(1, 3, 2))                  //> res0: Int = 28
	sumCubesR(List(1, 3, 2))                  //> res1: Int = 28
	sumCubesT(List(1, 3, 2))                  //> res2: Int = 28
	sumCubesM(List(1, 3, 2))                  //> res3: Int = 28


	
	//2
	
	//iterative
	def sumOfSumsI(list: List[List[Int]]) = {
		var total = 0
		for (f <- list) {
			for (g <- f) {
				total += g
			}
		}
		total
	}                                         //> sumOfSumsI: (list: List[List[Int]])Int
	
	
	//recursive
	def sumOfSumsR(list: List[List[Int]]): Int = {
		if (list == Nil) 0
		else {
			sum(list.head) + sumOfSumsR(list.tail)
		}
	}                                         //> sumOfSumsR: (list: List[List[Int]])Int
	
	//tail recursive
	def sumOfSumsT(list: List[List[Int]]) = {
		def helper(count: Int, result: Int): Int =
			if (list.length-1 < count) result
			else {
				helper(count+1, result+sum(list(count)))
			}
		helper(0,0)
	}                                         //> sumOfSumsT: (list: List[List[Int]])Int
	
	//map filter
	def sumOfSumsM(list: List[List[Int]]) = {
		//map and reduce
		list.map(sum).reduce(_ + _)
	}                                         //> sumOfSumsM: (list: List[List[Int]])Int
	
	def sum(list:List[Int]): Int = {		//finds sum of a list
		list.reduce(_+_)
	}                                         //> sum: (list: List[Int])Int
	
	
	sumOfSumsI(List(List(1, 2, 3), List(4, 5, 6), List(3,2)))
                                                  //> res4: Int = 26
  sumOfSumsR(List(List(1, 2, 3), List(4, 5, 6), List(3,2)))
                                                  //> res5: Int = 26
  sumOfSumsT(List(List(1, 2, 3), List(4, 5, 6), List(3,2)))
                                                  //> res6: Int = 26
	sumOfSumsM(List(List(1, 2, 3), List(4, 5, 6), List(3,2)))
                                                  //> res7: Int = 26
	
	//3
	def depth(list: Any): Int = {
		list match {
			case Nil => 0
			case h::t => math.max(depth(h) + 1, depth(t))
			case _ => 0
		}
	}                                         //> depth: (list: Any)Int
	
	depth(List(List(List(1, 2), List(3))))    //> res8: Int = 3
	depth(List(List(List(1, 2, List(3)))))    //> res9: Int = 4
	
	
	//6
	
	//iterative
	def satisfyNumI[T](list: List[T], predicate: T=>Boolean):Int = {
		var total = 0;
		for (f <- list) {
			if (predicate(f)) total += 1
		}
		total
	}                                         //> satisfyNumI: [T](list: List[T], predicate: T => Boolean)Int
	
	//recursive
	def satisfyNumR[T](list: List[T], predicate: T=> Boolean) = {
		satNumR(list, predicate, 0, 0)
	}                                         //> satisfyNumR: [T](list: List[T], predicate: T => Boolean)Int

	def satNumR[T](list: List[T], predicate: T=>Boolean, index: Int, total:Int): Int = {
		if (list.length == index) total
		else {
			val value = list(index)
			if (predicate(value)) satNumR(list, predicate, index+1, total+1)
			else satNumR(list, predicate, index+1, total)
		}
	}                                         //> satNumR: [T](list: List[T], predicate: T => Boolean, index: Int, total: Int
                                                  //| )Int
	//tail recursive
	def satisfyNumT[T](list: List[T], predicate: T=> Boolean) = {
		def helper(count: Int, result: Int): Int =
			if (count > list.length-1) result
			else {
				if (predicate(list(count))) helper(count+1, result+1)
				else helper(count+1, result)
			}
		helper(0,0)
	}                                         //> satisfyNumT: [T](list: List[T], predicate: T => Boolean)Int

	
	//map filter
	def satisfyNumM[T](list: List[T], predicate: T=> Boolean) = {
		//filter by predicate and return length
		list.filter(predicate).length
	
	}                                         //> satisfyNumM: [T](list: List[T], predicate: T => Boolean)Int
	
	
	satisfyNumI(List(2, 3, 4, 5), (x: Int) => x % 2 == 0)
                                                  //> res10: Int = 2
  satisfyNumR(List(2, 3, 4, 5), (x: Int) => x % 2 == 0)
                                                  //> res11: Int = 2
 	satisfyNumT(List(2, 3, 4, 5), (x: Int) => x % 2 == 0)
                                                  //> res12: Int = 2
	satisfyNumM(List(2, 3, 4, 5), (x: Int) => x % 2 == 0)
                                                  //> res13: Int = 2
	
	
	
	
	
	//7
	
	//iterative
	def satisfyAllI[T](list: List[T], predicate: T=>Boolean) = {
		//if input list is empty, return true
		var toReturn = true;
		for (f <- list) {
			if (predicate(f) == false) toReturn = false
		}
		toReturn
	}                                         //> satisfyAllI: [T](list: List[T], predicate: T => Boolean)Boolean
	
	//recursive
	def satisfyAllR[T](list: List[T], predicate: T=>Boolean) = {
		satAllR(list, predicate, 0, true)
	}                                         //> satisfyAllR: [T](list: List[T], predicate: T => Boolean)Boolean
	
	def satAllR[T](list: List[T], predicate: T=>Boolean, index: Int, result: Boolean): Boolean = {
		if (list.length == index) result
		else {
			if (predicate(list(index)) == false) satAllR(list, predicate, index+1, false)
			else satAllR(list, predicate, index+1, result)
		}
	}                                         //> satAllR: [T](list: List[T], predicate: T => Boolean, index: Int, result: Bo
                                                  //| olean)Boolean
	
	//tail recursive
	def satisfyAllT[T](list: List[T], predicate: T=>Boolean): Boolean = {
		def helper(count: Int, result: Boolean): Boolean =
			if (list.length-1 < count) result
			else if (result == false) result
			else {
				if (predicate(list(count)) == false)  helper(count+1, false)
				else helper(count+1, result)
			}
		helper(0,true)
	}                                         //> satisfyAllT: [T](list: List[T], predicate: T => Boolean)Boolean
	
	//map filter
	def satisfyAllM[T](list: List[T], predicate: T=>Boolean) = {
		val test = list.length - list.filter(predicate).length
		test == 0
	}                                         //> satisfyAllM: [T](list: List[T], predicate: T => Boolean)Boolean
	
	satisfyAllI(List(1,2,3,4,5), (x: Int) => x % 2 == 0)
                                                  //> res14: Boolean = false
	satisfyAllI(List(2,4,10), (x: Int) => x % 2 == 0)
                                                  //> res15: Boolean = true
	satisfyAllR(List(1,2,3,4,5), (x: Int) => x % 2 == 0)
                                                  //> res16: Boolean = false
	satisfyAllR(List(2,4,10), (x: Int) => x % 2 == 0)
                                                  //> res17: Boolean = true
  satisfyAllT(List(1,2,3,4,5), (x: Int) => x % 2 == 0)
                                                  //> res18: Boolean = false
	satisfyAllT(List(2,4,10), (x: Int) => x % 2 == 0)
                                                  //> res19: Boolean = true
	satisfyAllM(List(1,2,3,4,5), (x: Int) => x % 2 == 0)
                                                  //> res20: Boolean = false
	satisfyAllM(List(2,4,10), (x: Int) => x % 2 == 0)
                                                  //> res21: Boolean = true
	
	
	
	
	//8
	
	//iterative
	def satisfyAnyI[T](list: List[T], predicate: T=>Boolean) = {
		//empty list returns false
		var toReturn = false;
		for (f <- list) {
			if (predicate(f)) toReturn = true
		}
		toReturn
	}                                         //> satisfyAnyI: [T](list: List[T], predicate: T => Boolean)Boolean
	
	
	//recursive
	def satisfyAnyR[T](list: List[T], predicate: T=>Boolean) = {
		satAnyR(list, predicate, 0, false)
	}                                         //> satisfyAnyR: [T](list: List[T], predicate: T => Boolean)Boolean
	
	def satAnyR[T](list: List[T], predicate: T=>Boolean, index: Int, result: Boolean):Boolean = {
		if (list.length == index) result
			else if (result) result 		//necessary?
			else {
				if (predicate(list(index)) == true) satAnyR(list, predicate, index+1, true)
				else satAnyR(list, predicate, index+1, result)
			}
	}                                         //> satAnyR: [T](list: List[T], predicate: T => Boolean, index: Int, result: Bo
                                                  //| olean)Boolean
	
	//tail recursive
	def satisfyAnyT[T](list: List[T], predicate: T=>Boolean) = {
		def helper(count: Int, result: Boolean): Boolean = {
			if (list.length-1 < count) result
			else if (result) result
			else {
				if(predicate(list(count))) helper(count+1, true)
				else helper(count+1, result)
			}
		}
		helper(0, false)
	}                                         //> satisfyAnyT: [T](list: List[T], predicate: T => Boolean)Boolean
	
	//map filter
	def satisfyAnyM[T](list: List[T], predicate: T=>Boolean) = {
		val test = list.length - list.filter(predicate).length
		test != list.length
	}                                         //> satisfyAnyM: [T](list: List[T], predicate: T => Boolean)Boolean
	
	
	satisfyAnyI(List(1,2,3,4,5), (x: Int) => x % 2 == 0)
                                                  //> res22: Boolean = true
	satisfyAnyI(List(3,7,9), (x: Int) => x % 2 == 0)
                                                  //> res23: Boolean = false
	satisfyAnyR(List(1,2,3,4,5), (x: Int) => x % 2 == 0)
                                                  //> res24: Boolean = true
	satisfyAnyR(List(3,7,11), (x: Int) => x % 2 == 0)
                                                  //> res25: Boolean = false
  satisfyAnyT(List(1,2,3,4,5), (x: Int) => x % 2 == 0)
                                                  //> res26: Boolean = true
	satisfyAnyT(List(3,7,11), (x: Int) => x % 2 == 0)
                                                  //> res27: Boolean = false
	satisfyAnyM(List(1,2,3,4,5), (x: Int) => x % 2 == 0)
                                                  //> res28: Boolean = true
	satisfyAnyM(List(3,7,1), (x: Int) => x % 2 == 0)
                                                  //> res29: Boolean = false
	
	
	
	//10
	def isSorted(list: List[Int]) = {
		isSortedR(list, 0, Int.MinValue, false, false)
	}                                         //> isSorted: (list: List[Int])Boolean
	
	def isSortedR(list: List[Int], index: Int, prevNum: Int, sorted: Boolean, done: Boolean):Boolean = {
		if (list.length == index) sorted
		else if (done) sorted
		else {
			if (list(index) > prevNum) isSortedR(list, index+1, list(index), true, false)
			else isSortedR(list, index+1, list(index), false, true)    	//not sorted
		}
	}                                         //> isSortedR: (list: List[Int], index: Int, prevNum: Int, sorted: Boolean, don
                                                  //| e: Boolean)Boolean
	
	isSorted(List(1,2,4,5))                   //> res30: Boolean = true
	isSorted(List(1,0,4,10))                  //> res31: Boolean = false
	
	
	
	//13
	def infiniteOnes(n: Int): Stream[Int] = n #:: infiniteOnes(n)				//1s forever
                                                  //> infiniteOnes: (n: Int)Stream[Int]
	
	infiniteOnes(1).take(5).foreach(println _)//> 1
                                                  //| 1
                                                  //| 1
                                                  //| 1
                                                  //| 1
  def nonNeg(n: Int): Stream[Int] = n #:: nonNeg(n-1)
                                                  //> nonNeg: (n: Int)Stream[Int]
  nonNeg(0).take(5).foreach(println _)            //> 0
                                                  //| -1
                                                  //| -2
                                                  //| -3
                                                  //| -4
  def nonNegEven(n: Int): Stream[Int] = n #:: nonNegEven(n-2)
                                                  //> nonNegEven: (n: Int)Stream[Int]
  nonNegEven(0).take(5).foreach(println _)        //> 0
                                                  //| -2
                                                  //| -4
                                                  //| -6
                                                  //| -8
  def squares(n: Int): Stream[Int] = (n*n) #:: squares(n+1)
                                                  //> squares: (n: Int)Stream[Int]
  squares(0).take(5).foreach(println _)           //> 0
                                                  //| 1
                                                  //| 4
                                                  //| 9
                                                  //| 16
}