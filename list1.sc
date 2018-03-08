//questions 1, 2, 3, 6, 7, 8, 10, 13

object list1 {
  println("Welcome to the Scala worksheet")
	
	
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
	}
	
	//recursive
	def sumCubesR(list: List[Int]) = {
		sumRecursive(list, 0, 0)
	}
	
	def sumRecursive(list: List[Int], index: Int, total:Int):Int = {
		if (index == list.length) total
		else {
			val value = list(index)
			if (value % 2 == 1){	//odd
				val cube = value*value*value
				sumRecursive(list, index+1, total+cube)
			}
			else {
				sumRecursive(list, index+1, total)
			}
		}
	}
	
	//tail-recursive
	def sumCubesT(list: List[Int]):Int = {
		def helper(count: Int, result: Int): Int =
			if (list.length > count) result else {
				val value = list(count)
				if (value % 2 == 1) {	//odd
					val cube = value*value*value
					helper(count+1, result+cube)
				}
				else {
					helper(count+1, result)
				}
			}
		helper(0,0)
	}
	
	//map filter
	def sumCubesM(list: List[Int]) = {
		//use filter to get odds
		//use map to cube
		//use reduce to sum
		list.filter(x => x % 2 == 1).map(x => x*x*x).reduce(_ + _)
	}
	
	
	sumCubesI(List(1, 3, 2))
	sumCubesR(List(1, 3, 2))
	sumCubesT(List(1, 3, 2))
	sumCubesM(List(1, 3, 2))


	
	//2
	
	//iterative
	def sumOfSumsI(list: List[List[Int]]) = {		//change it lolz
		var total = 0
		for (f <- list) {
			for (g <- f) {
				total += g
			}
		}
	}
	
	/*
	//recursive
	def sumOfSumR(list: List[List[Int]]) = {
		sumOfSumRecursive(list, 0, 0)
	
	}
	
	def sumOfSumRecursive(list: List[List[Int]], index: Int, total: Int): Int = {
		//sumOfSumRecursive(list.drop(1), total+list(0)(0))
		nil
	}
	*/
	
	//tail recursive
	
	
	//map filter
	
	
	//3
	
	
	
	//6
	
	//iterative
	def satisfyNumI[T](list: List[T], predicate: T=>Boolean):Int = {
		var total = 0;
		for (f <- list) {
			if (predicate(f)) total += 1
		}
		total
	}
	
	//recursive
	def satisfyNumR[T](list: List[T], predicate: T=> Boolean) = {
		satNumR(list, predicate, 0, 0)
	}

	def satNumR[T](list: List[T], predicate: T=>Boolean, index: Int, total:Int): Int = {
		if (list.length == index) total
		else {
			val value = list(index)
			if (predicate(value)) satNumR(list, predicate, index+1, total+1)
			else satNumR(list, predicate, index+1, total)
		}
	}
	
	
	
	/*
	//tail recursive
	def satisfyNumT[T](list: List[T], predicate: T=> Boolean) = {
		satNumT(list, predicate)
	}

	def satNumT[T](list: List[T], predicate: T=>Boolean): Int = {
		
	}
	*/
	
	//map filter
	
	def satisfyNumM[T](list: List[T], predicate: T=> Boolean) = {
		//filter by predicate and return length
		list.filter(predicate).length
	
	}
	
	satisfyNumI(List(2, 3, 4, 5), (x: Int) => x % 2 == 0)
  satisfyNumR(List(2, 3, 4, 5), (x: Int) => x % 2 == 0)
  //TEST FOR TAIL RECURSION!!!!!!!!
  
	satisfyNumM(List(2, 3, 4, 5), (x: Int) => x % 2 == 0)
	
	
	
	
	
	//7
	
	//iterative
	def satisfyAllI[T](list: List[T], predicate: T=>Boolean) = {
		//if input list is empty, return true
		var toReturn = true;
		for (f <- list) {
			if (predicate(f) == false) toReturn = false
		}
		toReturn
	}
	
	//recursive
	def satisfyAllR[T](list: List[T], predicate: T=>Boolean) = {
		satAllR(list, predicate, 0, true)
	}
	
	def satAllR[T](list: List[T], predicate: T=>Boolean, index: Int, result: Boolean): Boolean = {
		if (list.length == index) result
		else {
			if (predicate(list(index)) == false) satAllR(list, predicate, index+1, false)
			else satAllR(list, predicate, index+1, result)
		}
	}
	
	//tail recursive
	
	
	//map filter
	def satisfyAllM[T](list: List[T], predicate: T=>Boolean) = {
		val test = list.length - list.filter(predicate).length
		test == 0
	}
	
	satisfyAllI(List(1,2,3,4,5), (x: Int) => x % 2 == 0)
	satisfyAllI(List(2,4,10), (x: Int) => x % 2 == 0)
	satisfyAllR(List(1,2,3,4,5), (x: Int) => x % 2 == 0)
	satisfyAllR(List(2,4,10), (x: Int) => x % 2 == 0)
	satisfyAllM(List(1,2,3,4,5), (x: Int) => x % 2 == 0)
	satisfyAllM(List(2,4,10), (x: Int) => x % 2 == 0)
	
	
	
	
	//8
	
	//iterative
	def satisfyAnyI[T](list: List[T], predicate: T=>Boolean) = {
		//empty list returns false
		var toReturn = false;
		for (f <- list) {
			if (predicate(f)) toReturn = true
		}
		toReturn
	}
	
	
	//recursive
	def satisfyAnyR[T](list: List[T], predicate: T=>Boolean, index: Int, result: Boolean):Boolean = {
		if (list.length == index) result
		else if (result) result 		//necessary?
		else {
			if (predicate(list(index)) == true) satisfyAnyR(list, predicate, index+1, true)
			else satisfyAnyR(list, predicate, index+1, result)
		}
	}
	
	//tail recursive
	
	
	//map filter
	
	def satisfyAnyM[T](list: List[T], predicate: T=>Boolean) = {
		val test = list.length - list.filter(predicate).length
		test != 0
	
	}
	
	satisfyAnyI(List(1,2,3,4,5), (x: Int) => x % 2 == 0)
	satisfyAnyI(List(2,4,10), (x: Int) => x % 2 == 0)
	satisfyAnyR(List(1,2,3,4,5), (x: Int) => x % 2 == 0)
	satisfyAnyR(List(2,4,10), (x: Int) => x % 2 == 0)
	satisfyAnyM(List(1,2,3,4,5), (x: Int) => x % 2 == 0)
	satisfyAnyM(List(2,4,10), (x: Int) => x % 2 == 0)
	
	
	
	//10
	def isSorted(list: List[Int]) = {
		isSortedR(list, 0, Int.MinValue, false, false)
	}
	
	def isSortedR(list: List[Int], index: Int, prevNum: Int, sorted: Boolean, done: Boolean):Boolean = {
		if (list.length == index) sorted
		else if (done) sorted
		else {
			if (list(index) > prevNum) isSortedR(list, index+1, list(index), true, false)
			else isSortedR(list, index+1, list(index), false, true)    	//not sorted
		}
	}
	
	isSorted(List(1,2,4,5))
	isSorted(List(1,0,4,10))
	
	
	
	//13
	
	
}