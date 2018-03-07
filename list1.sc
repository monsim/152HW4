//questions 1, 2, 3, 6, 7, 8, 10, 13

object list1 {
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
	}                                         //> sumCubesI: (list#370851: List#358347[Int#921])Int#921
	
	//recursive
	def sumCubesR(list: List[Int]) = {
		sumRecursive(list, 0, 0)
	}                                         //> sumCubesR: (list#370887: List#358347[Int#921])Int#921
	
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
	}                                         //> sumRecursive: (list#370888: List#358347[Int#921], index#370889: Int#921, tot
                                                  //| al#370890: Int#921)Int#921
	
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
	}                                         //> sumCubesT: (list#370913: List#358347[Int#921])Int#921
	
	//map filter
	def sumCubesM(list: List[Int]) = {
		
	
	}                                         //> sumCubesM: (list#370938: List#358347[Int#921])Unit#1824
	
	
	sumCubesI(List(1, 3, 2))                  //> res0: Int#921 = 28
	sumCubesR(List(1, 3, 2))                  //> res1: Int#921 = 28
	sumCubesT(List(1, 3, 2))                  //> res2: Int#921 = 0


	//map filter


	
	//2
	
	//iterative
	def sumOfSumsI(list: List[List[Int]]) = {
		var total = 0
		for (f <- list) {
			for (g <- f) {
				total += g
			}
		}
	}                                         //> sumOfSumsI: (list#370960: List#358347[List#358347[Int#921]])Unit#1824
	
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
	}                                         //> satisfyNumI: [T#370824](list#371014: List#358347[T#370824], predicate#37101
                                                  //| 5: T#370824 => Boolean#1740)Int#921
	
	//recursive
	def satisfyNumR[T](list: List[T], predicate: T=> Boolean) = {
		satNumR(list, predicate, 0, 0)
	}                                         //> satisfyNumR: [T#370827](list#371063: List#358347[T#370827], predicate#37106
                                                  //| 4: T#370827 => Boolean#1740)Int#921

	def satNumR[T](list: List[T], predicate: T=>Boolean, index: Int, total:Int): Int = {
		if (list.length == index) total
		else {
			val value = list(index)
			if (predicate(value)) satNumR(list, predicate, index+1, total+1)
			else satNumR(list, predicate, index+1, total)
		}
	}                                         //> satNumR: [T#370830](list#371050: List#358347[T#370830], predicate#371051: T
                                                  //| #370830 => Boolean#1740, index#371052: Int#921, total#371053: Int#921)Int#9
                                                  //| 21
	
	/*
	//tail recursive
	def satisfyNumT[T](list: List[T], predicate: T=> Boolean) = {
		satNumT(list, predicate)
	}

	def satNumT[T](list: List[T], predicate: T=>Boolean): Int = {
		
	}
	*/
	
	//map filter
	
	
	//7
	
	//iterative
	def satisfyAllI[T](list: List[T], predicate: T=>Boolean) = {
		var toReturn = true;
		for (f <- list) {
			if (predicate(f) == false) toReturn = false
		}
		toReturn
	}                                         //> satisfyAllI: [T#370833](list#371129: List#358347[T#370833], predicate#37113
                                                  //| 0: T#370833 => Boolean#1740)Boolean#1740
	
	//recursive
	def satisfyAllR[T](list: List[T], predicate: T=>Boolean) = {
		satAllR(list, predicate, 0, true)
	}                                         //> satisfyAllR: [T#370836](list#371150: List#358347[T#370836], predicate#37115
                                                  //| 1: T#370836 => Boolean#1740)Boolean#1740
	
	def satAllR[T](list: List[T], predicate: T=>Boolean, index: Int, result: Boolean): Boolean = {
		if (list.length == index) result
		else {
			if (predicate(list(index)) == false) satAllR(list, predicate, index+1, false)
			else satAllR(list, predicate, index+1, result)
		}
	}                                         //> satAllR: [T#370839](list#371137: List#358347[T#370839], predicate#371138: T
                                                  //| #370839 => Boolean#1740, index#371139: Int#921, result#371140: Boolean#1740
                                                  //| )Boolean#1740
	
	//tail recursive
	
	
	//map filter
	
	//8
	
	//iterative
	def satisfyAnyI[T](list: List[T], predicate: T=>Boolean) = {
		var toReturn = false;
		for (f <- list) {
			if (predicate(f)) toReturn = true
		}
		toReturn
	}                                         //> satisfyAnyI: [T#370842](list#371213: List#358347[T#370842], predicate#37121
                                                  //| 4: T#370842 => Boolean#1740)Boolean#1740
	
	
	//recursive
	def satisfyAnyR[T](list: List[T], predicate: T=>Boolean, index: Int, result: Boolean):Boolean = {
		if (list.length == index) result
		else if (result) result 		//necessary?
		else {
			if (predicate(list(index)) == true) satisfyAnyR(list, predicate, index+1, true)
			else satisfyAnyR(list, predicate, index+1, result)
		}
	}                                         //> satisfyAnyR: [T#370845](list#371219: List#358347[T#370845], predicate#37122
                                                  //| 0: T#370845 => Boolean#1740, index#371221: Int#921, result#371222: Boolean#
                                                  //| 1740)Boolean#1740
	
	//tail recursive
	
	
	//map filter
	
	
	//10
	def isSorted(list: List[Int]) = {
		isSortedR(list, 0, Int.MinValue, false, false)
	}                                         //> isSorted: (list#371257: List#358347[Int#921])Boolean#1740
	
	def isSortedR(list: List[Int], index: Int, prevNum: Int, sorted: Boolean, done: Boolean):Boolean = {
		if (list.length == index) sorted
		else if (done) sorted
		else {
			if (list(index) > prevNum) isSortedR(list, index+1, list(index), true, false)
			else isSortedR(list, index+1, list(index), false, true)    	//not sorted
		}
	}                                         //> isSortedR: (list#371258: List#358347[Int#921], index#371259: Int#921, prevN
                                                  //| um#371260: Int#921, sorted#371261: Boolean#1740, done#371262: Boolean#1740)
                                                  //| Boolean#1740
	
	isSorted(List(1,2,4,5))                   //> res3: Boolean#1740 = true
	isSorted(List(1,0,4,10))                  //> res4: Boolean#1740 = false
	
	
	
	//13
	
	
}