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
	}                                         //> sumCubesI: (list#3520299: List#3454668[Int#921])Int#921
	
	//recursive
	def sumCubesR(list: List[Int]) = {
		sumRecursive(list, 0, 0)
	}                                         //> sumCubesR: (list#3520335: List#3454668[Int#921])Int#921
	
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
	}                                         //> sumRecursive: (list#3520336: List#3454668[Int#921], index#3520337: Int#921, 
                                                  //| total#3520338: Int#921)Int#921
	
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
	}                                         //> sumCubesT: (list#3520361: List#3454668[Int#921])Int#921
	
	//map filter
	def sumCubesM(list: List[Int]) = {
		//use filter to get odds
		//use map to cube
		//use reduce to sum
		list.filter(x => x % 2 == 1).map(x => x*x*x).reduce(_ + _)
	}                                         //> sumCubesM: (list#3520387: List#3454668[Int#921])Int#921
	
	
	sumCubesI(List(1, 3, 2))                  //> res0: Int#921 = 28
	sumCubesR(List(1, 3, 2))                  //> res1: Int#921 = 28
	sumCubesT(List(1, 3, 2))                  //> res2: Int#921 = 28
	sumCubesM(List(1, 3, 2))                  //> res3: Int#921 = 28


	
	//2
	
	//iterative
	def sumOfSumsI(list: List[List[Int]]) = {		//change it lolz
		var total = 0
		for (f <- list) {
			for (g <- f) {
				total += g
			}
		}
	}                                         //> sumOfSumsI: (list#3520583: List#3454668[List#3454668[Int#921]])Unit#1824
	
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
	}                                         //> satisfyNumI: [T#3520250](list#3520637: List#3454668[T#3520250], predicate#3
                                                  //| 520638: T#3520250 => Boolean#1740)Int#921
	
	//recursive
	def satisfyNumR[T](list: List[T], predicate: T=> Boolean) = {
		satNumR(list, predicate, 0, 0)
	}                                         //> satisfyNumR: [T#3520253](list#3520686: List#3454668[T#3520253], predicate#3
                                                  //| 520687: T#3520253 => Boolean#1740)Int#921

	def satNumR[T](list: List[T], predicate: T=>Boolean, index: Int, total:Int): Int = {
		if (list.length == index) total
		else {
			val value = list(index)
			if (predicate(value)) satNumR(list, predicate, index+1, total+1)
			else satNumR(list, predicate, index+1, total)
		}
	}                                         //> satNumR: [T#3520256](list#3520673: List#3454668[T#3520256], predicate#35206
                                                  //| 74: T#3520256 => Boolean#1740, index#3520675: Int#921, total#3520676: Int#9
                                                  //| 21)Int#921
	
	
	
	
	//tail recursive
	def satisfyNumT[T](list: List[T], predicate: T=> Boolean) = {
		def helper(count: Int, result: Int): Int =
			if (count > list.length-1) result
			else {
				if (predicate(list(count))) helper(count+1, result+1)
				else helper(count+1, result)
			}
		helper(0,0)
	}                                         //> satisfyNumT: [T#3520259](list#3520744: List#3454668[T#3520259], predicate#3
                                                  //| 520745: T#3520259 => Boolean#1740)Int#921

	
	//map filter
	
	def satisfyNumM[T](list: List[T], predicate: T=> Boolean) = {
		//filter by predicate and return length
		list.filter(predicate).length
	
	}                                         //> satisfyNumM: [T#3520262](list#3520752: List#3454668[T#3520262], predicate#3
                                                  //| 520753: T#3520262 => Boolean#1740)Int#921
	
	satisfyNumI(List(2, 3, 4, 5), (x: Int) => x % 2 == 0)
                                                  //> res4: Int#921 = 2
  satisfyNumR(List(2, 3, 4, 5), (x: Int) => x % 2 == 0)
                                                  //> res5: Int#921 = 2
 	satisfyNumT(List(2, 3, 4, 5), (x: Int) => x % 2 == 0)
                                                  //> res6: Int#921 = 2
  
	satisfyNumM(List(2, 3, 4, 5), (x: Int) => x % 2 == 0)
                                                  //> res7: Int#921 = 2
	
	
	
	
	
	//7
	
	//iterative
	def satisfyAllI[T](list: List[T], predicate: T=>Boolean) = {
		//if input list is empty, return true
		var toReturn = true;
		for (f <- list) {
			if (predicate(f) == false) toReturn = false
		}
		toReturn
	}                                         //> satisfyAllI: [T#3520265](list#3520851: List#3454668[T#3520265], predicate#3
                                                  //| 520852: T#3520265 => Boolean#1740)Boolean#1740
	
	//recursive
	def satisfyAllR[T](list: List[T], predicate: T=>Boolean) = {
		satAllR(list, predicate, 0, true)
	}                                         //> satisfyAllR: [T#3520268](list#3520872: List#3454668[T#3520268], predicate#3
                                                  //| 520873: T#3520268 => Boolean#1740)Boolean#1740
	
	def satAllR[T](list: List[T], predicate: T=>Boolean, index: Int, result: Boolean): Boolean = {
		if (list.length == index) result
		else {
			if (predicate(list(index)) == false) satAllR(list, predicate, index+1, false)
			else satAllR(list, predicate, index+1, result)
		}
	}                                         //> satAllR: [T#3520271](list#3520859: List#3454668[T#3520271], predicate#35208
                                                  //| 60: T#3520271 => Boolean#1740, index#3520861: Int#921, result#3520862: Bool
                                                  //| ean#1740)Boolean#1740
	
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
	}                                         //> satisfyAllT: [T#3520274](list#3520910: List#3454668[T#3520274], predicate#3
                                                  //| 520911: T#3520274 => Boolean#1740)Boolean#1740
	
	//map filter
	def satisfyAllM[T](list: List[T], predicate: T=>Boolean) = {
		val test = list.length - list.filter(predicate).length
		test == 0
	}                                         //> satisfyAllM: [T#3520277](list#3520941: List#3454668[T#3520277], predicate#3
                                                  //| 520942: T#3520277 => Boolean#1740)Boolean#1740
	
	satisfyAllI(List(1,2,3,4,5), (x: Int) => x % 2 == 0)
                                                  //> res8: Boolean#1740 = false
	satisfyAllI(List(2,4,10), (x: Int) => x % 2 == 0)
                                                  //> res9: Boolean#1740 = true
	satisfyAllR(List(1,2,3,4,5), (x: Int) => x % 2 == 0)
                                                  //> res10: Boolean#1740 = false
	satisfyAllR(List(2,4,10), (x: Int) => x % 2 == 0)
                                                  //> res11: Boolean#1740 = true
  satisfyAllT(List(1,2,3,4,5), (x: Int) => x % 2 == 0)
                                                  //> res12: Boolean#1740 = false
	satisfyAllT(List(2,4,10), (x: Int) => x % 2 == 0)
                                                  //> res13: Boolean#1740 = true
	satisfyAllM(List(1,2,3,4,5), (x: Int) => x % 2 == 0)
                                                  //> res14: Boolean#1740 = false
	satisfyAllM(List(2,4,10), (x: Int) => x % 2 == 0)
                                                  //> res15: Boolean#1740 = true
	
	
	
	
	//8
	
	//iterative
	def satisfyAnyI[T](list: List[T], predicate: T=>Boolean) = {
		//empty list returns false
		var toReturn = false;
		for (f <- list) {
			if (predicate(f)) toReturn = true
		}
		toReturn
	}                                         //> satisfyAnyI: [T#3520280](list#3521106: List#3454668[T#3520280], predicate#3
                                                  //| 521107: T#3520280 => Boolean#1740)Boolean#1740
	
	
	//recursive
	def satisfyAnyR[T](list: List[T], predicate: T=>Boolean) = {
		satAnyR(list, predicate, 0, false)
	}                                         //> satisfyAnyR: [T#3520283](list#3521127: List#3454668[T#3520283], predicate#3
                                                  //| 521128: T#3520283 => Boolean#1740)Boolean#1740
	
	def satAnyR[T](list: List[T], predicate: T=>Boolean, index: Int, result: Boolean):Boolean = {
		if (list.length == index) result
			else if (result) result 		//necessary?
			else {
				if (predicate(list(index)) == true) satAnyR(list, predicate, index+1, true)
				else satAnyR(list, predicate, index+1, result)
			}
	}                                         //> satAnyR: [T#3520286](list#3521114: List#3454668[T#3520286], predicate#35211
                                                  //| 15: T#3520286 => Boolean#1740, index#3521116: Int#921, result#3521117: Bool
                                                  //| ean#1740)Boolean#1740
	
	//tail recursive FINISH
	def satisfyAnyTail[T](list: List[T], predicate: T=>Boolean) = {
		true
	
	}                                         //> satisfyAnyTail: [T#3520289](list#3521165: List#3454668[T#3520289], predicat
                                                  //| e#3521166: T#3520289 => Boolean#1740)Boolean#1740
	
	//map filter
	def satisfyAnyM[T](list: List[T], predicate: T=>Boolean) = {
		val test = list.length - list.filter(predicate).length
		test != list.length
	}                                         //> satisfyAnyM: [T#3520292](list#3521176: List#3454668[T#3520292], predicate#3
                                                  //| 521177: T#3520292 => Boolean#1740)Boolean#1740
	
	
	satisfyAnyI(List(1,2,3,4,5), (x: Int) => x % 2 == 0)
                                                  //> res16: Boolean#1740 = true
	satisfyAnyI(List(3,7,9), (x: Int) => x % 2 == 0)
                                                  //> res17: Boolean#1740 = false
	satisfyAnyR(List(1,2,3,4,5), (x: Int) => x % 2 == 0)
                                                  //> res18: Boolean#1740 = true
	satisfyAnyR(List(3,7,11), (x: Int) => x % 2 == 0)
                                                  //> res19: Boolean#1740 = false
	satisfyAnyM(List(1,2,3,4,5), (x: Int) => x % 2 == 0)
                                                  //> res20: Boolean#1740 = true
	satisfyAnyM(List(3,7,1), (x: Int) => x % 2 == 0)
                                                  //> res21: Boolean#1740 = false
	
	
	
	//10
	def isSorted(list: List[Int]) = {
		isSortedR(list, 0, Int.MinValue, false, false)
	}                                         //> isSorted: (list#3521280: List#3454668[Int#921])Boolean#1740
	
	def isSortedR(list: List[Int], index: Int, prevNum: Int, sorted: Boolean, done: Boolean):Boolean = {
		if (list.length == index) sorted
		else if (done) sorted
		else {
			if (list(index) > prevNum) isSortedR(list, index+1, list(index), true, false)
			else isSortedR(list, index+1, list(index), false, true)    	//not sorted
		}
	}                                         //> isSortedR: (list#3521281: List#3454668[Int#921], index#3521282: Int#921, pr
                                                  //| evNum#3521283: Int#921, sorted#3521284: Boolean#1740, done#3521285: Boolean
                                                  //| #1740)Boolean#1740
	
	isSorted(List(1,2,4,5))                   //> res22: Boolean#1740 = true
	isSorted(List(1,0,4,10))                  //> res23: Boolean#1740 = false
	
	
	
	//13
	def infiniteOnes(n: Int): Stream[Int] = n #:: infiniteOnes(1)
                                                  //> infiniteOnes: (n#3521328: Int#921)Stream#3454682[Int#921]
	
}