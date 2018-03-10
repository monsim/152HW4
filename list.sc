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
	}                                         //> sumCubesI: (list#4684412: List#4519004[Int#921])Int#921
	
	//recursive
	def sumCubesR(list: List[Int]) = {
		sumRecursive(list, 0, 0)
	}                                         //> sumCubesR: (list#4684448: List#4519004[Int#921])Int#921
	
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
	}                                         //> sumRecursive: (list#4684449: List#4519004[Int#921], index#4684450: Int#921, 
                                                  //| total#4684451: Int#921)Int#921
	
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
	}                                         //> sumCubesT: (list#4684474: List#4519004[Int#921])Int#921
	
	//map filter
	def sumCubesM(list: List[Int]) = {
		//use filter to get odds
		//use map to cube
		//use reduce to sum
		list.filter(x => x % 2 == 1).map(x => x*x*x).reduce(_ + _)
	}                                         //> sumCubesM: (list#4684500: List#4519004[Int#921])Int#921
	
	
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
	}                                         //> sumOfSumsI: (list#4684696: List#4519004[List#4519004[Int#921]])Unit#1824
	
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
	def depth(list: Any): Int = {
  		list match {
  			case Nil => 0
   		//case h::t => if (depth((list asInstanceOf List).head) > depth(list.tail)) list.head+1 else list.tail
   		case _ => 0
  		}
	}                                         //> depth: (list#4684748: Any#695)Int#921
	
	
	
	//6
	
	//iterative
	def satisfyNumI[T](list: List[T], predicate: T=>Boolean):Int = {
		var total = 0;
		for (f <- list) {
			if (predicate(f)) total += 1
		}
		total
	}                                         //> satisfyNumI: [T#4684360](list#4684751: List#4519004[T#4684360], predicate#4
                                                  //| 684752: T#4684360 => Boolean#1740)Int#921
	
	//recursive
	def satisfyNumR[T](list: List[T], predicate: T=> Boolean) = {
		satNumR(list, predicate, 0, 0)
	}                                         //> satisfyNumR: [T#4684363](list#4684800: List#4519004[T#4684363], predicate#4
                                                  //| 684801: T#4684363 => Boolean#1740)Int#921

	def satNumR[T](list: List[T], predicate: T=>Boolean, index: Int, total:Int): Int = {
		if (list.length == index) total
		else {
			val value = list(index)
			if (predicate(value)) satNumR(list, predicate, index+1, total+1)
			else satNumR(list, predicate, index+1, total)
		}
	}                                         //> satNumR: [T#4684366](list#4684787: List#4519004[T#4684366], predicate#46847
                                                  //| 88: T#4684366 => Boolean#1740, index#4684789: Int#921, total#4684790: Int#9
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
	}                                         //> satisfyNumT: [T#4684369](list#4684858: List#4519004[T#4684369], predicate#4
                                                  //| 684859: T#4684369 => Boolean#1740)Int#921

	
	//map filter
	
	def satisfyNumM[T](list: List[T], predicate: T=> Boolean) = {
		//filter by predicate and return length
		list.filter(predicate).length
	
	}                                         //> satisfyNumM: [T#4684372](list#4684866: List#4519004[T#4684372], predicate#4
                                                  //| 684867: T#4684372 => Boolean#1740)Int#921
	
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
	}                                         //> satisfyAllI: [T#4684375](list#4684965: List#4519004[T#4684375], predicate#4
                                                  //| 684966: T#4684375 => Boolean#1740)Boolean#1740
	
	//recursive
	def satisfyAllR[T](list: List[T], predicate: T=>Boolean) = {
		satAllR(list, predicate, 0, true)
	}                                         //> satisfyAllR: [T#4684378](list#4684986: List#4519004[T#4684378], predicate#4
                                                  //| 684987: T#4684378 => Boolean#1740)Boolean#1740
	
	def satAllR[T](list: List[T], predicate: T=>Boolean, index: Int, result: Boolean): Boolean = {
		if (list.length == index) result
		else {
			if (predicate(list(index)) == false) satAllR(list, predicate, index+1, false)
			else satAllR(list, predicate, index+1, result)
		}
	}                                         //> satAllR: [T#4684381](list#4684973: List#4519004[T#4684381], predicate#46849
                                                  //| 74: T#4684381 => Boolean#1740, index#4684975: Int#921, result#4684976: Bool
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
	}                                         //> satisfyAllT: [T#4684384](list#4685024: List#4519004[T#4684384], predicate#4
                                                  //| 685025: T#4684384 => Boolean#1740)Boolean#1740
	
	//map filter
	def satisfyAllM[T](list: List[T], predicate: T=>Boolean) = {
		val test = list.length - list.filter(predicate).length
		test == 0
	}                                         //> satisfyAllM: [T#4684387](list#4685055: List#4519004[T#4684387], predicate#4
                                                  //| 685056: T#4684387 => Boolean#1740)Boolean#1740
	
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
	}                                         //> satisfyAnyI: [T#4684390](list#4685220: List#4519004[T#4684390], predicate#4
                                                  //| 685221: T#4684390 => Boolean#1740)Boolean#1740
	
	
	//recursive
	def satisfyAnyR[T](list: List[T], predicate: T=>Boolean) = {
		satAnyR(list, predicate, 0, false)
	}                                         //> satisfyAnyR: [T#4684393](list#4685241: List#4519004[T#4684393], predicate#4
                                                  //| 685242: T#4684393 => Boolean#1740)Boolean#1740
	
	def satAnyR[T](list: List[T], predicate: T=>Boolean, index: Int, result: Boolean):Boolean = {
		if (list.length == index) result
			else if (result) result 		//necessary?
			else {
				if (predicate(list(index)) == true) satAnyR(list, predicate, index+1, true)
				else satAnyR(list, predicate, index+1, result)
			}
	}                                         //> satAnyR: [T#4684396](list#4685228: List#4519004[T#4684396], predicate#46852
                                                  //| 29: T#4684396 => Boolean#1740, index#4685230: Int#921, result#4685231: Bool
                                                  //| ean#1740)Boolean#1740
	
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
	}                                         //> satisfyAnyT: [T#4684399](list#4685296: List#4519004[T#4684399], predicate#4
                                                  //| 685297: T#4684399 => Boolean#1740)Boolean#1740
	
	//map filter
	def satisfyAnyM[T](list: List[T], predicate: T=>Boolean) = {
		val test = list.length - list.filter(predicate).length
		test != list.length
	}                                         //> satisfyAnyM: [T#4684402](list#4685307: List#4519004[T#4684402], predicate#4
                                                  //| 685308: T#4684402 => Boolean#1740)Boolean#1740
	
	
	satisfyAnyI(List(1,2,3,4,5), (x: Int) => x % 2 == 0)
                                                  //> res16: Boolean#1740 = true
	satisfyAnyI(List(3,7,9), (x: Int) => x % 2 == 0)
                                                  //> res17: Boolean#1740 = false
	satisfyAnyR(List(1,2,3,4,5), (x: Int) => x % 2 == 0)
                                                  //> res18: Boolean#1740 = true
	satisfyAnyR(List(3,7,11), (x: Int) => x % 2 == 0)
                                                  //> res19: Boolean#1740 = false
  satisfyAnyT(List(1,2,3,4,5), (x: Int) => x % 2 == 0)
                                                  //> res20: Boolean#1740 = true
	satisfyAnyT(List(3,7,11), (x: Int) => x % 2 == 0)
                                                  //> res21: Boolean#1740 = false
	satisfyAnyM(List(1,2,3,4,5), (x: Int) => x % 2 == 0)
                                                  //> res22: Boolean#1740 = true
	satisfyAnyM(List(3,7,1), (x: Int) => x % 2 == 0)
                                                  //> res23: Boolean#1740 = false
	
	
	
	//10
	def isSorted(list: List[Int]) = {
		isSortedR(list, 0, Int.MinValue, false, false)
	}                                         //> isSorted: (list#4685445: List#4519004[Int#921])Boolean#1740
	
	def isSortedR(list: List[Int], index: Int, prevNum: Int, sorted: Boolean, done: Boolean):Boolean = {
		if (list.length == index) sorted
		else if (done) sorted
		else {
			if (list(index) > prevNum) isSortedR(list, index+1, list(index), true, false)
			else isSortedR(list, index+1, list(index), false, true)    	//not sorted
		}
	}                                         //> isSortedR: (list#4685446: List#4519004[Int#921], index#4685447: Int#921, pr
                                                  //| evNum#4685448: Int#921, sorted#4685449: Boolean#1740, done#4685450: Boolean
                                                  //| #1740)Boolean#1740
	
	isSorted(List(1,2,4,5))                   //> res24: Boolean#1740 = true
	isSorted(List(1,0,4,10))                  //> res25: Boolean#1740 = false
	
	
	
	//13
	def infiniteOnes(n: Int): Stream[Int] = n #:: infiniteOnes(n)				//1s forever
                                                  //> infiniteOnes: (n#4685493: Int#921)Stream#4519018[Int#921]
	
	infiniteOnes(1).take(5).foreach(println _)//> 1
                                                  //| 1
                                                  //| 1
                                                  //| 1
                                                  //| 1
  def nonNeg(n: Int): Stream[Int] = n #:: nonNeg(n-1)
                                                  //> nonNeg: (n#4685533: Int#921)Stream#4519018[Int#921]
  nonNeg(0).take(5).foreach(println _)            //> 0
                                                  //| -1
                                                  //| -2
                                                  //| -3
                                                  //| -4
  def nonNegEven(n: Int): Stream[Int] = n #:: nonNegEven(n-2)
                                                  //> nonNegEven: (n#4685573: Int#921)Stream#4519018[Int#921]
  nonNegEven(0).take(5).foreach(println _)        //> 0
                                                  //| -2
                                                  //| -4
                                                  //| -6
                                                  //| -8
  def squares(n: Int): Stream[Int] = (n*n) #:: squares(n+1)
                                                  //> squares: (n#4685613: Int#921)Stream#4519018[Int#921]
  squares(0).take(5).foreach(println _)           //> 0
                                                  //| 1
                                                  //| 4
                                                  //| 9
                                                  //| 16
}