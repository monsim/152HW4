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
	}                                         //> sumCubesI: (list#5750659: List#5507676[Int#921])Int#921
	
	//recursive
	def sumCubesR(list: List[Int]):Int = {
		if (list == Nil) 0
		else {
			if (list.head % 2 == 1) list.head*list.head*list.head + sumCubesR(list.tail)
			else sumCubesR(list.tail)
		}
	}                                         //> sumCubesR: (list#5750695: List#5507676[Int#921])Int#921

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
	}                                         //> sumCubesT: (list#5750704: List#5507676[Int#921])Int#921
	
	//map filter
	def sumCubesM(list: List[Int]) = {
		//use filter to get odds
		//use map to cube
		//use reduce to sum
		list.filter(x => x % 2 == 1).map(x => x*x*x).reduce(_ + _)
	}                                         //> sumCubesM: (list#5750730: List#5507676[Int#921])Int#921
	
	
	sumCubesI(List(1, 3, 2))                  //> res0: Int#921 = 28
	sumCubesR(List(1, 3, 2))                  //> res1: Int#921 = 28
	sumCubesT(List(1, 3, 2))                  //> res2: Int#921 = 28
	sumCubesM(List(1, 3, 2))                  //> res3: Int#921 = 28


	
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
	}                                         //> sumOfSumsI: (list#5750926: List#5507676[List#5507676[Int#921]])Int#921
	
	
	//recursive
	def sumOfSumsR(list: List[List[Int]]): Int = {
		if (list == Nil) 0
		else {
			sum(list.head) + sumOfSumsR(list.tail)
		}
	}                                         //> sumOfSumsR: (list#5750978: List#5507676[List#5507676[Int#921]])Int#921
	
	//tail recursive
	def sumOfSumsT(list: List[List[Int]]) = {
		def helper(count: Int, result: Int): Int =
			if (list.length-1 < count) result
			else {
				helper(count+1, result+sum(list(count)))
			}
		helper(0,0)
	}                                         //> sumOfSumsT: (list#5750982: List#5507676[List#5507676[Int#921]])Int#921
	
	//map filter
	def sumOfSumsM(list: List[List[Int]]) = {
		//map and reduce
		list.map(sum).reduce(_ + _)
	}                                         //> sumOfSumsM: (list#5750999: List#5507676[List#5507676[Int#921]])Int#921
	
	def sum(list:List[Int]): Int = {		//finds sum of a list
		if (list == Nil) 0
		else {
			list.head + sum(list.tail)
		}
	}                                         //> sum: (list#5750979: List#5507676[Int#921])Int#921
	
	
	sumOfSumsI(List(List(1, 2, 3), List(4, 5, 6), List(3,2)))
                                                  //> res4: Int#921 = 26
  sumOfSumsR(List(List(1, 2, 3), List(4, 5, 6), List(3,2)))
                                                  //> res5: Int#921 = 26
  sumOfSumsT(List(List(1, 2, 3), List(4, 5, 6), List(3,2)))
                                                  //> res6: Int#921 = 26
	sumOfSumsM(List(List(1, 2, 3), List(4, 5, 6), List(3,2)))
                                                  //> res7: Int#921 = 26
	
	//3
	def depth(list: Any): Int = {
		list match {
			case Nil => 0
			case h::t => math.max(depth(h) + 1, depth(t))
			case _ => 0
		}
	}                                         //> depth: (list#5751182: Any#695)Int#921
	
	depth(List(List(List(1, 2), List(3))))    //> res8: Int#921 = 3
	depth(List(List(List(1, 2, List(3)))))    //> res9: Int#921 = 4
	
	
	//6
	
	//iterative
	def satisfyNumI[T](list: List[T], predicate: T=>Boolean):Int = {
		var total = 0;
		for (f <- list) {
			if (predicate(f)) total += 1
		}
		total
	}                                         //> satisfyNumI: [T#5750607](list#5751253: List#5507676[T#5750607], predicate#5
                                                  //| 751254: T#5750607 => Boolean#1740)Int#921
	
	//recursive
	def satisfyNumR[T](list: List[T], predicate: T=> Boolean) = {
		satNumR(list, predicate, 0, 0)
	}                                         //> satisfyNumR: [T#5750610](list#5751302: List#5507676[T#5750610], predicate#5
                                                  //| 751303: T#5750610 => Boolean#1740)Int#921

	def satNumR[T](list: List[T], predicate: T=>Boolean, index: Int, total:Int): Int = {
		if (list.length == index) total
		else {
			val value = list(index)
			if (predicate(value)) satNumR(list, predicate, index+1, total+1)
			else satNumR(list, predicate, index+1, total)
		}
	}                                         //> satNumR: [T#5750613](list#5751289: List#5507676[T#5750613], predicate#57512
                                                  //| 90: T#5750613 => Boolean#1740, index#5751291: Int#921, total#5751292: Int#9
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
	}                                         //> satisfyNumT: [T#5750616](list#5751360: List#5507676[T#5750616], predicate#5
                                                  //| 751361: T#5750616 => Boolean#1740)Int#921

	
	//map filter
	def satisfyNumM[T](list: List[T], predicate: T=> Boolean) = {
		//filter by predicate and return length
		list.filter(predicate).length
	
	}                                         //> satisfyNumM: [T#5750619](list#5751368: List#5507676[T#5750619], predicate#5
                                                  //| 751369: T#5750619 => Boolean#1740)Int#921
	
	
	satisfyNumI(List(2, 3, 4, 5), (x: Int) => x % 2 == 0)
                                                  //> res10: Int#921 = 2
  satisfyNumR(List(2, 3, 4, 5), (x: Int) => x % 2 == 0)
                                                  //> res11: Int#921 = 2
 	satisfyNumT(List(2, 3, 4, 5), (x: Int) => x % 2 == 0)
                                                  //> res12: Int#921 = 2
	satisfyNumM(List(2, 3, 4, 5), (x: Int) => x % 2 == 0)
                                                  //> res13: Int#921 = 2
	
	
	
	
	
	//7
	
	//iterative
	def satisfyAllI[T](list: List[T], predicate: T=>Boolean) = {
		//if input list is empty, return true
		var toReturn = true;
		for (f <- list) {
			if (predicate(f) == false) toReturn = false
		}
		toReturn
	}                                         //> satisfyAllI: [T#5750622](list#5751467: List#5507676[T#5750622], predicate#5
                                                  //| 751468: T#5750622 => Boolean#1740)Boolean#1740
	
	//recursive
	def satisfyAllR[T](list: List[T], predicate: T=>Boolean) = {
		satAllR(list, predicate, 0, true)
	}                                         //> satisfyAllR: [T#5750625](list#5751488: List#5507676[T#5750625], predicate#5
                                                  //| 751489: T#5750625 => Boolean#1740)Boolean#1740
	
	def satAllR[T](list: List[T], predicate: T=>Boolean, index: Int, result: Boolean): Boolean = {
		if (list.length == index) result
		else {
			if (predicate(list(index)) == false) satAllR(list, predicate, index+1, false)
			else satAllR(list, predicate, index+1, result)
		}
	}                                         //> satAllR: [T#5750628](list#5751475: List#5507676[T#5750628], predicate#57514
                                                  //| 76: T#5750628 => Boolean#1740, index#5751477: Int#921, result#5751478: Bool
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
	}                                         //> satisfyAllT: [T#5750631](list#5751526: List#5507676[T#5750631], predicate#5
                                                  //| 751527: T#5750631 => Boolean#1740)Boolean#1740
	
	//map filter
	def satisfyAllM[T](list: List[T], predicate: T=>Boolean) = {
		val test = list.length - list.filter(predicate).length
		test == 0
	}                                         //> satisfyAllM: [T#5750634](list#5751557: List#5507676[T#5750634], predicate#5
                                                  //| 751558: T#5750634 => Boolean#1740)Boolean#1740
	
	satisfyAllI(List(1,2,3,4,5), (x: Int) => x % 2 == 0)
                                                  //> res14: Boolean#1740 = false
	satisfyAllI(List(2,4,10), (x: Int) => x % 2 == 0)
                                                  //> res15: Boolean#1740 = true
	satisfyAllR(List(1,2,3,4,5), (x: Int) => x % 2 == 0)
                                                  //> res16: Boolean#1740 = false
	satisfyAllR(List(2,4,10), (x: Int) => x % 2 == 0)
                                                  //> res17: Boolean#1740 = true
  satisfyAllT(List(1,2,3,4,5), (x: Int) => x % 2 == 0)
                                                  //> res18: Boolean#1740 = false
	satisfyAllT(List(2,4,10), (x: Int) => x % 2 == 0)
                                                  //> res19: Boolean#1740 = true
	satisfyAllM(List(1,2,3,4,5), (x: Int) => x % 2 == 0)
                                                  //> res20: Boolean#1740 = false
	satisfyAllM(List(2,4,10), (x: Int) => x % 2 == 0)
                                                  //> res21: Boolean#1740 = true
	
	
	
	
	//8
	
	//iterative
	def satisfyAnyI[T](list: List[T], predicate: T=>Boolean) = {
		//empty list returns false
		var toReturn = false;
		for (f <- list) {
			if (predicate(f)) toReturn = true
		}
		toReturn
	}                                         //> satisfyAnyI: [T#5750637](list#5751722: List#5507676[T#5750637], predicate#5
                                                  //| 751723: T#5750637 => Boolean#1740)Boolean#1740
	
	
	//recursive
	def satisfyAnyR[T](list: List[T], predicate: T=>Boolean) = {
		satAnyR(list, predicate, 0, false)
	}                                         //> satisfyAnyR: [T#5750640](list#5751743: List#5507676[T#5750640], predicate#5
                                                  //| 751744: T#5750640 => Boolean#1740)Boolean#1740
	
	def satAnyR[T](list: List[T], predicate: T=>Boolean, index: Int, result: Boolean):Boolean = {
		if (list.length == index) result
			else if (result) result 		//necessary?
			else {
				if (predicate(list(index)) == true) satAnyR(list, predicate, index+1, true)
				else satAnyR(list, predicate, index+1, result)
			}
	}                                         //> satAnyR: [T#5750643](list#5751730: List#5507676[T#5750643], predicate#57517
                                                  //| 31: T#5750643 => Boolean#1740, index#5751732: Int#921, result#5751733: Bool
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
	}                                         //> satisfyAnyT: [T#5750646](list#5751798: List#5507676[T#5750646], predicate#5
                                                  //| 751799: T#5750646 => Boolean#1740)Boolean#1740
	
	//map filter
	def satisfyAnyM[T](list: List[T], predicate: T=>Boolean) = {
		val test = list.length - list.filter(predicate).length
		test != list.length
	}                                         //> satisfyAnyM: [T#5750649](list#5751809: List#5507676[T#5750649], predicate#5
                                                  //| 751810: T#5750649 => Boolean#1740)Boolean#1740
	
	
	satisfyAnyI(List(1,2,3,4,5), (x: Int) => x % 2 == 0)
                                                  //> res22: Boolean#1740 = true
	satisfyAnyI(List(3,7,9), (x: Int) => x % 2 == 0)
                                                  //> res23: Boolean#1740 = false
	satisfyAnyR(List(1,2,3,4,5), (x: Int) => x % 2 == 0)
                                                  //> res24: Boolean#1740 = true
	satisfyAnyR(List(3,7,11), (x: Int) => x % 2 == 0)
                                                  //> res25: Boolean#1740 = false
  satisfyAnyT(List(1,2,3,4,5), (x: Int) => x % 2 == 0)
                                                  //> res26: Boolean#1740 = true
	satisfyAnyT(List(3,7,11), (x: Int) => x % 2 == 0)
                                                  //> res27: Boolean#1740 = false
	satisfyAnyM(List(1,2,3,4,5), (x: Int) => x % 2 == 0)
                                                  //> res28: Boolean#1740 = true
	satisfyAnyM(List(3,7,1), (x: Int) => x % 2 == 0)
                                                  //> res29: Boolean#1740 = false
	
	
	
	//10
	def isSorted(list: List[Int]) = {
		isSortedR(list, 0, Int.MinValue, false, false)
	}                                         //> isSorted: (list#5751947: List#5507676[Int#921])Boolean#1740
	
	def isSortedR(list: List[Int], index: Int, prevNum: Int, sorted: Boolean, done: Boolean):Boolean = {
		if (list.length == index) sorted
		else if (done) sorted
		else {
			if (list(index) > prevNum) isSortedR(list, index+1, list(index), true, false)
			else isSortedR(list, index+1, list(index), false, true)    	//not sorted
		}
	}                                         //> isSortedR: (list#5751948: List#5507676[Int#921], index#5751949: Int#921, pr
                                                  //| evNum#5751950: Int#921, sorted#5751951: Boolean#1740, done#5751952: Boolean
                                                  //| #1740)Boolean#1740
	
	isSorted(List(1,2,4,5))                   //> res30: Boolean#1740 = true
	isSorted(List(1,0,4,10))                  //> res31: Boolean#1740 = false
	
	
	
	//13
	def infiniteOnes(n: Int): Stream[Int] = n #:: infiniteOnes(n)				//1s forever
                                                  //> infiniteOnes: (n#5751995: Int#921)Stream#5507690[Int#921]
	
	infiniteOnes(1).take(5).foreach(println _)//> 1
                                                  //| 1
                                                  //| 1
                                                  //| 1
                                                  //| 1
  def nonNeg(n: Int): Stream[Int] = n #:: nonNeg(n-1)
                                                  //> nonNeg: (n#5752035: Int#921)Stream#5507690[Int#921]
  nonNeg(0).take(5).foreach(println _)            //> 0
                                                  //| -1
                                                  //| -2
                                                  //| -3
                                                  //| -4
  def nonNegEven(n: Int): Stream[Int] = n #:: nonNegEven(n-2)
                                                  //> nonNegEven: (n#5752075: Int#921)Stream#5507690[Int#921]
  nonNegEven(0).take(5).foreach(println _)        //> 0
                                                  //| -2
                                                  //| -4
                                                  //| -6
                                                  //| -8
  def squares(n: Int): Stream[Int] = (n*n) #:: squares(n+1)
                                                  //> squares: (n#5752115: Int#921)Stream#5507690[Int#921]
  squares(0).take(5).foreach(println _)           //> 0
                                                  //| 1
                                                  //| 4
                                                  //| 9
                                                  //| 16
}