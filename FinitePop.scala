package gameTheory

import scala.collection.mutable.ArrayStack

object FinitePop {
	var A:Double = 1	//starting values
	var B:Double = 2000
	var Total = A+B
	val r:Double = -0.5
	
	val Individuals = new ArrayStack[Boolean]		//going to ignore all this for a moment
	
	def MakePop(A: Int, B:Int) {
		for (x <- 0 until A) {
			Individuals.push(true)
		}
		for (y <- 0 until B) {
			Individuals.push(false)
		}
	}
	
	def UpdatePop() {
		
		val x:Double = (A*r)/(A*r + (A+B) - A)
		
		var rand:Double = scala.util.Random.nextDouble()	//reproduction
		
		if(rand < x) A += 1
		else B += 1
		
		val y:Double = A/(A+B)
		rand = scala.util.Random.nextDouble()        //elimination
		if(rand < y) A -= 1
		else B -= 1
		
		
	}
	
	def Printstats(gen:Int) {
		val FA:Double = A/Total
		val FB:Double = B/Total
		println("Gen " + gen + ":  A: " + A + "   B: " + B + "   FA: " + FA + "   FB: " + FB)
		//println(gen + " " + FA + " " + FB)
		
	}
	
	def main(args: Array[String]): Unit = { 
		var t = 1
		while(((B > 0)&&(A > 0))) {
			UpdatePop()
			Printstats(t)
			t += 1
		}
		
	}
}