package gameTheory

object FiniteGame {
	//A and B are starting values of the two populations. w is how much fitness is affected by the outcome of the games
	//w=1 means that the outcome of the games determine fitness, w=0 would mean the games don't affect fitness.
	var A = 50.0	//starting values, must use as double so I can get all of the percentages, but will only be incresing in whole numbers
	var B = 50.0
	val w = 1.0			//graph both w = 1 and w = 0.01
	
	//values in the payoff matrix
	val beta = 1.0
	val epsilon = 0.0
	
	val a = 1.0     //two cooperators
	val b = 0.0		//cooperator with defector		
	val c = beta	//defector with cooperator
	val d = epsilon // two defectors, cancels out
	
	
	//val benefit = 0.90
	//val cost = 0.19
	
	//val a = benefit-cost //benefit/2.0
	//val b = -cost //0.0
	//val c = benefit
	//val d = 0.0  //(benefit-cost)/2
	
	//OR USE BENEFIT, COST, AND 0 FOR SNOWDRIFT OR HAWK-DOVE
	
	//averaged payoffs used with w to determine fitness
	var payoffAsum = 0.0
	var Acount = 1
	var payoffBsum = 0.0
	var Bcount = 1
	
	
	//val payoffMatrix = Array(Array(a, b), Array(c, d))		//going to ignore all this for a moment
	
	def PlayGame() {
		val AAmeet:Double = (A-1)/(A+B-1)
		val ABmeet:Double = B/(A+B-1)
		val BBmeet:Double = (B-1)/(A+B-1)
		
		var rand:Double = scala.util.Random.nextDouble()
		
		if(rand < AAmeet) {
			payoffAsum += a*2
			Acount += 2
		}
			else if(rand < (AAmeet+ABmeet)) {
				payoffAsum += b
				Acount += 1
				payoffBsum += c
				Bcount += 1
			}
				else {
					payoffBsum += d*2
					Bcount += 2
				}
	}
	
	def UpdatePop() {
		val PayoffA = payoffAsum/Acount
		val PayoffB = payoffBsum/Bcount
		val FitnessA = 1 - w + w*PayoffA
		val FitnessB = 1 - w + w*PayoffB
		
		//val offset = 1 - FitnessB  		//normalize B to 1, A to r
		val r = (1 - FitnessB) + FitnessA
		
		
		var rand:Double = scala.util.Random.nextDouble()	//reproduction
		val x:Double = (A*r)/(A*r + (A+B) - A)
		
		
		if(rand < x) {
		  A += 1
		}
		else {
		  B += 1
		}
		
		val y:Double = A/(A+B)
		rand = scala.util.Random.nextDouble()        //elimination
		if(rand < y) {
		  A -= 1
		}
		else {
		  B -= 1
		}
		
		
	}
	
	def Printstats(gen:Int) {
		val FA:Double = A/(A+B)
		val FB:Double = B/(A+B)
		println("Gen " + gen + ":  A: " + A + "   B: " + B + "   FA: " + FA + "   FB: " + FB)
		//println(gen + " " + FA + " " + FB)
		
	}
	
	def main(args: Array[String]): Unit = { 
		var t = 1
		while(((B > 0)&&(A > 0))) {
			
			for(x <- 1 until (A.toInt+B.toInt)) {
				PlayGame()
			}
			UpdatePop()
			Printstats(t)
			t += 1
		}
	}
	
}