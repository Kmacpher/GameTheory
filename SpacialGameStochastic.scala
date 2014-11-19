package gameTheory

import scala.collection.mutable.ArrayStack
import java.awt.{Graphics2D,Color}
import java.awt.geom._
import scala.swing._

object SpacialGameStochastic {

	val drawPanel = new Panel {
	
		override def paint(g:Graphics2D) {
			for(x <- 0 until max) {
				for (y <- 0 until max) {
					g.setPaint(Cells(x)(y).color)
					g.fill(new Rectangle2D.Double(x*5,y*5,10,10))
				}
			}
		}
	}
	
	val frame = new MainFrame {
		title = "Matrix"
		contents = drawPanel
		size = new Dimension((500),(500))
	}
		
	
	val max = 100
	val startPercentA = 0.50
	
	var A = 0.0
	var B = 0.0
	//var A:Int = (startPercentA*100*100).toInt
	//var B:Int = ((1-startPercentA)*100*100).toInt
	
	//values in the payoff matrix
	val beta = 1.50
	val epsilon = 0.0
	
	val a = 1.0     //two cooperators
	val b = 0.0		//cooperator with defector
	val c = beta
	val d = epsilon
	
	
	
	class Cell {
		var color = Color.blue
		var strategy = 'A'
		var payoffSum = 0.0
		val Neighbors = new ArrayStack[Cell]
		
	}
	
	//val Cells = new Array[Array[Cell]](100,100)
	val Cells = Array.fill[Cell](100,100) {new Cell}


	
	def MakeSpace() {
		for (x <- 0 until max) {			
			for (y <- 0 until max) {
				//val rand = scala.util.Random.nextDouble()
				//if (rand < startPercentA) {
				//	Cells(x)(y).color = Color.blue   
				//	Cells(x)(y).strategy = 'A'
				//	A += 1
				//}
				//else {
					Cells(x)(y).color = Color.red
					Cells(x)(y).strategy = 'B'
					B += 1
				//}
			
				
				if((y-1) >= 0) {
					Cells(x)(y).Neighbors.push(Cells(x)(y-1))
					//if((x-1) >= 0) Cells(x)(y).Neighbors.push(Cells(x-1)(y-1))
					//if((x+1) < max) Cells(x)(y).Neighbors.push(Cells(x+1)(y-1))
				}
				else Cells(x)(y).Neighbors.push(Cells(x)(max-1))
				if((y+1) < max) {
					Cells(x)(y).Neighbors.push(Cells(x)(y+1))
					//if((x-1) >= 0) Cells(x)(y).Neighbors.push(Cells(x-1)(y+1))
					//if((x+1) < max) Cells(x)(y).Neighbors.push(Cells(x+1)(y+1))
				}
				else Cells(x)(y).Neighbors.push(Cells(x)(0))
				if((x-1) >= 0) {
					Cells(x)(y).Neighbors.push(Cells(x-1)(y))
					if((y-1) >= 0) Cells(x)(y).Neighbors.push(Cells(x-1)(y-1))
					else Cells(x)(y).Neighbors.push(Cells(x-1)(max-1))
					
					if((y+1) < max) Cells(x)(y).Neighbors.push(Cells(x-1)(y+1))
					else Cells(x)(y).Neighbors.push(Cells(x-1)(0))		
				}
				else {
					Cells(x)(y).Neighbors.push(Cells(max-1)(y))
					if((y-1) >= 0) Cells(x)(y).Neighbors.push(Cells(max-1)(y-1))
					else Cells(x)(y).Neighbors.push(Cells(max-1)(max-1))
					
					if((y+1) < max) Cells(x)(y).Neighbors.push(Cells(max-1)(y+1))
					else Cells(x)(y).Neighbors.push(Cells(max-1)(0))					
				}
				
				if((x+1) < max) {
					Cells(x)(y).Neighbors.push(Cells(x+1)(y))
					if((y-1) >= 0) Cells(x)(y).Neighbors.push(Cells(x+1)(y-1))
					else Cells(x)(y).Neighbors.push(Cells(x+1)(max-1))
					if((y+1) < max) Cells(x)(y).Neighbors.push(Cells(x+1)(y+1))
					else Cells(x)(y).Neighbors.push(Cells(x+1)(0))
				}
				else {
					Cells(x)(y).Neighbors.push(Cells(0)(y))
					if((y-1) >= 0) Cells(x)(y).Neighbors.push(Cells(0)(y-1))
					else Cells(x)(y).Neighbors.push(Cells(0)(max-1))
					
					if((y+1) < max) Cells(x)(y).Neighbors.push(Cells(0)(y+1))
					else Cells(x)(y).Neighbors.push(Cells(0)(0))
				}
			}
		}
			
				for(x<- 47 to 51) {
					for(y <- 47 to 51) {
						Cells(x)(y).color = Color.blue
						Cells(x)(y).strategy = 'A'
						B -= 1
						A += 1	
					}
				}
	}
	
	def PlayGame(Individual: Cell) {
		
		for(neighbor <- Individual.Neighbors) {
			if (Individual.strategy == 'A') {
				if(neighbor.strategy == 'A') Individual.payoffSum += a
				if(neighbor.strategy == 'B') Individual.payoffSum += b
			}
			if (Individual.strategy == 'B') {
				if(neighbor.strategy == 'A') Individual.payoffSum += c
				if(neighbor.strategy == 'B') Individual.payoffSum += d
			}
			
		}
		
	}
	
	def Update(Individual:Cell) {
		
		val max = Individual.Neighbors.reduceRight((a, b) => if(a.payoffSum>b.payoffSum) a else b)
		
		if (max.payoffSum > Individual.payoffSum) {
			if((Individual.strategy == 'A') && (max.strategy == 'B')) Individual.color = Color.yellow
			if((Individual.strategy == 'B') && (max.strategy == 'A')) Individual.color = Color.green
			if((Individual.strategy == 'B') && (max.strategy == 'B')) Individual.color = Color.red
			if((Individual.strategy == 'A') && (max.strategy == 'A')) Individual.color = Color.blue
			
			if((max.strategy == 'A')&&(Individual.strategy == 'B')) {
				A += 1
				B -= 1
			}
			if((max.strategy == 'B') && (Individual.strategy == 'A')){
				A -= 1
				B += 1
			}
			Individual.strategy = max.strategy
		}	
		else {
			if(Individual.strategy == 'A') Individual.color = Color.blue
			if(Individual.strategy == 'B') Individual.color = Color.red
		}
		Individual.payoffSum = 0.0
	}
	
	def Printstats(gen:Int) {
		//println("A is: " + A)
		//println("B is: " + B)
		val FA:Double = A/(A+B)
		val FB:Double = B/(A+B)
		//println("Gen " + gen + ":  A: " + A + "   B: " + B + "   FA: " + FA + "   FB: " + FB)
		println(gen + " " + FA + " " + FB)
		
	}
	
	def main(args: Array[String]): Unit = { 
		var t = 1
		MakeSpace()
		frame.visible = true
		drawPanel.repaint()
		Thread.sleep(20)
		
		Printstats(t)
		while(((B > 0)&&(A > 0))) {
			
			for(x <- 0 until max) {
				for(y <- 0 until max)
				PlayGame(Cells(x)(y))
			}
			for(x <- 0 until 20) {
				val rand = scala.util.Random.nextInt(100)
				val rand2 = scala.util.Random.nextInt(100)
				Update(Cells(rand)(rand2))
			}
			drawPanel.repaint()
			//Thread.sleep(5)	
			
			
			Printstats(t)
			t += 1
		}
		
	
		
	}
  	
  	
}