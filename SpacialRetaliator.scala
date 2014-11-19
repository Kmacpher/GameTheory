package gameTheory

import scala.collection.mutable.ArrayStack
import java.awt.{Graphics2D,Color}
import java.awt.geom._
import scala.swing._

object SpatialRetaliator {

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
		
	val startPercentA = 0.33
	val startPercentB = 0.33
	
	val max = 100
	
	var A = 0.0
	var B = 0.0
	var C = 0.0
	//var A:Int = (startPercentA*100*100).toInt
	//var B:Int = ((1-startPercentA)*100*100).toInt
	
	//values in the payoff matrix
	val beta = 5.0
	val ep = 0.0
	
	val a = 1.0     //two cooperators
	val b = 0.0		//cooperator with defector
	val c = 1.0 - ep
	val d = 2
	val e = 1.0 - beta
	val f = 1.0-beta+ep
	val g = 1.0 + ep
	val h = 1.0-beta-ep
	val i = 1
	
	
	
	class Cell {
		var color = Color.blue
		var strategy = 'A'
		var nextStrategy = 'A'
		var payoffSum = 0.0
		val Neighbors = new ArrayStack[Cell]
		
	}
	
	val Cells = Array.fill[Cell](100,100) {new Cell}
	
def MakeSpace() {
		for (x <- 0 until max) {			
			for (y <- 0 until max) {
				val rand = scala.util.Random.nextDouble()
				if (rand < startPercentA) {
					Cells(x)(y).color = Color.blue   
					Cells(x)(y).strategy = 'A'
					A += 1
				}
				else if (rand < (startPercentA+startPercentB)){
					Cells(x)(y).color = Color.red
					Cells(x)(y).strategy = 'B'
					B += 1
				}
				else {
					Cells(x)(y).color = Color.green
					Cells(x)(y).strategy = 'C'
					C += 1
				}
			
				
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
		
	}
	
	def PlayGame(Individual: Cell) {
		
		for(neighbor <- Individual.Neighbors) {
		
			if (Individual.strategy == 'A') {
				if(neighbor.strategy == 'A') Individual.payoffSum += a
				if(neighbor.strategy == 'B') Individual.payoffSum += b
				if(neighbor.strategy == 'C') Individual.payoffSum += c
			}
			if (Individual.strategy == 'B') {
				if(neighbor.strategy == 'A') Individual.payoffSum += d
				if(neighbor.strategy == 'B') Individual.payoffSum += e
				if(neighbor.strategy == 'C') Individual.payoffSum += f
			}
			if (Individual.strategy == 'C') {
				if(neighbor.strategy == 'A') Individual.payoffSum += g
				if(neighbor.strategy == 'B') Individual.payoffSum += h
				if(neighbor.strategy == 'C') Individual.payoffSum += i
			}
		}
	}
	
	def UpdateCell(Individual:Cell) {
		
		val max = Individual.Neighbors.reduceRight((a, b) => if(a.payoffSum>b.payoffSum) a else b)
	
		if (max.payoffSum > Individual.payoffSum) {
			Individual.color = max.color
			
			if((max.strategy == 'A')&&(Individual.strategy == 'C')) {
				A += 1
				C -= 1
			}			
			if((max.strategy == 'A')&&(Individual.strategy == 'B')) {
				A += 1
				B -= 1
			}
			if((max.strategy == 'B') && (Individual.strategy == 'A')){
				A -= 1
				B += 1
			}
			if((max.strategy == 'B') && (Individual.strategy == 'C')){
				C -= 1
				B += 1
			}
			if((max.strategy == 'C') && (Individual.strategy == 'A')){
				A -= 1
				C += 1
			}
			if((max.strategy == 'C') && (Individual.strategy == 'B')){
				B -= 1
				C += 1
			}
			Individual.nextStrategy = max.strategy
		}	
		else {
			Individual.nextStrategy = Individual.strategy
			if (Individual.nextStrategy == 'A') Individual.color = Color.blue
			if (Individual.nextStrategy == 'B') Individual.color = Color.red
			if (Individual.nextStrategy == 'C') Individual.color = Color.green
			
		}
		
	}
	
	def Printstats(gen:Int) {
		//println("A is: " + A)
		//println("B is: " + B)
		val FA:Double = A/(A+B+C)
		val FB:Double = B/(A+B+C)
		val FC:Double = C/(A+B+C)
		println("Gen " + gen + ":  A: " + A + "   B: " + B + "   C: " + C + "   FA: " + FA + "   FB: " + FB)
		println(gen + " " + FA + " " + FB + " " + FC)
		
	}
	
	def main(args: Array[String]): Unit = { 
		var t = 1
		MakeSpace()
		frame.visible = true
		drawPanel.repaint()
		
		
		Printstats(t)
		while(((B > 0)&&(A > 0))||((C > 0)&&(B > 0))||((C > 0)&&(A > 0))) {
		//while(true){	
			for(x <- 0 until max) {
				for(y <- 0 until max)
				Cells(x)(y).payoffSum = 0.0
			}
			
			for(x <- 0 until max) {
				for(y <- 0 until max)
				PlayGame(Cells(x)(y))
			}
	/*		
			for (x <- 0 to 10){
				for(y <- 0 to 10) {
					print(Cells(x)(y).strategy + "     ")
				}
				println
			}
		*/	
			for(x <- 0 until max) {
				for(y <- 0 until max)
				UpdateCell(Cells(x)(y))
			}
			
			for(x <- 0 until max) {
				for(y <- 0 until max)
				Cells(x)(y).strategy = Cells(x)(y).nextStrategy
			}
		/* (x <- 0 to 10){
				for(y <- 47 to 51) {
					printf("%4.2f  ", Cells(x)(y).payoffSum)
				}
				println
			}
			
			for (x <- 0 to 10){
				for(y <- 47 to 51) {
					print(Cells(x)(y).strategy + "     ")
				}
				println
			}
			*/
			//readInt
			drawPanel.repaint()
			Thread.sleep(5)	
			Printstats(t)
			t += 1
		}
		
		drawPanel.repaint()
	
		
	}
  	
  	
}
