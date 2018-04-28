import scala.math.{abs, cos, exp, sqrt, Pi}

object NumInt {

  def main(args: Array[String]) {
  	val numInt = new NumInt( (x: Double) => cos(2*x)/(exp(x)) )
  	val exactValue: Double = (1-exp(-2*Pi))/5
  	
  	val trapRule = numInt.trapRule(0, Pi * 2, 120)
  	val simpRule = numInt.simpRule(0, Pi * 2, 120)
  	val simpThreeEighths = numInt.simpThreeEighths(0, Pi * 2, 120)
  	val gaussianThreePoint = numInt.gaussianThreePoint(0, Pi * 2, 120)
  	
  	println("Trapezoidal rule sum = " + trapRule + "\n" +
  			"Error: " + abs(trapRule - exactValue) + "\n\n" +
  		
  			"Simpson's rule sum = " + simpRule + "\n" +
  			"Error: " + abs(simpRule - exactValue) + "\n\n" +
  			
  			"Simpson's 3/8 rule sum = " + simpThreeEighths + "\n" +
  			"Error: " + abs(simpThreeEighths - exactValue) + "\n\n" +
  			
  			"Gaussian three-point sum = " + gaussianThreePoint + "\n" +
  			"Error: " + abs(gaussianThreePoint - exactValue))
  }
  
  /*
  OUTPUT:
  
  	> Trapezoidal rule sum = 0.19985466252007325
	> Error: 2.281510664148556E-4
	>
	> Simpson's rule sum = 0.18945003737784433
	> Error: 0.010176474075814068
	>
	> Simpson's 3/8 rule sum = 0.19962649871517368
	> Error: 1.2738484711505293E-8
	> 
	> Gaussian three-point sum = 0.19124790575039158
	> Error: 0.008378605703266817
  */
  
}

class NumInt(f: Double => Double) {

	def trapRule(a: Double, b: Double, n: Int): Double = {
		val h = (b-a)/n
		val sum = (1 to n-1).map(i => 
			f(a+i*h)
			).sum
		(h/2)*(f(a)+f(b)) + h*sum
	}
	
	def simpRule(a: Double, b: Double, n: Int): Double = {
		val h = (b-a)/(2*n)
		val sum = (1 to (n/2)-1).map{i =>
			val xi = a+2*i*h
			val xj = a+(2*i-1)*h
			(2f/3)*h*f(xi) + (4f/3)*h*f(xj)
			}.sum
		(h/3)*(f(a)+f(b)) + sum
	}
	
	def simpThreeEighths(a: Double, b: Double, n: Int): Double = {
		if (n%3 != 0) println("n must be a multiple of 3")
		
		val h = (b-a)/(3*n)
		val sum = (1 to n).map{i =>
			val xi = a + (3*i-3)*h
			val xj = a + (3*i-2)*h
			val xk = a + (3*i-1)*h
			val xl = a + (3*i)*h
			f(xi) + 3*f(xj) + 3*f(xk) + f(xl)
			}.sum
		((3f*h)/8)*sum
	}
	
	def gaussianThreePoint(a: Double, b: Double, n: Int): Double = {
		if (n%2 != 0) println("n must be a multiple of 2")
		
		val h = (b-a)/(2*n)
		val k = h*sqrt(3f/5)
		val sum = (1 to (n/2)).map{i =>
			val xi = a + (2*i-1)*h
			(5f/9)*f(xi - h*k) + (5f/9)*f(xi + h*k) + (8f/9)*f(xi)
			}.sum
		h*sum
	}

}