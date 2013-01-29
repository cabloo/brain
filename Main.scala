package runtime

import brain._

object Main {
  def time[A](f: => A) = {
	val s = System.nanoTime
	val ret = f
	println( "Time: " + (System.nanoTime-s)/1e6 + "ms" );
	ret
  }
  /**
   * @param args the command line arguments
   */
  def main(args: Array[String]): Unit = {
	var xor = new NeuralNetwork( Array( 2, 3, 1 ) );
	time {
	  xor.train( 10000, Map( Array(0.0,1.0)->Array(1.0),Array(1.0,0.0)->Array(1.0),Array(1.0,1.0)->Array(0.0),Array(0.0,0.0)->Array(0.0)))
	}
	xor.truth_table
	xor.killNeurons
  }
}