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
	time {
	  /*var n_six = new NeuralNetwork( Array( 25, 1000, 100, 1 ) )
	  //var n_six = new NeuralNetwork( Array( 25, 100, 1 ) )
	  val zero = Array[Double](0,1,1,1,0,0,1,0,1,0,0,1,0,1,0,0,1,0,1,0,0,1,1,1,0)
	  val two = Array[Double](0,1,1,1,0,0,0,0,1,0,0,1,1,1,0,0,1,0,0,0,0,1,1,1,0)
	  val t = Array(1.0)
	  val f = Array(0.0)*/

	  var xor = new NeuralNetwork( Array( 2, 3, 1 ) )
	  xor.learning_rate = .5
	  xor.train( 100, Map( Array(0.0,0.0)->Array(0.0), Array(0.0,1.0)->Array(1.0), Array(1.0,0.0)->Array(1.0), Array(1.0,1.0)->Array(0.0) ))
	  //n_six.learning_rate = .3
	  //n_six.train( 100, Map( zero->t, two->f ) )
	  time {
		//println( n_six.get_output(zero)(0) + " | " + n_six.get_output(two)(0) )
		xor.truth_table()
	  }
	  xor.killNeurons
	}
  }
}