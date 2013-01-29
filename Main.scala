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
	  var n_six = new NeuralNetwork( Array( 25, 1000, 1000, 1000, 1 ) )
	  val zero = Array[Double](0,1,1,1,0,0,1,0,1,0,0,1,0,1,0,0,1,0,1,0,0,1,1,1,0)
	  val one = Array[Double](0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0)
	  val t = Array(1.0)
	  val f = Array(0.0)
	  n_six.learning_rate = .5
	  n_six.train( 1000, Map( zero->t, one->f ) )
	  time {
		println( n_six.get_output(zero)(0) + " | " + n_six.get_output(one)(0) )
	  }
	  n_six.killNeurons
	}
  }
}