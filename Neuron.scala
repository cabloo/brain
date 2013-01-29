package brain

import scala.math.random
import scala.math.exp
import scala.actors.Actor
import scala.actors.Actor._

class Neuron(var weights: Array[Double]) extends Actor {
  var last_inputs: Array[Double] = new Array[Double](0)
  var output: Double = 0.0
  var bias_weight:Double = random * 2 - 1
  def act(){ Actor.loop{ receive {
	case inputs: Array[Double] =>
	  last_inputs = inputs
	  output = 0.0
	  for( i <- 0 to weights.length - 1 ){
	    output += weights(i) * inputs(i)
	  }
	  output += bias_weight
	  output = 1.0 / ( 1.0 + exp( -output ) )
	  reply(output)
	case _ => exit()
  }}}
}