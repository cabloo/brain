package brain

import scala.math.random
import scala.math.exp
import scala.actors.Actor
import scala.actors.Actor._

case class NewInput(in: Double)
case class UpdateError(err: Double)

class Neuron(var weights: Array[Double], next_layer: NeuronLayer) extends Actor {
  var output_neurons = next_layer.neurons
  var last_inputs: Array[Double] = new Array[Double](weights.length)
  var output: Double = 0.0
  var bias_weight:Double = random * 2 - 1
  var is_computed = false
  var input_ct = 0
  var is_updated = true
  def act(){ Actor.loop{ react {
		case NewInput(in) =>
		  input_ct += 1
		  if( last_inputs.length > 0 ) last_inputs(input_ct-1) = in
		  if( input_ct >= last_inputs.length ) {
				output = 0.0
				for( i <- 0 to weights.length - 1 ){
				  output += weights(i) * last_inputs(i)
				}
				output += bias_weight
				output = 1.0 / ( 1.0 + exp( -output ) )
				if( output_neurons.length > 0 ) {
				  for( neuron <- 0 until output_neurons.length ) {
					next_layer.neurons(neuron) ! NewInput(output)
				  }
				}
				input_ct = 0
				is_computed = true
		  }
		case UpdateError(err) =>
		  for( bond <- 0 until last_inputs.length ){
				weights(bond) += err * last_inputs(bond) * output * ( 1 - output )
		  }
		  bias_weight += err * output * ( 1 - output )
		  is_updated = true
		case _ => exit()
  }}}
}
