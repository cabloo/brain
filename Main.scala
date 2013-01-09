import scala.collection.mutable.ArrayBuffer
import scala.math.random
import scala.math.exp
import scala.math.abs

class Neuron(var weights: Array[Double]) {
  var last_inputs: Array[Double] = new Array[Double](0)
  var output: Double = 0.0
  var bias_weight:Double = random * 2 - 1
  def getOutput(inputs: Array[Double]): Double = {
	last_inputs = inputs
	output = 0.0
	for( i <- 0 to weights.length - 1 ){
	  output += weights(i) * inputs(i)
	}
	output += bias_weight
	output = 1.0 / ( 1.0 + exp( -output ) )
	output
  }
}

class NeuronLayer {
  var neurons = new ArrayBuffer[Neuron]()

  def add(neuron: Neuron) {
	neurons += neuron
  }
}

class NeuralNetwork(neurons_per_layer: Array[Int]) {
  var inputs = Array[Double]()
  var expected_outputs = Array[Double]()
  val count_layers = neurons_per_layer.length
  var layers = new Array[NeuronLayer](count_layers)
  var learning_rate = .25
  var error_sum = 0.0
  //Build the network

  for( i <- 0 to count_layers - 1 ){
	layers(i) = new NeuronLayer
	val len = if( i > 0 ) layers(i-1).neurons.length else 0
	for( j <- 0 to neurons_per_layer(i) - 1 ) {
	  var weights = Array[Double]()
	  if( i > 0 ){
		weights = new Array[Double](len)
		for( k <- 0 to len - 1 ){
		  weights(k) = random * 2 - 1
		}
	  }
	  layers(i).add(new Neuron(weights))
	}
  }

  def parse(in: Array[Double], out: Array[Double]) {
	inputs = in
	expected_outputs = out
	parse
  }

  def get_output(in: Array[Double]): Array[Double] = {
	var current_outputs = in

	for( layer <- 1 to count_layers - 1 ){
	  val l = layers(layer).neurons.length
	  var new_outputs = new Array[Double](l)
	  for( neuron <- 0 to l - 1 ){
		new_outputs(neuron) = layers(layer).neurons(neuron).getOutput(current_outputs)
	  }

	  current_outputs = new_outputs
	}

	current_outputs
  }

  def parse() {
	val current_outputs = get_output( inputs )
	error_sum = 0.0
	
	for( i <- 0 to current_outputs.length - 1 ){
          val error = expected_outputs(i) - current_outputs(i)
          error_sum += error
          if( abs(error) > .0005 ) {
          	for( l <- (1 to count_layers - 1).reverse ){
                  val neurons = layers(l).neurons
                  for( n <- 0 to neurons.length - 1 ){
                	if( !( l == count_layers - 1 && n != i ) ){
                  	  val neuron = neurons(n)
                  	  for( bond <- 0 to neuron.last_inputs.length - 1 ){
                          	layers(l).neurons(n).weights(bond) += learning_rate * error_sum * neuron.last_inputs(bond) * neuron.output * 
( 1 - neuron.output )
                  	  }
                  	  layers(l).neurons(n).bias_weight += learning_rate * error_sum * neuron.output * ( 1 - neuron.output )
		  	}
          	  }
		}
          }
        }
/*	for( i <- 0 to current_outputs.length - 1 ){
	  error_sum = expected_outputs(i) - current_outputs(i)
	}
	
	for( l <- (1 to count_layers - 1).reverse ){
	  val neurons = layers(l).neurons
	  for( n <- 0 to neurons.length - 1 ){
		val neuron = neurons(n)
		for( bond <- 0 to neuron.last_inputs.length - 1 ){
		  layers(l).neurons(n).weights(bond) += learning_rate * error_sum * neuron.last_inputs(bond) * neuron.output * ( 1 - neuron.output )
		}
		layers(l).neurons(n).bias_weight += learning_rate * error_sum * neuron.output * ( 1 - neuron.output )
	  }
	}*/
	println( "Error: " + error_sum )
  }

  def train(times: Int, in_out: Map[Array[Double],Array[Double]]) {
	for( i <- 0 to times - 1 ) {
	  for( (k,v) <- in_out ) {
		parse( k, v )
	  }
	}
  }
}
