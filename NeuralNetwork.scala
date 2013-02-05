package brain

import scala.math.random
import scala.math.abs
import scala.actors.Future
import scala.actors.Futures._
import scala.actors.Actor
import scala.actors.Actor._

class NeuralNetwork(neurons_per_layer: Array[Int]) {
  var inputs = Array[Double]()
  var expected_outputs = Array[Double]()
  val count_layers = neurons_per_layer.length
  var layers = new Array[NeuronLayer](count_layers)
  var learning_rate = .3
  var error_sum = 0.0
  //Build the network

  System.setProperty("actors.maxPoolSize", "10000")

  for( i <- count_layers - 1 to 0 by -1  ){
	layers(i) = new NeuronLayer
	val len = if( i > 0 ) neurons_per_layer(i-1) else 0
	for( j <- 0 until neurons_per_layer(i) ) {
	  var weights = Array[Double]()
	  if( i > 0 ){
		weights = new Array[Double](len)
		for( k <- 0 until len ){
		  weights(k) = random * 2 - 1
		}
	  }

	  layers(i).add(new Neuron(weights,if( i == count_layers - 1 ) new NeuronLayer else layers(i+1) ))
	}
  }

  startNeurons

  def time[A](f: => A) = {
	val s = System.nanoTime
	val ret = f
	println( "Time: " + (System.nanoTime-s)/1e6 + "ms" );
	ret
  }

  def startNeurons(){
	for( layer <- 0 until count_layers ){
	  for( neuron <- 0 until layers(layer).neurons.length ){
		layers(layer).neurons(neuron).getState match {
		  case Actor.State.Terminated => layers(layer).neurons(neuron).restart
		  case Actor.State.New => layers(layer).neurons(neuron).start
		  case _ => //Still running
		}
	  }
	}
  }

  def killNeurons(){
	for( layer <- 0 until count_layers ){
	  for( neuron <- 0 until layers(layer).neurons.length ){
		layers(layer).neurons(neuron).getState match {
		  case Actor.State.Terminated =>
		  case Actor.State.New =>
		  case _ => layers(layer).neurons(neuron) ! None
		}
	  }
	}
  }

  def parse(in: Array[Double], out: Array[Double]): Double = {
	inputs = in
	expected_outputs = out
	parse
  }

  def still_computing(): Boolean = {
	for( neuron <- 0 until neurons_per_layer(layers.length - 1) ){
	  if( !layers(layers.length-1).neurons(neuron).is_computed ) return true
	}

	return false
  }

  def start_computing() {
	for( neuron <- 0 until neurons_per_layer(layers.length - 1) ){
	  layers(layers.length-1).neurons(neuron).is_computed = false
	}
  }

  def get_output(in: Array[Double]): Array[Double] = {
	val llc = neurons_per_layer(layers.length - 1)
	var current_outputs = Array[Double](llc)

	for( layer <- 0 until layers.length )
	  while( still_updating(layer) ){}

	for( neuron <- 0 until neurons_per_layer(0) ){
	  layers(0).neurons(neuron) ! NewInput( in(neuron) )
	}

	start_computing

	val f = future { while(still_computing()){} }

	//Sometimes there is a thread that gets blocked or something. Limiting run time to 10s can break it out of a lost thread
	awaitAll( 10000, f )

	for( neuron <- 0 until llc ){
	  current_outputs(neuron) = layers(layers.length-1).neurons(neuron).output
	}

	/*for( layer <- 1 until count_layers ){
	  val l = layers(layer).neurons.length
	  var futures = new Array[Future[Any]](l)
	  var new_outputs = new Array[Double](l)
	  for( neuron <- 0 until l ){
		/*layers(layer).neurons(neuron).getState match {
		  case Actor.State.Terminated => layers(layer).neurons(neuron).restart
		  case Actor.State.New => layers(layer).neurons(neuron).start
		  case _ => println( "Still Running!?" )
		}*/
		//time {
		  //layers(layer).neurons(neuron).start
		  futures(neuron) = layers(layer).neurons(neuron) !! current_outputs
		//}
	  }
		//awaitAll(100000000,futures: _*)
	  for( neuron <- 0 until l ){
	    new_outputs(neuron) = futures(neuron)().asInstanceOf[Double]
	  }

	  current_outputs = new_outputs
	}*/
	current_outputs
  }

  def still_updating(i: Int): Boolean = {
	for( l <- 1 until count_layers ){
	  for( n <- 0 until layers(l).neurons.length ){
		if( !( l == count_layers - 1 && n != i ) && !layers(l).neurons(n).is_updated ) return true
	  }
	}

	return false
  }

  def parse(): Double = {
	val current_outputs = get_output( inputs )
	error_sum = 0.0
	println( "Updating!" )

	for( i <- 0 until current_outputs.length ){
	  val error = expected_outputs(i) - current_outputs(i)
      error_sum += error
      for( l <- (1 until count_layers).reverse ){
		for( n <- 0 until layers(l).neurons.length ){
		  //if( !( l == count_layers - 1 && n != i ) ){
		    layers(l).neurons(n).is_updated = false
		    layers(l).neurons(n) ! UpdateError( learning_rate * error )
		  //}
		}
	  }
    }
	/*for( i <- 0 until current_outputs.length ){
	  error_sum = expected_outputs(i) - current_outputs(i)
	}

	for( l <- (1 until count_layers).reverse ){
	  val neurons = layers(l).neurons
	  for( n <- 0 until neurons.length ){
		val neuron = neurons(n)
		for( bond <- 0 until neuron.last_inputs.length ){
		  layers(l).neurons(n).weights(bond) += learning_rate * error_sum * neuron.last_inputs(bond) * neuron.output * ( 1 - neuron.output )
		}
		layers(l).neurons(n).bias_weight += learning_rate * error_sum * neuron.output * ( 1 - neuron.output )
	  }
	}*/
	error_sum
  }

  def train(times: Int, in_out: Map[Array[Double],Array[Double]]) {
	for( i <- 0 until times ) {
	  for( (k,v) <- in_out ) {
		parse( k, v )
	  }
	  println( "Test " + i + " complete." )
	}
  }

  def train(error_max: Double, in_out: Map[Array[Double],Array[Double]]) {
	var error:Double = error_max+1
	var count = 0
	while( error > error_max ) {
	  count += 1
	  error = 0.0
	  for( (k,v) <- in_out ) {
		error += abs( parse( k, v ) )
	  }
	  println( "Test " + count + ": " + error )
	}
  }

  def truth_table() {
	val n = neurons_per_layer(0)
	val o = neurons_per_layer(neurons_per_layer.length - 1)

	println( "_" * (n * 2 + o * 3 + 1) )
	print( "|" )
	for( i <- 0 until n ) {
	  print( i + "|" )
	}

	for( i <- 0 until o ) {
	  print( "O" + i + "|" )
	}
	print( "\n" )

	for( r <- 0 to 2^n - 1 ) {
	  val bin = (("%" + n + "s") format r.toBinaryString).replace(' ', '0')
	  var in = new Array[Double](n)

	  for( i <- 0 until n ) {
		in(i) = Integer.parseInt("" + bin(i),10).toDouble
		print( "|" + bin(i) )
	  }

	  val out = get_output( in )
	  for( i <- 0 until out.length ) {
		print( "|" + (("%" + (i.toString.length + 1) + "s") format out( i ).round) )
	  }

	  print( "|\n" )
	}

	println( "-" * (n * 2 + o * 3 + 1) )
  }
}