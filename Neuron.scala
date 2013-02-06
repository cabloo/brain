package brain

import scala.math.random
import scala.math.exp
import scala.actors.Actor
import scala.actors.Actor._
import scala.actors.remote._
import scala.actors.remote.RemoteActor._

case class NewInput(in: Double)
case class UpdateError(err: Double)
case class GetState()
case class IsComputed()
case class StartComputing()
case class IsUpdated()
case class StartUpdating()
case class GetOutput()

class Neuron(var weights: Array[Double], name: String, output_to: Array[String]) extends Actor {
  RemoteActor.classLoader = getClass().getClassLoader()
  var last_inputs: Array[Double] = new Array[Double](weights.length)
  var output: Double = 0.0
  var bias_weight:Double = random * 2 - 1
  var is_computed = false
  var input_ct = 0
  var is_updated = true
  def act(){
	alive(8000)
	register(Symbol(name), self)
	Actor.loop{ react {
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
		  if( output_to.length > 0 ) {
			for( neuron <- 0 until output_to.length ) {
			  select(Node("127.0.0.1",8000),Symbol(output_to(neuron))) ! NewInput(output)
			}
		  }
		  input_ct = 0
		  is_computed = true
		}
	  case UpdateError(err) =>
		is_updated = false
		for( bond <- 0 until last_inputs.length ){
		  weights(bond) += err * last_inputs(bond) * output * ( 1 - output )
		}
		bias_weight += err * output * ( 1 - output )
		is_updated = true
	  //case GetState() => getState().toString
	  case IsComputed() => reply( is_computed )
	  case IsUpdated() => reply( is_updated )
	  case StartComputing() => is_computed = false
	  case StartUpdating() => is_updated = false
	  case GetOutput() => reply( output )
	  case _ => exit()
  }}}
}
