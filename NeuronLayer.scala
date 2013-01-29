package brain

import scala.collection.mutable.ArrayBuffer

class NeuronLayer {
  var neurons = new ArrayBuffer[Neuron]()

  def add(neuron: Neuron) {
	neurons += neuron
  }
}