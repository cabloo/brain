function Neuron(weights){
	this.weights = weights
	this.last_inputs = new Array(0)
	this.output = 0.0
	this.bias_weight = Math.random() * 2 - 1
	this.getOutput = function(inputs) {
		this.last_inputs = inputs
		this.output = 0.0
		for( var i = 0; i < this.weights.length; i++ ){
			this.output += this.weights[i] * inputs[i];
		}

		this.output += this.bias_weight
		this.output = 1/(1+Math.exp(-this.output))
		return this.output
	}
}

function NeuronLayer(){
	this.neurons = new Array()
	this.add = function( neuron ){
		this.neurons.push(neuron)
	}
}

function NeuralNetwork(neurons_per_layers){
	this.count_layers = neurons_per_layer.length
	this.layers = new Array(this.count_layers)
	this.learning_rate = .4
	this.error_sum = 0.0
	this.inputs = []
	this.expected_outputs = []

	for( var i = 0; i < this.count_layers; i++ ){
		this.layers[i] = new NeuronLayer()
		var len = i > 0 ? this.layers[i-1].neurons.length : 0
		for( var j = 0; j < neurons_per_layer[i]; j++ ){
			var weights = new Array(0);
			if( i > 0 ){
				weights = new Array(len);
				for( var k = 0; k < len; k++ ){
					weights[k] = Math.random() * 2 - 1
				}
			}
			this.layers[i].add(new Neuron(weights))
		}
	}

	this.parse = function() {
		var current_outputs = this.get_output( this.inputs )

		this.error_sum = 0.0;

		for( var i = 0; i < current_outputs.length; i++ ){
			this.error_sum += this.expected_outputs[i] - current_outputs[i]
		}

		for( var l = this.count_layers - 1; l > 0; l-- ){
			var neurons = this.layers[l].neurons;
			for( var n = 0; n < neurons.length; n++ ){
				var neuron = neurons[n];
				for( var bond = 0; bond < neuron.last_inputs.length; bond++ ){
					this.layers[l].neurons[n].weights[bond] += this.learning_rate * this.error_sum * neuron.last_inputs[bond] * neuron.output * ( 1 - neuron.output )
				}

				this.layers[l].neurons[n].bias_weight += this.learning_rate * this.error_sum * neuron.output * ( 1 - neuron.output )
			}
		}

		//console.log("Error: " + this.error_sum)
	}

	this.parse_other = function( input, output ) {
		this.inputs = input
		this.expected_outputs = output
		this.parse()
	}

	this.get_output = function( input ) {
		var current_outputs = input
		for( var layer = 1; layer < this.count_layers; layer++ ){
			var l = this.layers[layer].neurons.length
			var new_outputs = new Array(l)
			for( var neuron = 0; neuron < l; neuron++ ){
				new_outputs[neuron] = this.layers[layer].neurons[neuron].getOutput(current_outputs)
			}

			current_outputs = new_outputs
		}

		return current_outputs
	}
	
	this.train = function(times, arr){
		for( var i = 0; i < times - 1; i++ ) {
			for( var io in arr ) {
				this.parse_other( io[0], io[1] )
			}
		}
	}
}

function check_ocr() {
	for( var i = 0; i < networks.length - 1; i++ ) {
		console.log(networks[i].get_output(active_boxes))
	}
}

var zero_1 = [0,1,1,1,0,0,1,0,1,0,0,1,0,1,0,0,1,0,1,0,0,1,1,1,0]
var two_1 = [0,1,1,1,0,0,0,0,1,0,0,1,1,1,0,0,1,0,0,0,0,1,1,1,0]
var three_1 = [0,1,1,1,0,0,0,0,1,0,0,1,1,1,0,0,0,0,1,0,0,1,1,1,0]
var four_1 = [0,1,0,1,0,0,1,0,1,0,0,1,1,1,0,0,0,0,1,0,0,0,0,1,0]

var networks = new Array(4)
for( var i = 0; i < networks.length - 1; i++ ) {
	networks[i] = new NeuralNetwork([25,5,5,1])
}

networks[0].train(1000,[[zero_1,1],[two_1,0],[three_1,0],[four_1,0]])
networks[1].train(1000,[[zero_1,0],[two_1,1],[three_1,0],[four_1,0]])
networks[2].train(1000,[[zero_1,0],[two_1,0],[three_1,1],[four_1,0]])
networks[3].train(1000,[[zero_1,0],[two_1,0],[three_1,0],[four_1,1]])

var active_boxes = new Array(boxes.length)
for( var i = 0; i < active_boxes.length - 1; i++ ) {
	active_boxes[i] = 0
}

var boxes = document.getElementsByTagName("a");
for( var i = 0; i < boxes.length - 1; i++ ){
	boxes[i].addEventListener("click", function(){
		this.className = this.className == "active" ? "" : "active"
		active_boxes[i] = this.className == "active" ? 1 : 0
		check_ocr(active_boxes)
	}, false)
}
