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

function NeuralNetwork(neurons_per_layer){
	this.count_layers = neurons_per_layer.length
	this.layers = new Array(this.count_layers)
	this.learning_rate = .5
	this.error_sum = 0.0
	this.inputs = new Array(0)
	this.expected_outputs = new Array(0)

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
		for( var i = 0; i < times; i++ ) {
			for( var j = 0; j < arr.length; j++ ) {
				this.parse_other( arr[j][0], arr[j][1] )
			}
		}
	}
}

function check_ocr() {
	var highest = 0
	var highest_id = 0
	for( var i = 0; i < networks.length; i++ ) {
		var out = networks[i].get_output(active_boxes)
		if( out > highest ) {
			highest = out
			highest_id = i
		}
	}
	console.log( highest_id + " (" + (highest*100) + "%)" )
}

var zero_1 = [0,1,1,1,0,0,1,0,1,0,0,1,0,1,0,0,1,0,1,0,0,1,1,1,0]
var one_1 = [0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0]
var two_1 = [0,1,1,1,0,0,0,0,1,0,0,1,1,1,0,0,1,0,0,0,0,1,1,1,0]
var three_1 = [0,1,1,1,0,0,0,0,1,0,0,1,1,1,0,0,0,0,1,0,0,1,1,1,0]
var four_1 = [0,1,0,1,0,0,1,0,1,0,0,1,1,1,0,0,0,0,1,0,0,0,0,1,0]
var five_1 = [0,1,1,1,0,0,1,0,0,0,0,1,1,1,0,0,0,0,1,0,0,1,1,1,0]
var six_1 = [0,1,1,1,0,0,1,0,0,0,0,1,1,1,0,0,1,0,1,0,0,1,1,1,0]
var seven_1 = [0,1,1,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0]
var eight_1 = [0,1,1,1,0,0,1,0,1,0,0,1,1,1,0,0,1,0,1,0,0,1,1,1,0]
var nine_1 = [0,1,1,1,0,0,1,0,1,0,0,1,1,1,0,0,0,0,1,0,0,0,0,1,0]

var networks = new Array(10)
for( var i = 0; i < networks.length; i++ ) {
	networks[i] = new NeuralNetwork([25,10,10,1])
}

networks[0].train(1000,[[zero_1,[1]],[one_1,[0]],[two_1,[0]],[three_1,[0]],[four_1,[0]],[five_1,[0]],[six_1,[0]],[seven_1,[0]],[eight_1,[0]],[nine_1,[0]]])
networks[1].train(1000,[[zero_1,[0]],[one_1,[1]],[two_1,[1]],[three_1,[0]],[four_1,[0]],[five_1,[0]],[six_1,[0]],[seven_1,[0]],[eight_1,[0]],[nine_1,[0]]])
networks[2].train(1000,[[zero_1,[0]],[one_1,[0]],[two_1,[1]],[three_1,[0]],[four_1,[0]],[five_1,[0]],[six_1,[0]],[seven_1,[0]],[eight_1,[0]],[nine_1,[0]]])
networks[3].train(1000,[[zero_1,[0]],[one_1,[0]],[two_1,[0]],[three_1,[1]],[four_1,[0]],[five_1,[0]],[six_1,[0]],[seven_1,[0]],[eight_1,[0]],[nine_1,[0]]])
networks[4].train(1000,[[zero_1,[0]],[one_1,[0]],[two_1,[0]],[three_1,[0]],[four_1,[1]],[five_1,[0]],[six_1,[0]],[seven_1,[0]],[eight_1,[0]],[nine_1,[0]]])
networks[5].train(1000,[[zero_1,[0]],[one_1,[0]],[two_1,[0]],[three_1,[0]],[four_1,[0]],[five_1,[1]],[six_1,[0]],[seven_1,[0]],[eight_1,[0]],[nine_1,[0]]])
networks[6].train(1000,[[zero_1,[0]],[one_1,[0]],[two_1,[0]],[three_1,[0]],[four_1,[0]],[five_1,[0]],[six_1,[1]],[seven_1,[0]],[eight_1,[0]],[nine_1,[0]]])
networks[7].train(1000,[[zero_1,[0]],[one_1,[0]],[two_1,[0]],[three_1,[0]],[four_1,[0]],[five_1,[0]],[six_1,[0]],[seven_1,[1]],[eight_1,[0]],[nine_1,[0]]])
networks[8].train(1000,[[zero_1,[0]],[one_1,[0]],[two_1,[0]],[three_1,[0]],[four_1,[0]],[five_1,[0]],[six_1,[0]],[seven_1,[0]],[eight_1,[1]],[nine_1,[0]]])
networks[9].train(1000,[[zero_1,[0]],[one_1,[0]],[two_1,[0]],[three_1,[0]],[four_1,[0]],[five_1,[0]],[six_1,[0]],[seven_1,[0]],[eight_1,[0]],[nine_1,[1]]])

var boxes = document.getElementsByTagName("a");
for( var i = 0; i < boxes.length; i++ ){
	boxes[i].index = i;
	boxes[i].addEventListener("click", function(){
		this.className = this.className == "active" ? "" : "active"
		active_boxes[this.index] = this.className == "active" ? 1 : 0
		check_ocr(active_boxes)
	}, false)
}

var active_boxes = new Array(boxes.length)
for( var i = 0; i < active_boxes.length; i++ ) {
	active_boxes[i] = 0
}
