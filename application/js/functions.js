function Poll (argURL, argHandler) {
	var varURL = document.location.protocol + "//" + document.location.host + argURL;
	console.log("tam " + argURL);
	$.get(varURL, function(data) {
		var varTuple = eval(data);
		if(!varTuple[0]) {
			argHandler (varTuple[1]);
			Poll (argURL, argHandler);
		}   
	})  
}
