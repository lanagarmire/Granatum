var trackOutboundLink = function(url, type) {
	ga("send", "event", "click", type, url, {
		transport: "beacon",
		hitCallback: function() {
			document.location = url;
		}
	});
};

// onclick = "openSurveyOnce();"
var openSurveyOnce = function() {
	var wasSurveyOpened = "wasSurveyOpened";
	if (localStorage.getItem(wasSurveyOpened)) return true;
	localStorage.setItem(wasSurveyOpened, "yes");
	window.open("http://garmiregroup.org/granatum/survey", "_blank");
	return true;
};
