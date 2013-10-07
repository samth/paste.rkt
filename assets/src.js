window.onload =
    function() {
        var footer1 = document.getElementById("credits");
        var footer2 = document.getElementById("creditsfull");
        var cb = function() { 
            footer1.style.display = "none";
            footer2.style.display = "inline";
	    footer1.removeEventListener("mouseover", cb);
        };
        footer1.addEventListener("mouseover", cb);
    };
