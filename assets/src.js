window.onload =
    function() {
        var footer1 = document.getElementById("credits");
        var footer2 = document.getElementById("creditsfull");
        
        footer1.addEventListener("mouseenter", function() { 
            footer1.style.display = "none";
            footer2.style.display = "inline";
        })
    };