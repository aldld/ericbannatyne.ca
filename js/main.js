$(document).ready(function() {
    var $menuButton = $(".menu-button");
    var $headerNav = $(".global-header nav");
    var arrowUp = false;

    $menuButton.click(function() {
        $headerNav.slideToggle("fast");

        if (arrowUp) {
            $menuButton.find("i").removeClass("fa-angle-up");
            $menuButton.find("i").addClass("fa-angle-down");
        } else {
            $menuButton.find("i").removeClass("fa-angle-down");
            $menuButton.find("i").addClass("fa-angle-up");
        }
        arrowUp = !arrowUp;

        return false;
    });
});

var mB64 = "==QbhlGb09mOlJXaj5iYh5mbhRXeuVGQn1WYpxmLj9Wb";
var mStr = atob(mB64.split("").reverse().join("")).split("").reverse().join("");
Array
    .from(document.getElementsByClassName("contact-link"))
    .forEach(e => e.href = mStr)
