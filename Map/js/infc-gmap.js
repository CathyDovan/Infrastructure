/**************
Infrastructure Canada Map 
Beta code
***************/

$(function(){
$(document).on("click", "a", function(e){e.preventDefault();});
/* Filters */
$(document).on("click", "#ion-btnPanel-show", function(){
	var filtxt = "Hide filters";
	$(this).attr("id", "ion-btnPanel-hide");
	$(this).attr("title", filtxt);
	$(this).text(filtxt);	
	$("#ion-panelLeft").removeClass("ion-hidden").addClass("ion-show");
});
	
$(document).on("click", "#ion-btnPanel-hide", function(){
	var filtxt = "Show filters";
	$(this).attr("id", "ion-btnPanel-show");
	$(this).attr("title", filtxt);
	$(this).text(filtxt);
	$("#ion-panelLeft").addClass("ion-hidden").removeClass("ion-show");
});	
	
/* Mobile menu */	
function MobileMenu(){
	var s = $(window).width();
		if(s<=650){
			var searchtxticon = $(".ion-search-wrapper button").has(".fa-search").html();
			var searchtxt = $(".ion-search-wrapper button").has(".fa-search").text();
			$(".ion-search-wrapper").removeClass("ion-show").addClass("ion-hidden");
			$(".ion-menu-links ul .ion-menu-search").remove();
			$(".ion-menu-links ul").prepend('<li id="ion-menu-search-show" class="ion-menu-search"><a href="#" title="'+searchtxt+'">'+searchtxticon+'</a></li>');
			
			$("#ion-btnPanel-hide").trigger("click");
			$("#ion-btnAggregate-hide").trigger("click");
			
		}else{
			$(".ion-search-wrapper").removeClass("ion-hidden").addClass("ion-show");						   
			$(".ion-menu-search").remove();
		}
}	
	
MobileMenu();

$(window).resize(function(){
		MobileMenu();
});	

/* Search */
$(document).on("click", "#ion-menu-search-show", function(){
	var TheSearchtxt = "Hide Search";
	$(this).attr("id", "ion-menu-search-hide");
	$(this).attr("title", TheSearchtxt);
	$(".ion-search-wrapper").removeClass("ion-hidden").addClass("ion-show");
});
	
$(document).on("click", "#ion-menu-search-hide", function(){
	var TheSearchtxt = "Show Search";
	$(this).attr("id", "ion-menu-search-show");
	$(this).attr("title", TheSearchtxt);
	$(".ion-search-wrapper").addClass("ion-hidden").removeClass("ion-show");
});
	
/* Aggregate bar */
$(document).on("click", "#ion-btnAggregate-show", function(){
	var TheBartxt = "Hide aggregate bar";
	$(this).attr("id", "ion-btnAggregate-hide");
	$(this).attr("title", TheBartxt);
	$(".ion-aggregate").removeClass("ion-aggregate-hide").addClass("ion-aggregate-show");
	$(this).find(".fa").removeClass("fa-angle-double-up").addClass("fa-angle-double-down");
	$("#map").addClass("map-default-height").removeClass("map-full-height");
});
	
$(document).on("click", "#ion-btnAggregate-hide", function(){
	var TheBartxt = "Show aggregate bar";
	$(this).attr("id", "ion-btnAggregate-show");
	$(this).attr("title", TheBartxt);
	$(".ion-aggregate").addClass("ion-aggregate-hide").removeClass("ion-aggregate-show");
	$(this).find(".fa").removeClass("fa-angle-double-down").addClass("fa-angle-double-up");
	$("#map").addClass("map-full-height").removeClass("map-default-height");
});
	
$(document).on("click", "button[type=reset]", function(){
	$(".ion-tabs input:checked").prop("checked", false);
	resetAggregate();
	filterChecker(checkedPlace, true);  
});	


	
	
});