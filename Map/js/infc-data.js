/**************
Infrastructure Canada Map 
Beta code
***************/

var gmarkers1 = [];
var markers1 = [];

var filters = {
  shower: false,
  vault: false,
  flush: false
}


/**
 * Function to get data from JSON file and insert into map
 */


function getJSONData(){
	$.getJSON("json/infc-projects.json", function(json1) {

	for (i = 0; i < json1.length; i++) {
		markers1.push(json1[i]);
		addMarker(markers1[i]);
	}

	});	
}


/**
 * Function to init map
 */

function initMap() {
        map = new google.maps.Map(document.getElementById('map'), {
          	center: new google.maps.LatLng(58.7897104,-96.0892755),
          	zoom: 4,
          	mapTypeId: 'terrain',
			mapTypeControl: true,
    		mapTypeControlOptions: {
        	style: google.maps.MapTypeControlStyle.DROPDOWN_MENU,
        	position: google.maps.ControlPosition.RIGHT_TOP
    		}			
        });
	
		getJSONData();
  
}



/**
 * Function to add marker to map
 */

function addMarker(marker) {
  var region = marker["Region"];
  var category = marker["Asset.Category"];
  var projectName = marker["Project.Name"];
  var projectNum = marker["Project.Num"];
  var pos = new google.maps.LatLng(marker["Latitude"], marker["Longitude"]);
  var infeaMultiplierCat = marker['INFEA.Multiplier.Category'];
  var program = marker['Program'];
  var totalEligibleCost = marker['Total.Eligible.Costs'];
  var programContribution = marker['Program.Contribution'];
  var constructionStartDate = marker['Construction.Start.Date'];
  var constructionEndDate = marker['Construction.End.Date'];
  var totalJobsEligible = marker['Total.Jobs.Eligible'];
  var directJobsEligible = marker['Direct.Jobs.Eligible'];
  var totalValueAddedEligible = marker['Total.Value.Added.Eligible'];
  var directValueAddedEligible = marker['Direct.Value.Added.Eligible'];
  var totalCompensationEligible = marker['Total.Compensation.Eligible'];
  var directCompensationEligible = marker['Direct.Compensation.Eligible'];
  var importsContribution = marker['Imports.Contribution'];
  var taxesContribution = marker['Taxes.Contribution'];
  
  var content = "<h3 class='mrgn-tp-0'>" + projectName + " (" + projectNum + ")</h3>" +
                "<dl class=\"mrgn-tp-md dl-horizontal\">" +
    			"<dt>Asset Type:</dt> <dd>"+ infeaMultiplierCat +"</dd>" +
    			"<dt>Program:</dt> <dd>"+ program +"</dd>" +
    			"<dt>INFEA Asset Category:</dt> <dd>"+ category +"</dd>" +
    			"<dt>Total Eligible Cost:</dt> <dd>"+ totalEligibleCost +"</dd>" +
    			"<dt>Federal Contribution:</dt> <dd>"+ programContribution +"</dd>" +
    			"<dt>Forecasted Start Date:</dt> <dd>"+ constructionStartDate +"</dd>" +
    			"<dt>Forecasted End Date:</dt> <dd>"+ constructionEndDate +"</dd>" +
    			"<dt>Jobs created:</dt> <dd>"+ totalJobsEligible +" ("+ directJobsEligible +" direct)</dd>" +
    			"<dt>Value Added:</dt> <dd>"+ totalValueAddedEligible +" ("+ directValueAddedEligible +" direct)</dd>" +
    			"<dt>Employee compensation:</dt> <dd>"+ totalCompensationEligible +" ("+ directCompensationEligible +" direct)</dd>" +
    			"<dt>Imports:</dt> <dd>"+ importsContribution +"</dd>" +
				"<dt>Taxes on products:</dt> <dd>"+ taxesContribution +"</dd>" +
    			"</dl>"
  
  marker1 = new google.maps.Marker({
    projectName: projectName,
	projectNum: projectNum,
    position: pos,
    region: region,
    category: category,
	infeaMultiplierCat: infeaMultiplierCat,
	program: program,
	totalEligibleCost: totalEligibleCost,
	programContribution: programContribution,
	constructionStartDate: constructionStartDate,
	constructionEndDate: constructionEndDate,
	totalJobsEligible: totalJobsEligible,
	directJobsEligible: directJobsEligible,
	totalValueAddedEligible: totalValueAddedEligible,
	directValueAddedEligible: directValueAddedEligible,
	totalCompensationEligible: totalCompensationEligible,
	directCompensationEligible: directCompensationEligible,
	importsContribution: importsContribution,
	taxesContribution: taxesContribution,
    map: map
  });

  gmarkers1.push(marker1);

 infowindow = new google.maps.InfoWindow({
  content: ''
});
  // Marker click listener
  google.maps.event.addListener(marker1, 'click', (function(marker1, content) {
    return function() {
      console.log('Gmarker 1 gets pushed');
      infowindow.setContent(content);
      infowindow.open(map, marker1);
      map.panTo(this.getPosition());
      //map.setZoom(15);
    }
  })(marker1, content));
}


var get_set_options = function() {
  ret_array = []
  for (option in filters) {
    if (filters[option]) {
      ret_array.push(option)
    }
  }
  return ret_array;
}

var filter_markers = function() {
  set_filters = get_set_options()

  // for each marker, check to see if all required options are set
  for (i = 0; i < markers.length; i++) {
    marker = markers[i];

    // start the filter check assuming the marker will be displayed
    // if any of the required features are missing, set 'keep' to false
    // to discard this marker
    keep = true
    for (opt = 0; opt < set_filters.length; opt++) {
      if (!marker.properties[set_filters[opt]]) {
        keep = false;
      }
    }
    marker.setVisible(keep)
  }
}

theProject = 0;
theSpending = 0;
theJobs = 0;
theEconoB = 0;

function resetAggregate(){
	
	$("#ion-aggregate-projects strong").text("1001");
	$("#ion-aggregate-spending strong").text("1 billion $");
	$("#ion-aggregate-jobs strong").text("1345");
	$("#ion-aggregate-econob strong").text("457k");	
	
	theProject = 0;
	theSpending = 0;
	theJobs = 0;
	theEconoB = 0;	
	
}

// Fuction for checkboxes
$(document).on("change", ".ion-tabs input", function(){
  checkedPlace = [];
  var theChecked= $(".ion-tabs input:checked");

  if (theChecked.length == 0){
	  filterChecker(checkedPlace, true);  
  }else{
	  
  $.each(theChecked, function(key, data) {
		checkedPlace.push(data.value)//creating array of checked items
  });
  
  //console.log(checkedPlace);
  

  filterChecker(checkedPlace) //passing to function for updating markers
}  
	
//Aggregation concept for demo only. The idea would be to aggregate from the data object.		

//AB
if ($(this).val() == "AB"){
	
	if ($(this).prop("checked") == true){	
		theProject += 123;
		theSpending += 100;
		theJobs += 43;
		theEconoB += 36;
		
	}else{
		
		theProject -= 123;
		theSpending -= 100;
		theJobs -= 43;
		theEconoB -= 36;
			
	}
	
//BC	
}else if ($(this).val() == "BC"){


	if ($(this).prop("checked") == true){	
		
		theProject += 118;
		theSpending += 65;
		theJobs += 73;
		theEconoB += 36;
		
	}else{	
		
		theProject -= 118;
		theSpending -= 65;
		theJobs -= 73;
		theEconoB -= 36;	
	}
	
	
//MB	
}else if ($(this).val() == "MB"){


	if ($(this).prop("checked") == true){	
		
		theProject += 30;
		theSpending += 35;
		theJobs += 43;
		theEconoB += 22;
		
	}else{	
		
		theProject -= 30;
		theSpending -= 35;
		theJobs -= 43;
		theEconoB -= 22;	
	}
	
//NB
}else if ($(this).val() == "NB"){


	if ($(this).prop("checked") == true){	
		
		theProject += 30;
		theSpending += 85;
		theJobs += 53;
		theEconoB += 22;
		
	}else{	
		
		theProject -= 30;
		theSpending -= 85;
		theJobs -= 53;
		theEconoB -= 22;	
	}

//NL	
}else if ($(this).val() == "NL"){


	if ($(this).prop("checked") == true){	
		
		theProject += 31;
		theSpending += 35;
		theJobs += 23;
		theEconoB += 21;
		
	}else{	
		
		theProject -= 31;
		theSpending -= 35;
		theJobs -= 23;
		theEconoB -= 21;	
	}	
	
	
//NS	
}else if ($(this).val() == "NS"){


	if ($(this).prop("checked") == true){	
		
		theProject += 41;
		theSpending += 41;
		theJobs += 73;
		theEconoB += 43;
		
	}else{	
		
		theProject -= 41;
		theSpending -= 41;
		theJobs -= 73;
		theEconoB -= 43;	
	}	
	
//PE
}else if ($(this).val() == "PE"){


	if ($(this).prop("checked") == true){	
		
		theProject += 24;
		theSpending += 32;
		theJobs += 63;
		theEconoB += 17;
		
	}else{	
		
		theProject -= 24;
		theSpending -= 32;
		theJobs -= 63;
		theEconoB -= 17;	
	}	
	
//ON
}else if ($(this).val() == "ON"){


	if ($(this).prop("checked") == true){	
		
		theProject += 124;
		theSpending += 147;
		theJobs += 289;
		theEconoB += 105;
		
	}else{	
		
		theProject -= 124;
		theSpending -= 147;
		theJobs -= 289;
		theEconoB -= 105;	
	}
	
//QC
}else if ($(this).val() == "QC"){


	if ($(this).prop("checked") == true){	
		
		theProject += 224;
		theSpending += 212;
		theJobs += 393;
		theEconoB += 47;
		
	}else{	
		
		theProject -= 224;
		theSpending -= 212;
		theJobs -= 393;
		theEconoB -= 47;	
	}	

//SK
}else if ($(this).val() == "SK"){


	if ($(this).prop("checked") == true){	
		
		theProject += 64;
		theSpending += 62;
		theJobs += 83;
		theEconoB += 27;
		
	}else{	
		
		theProject -= 64;
		theSpending -= 62;
		theJobs -= 83;
		theEconoB -= 27;	
	}
	
//NT
}else if ($(this).val() == "NT"){


	if ($(this).prop("checked") == true){	
		
		theProject += 64;
		theSpending += 62;
		theJobs += 83;
		theEconoB += 27;
		
	}else{	
		
		theProject -= 64;
		theSpending -= 62;
		theJobs -= 83;
		theEconoB -= 27;	
	}
	
//NU
}else if ($(this).val() == "NU"){


	if ($(this).prop("checked") == true){	
		
		theProject += 64;
		theSpending += 62;
		theJobs += 63;
		theEconoB += 27;
		
	}else{	
		
		theProject -= 64;
		theSpending -= 62;
		theJobs -= 63;
		theEconoB -= 27;	
	}	
	
//YT
}else if ($(this).val() == "YT"){


	if ($(this).prop("checked") == true){	
		
		theProject += 64;
		theSpending += 62;
		theJobs += 63;
		theEconoB += 27;
		
	}else{	
		
		theProject -= 64;
		theSpending -= 62;
		theJobs -= 63;
		theEconoB -= 27;	
	}		
	
}
	
//Reset or Summarize the aggregation	
 if (theChecked.length == 0){ 
	  resetAggregate();
	 
  }else{	



$("#ion-aggregate-projects strong").text(theProject);
if (theSpending == 1000){	
	$("#ion-aggregate-spending strong").text(1 + " billion $");
}else{
	$("#ion-aggregate-spending strong").text(theSpending + " million $");
}
$("#ion-aggregate-jobs strong").text(theJobs);
$("#ion-aggregate-econob strong").text(theEconoB + "k");

}

});


var filterChecker = function(filta, all) {
infowindow.close();
//Reset to default if none is selected
	if (all == true){	  
		  getJSONData();		  
	 }	
	
  for (i = 0; i < markers1.length; i++) {
    marker = gmarkers1[i];
	
    //console.log(marker);
    	if (in_array(this.marker.region, filta) != -1) {
      		marker.setVisible(true);
			
    	} else {
      		marker.setVisible(false);
    	}
  }
}

function in_array(needle, haystack) {
  var found = 0;
  for (var i = 0, len = haystack.length; i < len; i++) {
    if (haystack[i] == needle) return i;
    found++;
  }
  return -1;
}
