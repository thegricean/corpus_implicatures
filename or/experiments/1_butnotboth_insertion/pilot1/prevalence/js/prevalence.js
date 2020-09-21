function make_slides(f) {
  var slides = {};

  slides.i0 = slide({
     name : "i0",
     start: function() {
      $("#n_trials").html(exp.n_trials);
      exp.startT = Date.now();
     }
  });


	
  slides.example1 = slide({
    name : "example1",

    start: function() {
	
	$("#sentenceGenerator").html('<table id="sentenceGenerator"> </table>');

	var dispRow = $(document.createElement('tr'))
             .attr("id", 'rowp' + 1);
	var dispRow2 = $(document.createElement('tr'))
             .attr("id", 'rowp' + 2);
	var refresh = $(document.createElement('tr'))
             .attr("id", 'rowp' + 7);
	var dispRow4 = $(document.createElement('tr'))
             .attr("id", 'rowp' + 5);
	var dispRow5 = $(document.createElement('tr'))
             .attr("id", 'rowp' + 6);
	var dispRow3 = $(document.createElement('tr'))
             .attr("id", 'rowp' + 3);	
	var res = $(document.createElement('tr'))
             .attr("id", 'rowp' + 4);
	var sent = "He's bringing meatballs or muffins.";
	var wordsBefore = sent.split("or")[0].split(" ");
	var words = sent.split(" ");
	var counter = 1;
	dispRow.append("<div class=row>");
	var refreshButton = document.createElement("button");
	
	//document.getElementById('continue').style.display='none';
	for (i=0; i<words.length; i++){
		if (wordsBefore.indexOf(words[i])>=0){
		dispRow.append("<font size=5>"+words[i]+" " + "</font>");}
		else {
		dispRow.append("<font size=5>"+words[i]+"</font>");
		var newButton  = document.createElement("button");
    		newButton.id = i;
		newButton.classList = 'arcButton';
		newButton.innerHTML='<sub>&#8593</sub>';
		dispRow.append(newButton);
		newButton.addEventListener("click", function(e) {
			if (this.id == 3) {
				alert("Please try again. What is the most natural location for the phrase 'but not both'?");
			}
			else {
				
			refreshButton.style.display='block';
			var firsthalf = words.slice(0, (parseInt(this.id)+1)).join(" ");
			var secondhalf = words.slice((parseInt(this.id)+1), i).join(" ");
			var targetsen = firsthalf + "<font color=red> but not both </font>" + secondhalf;
			dispRow2.append("<div class=targetSen>" + targetsen + "</div>");
			document.getElementById('rowp1').style.display='none';
			var tod = true;
			
			dispRow3.append("<div align=center><button class=continueButton onclick= _s.button()>Continue</button></div>");
			refreshButton.id = 'refresh';
			refreshButton.classList='retryButton';
			refreshButton.innerHTML='retry';
			refresh.append("<div align=right class=row>");
			refresh.append(refreshButton);
			refresh.append("</div>");
			refreshButton.addEventListener("click", function(e) {
				document.getElementById('rowp1').style.display='block';
				for (i=2; i<=6; i++) {
				document.getElementById('rowp' + i.toString()).innerHTML = '';
				}
			});
			res.append("<div class=instruction><i>This is the result of your choice:</i></div>");
			dispRow5.append("<div class=instruction><i>If you are satisfied with your choice, click Continue. If you aren't satisfied choose a better location by clicking the retry button. If there is no better location, click the No good location radio button, then click Continue</i></div>");
			dispRow4.append("<div class = row align=center id=location><input type=checkbox id=loc/><font color=#008CBA> No good location</font></div>");

			
			
			}});

	}}
			
	
			
        dispRow.append('<td/>');
	dispRow.append('</div>');
        dispRow.appendTo("#sentenceGenerator");
	res.appendTo("#sentenceGenerator");
	dispRow2.appendTo("#sentenceGenerator");
	dispRow5.appendTo("#sentenceGenerator");
	refresh.appendTo("#sentenceGenerator");
	dispRow4.appendTo("#sentenceGenerator");
	dispRow3.appendTo("#sentenceGenerator");
},
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });


   slides.example2 = slide({
    name : "example2",

    start: function() {
	$("#sentenceGenerator").html('<table id="sentenceGenerator2"> </table>');
	var dispRow = $(document.createElement('tr'))
             .attr("id", 'rowp' + 1);
	var dispRow2 = $(document.createElement('tr'))
             .attr("id", 'rowp' + 2);
	var refresh = $(document.createElement('tr'))
             .attr("id", 'rowp' + 7);
	var dispRow4 = $(document.createElement('tr'))
             .attr("id", 'rowp' + 5);
	var dispRow5 = $(document.createElement('tr'))
             .attr("id", 'rowp' + 6);
	var dispRow3 = $(document.createElement('tr'))
             .attr("id", 'rowp' + 3);	
	var res = $(document.createElement('tr'))
             .attr("id", 'rowp' + 4);
	var sent = "I don't think it will stop raining or actually will start being sunny";
	var wordsBefore = sent.split("or")[0].split(" ");
	var words = sent.split(" ");
	var counter = 1;
	dispRow.append("<div class=row>");
	var refreshButton = document.createElement("button");
	
	//document.getElementById('continue').style.display='none';
	for (i=0; i<words.length; i++){
		if (wordsBefore.indexOf(words[i])>=0){
		dispRow.append("<font size=5>"+words[i]+" " + "</font>");}
		else {
		dispRow.append("<font size=5>"+words[i]+"</font>");
		var newButton  = document.createElement("button");
    		newButton.id = i;
		newButton.classList = 'arcButton';
		newButton.innerHTML='<sub>&#8593</sub>';
		dispRow.append(newButton);
		newButton.addEventListener("click", function(e) {
			
			var firsthalf = words.slice(0, (parseInt(this.id)+1)).join(" ");
			var secondhalf = words.slice((parseInt(this.id)+1), i).join(" ");
			dispRow2.append("<div class=targetSen>" + firsthalf + "<font color=red> but not both </font>" + secondhalf + "</div>");
			document.getElementById('rowp1').style.display='none';
			
			dispRow3.append("<div align=center><button class=continueButton onclick=_s.button()>Continue</button></div>");
			refreshButton.id = 'refresh';
			refreshButton.classList='retryButton';
			refreshButton.innerHTML='retry';
			refresh.append("<div align=right class=row>");
			refresh.append(refreshButton);
			refresh.append("</div>");
			refreshButton.addEventListener("click", function(e) {
				document.getElementById('rowp1').style.display='block';
				for (i=2; i<=6; i++) {
				document.getElementById('rowp' + i.toString()).innerHTML = '';
				}
			});			

			res.append("<div class=instruction><i>This is the result of your choice:</i></div>");
			dispRow5.append("<div class=instruction><i>If you are satisfied with your choice, click Continue. If you aren't satisfied choose a better location by clicking the retry button. If there is no better location click the No good location radio button, then click Continue</i></div>");
			dispRow4.append("<div class = row align=center id=location><input type=checkbox id=loc/><font color=#008CBA> No good location</font></div>");
			
			});

	}}
	
			
        dispRow.append('<td/>');
	dispRow.append('</div>');
        dispRow.appendTo("#sentenceGenerator2");
	res.appendTo("#sentenceGenerator2");
	dispRow2.appendTo("#sentenceGenerator2");
	dispRow5.appendTo("#sentenceGenerator2");
	refresh.appendTo("#sentenceGenerator2");
	dispRow4.appendTo("#sentenceGenerator2");
	dispRow3.appendTo("#sentenceGenerator2");
    
   },
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

  slides.generateEntities = slide({
    name: "generateEntities",
    present: exp.stimuli,

    present_handle : function(stim) {
      this.counter = 1;
	 
      var generic = stim;
      this.generic = generic;
      usentence = generic.Sentence.replace(/--n((\d+)|[a-z]+)+/g, "");
      var contexthtml = this.format_context(generic.Context);
      $(".case").html(contexthtml + " " + usentence); // Replace .Sentence with the name of your sentence column
      $(".Sentence").html(generic.Sentence);
      $(".but_not_both").html(generic.sentence_butNotBoth);
      $(".err").hide();



      this.counter++;
      

      $("#tableGenerator").html('<table id="tableGenerator"> </table>');

      // create response table
      for(i=0; i<exp.n_entities; i++){


       var dispRow = $(document.createElement('tr'))
             .attr("id", 'rowf' + 1);
	var dispRow2 = $(document.createElement('tr'))
             .attr("id", 'rowf' + 2);
	var xrefresh = $(document.createElement('tr'))
             .attr("id", 'rowf' + 7);
	var dispRow4 = $(document.createElement('tr'))
             .attr("id", 'rowf' + 5);
	var dispRow5 = $(document.createElement('tr'))
             .attr("id", 'rowf' + 6);
	var dispRow3 = $(document.createElement('tr'))
             .attr("id", 'rowf' + 3);	
	var res = $(document.createElement('tr'))
             .attr("id", 'rowf' + 4);
	sent = usentence.trim()
	var words = sent.split(" ");
	var wordsBefore = words.indexOf("or");

	var counter = 1;
	dispRow.append("<div class=row>");
	var xrefreshButton = document.createElement("button");
	var checked = false;
	//document.getElementById('continue').style.display='none';
	for (i=0; i<words.length; i++){
		if (i<wordsBefore){
		dispRow.append("<font size=5>"+words[i]+" " + "</font>");}
		else {
		if (words[i].length != 0){
		dispRow.append("<font size=5>"+words[i]+"</font>");
		var newButton  = document.createElement("button");
    		newButton.id = i;
		newButton.classList = 'arcButton';
		newButton.innerHTML='<sub>&#8593</sub>';
		dispRow.append(newButton);
		newButton.addEventListener("click", function(e) {
			var firsthalf = words.slice(0, (parseInt(this.id)+1)).join(" ");
			var secondhalf = words.slice((parseInt(this.id)+1), words.length).join(" ");
			var targetsen = firsthalf + "<font color=red> but not both </font>" + secondhalf;
			dispRow2.append("<div class=targetSen>" + targetsen + "</div>");
			document.getElementById('rowf1').style.display='none';
			
			dispRow3.append("<div align=center><button class=continueButton onclick=_s.button()>Continue</button></div>")
			res.append("<div class=instruction><i>This is the result of your choice:</i></div>");
			dispRow5.append("<div class=instruction><i>If you are satisfied with your choice, click Continue. If you aren't satisfied, choose a better location, by clicking the retry button. If there is no better location, click the No good location radio button, then click Continue</i></div>")
			dispRow4.append("<div class = row align=center id=location><input type=checkbox onclick=checked.value=true id=loc/><font color=#008CBA> No good location</font></div>");
			
			});

	}}}
	
			xrefreshButton.id = 'xrefresh';
			xrefreshButton.classList='retryButton';
			xrefreshButton.innerHTML='retry';
			xrefresh.append("<div align=right class=row>");
			xrefresh.append(xrefreshButton);
			xrefresh.append("</div>");
			xrefreshButton.addEventListener("click", function(e) {
				document.getElementById('rowf1').style.display='block';
				//hier muss der satz irgendwie gespeichert werden
				for (i=2; i<=6; i++) {
				document.getElementById('rowf' + i.toString()).innerHTML = '';
				}
			});
        dispRow.append('<td/>');
	dispRow.append('</div>');
        dispRow.appendTo("#tableGenerator");
	res.appendTo("#tableGenerator");
	dispRow2.appendTo("#tableGenerator");
	dispRow5.appendTo("#tableGenerator");
	xrefresh.appendTo("#tableGenerator");
	dispRow4.appendTo("#tableGenerator");
	dispRow3.appendTo("#tableGenerator");
      }
      $(".err").hide();
      this.init_numeric_sliders();
      exp.sliderPost = []; 


    },

    format_context : function(context) {
        contexthtml = context.replace(/###speakera(\d+).t(\d+)/g, "<br><b>Speaker #1:</b>");
        contexthtml = contexthtml.replace(/###speakerb(\d+).t(\d+)/g, "<br><b>Speaker #2:</b>");
	contexthtml = contexthtml.replace(/--n((\d+)|[a-z]+)+/g, "");
        contexthtml = contexthtml.replace(/###/g, " ");
        if (!contexthtml.startsWith("<br><b>Speaker #")) {
            var ssi = contexthtml.indexOf("Speaker #");
            switch(contexthtml[ssi+"Speaker #".length]) {
            case "1":
                contexthtml = "<br><b>Speaker #2:</b> " + contexthtml;
                break;
            case "2":
                contexthtml = "<br><b>Speaker #1:</b> " + contexthtml;
                break;
            default:
                break;
            }
        };
        return contexthtml;
    },

    init_numeric_sliders : function() {
        for (i=0; i<exp.n_entities; i++) {
            utils.make_slider("#freqbox_response" + i, this.make_slider_callback(i));
        }
    },

    make_slider_callback : function(i) {
      return function(event, ui) {
        exp.sliderPost[i] = ui.value;
        $("#slider_number" + i).html(Math.round(exp.sliderPost[i]*100) + "%");
      };
    },

    button : function() {
      var freqs = [], intervals = [];

      for(i=0; i<exp.n_entities; i++){
        var f = parseInt(exp.sliderPost[i]*100);
        freqs.push(isNaN(f) ? "" : f)
      }

      // check if all fields are filled
      if (true) {
            this.log_responses();
            exp.stimcounter++;
            _stream.apply(this);
      } else {
        $(".err").show();
      }

    },


    log_responses : function() {
      exp.data_trials.push({
        "trial_type" : "single_generic_trial",
	"tgrep id" : this.generic.Item_ID,
	"entire sentence" : this.generic.Sentence,
    "but_not_both" : this.targetsen,
      });
	
    },

  });

  
  slides.subj_info =  slide({
    name : "subj_info",
    submit : function(e){
      //ifFdata (e.preventDefault) e.preventDefault(); // I don't know what this means.
      exp.subj_data = _.extend({
        language : $("#language").val(),
        enjoyment : $("#enjoyment").val(),
        asses : $('input[name="assess"]:checked').val(),
        age : $("#age").val(),
        gender : $("#gender").val(),
        education : $("#education").val(),
        problems: $("#problems").val(),
        fairprice: $("#fairprice").val(),
        comments : $("#comments").val()
      });
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

  slides.thanks = slide({
    name : "thanks",
    start : function() {
      exp.data= {
          "trials" : exp.data_trials,
          "catch_trials" : exp.catch_trials,
          "system" : exp.system,
          "condition" : exp.condition,
          "subject_information" : exp.subj_data,
          "time_in_minutes" : (Date.now() - exp.startT)/60000
      };
      setTimeout(function() {turk.submit(exp.data);}, 1000);
    }
  });

  return slides;
}

/// init ///
function init() {

  repeatWorker = false;
  (function(){
      var ut_id = "corpgen-preval-20170421";
      if (UTWorkerLimitReached(ut_id)) {
        $('.slide').empty();
        repeatWorker = true;
        alert("You have already completed the maximum number of HITs allowed by this requester. Please click 'Return HIT' to avoid any impact on your approval rating.");
      }
  })();

  exp.n_entities = 1;
  exp.names = [];
  exp.all_names = []; 
  exp.trials = [];
  exp.catch_trials = [];
  var stimuli = generate_stim(19, true);
  console.log(stimuli.length);
  //exp.stimuli = _.shuffle(stimuli).slice(0, 15);
  exp.stimuli = stimuli.slice();
  exp.n_trials = exp.stimuli.length;
  exp.stimcounter = 0;

  // exp.womenFirst = _.sample([true, false])
  // debugger;
  exp.stimscopy = exp.stimuli.slice();

  // exp.condition = _.sample(["CONDITION 1", "condition 2"]); //can randomize between subject conditions here
  exp.system = {
      Browser : BrowserDetect.browser,
      OS : BrowserDetect.OS,
      screenH: screen.height,
      screenUH: exp.height,
      screenW: screen.width,
      screenUW: exp.width
    };
  //blocks of the experiment:
  exp.structure=[
    "i0",
    "example1",
    "example2",
    "generateEntities",
    //"priors",
    "subj_info",
    "thanks"
  ];

  exp.data_trials = [];
  //make corresponding slides:
  exp.slides = make_slides(exp);

  exp.nQs = utils.get_exp_length(); //this does not work if there are stacks of stims (but does work for an experiment with this structure)
                    //relies on structure and slides being defined

  $('.slide').hide(); //hide everything

  //make sure turkers have accepted HIT (or you're not in mturk)
  $("#start_button").click(function() {
    if (turk.previewMode) {
      $("#mustaccept").show();
    } else {
      $("#start_button").click(function() {$("#mustaccept").show();});
      exp.go();
    }
  });

  exp.go(); //show first slide
}
