function make_slides(f) {
  var slides = {};

  slides.i0 = slide({
     name : "i0",
     start: function() {
      $("#n_trials").html(exp.n_trials);
      exp.startT = Date.now();
     }
  });

// EXAMPLE 1
slides.example1 = slide({
  name : "example1",
  start: function() {
    $("#sentenceGenerator").html('<table class=table1 id="sentenceGenerator"> </table>');
    $('#ifclicked').hide();
    $('#iftrue').hide();
    $('#iffalse').hide();
    $('#ifunclear').hide();
    var dispRow2 = $(document.createElement('tr'))
        .attr("id", 'rowp' + 2);
    var sent = "Mary didn't go to school today.";
  	var has_val = exp.has_val = false;
  	dispRow2.append("<div class=targetSen>" + sent + "</div>");
        // exp.sliderPost = {};
  	dispRow2.appendTo("#sentenceGenerator");
  },
  checkbox : function() {
    this.response = $('input[name="truth"]:checked').val();
    if (this.response == undefined) {
      $(".err").show();
    } else if (this.response == "false"){
      // $('#iffalse').show();
      this.log_checkbox();
      alert("If it's true that Mary didn't go to school this week, it is true that she didn't go to school today. Let's trust that Speaker #2 is telling the truth given that we don't have any reason not to");
    } else if (this.response == "not_sure") {
      //$('#ifunclear').show();
      this.log_checkbox();
      alert("If it's true that Mary didn't go to school this week, it is true that she didn't go to school today. Let's trust that Speaker #2 is telling the truth given that we don't have any reason not to");
    } else { // this.response == "true"
      $('#beforeclicked').hide();
      $('#ifclicked').show();
      $('#iftrue').show();
      this.init_sliders();
      exp.sliderPost = {};
      // _stream.apply(this); 
    }
   },
  log_checkbox : function() {
    exp.data_trials.push({
      "slide_number_in_experiment" : exp.phase,
      "tgrep_id" : "examplecheckbox1",
      "response" : [0, this.response]
    });
  },
  init_sliders : function() {
    utils.make_slider("#single_slider", function(event, ui) {
    exp.sliderPost = ui.value;
    exp.has_val = true;
  })},
  button : function() {
    if (exp.has_val == true && exp.sliderPost > 0.5){
    this.log_responses();
    exp.go();}
    else if (exp.has_val == true && exp.sliderPost < 0.5) {
    this.log_responses();
    alert("If Speaker #2 knows that Mary didn't go to school this week, they also know that she didn't go to school today. Please set the slider closer to 'speaker knows'.");
    }
    else {
    alert("Please set the slider.");
	}      
    //use exp.go() if and only if there is no "present" data.
  },
  log_responses : function() {
    exp.data_trials.push({
      "slide_number_in_experiment" : exp.phase,
      "tgrep_id" : "example1",
      "response" : [exp.sliderPost, this.response]
    });
  },
 });

// EXAMPLE 2
slides.example2 = slide({
  name : "example2",
  start: function() {
    $("#sentenceGenerator").html('<table class=table1 id="sentenceGenerator2"> </table>');
    $('#ifclicked2').hide();
    $('#iftrue2').hide();
    $('#iffalse2').hide();
    $('#ifunclear2').hide();
    var dispRow2 = $(document.createElement('tr'))
        .attr("id", 'rowp' + 2);
    var sent = "The Patriots won the Super Bowl in 2019.";
    var has_val = exp.has_val = false;
    dispRow2.append("<div class=targetSen>" + sent + "</div>");
        // exp.sliderPost = {};
    dispRow2.appendTo("#sentenceGenerator2");
  },
  checkbox : function() {
    this.response = $('input[name="truth"]:checked').val();
    if (this.response == undefined) {
      $(".err").show();
    } else if (this.response == "false"){
      //$('#iffalse2').show();
      this.log_checkbox();
      alert("It is true, the Patriots won the Super Bowl in 2019.");
    } else if (this.response == "not_sure") {
      //$('#ifunclear2').show();
      this.log_checkbox();
      alert("It is true, the Patriots won the Super Bowl in 2019.");
    } else {
      $('#beforeclicked2').hide();
      $('#ifclicked2').show();
      $('#iftrue2').show();
      this.init_sliders();
      exp.sliderPost = {};
      // _stream.apply(this);
    }
   },
   log_checkbox : function() {
    exp.data_trials.push({
      "slide_number_in_experiment" : exp.phase,
      "tgrep_id" : "examplecheckbox2",
      "response" : [0, this.response]
    });
  },
  init_sliders : function() {
    utils.make_slider("#single_slider2", function(event, ui) {
    exp.sliderPost = ui.value;
    exp.has_val = true;
  })},
  button : function() {
    if (exp.has_val == true && exp.sliderPost < 0.5){
    this.log_responses();
    exp.go();}
    else if (exp.has_val == true && exp.sliderPost > 0.5) {
    this.log_responses();
    alert("If Speaker #2 doesn't know that the Super Bowl is about football, then they probably don't know who won. Please set the slider closer to 'speaker doesn't know'.");
  }
    else {
    alert("Please set the slider.");
  }      
    //use exp.go() if and only if there is no "present" data.
  },
  log_responses : function() {
    exp.data_trials.push({
      "slide_number_in_experiment" : exp.phase,
      "tgrep_id" : "example2",
      "response" : [exp.sliderPost, this.response]
    });
  },
 });

// EXAMPLE 3
slides.example3 = slide({
  name : "example3",
  start: function() {
    $("#sentenceGenerator").html('<table class=table1 id="sentenceGenerator3"> </table>');
    $('#ifclicked3').hide();
    $('#iftrue3').hide();
    $('#iffalse3').hide();
    $('#ifunclear3').hide();
    var dispRow2 = $(document.createElement('tr'))
        .attr("id", 'rowp' + 2);
    var sent = "California is on the East Coast.";
    var has_val = exp.has_val = false;
    dispRow2.append("<div class=targetSen>" + sent + "</div>");
        // exp.sliderPost = {};
    dispRow2.appendTo("#sentenceGenerator3");
  },
  checkbox : function() {
    this.response = $('input[name="truth"]:checked').val();
    if (this.response == undefined) {
      $(".err").show();
    } else if (this.response == "true") {
      //$('#iftrue3').show();
      this.log_checkbox();
      alert("It's false, California is on the West Coast.");
    } else if (this.response == "not_sure"){
      //$('#ifunclear3').show();
      this.log_checkbox();
      alert("It's false, California is on the West Coast.");
    } else {
      $('#beforeclicked3').hide();
      $('#ifclicked3').show();
      $('#iffalse3').show();
      this.init_sliders();
      exp.sliderPost = {};
      // _stream.apply(this);
    }
   },
   log_checkbox : function() {
    exp.data_trials.push({
      "slide_number_in_experiment" : exp.phase,
      "tgrep_id" : "examplecheckbox3",
      "response" : [0, this.response]
    });
  },
  init_sliders : function() {
    utils.make_slider("#single_slider3", function(event, ui) {
    exp.sliderPost = ui.value;
    exp.has_val = true;
  })},
  button : function() {
    if (exp.has_val == true && exp.sliderPost > 0.5){
    this.log_responses();
    exp.go();}
    else if (exp.has_val == true && exp.sliderPost < 0.5) {
    this.log_responses();
    alert("Because Speaker #2 is complaining about the claim that California is on the East Coast, we know that they know that it is not true. Please set the slider closer to 'speaker knows'.");
    }
    else {
    alert("Please set the slider.");
    }      
    //use exp.go() if and only if there is no "present" data.
  },
  log_responses : function() {
    exp.data_trials.push({
      "slide_number_in_experiment" : exp.phase,
      "tgrep_id" : "example3",
      "response" : [exp.sliderPost, this.response]
    });
  },
 });

// EXAMPLE 4
slides.example4 = slide({
  name : "example4",
  start: function() {
    $("#sentenceGenerator").html('<table class=table1 id="sentenceGenerator4"> </table>');
    $('#ifclicked4').hide();
    $('#iftrue4').hide();
    $('#iffalse4').hide();
    $('#ifunclear4').hide();
    var dispRow2 = $(document.createElement('tr'))
        .attr("id", 'rowp' + 2);
    var sent = "I did all of my homework.";
    var has_val = exp.has_val = false;
    dispRow2.append("<div class=targetSen>" + sent + "</div>");
        // exp.sliderPost = {};
    dispRow2.appendTo("#sentenceGenerator4");
  },
  checkbox : function() {
    this.response = $('input[name="truth"]:checked').val();
    if (this.response == undefined) {
      $(".err").show();
    } else {
      $('#beforeclicked4').hide();
      $('#ifclicked4').show();
      if (this.response == "true") {
        $('#iftrue4').show();
      }
      else if (this.response == "false"){
        $('#iffalse4').show();
      }
      else {
        $('#ifunclear4').show();
      }
      this.init_sliders();
      exp.sliderPost = {};
      // _stream.apply(this); 
      }
   },
  init_sliders : function() {
    utils.make_slider("#single_slider4", function(event, ui) {
    exp.sliderPost = ui.value;
    exp.has_val = true;
  })},
  button : function() {
    if (exp.has_val == true && exp.sliderPost > 0.5){
    this.log_responses();
    exp.go();}
    else if (exp.has_val == true && exp.sliderPost < 0.5) {
    this.log_responses();
    alert("Speaker #2 is talking about themselves so they must know whether or not they did all of the school work. Please set the slider closer to 'speaker knows'.");
    }
    else {
    alert("Please set the slider.");
    }      
    //use exp.go() if and only if there is no "present" data.
  },
  log_responses : function() {
    exp.data_trials.push({
      "slide_number_in_experiment" : exp.phase,
      "tgrep_id" : "example4",
      "response" : [exp.sliderPost, this.response]
    });
  },
 });
	
// EXAMPLE 5
slides.example5 = slide({
  name : "example5",
  start: function() {
    $("#sentenceGenerator").html('<table class=table1 id="sentenceGenerator5"> </table>');
    $('#ifclicked5').hide();
    $('#iftrue5').hide();
    $('#iffalse5').hide();
    $('#ifunclear5').hide();
    var dispRow2 = $(document.createElement('tr'))
        .attr("id", 'rowp' + 2);
    var sent = "All of the things in the fridge went bad.";
    var has_val = exp.has_val = false;
    dispRow2.append("<div class=targetSen>" + sent + "</div>");
        // exp.sliderPost = {};
    dispRow2.appendTo("#sentenceGenerator5");
  },
  checkbox : function() {
    this.response = $('input[name="truth"]:checked').val();
    if (this.response == undefined) {
      $(".err").show();
    } else {
      $('#beforeclicked5').hide();
      $('#ifclicked5').show();
      if (this.response == "true") {
        $('#iftrue5').show();
      }
      else if (this.response == "false"){
        $('#iffalse5').show();
      }
      else {
        $('#ifunclear5').show();
      }
      this.init_sliders();
      exp.sliderPost = {};
      // _stream.apply(this); 
      }
   },
  init_sliders : function() {
    utils.make_slider("#single_slider5", function(event, ui) {
    exp.sliderPost = ui.value;
    exp.has_val = true;
  })},
  button : function() {
    if (exp.has_val == true && exp.sliderPost < 0.5){
    this.log_responses();
    exp.go();}
    else if (exp.has_val == true && exp.sliderPost > 0.5) {
    this.log_responses();
    alert("If Speaker #2 is not sure whether some of the things in the fridge went bad, they don't know whether all the things in the fridge went bad. Please set the slider closer to 'speaker doesn't know'.");
    }
    else {
    alert("Please set the slider.");
    }      
    //use exp.go() if and only if there is no "present" data.
  },
  log_responses : function() {
    exp.data_trials.push({
      "slide_number_in_experiment" : exp.phase,
      "tgrep_id" : "example5",
      "response" : [exp.sliderPost, this.response]
    });
  },
 });

 // EXPERIMENT
 slides.startExp = slide({
     name : "startExp",
     start: function(){
     $("#instrunctionGen").html('<table class=table1 id="instructionGen"> </table>');
     var dispRow = $(document.createElement('tr')).attr("id", 'rowp' + 1);
     dispRow.append("<div class=row>");
     dispRow.append("<div align=center><button class=continueButton onclick= _s.button()>Continue</button></div>");
     dispRow.append('<td/>');
     dispRow.append('</div>');
     dispRow.appendTo("#instructionGen");
},
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    },
  });

  slides.generateEntities = slide({
    name: "generateEntities",
    present: exp.stimuli,

    present_handle : function(stim) {
      this.counter = 1;
	 
      var generic = stim;
      this.generic = generic;
      this.response = response = false;
      usentence = generic.EntireSentence.replace(/--n((\d+)|[a-z]+)+/g, "");
      butnotboth = generic.BestResponse;
      butnotboth = butnotboth.replace('.', "");
	    butnotboth = butnotboth.replace(/--n((\d+)|[a-z]+|(\d+))+/g, "");
      if(usentence.endsWith(".")){
          butnotboth +='.';
      };

      var contexthtml = this.format_context(generic.context);
      $(".case").html(contexthtml + " " + "<font color=#E7532B>"+usentence+"</font>"); // Replace .Sentence with the name of your sentence column
      $(".EntireSentence").html(generic.EntireSentence);
      $(".BestResponse").html(generic.BestResponse);
      $(".err").hide();
      
      this.counter++;
      
      $("#tableGenerator").html('<table id="tableGenerator" align="left"> </table>');

      // create response table
      for(i=0; i<exp.n_entities; i++){

        var dispRow = $(document.createElement('tr'))
                   .attr("id", 'rowf' + 1);
      	var dispRow2 = $(document.createElement('tr'))
                   .attr("id", 'rowf' + 2);
      	var dispRow4 = $(document.createElement('tr'))
                   .attr("id", 'rowf' + 5);
      	var dispRow3 = $(document.createElement('tr'))
                   .attr("id", 'rowf' + 3);	
      	var has_val1 = exp.has_val = false;
      	sent = usentence.trim();
      	var targetsen = butnotboth;
      	//targetsen = targetsen.replace("all", "<b>all</b>"); // have to compare sentences to find the right "all"
      	dispRow2.attr('align', 'left');
      	dispRow2.append("<div class=targetSen>" + targetsen + "</div>");
      	
      	var checkbox = document.createElement('input'); 
      	checkbox.type= 'checkbox';
      	checkbox.id = "check";
      	checkbox.name = name;
      	checkbox.classList = 'row';
      	checkbox.setAttribute('align', 'center');
      	checkbox.value = '<font color=#008CBA Strange Sentence</font>';
      	exp.checkbox = document.getElementById("myCheck");
      	dispRow2.appendTo("#tableGenerator");
      	dispRow3.appendTo("#tableGenerator");
      	this.init_numeric_sliders();
            	exp.sliderPost = []; 
            }
            $(".err").hide();
    },

    format_context : function(context) {

   	contexthtml = context.replace (/###/, "<br><b>Speaker #</b>");
    contexthtml = contexthtml.replace(/speaker(([a-z]+)|(\d+)|(-)|(\d+)|(\*))+/g, "");
    contexthtml = contexthtml.replace (/###.###/g, "<br><b>Speaker #</b>");
    //contexthtml = contexthtml.replace(/E_S/g, "");
    //contexthtml = contexthtml.replace(/(\\\[| \\\+|\\\])/g, "");
    contexthtml = contexthtml.replace(/--n((\d+)|[a-z]+|(\d+))+/g, "");
    contexthtml = contexthtml.replace(/.###/g, " ");
    contexthtml = contexthtml.replace(/-N((\d+)|[A-Z]+)+/g, "");
	
// FIX THIS!
  contexthtml = contexthtml.replace("<br><b>Speaker #</b>", "<br><b>Speaker #1: </b>");
  contexthtml = contexthtml.replace("<br><b>Speaker #</b>", "<br><b>Speaker #2: </b>");
	contexthtml = contexthtml.replace("<br><b>Speaker #</b>", "<br><b>Speaker #1: </b>");
	contexthtml = contexthtml.replace("<br><b>Speaker #</b>", "<br><b>Speaker #2: </b>");
	contexthtml = contexthtml.replace("<br><b>Speaker #</b>", "<br><b>Speaker #1: </b>");
	contexthtml = contexthtml.replace("<br><b>Speaker #</b>", "<br><b>Speaker #2: </b>");
	contexthtml = contexthtml.replace("<br><b>Speaker #</b>", "<br><b>Speaker #1: </b>");
	contexthtml = contexthtml.replace("<br><b>Speaker #</b>", "<br><b>Speaker #2: </b>");
	contexthtml = contexthtml.replace("<br><b>Speaker #</b>", "<br><b>Speaker #1: </b>");
	contexthtml = contexthtml.replace("<br><b>Speaker #</b>", "<br><b>Speaker #2: </b>");
	contexthtml = contexthtml.replace("<br><b>Speaker #</b>", "<br><b>Speaker #1: </b>");
	contexthtml = contexthtml.replace("<br><b>Speaker #</b>", "<br><b>Speaker #2: </b>");
	contexthtml = contexthtml.replace("<br><b>Speaker #</b>", "<br><b>Speaker #1: </b>");
	contexthtml = contexthtml.replace("<br><b>Speaker #</b>", "<br><b>Speaker #2: </b>");
	contexthtml = contexthtml.replace("<br><b>Speaker #</b>", "<br><b>Speaker #1: </b>");
	contexthtml = contexthtml.replace("<br><b>Speaker #</b>", "<br><b>Speaker #2: </b>");

        // if (!contexthtml.startsWith("<br><b>Speaker #</b>")) { 
        // 	console.log(contexthtml);
        //     var ssi = contexthtml.indexOf("<br><b>Speaker # </b>");
        //     console.log(ssi);
        //     switch(contexthtml[ssi+"Speaker #".length]) {
        //     case "1":
        //         contexthtml = "<br><b>Speaker #2: </b>" + contexthtml;
        //         break;
        //     case "2":
        //         contexthtml = "<br><b>Speaker #1: </b>" + contexthtml;
        //         break;
        //     default:
        //         break;
        //     }
        // };
        return contexthtml;
    },
    
 
    init_numeric_sliders : function() {
        for (i=0; i<exp.n_entities; i++) {
            utils.make_slider("#single_slider6", this.make_slider_callback(i));
	    exp.has_val1 = false;
	   
        }
    },

    make_slider_callback : function(i) {
      return function(event, ui) {
        exp.sliderPost = ui.value;
	 exp.has_val1 = true;
        $("#slider_number" + i).html(Math.round(exp.sliderPost[i]*100) + "%");
	
      };
    },

    button : function() {
	 if (exp.has_val1 == true){
      // check if all fields are filled
            this.log_responses();
            exp.checkbox.checked  = false;
            exp.stimcounter++;
            _stream.apply(this);}
     else {
	alert("Please set the slider.");
	}

    },

    log_responses : function() {
      exp.data_trials.push({
    "slide_number_in_experiment" : exp.phase,
	"tgrep_id" : this.generic.TGrepID,
    "response" : [exp.sliderPost, exp.checkbox.checked],
	
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
  var ut_id = "bafb6578a5f5ffe7571216e39b602b74";  // remember to replace this!
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
  //var stimuli = generate_stim();
  var stimuli = _.shuffle(generate_stim());
 // var attentioncheck_stimuli = generate_attention_stim();

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
   	"example3",
    "example4",
    "example5",
    "startExp",
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

