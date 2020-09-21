function make_slides(f) {
  var slides = {};

  slides.bot = slide({
    name : "bot",
    start: function() {
      $('.err1').hide();
      $('.err2').hide();
      $('.disq').hide();
      exp.speaker = _.shuffle(["James", "John", "Robert", "Michael", "William", "David", "Richard", "Joseph", "Thomas", "Charles"])[0];
      exp.listener = _.shuffle(["Mary", "Patricia", "Jennifer", "Linda", "Elizabeth", "Barbara", "Susan", "Jessica", "Sarah", "Margaret"])[0];
      exp.lives = 0;
      var story = exp.speaker + ' says to ' + exp.listener + ': "It\'s a beautiful day, isn\'t it?"'
      var question = 'Who does ' + exp.speaker + ' talk to?';
      document.getElementById("s").innerHTML = story;
      document.getElementById("q").innerHTML = question;
    },
    button : function() {
      exp.text_input = document.getElementById("text_box").value;
      var lower = exp.listener.toLowerCase();
      var upper = exp.listener.toUpperCase();

      if ((exp.lives < 3) && ((exp.text_input == exp.listener)|(exp.text_input == lower) | (exp.text_input== upper))){
        exp.data_trials.push({
          "slide_number_in_experiment" : exp.phase,
          "tgrep_id" : "bot_check",
          "response" : [exp.text_input,exp.listener],
          "sentence" : "",
        });
        exp.go();
      }
      else {
        exp.data_trials.push({
          "slide_number_in_experiment" : exp.phase,
          "tgrep_id" : "bot_check",
          "response" : [exp.text_input,exp.listener],
          "sentence" : "",
        });
        if (exp.lives == 0){
          $('.err1').show();
        }if (exp.lives == 1){
          $('.err1').hide();
          $('.err2').show();
        }if (exp.lives == 2){
          $('.err2').hide();
          $('.disq').show();
          $('.button').hide();
        }
        exp.lives++;
      } 
    },
  });

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
	$('.err').hide();
	$("#sentenceGenerator").html('<table class=table1 id="sentenceGenerator"> </table>');
    	
	var dispRow2 = $(document.createElement('tr'))
             .attr("id", 'rowp' + 2);
	var sent = "I had <b>some, but not all</b> of the banana yogurt";
	var has_val = exp.has_val = false;
	
	dispRow2.append("<div class=targetSen>" + sent + "</div>");
	
        //this.init_sliders();
      	//exp.sliderPost = {};
	
	dispRow2.appendTo("#sentenceGenerator");

	
},

     button : function() {
      this.radio  = $("input[name='number']:checked").val();
      if (this.radio == "5" |this.radio == "6" | this.radio == "7"  ) {
        this.log_responses();
        exp.go();
      } 
      else{
        $('.err').show();
        this.log_responses();
      }
    },

    log_responses : function() {
      exp.data_trials.push({
        "slide_number_in_experiment" : exp.phase,
        "tgrep_id" : "example1",
    	  "response" : [this.radio, ""],
        "sentence" : "",
      });
	},
  });


   slides.example2 = slide({
    name : "example2",
    
    start: function() {
      $(".err").hide();
	$("#sentenceGenerator").html('<table class= table1 id="sentenceGenerator2"> </table>');
	var dispRow = $(document.createElement('tr'))
             .attr("id", 'rowp' + 1);
	
	var sent = "There's probably <b>some, but not all</b> peanuts in the pantry. ";
	var has_val = exp.has_val = false;
	
	dispRow.append("<div class=targetSen>" + sent + "</div>");

        //this.init_sliders();
      	//exp.sliderPost = {};
	
	dispRow.appendTo("#sentenceGenerator2");
	
},
    button : function() {
      this.radio  = $("input[name='number']:checked").val();
      if (this.radio == "1" |this.radio == "2" | this.radio == "3"  ) {
        this.log_responses();
        exp.go();
      } 
      else{
        $('.err').show();
        this.log_responses();
      }
    },

    log_responses : function() {
      exp.data_trials.push({
        "slide_number_in_experiment" : exp.phase,
        "tgrep_id" : "example2",
        "response" : [this.radio, ""],
        "sentence" : "",
      });
  },
// init_sliders : function() {
//       utils.make_slider("#single_slider2", function(event, ui) {
//         exp.sliderPost = ui.value;
// 	exp.has_val = true;
//       })},

//    button : function() {
//       if (exp.has_val == true && exp.sliderPost > 0.5){
//       this.log_responses();
//       exp.go();}
//       else if (exp.has_val == true && exp.sliderPost < 0.5) {
// 	alert("In this case, not being in both Paris and London is part of what Speaker #2 meant anyway, so adding 'but not both' doesn't really change the meaning of the sentence. That makes these sentences pretty similar. In this kind of case, please set the slider closer to 'same meaning'");
// 	}
//       else {
// 	alert("Please set the slider");
// 	}
          
//        //use exp.go() if and only if there is no "present" data.
//     },
// log_responses : function() {
//       exp.data_trials.push({
//         "slide_number_in_experiment" : exp.phase,
//     	"response" : [exp.sliderPost,  ""]
//       });
// 	},
  });
 
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

    $("input[name='number']:checked").prop('checked', false);

      this.counter = 1;
	 
      var generic = stim;
      this.generic = generic;
      this.response = response = false;

      usentence = generic.EntireSentence;
      butnotboth = generic.BestResponse;
      
      // usentence = generic.EntireSentence.replace(/--n((\d+)|[a-z]+)+/g, "");
      // butnotboth = generic.BestResponse;
      // butnotboth = butnotboth.replace('.', "");
      // if(usentence.endsWith(".")){
      //     butnotboth +='.';
      // };
      
      //var contexthtml = this.format_context(generic.context);

      var contexthtml = generic.Context

      contexthtml = contexthtml.replace("Speaker A:", "<b>Speaker #1:</b>");
      contexthtml = contexthtml.replace("Speaker B:", "<b>Speaker #2:</b>");
      contexthtml = contexthtml.replace("Speaker A:", "<b>Speaker #1:</b>");
      contexthtml = contexthtml.replace("Speaker B:", "<b>Speaker #2:</b>");
      contexthtml = contexthtml.replace("Speaker A:", "<b>Speaker #1:</b>");
      contexthtml = contexthtml.replace("Speaker B:", "<b>Speaker #2:</b>");
      contexthtml = contexthtml.replace("Speaker A:", "<b>Speaker #1:</b>");
      contexthtml = contexthtml.replace("Speaker B:", "<b>Speaker #2:</b>");
      contexthtml = contexthtml.replace("Speaker A:", "<b>Speaker #1:</b>");
      contexthtml = contexthtml.replace("Speaker B:", "<b>Speaker #2:</b>");
      contexthtml = contexthtml.replace("Speaker A:", "<b>Speaker #1:</b>");
      contexthtml = contexthtml.replace("Speaker B:", "<b>Speaker #2:</b>");


      //$(".case").html(contexthtml + " " + "<font color=#E7532B>"+usentence+"</font>"); // Replace .Sentence with the name of your sentence column
      $(".case").html("<font color=#E7532B>"+usentence+"</font>");
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
	targetsen = targetsen.replace("but not both", "<b>but not both</b>");
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
	//this.init_numeric_sliders();
  //    	exp.sliderPost = []; 
      }
      $(".err").hide();
     

    },

    // format_context : function(context) {
    //   contexthtml = context.replace(/###SpeakerA(\d+).(\d+)?t(\d+)(.\d+)?/g, "<br><b>Speaker #1:</b>");
    // 	contexthtml = contexthtml.replace(/E_S/g, "");
    // 	contexthtml = contexthtml.replace(/(\\\[| \\\+|\\\])/g, "");
    //   contexthtml = contexthtml.replace(/###SpeakerB(\d+).t(\d+)/g, "<br><b>Speaker #2:</b>");
    // 	contexthtml = contexthtml.replace(/-N((\d+)|[A-Z]+)+/g, "");
    //   contexthtml = contexthtml.replace(/###/g, " ");
    //     if (!contexthtml.startsWith("<br><b>Speaker #")) {
    //         var ssi = contexthtml.indexOf("Speaker #");
    //         switch(contexthtml[ssi+"Speaker #".length]) {
    //         case "1":
    //             contexthtml = "<br><b>Speaker #2:</b> " + contexthtml;
    //             break;
    //         case "2":
    //             contexthtml = "<br><b>Speaker #1:</b> " + contexthtml;
    //             break;
    //         default:
    //             break;
    //         }
    //     };
    //     return contexthtml;
    // },
    
 
  //   init_numeric_sliders : function() {
  //       for (i=0; i<exp.n_entities; i++) {
  //           utils.make_slider("#single_slider3", this.make_slider_callback(i));
	 //    exp.has_val1 = false;
	   
  //       }
  //   },

  //   make_slider_callback : function(i) {
  //     return function(event, ui) {
  //       exp.sliderPost = ui.value;
	 // exp.has_val1 = true;
  //       $("#slider_number" + i).html(Math.round(exp.sliderPost[i]*100) + "%");
	
  //     };
  //   },

      button : function() {
      this.radio  = $("input[name='number']:checked").val();
      if (this.radio) {
        this.log_responses();
        $(this.radio).prop('checked', false);
        exp.checkbox.checked  = false;
        //this.checked = false;
        _stream.apply(this);
      } 
      else{
        $('.err').show();
        //this.log_responses();
      }
    },

    log_responses : function() {
      exp.data_trials.push({
        "slide_number_in_experiment" : exp.phase,
        "tgrep_id" : this.generic.TGrep,
        "response" : [this.radio, exp.checkbox.checked],
        "sentence" : this.generic.BestResponse,
      });
  },

 //    button : function() {
	//  if (exp.has_val1 == true){
 //      // check if all fields are filled
 //            this.log_responses();
 //            exp.checkbox.checked  = false;
 //            exp.stimcounter++;
 //            _stream.apply(this);}
 //     else {
	// alert("Please set the slider");
	// }

 //    },


 //    log_responses : function() {
 //      exp.data_trials.push({
 //    "slide_number_in_experiment" : exp.phase,
 //    "tgrep_id" : this.generic.TGrepID,
 //    "response" : [exp.sliderPost, exp.checkbox.checked],
	
 //      });
	
 //    },

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
 

  exp.n_entities = 1;
  exp.names = [];
  exp.all_names = []; 
  exp.trials = [];
  exp.catch_trials = [];
  var stimuli = generate_stim();

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
    "bot",
    "i0",
    "example1",
    "example2",
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

