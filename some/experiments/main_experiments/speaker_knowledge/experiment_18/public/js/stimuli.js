
var path = 'https://raw.githubusercontent.com/leylakursat/some_replacement/master/corpus/corpus18.txt';
var path_attention = 'https://raw.githubusercontent.com/leylakursat/some_replacement/master/corpus/attention_check_items.txt';


function get_data(fullpath) {
    var response = $.ajax({
        type: "GET",
        async: false,
        url: fullpath,
        dataType: "text",
    });
    return response.responseText;
}
 

function generate_stim() {
   
    var contents = get_data(path);
    
    var attention_check = get_data(path_attention);
    contents = contents + attention_check;
    
    var raw = contents;
        var rows = raw.split('\n');
 
        var data = [];
        headings = rows[0].split('\t');
        var total = rows.length
        ;
        //var total = rows.length - 1;
   	
         
        for (var i = 1; i < total; i++) {
		
                data.push(rows[i].split('\t'));
            }
 
        var stim = data.map(function(row) {
            return row.reduce(function(result, field, index) {
                result[headings[index]] = field;
                return result;
            }, {});
        });
   
    return stim;
}
 
function generate_training_stim(n) {
    var make_obj = function(arr) {
        var s1 = arr[0];
        var s2 = arr[1];
        var s3 = arr[2];
	   var s4 = arr[3];
        obj = {
            "TGrepID" : s1,
            "EntireSentence" : s2,
            "context" : s3,
	    "BestResponse" : s4
        }
        return obj;
    }

    examples = [
       ["362:41",
        "that's just a matter of defining priorities, i guess or some priorities anyway.",
        "that's just a matter of defining priorities, i guess or some priorities anyway, but not both.",
	"###that would, certainly help.###speakera159t159.###i mean, i mean we may not, we may not have as high a standard of living###speakerb160t159.###speakerb162t162.###uh-huh.###speakera163t163.### but the qua-, but actually have a truer standard of living.###speakerb164t164.###right."
        ],
 
       ["362:41",
        "that's just a matter of defining priorities, i guess or some priorities anyway.",
        "that's just a matter of defining priorities, i guess or some priorities anyway, but not both.",
	"###that would, certainly help.###speakera159t159.###i mean, i mean we may not, we may not have as high a standard of living###speakerb160t159.###speakerb162t162.###uh-huh.###speakera163t163.### but the qua-, but actually have a truer standard of living.###speakerb164t164.###right."
        ],
 
        ["362:41",
        "that's just a matter of defining priorities, i guess or some priorities anyway.",
        "that's just a matter of defining priorities, i guess or some priorities anyway, but not both.",
	"###that would, certainly help.###speakera159t159.###i mean, i mean we may not, we may not have as high a standard of living###speakerb160t159.###speakerb162t162.###uh-huh.###speakera163t163.### but the qua-, but actually have a truer standard of living.###speakerb164t164.###right."
        ],
 
        ["362:41",
        "that's just a matter of defining priorities, i guess or some priorities anyway.",
        "that's just a matter of defining priorities, i guess or some priorities anyway, but not both.",
	"###that would, certainly help.###speakera159t159.###i mean, i mean we may not, we may not have as high a standard of living###speakerb160t159.###speakerb162t162.###uh-huh.###speakera163t163.### but the qua-, but actually have a truer standard of living.###speakerb164t164.###right."
        ],
    ]
    training_examples = [];
    var ntex = examples.length;
    
    var rnums = [];
    while (rnums.length < n) {
        rnum = Math.floor((Math.random() * ntex));
        if (rnums.indexOf(rnum) > -1) continue;
        rnums.push(rnum);
    }
    for (num of rnums) {
        training_examples.push(make_obj(examples[num]));
    }
    return training_examples;
}

