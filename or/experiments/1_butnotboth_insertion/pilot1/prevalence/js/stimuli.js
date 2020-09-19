//var path = 'https://web.stanford.edu/~chakia/CoCoLab/corpus_generics/generics_project/results/';
var path = 'http://localhost:8000/prevalence/js/final_database.txt';
//var fname = 'sample19_plus.tab';
 
function get_data(fullpath) {
    var response = $.ajax({
        type: "GET",
        async: false,
        url: fullpath,
        dataType: "text",
    });
    return response.responseText;
}
 
function generate_stim(n, rand) {
    var contents = get_data(path);
    var raw = contents;
        var rows = raw.split('\n');
 
        var data = [];
        headings = rows[0].split('\t');
 
        if (rand == true) {
            var total = rows.length - 1;
            var rnums = [];
            while (rnums.length < n) {
                rnum = Math.floor((Math.random() * total) + 1);
                if (rnums.indexOf(rnum) > -1) continue;
                rnums.push(rnum);
            }
 
            for (num of rnums) {
                data.push(rows[num].split('\t'));
            }
        } else {
            for (var i = 1; i < n + 1; i++) {
                data.push(rows[i].split('\t'));
            }
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
            "Item_ID" : s1,
            "Sentence" : s2,
            "sentence_butNotBoth" : s3,
	    "Context" : s4
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
