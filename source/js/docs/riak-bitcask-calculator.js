/* NOTE: we'd like to thank Nico Meyer <nico.meyer at adition.com> for his excellent
   post on this topic.
   http://lists.basho.com/pipermail/riak-users_lists.basho.com/2011-May/004292.html
*/

bites_per = {
    "KiB": 1024,
    "MiB": 1048576,
    "GiB": 1073741824,
    "TiB": 1099511627776,
    "EiB": 1152921504606846976,
    "ZiB": 1180591620717411303424,
    "YiB": 1208925819614629174706176
};

function format_number(str)
{
    str += '';
    x = str.split('.');
    x1 = x[0];
    x2 = x.length > 1 ? '.' + x[1] : '';
    var rgx = /(\d+)(\d{3})/;
    while (rgx.test(x1)) {
    x1 = x1.replace(rgx, '$1' + ',' + '$2');
    }
    return x1 + x2;
};

function format_bytes(bytes) {
    var sizes = ['bytes', 'KiB', 'MiB', 'GiB', 'TiB', 'PiB', 'EiB', 'ZiB', 'YiB'];
    if (bytes == 0) return '';
    if (bytes == 1) return '1 byte';
    var i = parseInt(Math.floor(Math.log(bytes) / Math.log(1024)));
    return ((i == 0)? (bytes / Math.pow(1024, i)) : (bytes / Math.pow(1024, i)).toFixed(1)) + ' ' + sizes[i]; // .round
};

function abbreviate_number(num) {
    var sizes = ['', 'thousand', 'million', 'billion', 'trillion', 'quadrillion', 'quintillion', 'sextillion', 'septillion'];
    if (num < 1000) return num;
    var i = parseInt(Math.floor(Math.log(num) / Math.log(1000)));
    return ((i == 0) ? (num / Math.pow(1000, i)) : (num / Math.pow(1000, i)).toFixed(1)) + ' ' + sizes[i]; // use .round() if you don't want the decimal
};

default_vals = {
    bytes_in_a_ptr: 8,         // 64  bit system
    n_total_keys: 183915891,
    n_bucket_size: 10,         // 10  avg byte length bucket names
    n_key_size: 36,            // 36  avg byte length keys
    n_record_size: 36,         // 36  values
    n_ram: 16,                 // 16  GB RAM
    n_nval: 3                  // "N" value of 3
};

$(document).ready(function() {
    $.each(default_vals, function(k,v) { $("#"+k).val(v) });

    //nodes handlers
    $('#n_total_keys, #n_bucket_size, #n_key_size, #n_record_size, #n_ram, #n_nval').keyup(function () {
        $.each(default_vals, function(k,v) {
            $("#"+k+"_error").text("");
            if(!/(^-?\d\d*$)/.test($("#"+k).val())) {
                $("#"+k+"_error").text("Must be an integer");
                return;
            }
        });
        update_calculations();
    });
    $('select').change(function () {
        update_calculations();
    });
    $('#n_total_keys').focusin(function () {
        $('#node_info').text("How many keys will be stored in your cluster?");
    });
    $('#n_bucket_size').focusin(function () {
        $('#node_info').text("How long will your average bucket name be? Keep in mind that if you are using multi-byte characters or URL enconding your length should reflect that.");
    });
    $('#n_key_size').focusin(function () {
        $('#node_info').text("How long will your average key be?  Here again, keep in mind that if you are using multi-byte characters or URL enconding your length should reflect that.");
    });
    $('#n_record_size').focusin(function () {
        $('#node_info').text("How much data will you store on average per bucket/key pair?");
    });
    $('#n_ram').focusin(function () {
        $('#node_info').text("How much physical RAM on each server will be dedicated to the storage engine?  This should not be the total amount of RAM available, at most it should be 80% of the total RAM.");
    });
    $('#n_nval').focusin(function () {
        $('#node_info').text("What will the cluster's 'N' value be?  How many copies of each item will you store?");
    });
    $('#total').focusin(function () {
        $('#node_info').text("");
    });

    update_calculations();
})

var sizeof_PTR = function () {
    s = parseInt($('#bytes_in_a_ptr').val());
    return (isNaN(s) ? 8 : 4)
}

var sizeof_NullBKP = function () {
    return 22 + 13 + sizeof_PTR();
}

var NumEntries = function () {
    v = Math.abs(parseFloat($('#n_total_keys').val()));
    return isNaN(v) ? 0 : v;
}

var Value = function () {
    v = Math.abs(parseFloat($('#n_record_size').val()));
    return isNaN(v) ? 0 : v;
}

var Key = function () {
    v = Math.abs(parseFloat($('#n_key_size').val()));
    return isNaN(v) ? 0 : v;
}

var N_Val = function () {
    v = Math.abs(parseFloat($('#n_nval').val()));
    return isNaN(v) ? 0 : v;
}

var Bucket = function () {
    v = Math.abs(parseFloat($('#n_bucket_size').val()));
    return isNaN(v) ? 0 : v;
}

var RAM = function () {
    v = Math.abs(parseFloat($('#n_ram').val()) * bites_per["GiB"]);
    return isNaN(v) ? 0 : v;
}

var API = function () {
    return "REST";
}

var estimate_keydir = function () {
    return (Key() + Bucket() + sizeof_NullBKP()) * NumEntries() * N_Val();
}

var estimate_nodes = function () {
    var m = (Key() + Bucket() + sizeof_NullBKP()) * NumEntries() * N_Val();
    return ((m / RAM()) < N_Val() + 1) ? N_Val() + 2 : Math.ceil(m / RAM());
}

var estimate_storage = function () {
    REST_API = 472; // 447 in 0.14.2
    PB_API = 381;   // 356 in 0.14.2

    // using REST/HTTP API (which creates HTTP headers in kb/p's creating unexpected overhead)
    return ( 14 + ( 13 + Bucket() + Key()) +
           ( ( (API() == 'REST') ? REST_API : PB_API ) +
           Bucket() + Key() + Value()) +
           (18 + ( 13 + Bucket() + Key() ) ) )
           * NumEntries() * N_Val();
    // still doesn't account for link headers, tombstones, etc.
}

function update_calculations() {
    if (NumEntries() > 99999999999999999999999999) {
        $('#recomend').text("You have more keys than sub-atomic particles in all known universes.  That's too many.");
        return;
    }
    if (Bucket() < 1) {
        $('#recomend').text("You'll need to have a non-zero bucket size.");
        return;
    }
    if (Key() < 1) {
        $('#recomend').text("You'll need to have a non-zero key size.");
        return;
    }
    if (Value() < 1) {
        $('#recomend').text("You'll need to have a non-zero value size.");
        return;
    }
    if (RAM() < 1) {
        $('#recomend').text("You'll need to allocate a non-zero amount of RAM to data storage.");
        return;
    }
    if (N_Val() < 3) {
        $('#recomend').text("You'll want to deploy at least 3 Riak nodes, 4 would be even better as a starting point.");
    }

    n = estimate_nodes();
    d = estimate_storage();
    r = estimate_keydir();
    $('#recomend').html("<p>To manage your estimated " + abbreviate_number(NumEntries()) + " key/bucket pairs " +
                            " where bucket names are ~" + format_bytes(Bucket()) + ", " +
                            " keys are ~" + format_bytes(Key()) + ", " +
                            " values are ~" + format_bytes(Value()) +
                            " and you are setting aside " + format_bytes(RAM()) +
                            " of RAM per-node for in-memory data management" +
                            " within a cluster that is configured to maintain " +
                            " " + N_Val() + " replicas per key (N = " + N_Val() + ")" +
                            " then Riak, using the Bitcask storage engine, will require at least:</p>" +
                            "<ul>" +
                            "<li>" + n + " nodes</li>" +
                            "<li>" + format_bytes(r/n) + " of RAM total across all nodes</li>" +
                            "<li> " + format_bytes(d/n) + " of storage space per node (" +
                            format_bytes(d) + " total storage space used across all nodes)</li></ul>"
                           );
};

// http://lists.basho.com/pipermail/riak-users_lists.basho.com/2011-May/004292.html
