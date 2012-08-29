These calculators will assist you in sizing your cluster if you plan to use the
default [[Bitcask]] storage back end.

This page is designed to give you a rough estimate when sizing your cluster.
The calculations are a _best guess_, and the tend to be a bit on the
conservative side.  It's important to include a bit of head-room as well as
room for unexpected growth so that if demand exceeds expectations you'll be
able to add more nodes to the cluster and stay ahead of your requirements.

<div id="node_info" class="calc_info"></div>
<div class="calculator">
   <ul>
     <li>
       <label for="n_total_keys">Total Number of Keys:</label>
       <input id="n_total_keys"  type="text" size="12" name="n_total_keys" value="" class="calc_input">
       <span class="error_span" id="n_total_keys_error"></span>
     </li>
     <li>
       <label for="n_bucket_size">Average Bucket Size (Bytes):</label>
       <input id="n_bucket_size"type="text" size="7" name="n_bucket_size" value="" class="calc_input">
       <span class="error_span"id="n_bucket_size_error"></span>
     </li>
     <li>
       <label for="n_key_size">Average Key Size (Bytes):</label>
       <input type="text" size="2" name="n_key_size" id="n_key_size" value="" class="calc_input">
       <span class="error_span" id="n_key_size_error"></span>
     </li>
     <li>
       <label for="n_record_size">Average Value Size (Bytes):</label>
       <input id="n_record_size"type="text" size="7" name="n_record_size" value="" class="calc_input">
       <span class="error_span"id="n_record_size_error"></span>
     </li>
     <li>
       <label for="n_ram">RAM Per Node (in GB):</label>
       <input type="text" size="4" name="n_ram" id="n_ram" value="" class="calc_input">
       <span class="error_span" id="n_ram_error"></span>
     </li>
     <li>
       <label for="n_nval"><i>N</i> (Number of Write Copies):</label>
       <input type="text" size="2" name="n_nval" id="n_nval" value="" class="calc_input">
       <span class="error_span" id="n_nval_error"></span>
     </li>
</ul>
</div>

### Recomendations

<span id="recomend"></span>
