<?php
include("../vm/vm.php");
function bar() { return "123"; }
function foo($input) { echo $input; }
__vm_run(base64_decode("cwEDZm9vcwEEdGVzdHMBA2JhcmkAZi5pAQFmUA=="));

