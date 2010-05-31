<?php
include("../vm/vm.php");
$code = base64_decode("cwEGc3RybGVucwEEY29kZWdpAQFmaQIH0D4AAABpASZjcwEBQgFzAQRjb2RlZ3MBBGNvZGVnLnMBBGNvZGVhUAAAAGkBD2pzAQFBAWkAcwEDcnVuYVA=");
$run = true;
while ($run)
	__vm_run($code);

echo "\n" . strlen($code) . "\n";