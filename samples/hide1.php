<?php
include("../vm/vm.php");
$code = base64_decode( "cwEIc2NyYW1ibGVzAQNrZXlzAQRkYXRhaQECRgAAAGkB92pzAHMBA291dGFQaQBzAQFpYVBzAQFpZ3MBBnN0cmxlbnMBBGRhdGFnaQEBZjwAAABpAbZDcwEGc3Vic3RycwEDa2V5Z3MBAWlncwEGc3RybGVucwEDa2V5Z2kBAWYlaQEBaQEDZnMBBmtleWNocmFQcwEGc3Vic3RycwEEZGF0YWdzAQFpZ2kBAWkBA2ZzAQdkYXRhY2hyYVBzAQNvdXRncwEDY2hycwEDb3JkcwEGa2V5Y2hyZ2kBAWZzAQNvcmRzAQdkYXRhY2hyZ2kBAWZeaQEBZi5zAQNvdXRhUHMBAWlJUAAAAGmB2GpzAQNvdXRnU3JpAFNycwEIc2NyYW1ibGVzAQNrZXlncwEEZGF0YWdpAQJmcwEIZGF0YV9vdXRhUA==");

$key = $code;
$data = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Suspendisse nec eros lectus. Suspendisse laoreet, urna ut fermentum aliquet, ligula ante accumsan velit, dapibus consequat nunc eros in lacus. Aenean iaculis dignissim nibh, vel ornare mi congue sit amet. Fusce molestie pretium ipsum quis commodo. Phasellus nec laoreet ante. Ut non egestas urna. Curabitur eros lacus, bibendum vitae mollis ac, aliquet quis est. Aliquam sed risus erat, et varius elit. Nulla facilisi. Donec et tellus nibh. Nulla eu nisi ac nulla tristique convallis.";
$data_out = "";
__vm_run($code);
var_dump(base64_encode($data_out));

$data = $data_out;
__vm_run($code);
var_dump($data_out);
