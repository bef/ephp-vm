load('../vm/vm.js');
vm = new __vm();
vm.echo = function(s) { print(s);};
window = new Object();
window.is_int = function(x) { return typeof(x) == 'number' ? 1 : 0; };
window.is_string = function(x) { return typeof(x) == 'string' ? 1 : 0; };
