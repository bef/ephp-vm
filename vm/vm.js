
function __vm_scope() {
	this.ss = []; // scope stack
	this.ss.push([]); // global scope
	this.enter = function() {
		this.ss.push([]);
	}
	this.leave = function() {
		this.ss.pop();
	}
	this.set = function(key, value) {
		key = key.replace(/[^a-zA-Z0-9_]/g, '') // restrict characters in key
		
		// set js var, but prefer local var
		if ((typeof this.ss[this.ss.length-1][key] == 'undefined') // not local var
		 	&& (typeof window[key] != 'undefined' || key[0] == '_')) { // and js var exists or starts with '_'
			window[key] = value
		}
		
		// set local var
		this.ss[this.ss.length-1][key] = value;
	}
	this.get = function(key) {
		key = key.replace(/[^a-zA-Z0-9_]/g, '') // restrict characters in key
		
		// local scope
		if (typeof this.ss[this.ss.length-1][key] != 'undefined') {
			return this.ss[this.ss.length-1][key];
		}
		
		// javascript variable
		if (typeof window[key] != 'undefined') {
			return window[key];
		}
		
		// not found
		return undefined;
	}
}



function __vm()
{
	this.bc = null; // bytecode
	this.p = 0; // code pointer
	this.s = [] // stack
	this.funcs = []; // functions
	this.v = new __vm_scope(); // variables
	
	this.echo = function(s) { document.write(s); } // internal function
	
	this.run = function(bc) {
		if (bc != null)
			this.bc = bc;
		var a, b, c;
		while (this.p < this.bc.length) {
			// document.write('['+this.p+'] -> ' + this.bc.charCodeAt(this.p) + '\n');
			switch (this.bc.charCodeAt(this.p)) {
				case 000: // nop
					break;
				//------------------------------------------------------------
				// internal functions
				//------------------------------------------------------------

				case 001: // echo
					this.echo(this.s.pop());
					break;

				//------------------------------------------------------------
				// variables and stack operations
				//------------------------------------------------------------
				
				case 0141: // (a) set/assign variable
					a = this.s.pop();
					b = this.s.pop();
					this.v.set(a, b);
					this.s.push(b);
					break;
				case 0147: // (g) get variable
					this.s.push(this.v.get(this.s.pop()));
					break;
				case 0120: // (P) pop stack and forget value
					this.s.pop();
					break;
				case 0123: // (S) swap / exchange last two stack values
					a = this.s.pop();
					b = this.s.pop();
					this.s.push(a);
					this.s.push(b);
					break;

				//------------------------------------------------------------
				// functions
				//------------------------------------------------------------

				case 0146: // (f) function call
					a = this.s.pop(); // argc
					b = [];
					while (a--)
						b.push(this.s.pop());
					b.reverse();
					c = this.s.pop(); // function name
					if (c in this.funcs) {
						this.v.enter(); // new variable scope

						// set args in new scope
						this.v.set('__args__', b);
						for (var i = 0; i < this.funcs[c]['args'].length; i++) {
							if (i < b.length)
								this.v.set(this.funcs[c]['args'][i], b[i]);
							else
								this.v.set(this.funcs[c]['args'][i], null);
						}

						this.s.push(this.p); // store return address
						this.p = this.funcs[c]['addr']; // set code pointer
					} else if (typeof window[c] == "function") {
						a = window[c].apply(window, b);
						this.s.push(a);
					} else {
						throw "no such function " + c;
					}
					break;
				case 0162: // (r) return from function
					this.v.leave();
					this.p = this.s.pop();
					break;
				case 0106: // (F) function definition
					a = this.s.pop(); // argc
					b = []; // argnames
					while (a--)
						b.push(this.s.pop());
					b.reverse();
					c = this.s.pop(); // function name
					this.funcs[c] = {
						'addr' : this.p + 6 + 1, // 6 bytes jump addr, 1 byte jump op
						'args' : b};
					break;

				//------------------------------------------------------------
				// conditions
				//------------------------------------------------------------

				case 0143: // (c) conditional jump
					a = this.s.pop(); // jump target
					b = this.s.pop(); // condition
					if (b)
						this.p += a;
					break;
				case 0103: // (C) !conditional jump
					a = this.s.pop(); // jump target
					b = this.s.pop(); // condition
					if (!b)
						this.p += a;
					break;
				case 0152: // (j) jump
					a = this.s.pop();
					this.p += a;
					break;

				//------------------------------------------------------------
				// data types
				//------------------------------------------------------------
				case 0151: // (i) integer
					b = this.__read_i();
					this.s.push(b);
					break;
				case 0163: // (s) string
					a = this.__read_i();
					b = "";
					while (a-- > 0)
						b += bc[++this.p];
					this.s.push(b);
					break;

				//------------------------------------------------------------
				// operators
				//------------------------------------------------------------
				case 053: // (+) add
				case 056: // (.) concat
					b = this.s.pop();
					a = this.s.pop();
					this.s.push(a + b);
					break;
				case 055: // (-) sub
					b = this.s.pop();
					a = this.s.pop();
					this.s.push(a - b);
					break;
				case 052: // (*)
					b = this.s.pop();
					a = this.s.pop();
					this.s.push(a * b);
					break;
				case 057: // (/)
					b = this.s.pop();
					a = this.s.pop();
					this.s.push(a / b);
					break;
				case 045: // (%) modulo
					b = this.s.pop();
					a = this.s.pop();
					this.s.push(a % b);
					break;
				case 0101: // (A) logical and
				case 054: // (,) bool and
					b = this.s.pop();
					a = this.s.pop();
					this.s.push(a && b);
					break;
				case 0117: // (O) logical or
				case 073: // (;) bool or
					b = this.s.pop();
					a = this.s.pop();
					this.s.push(a || b);
					break;
				case 0130: // (X) logical xor
					b = this.s.pop();
					a = this.s.pop();
					this.s.push(a && !b || !a && b);
					break;
				case 046: // (&) binary and
					b = this.s.pop();
					a = this.s.pop();
					this.s.push(a & b);
					break;
				case 0174: // (|) binary or
					b = this.s.pop();
					a = this.s.pop();
					this.s.push(a | b);
					break;
				case 0136: // (^) binary xor
					b = this.s.pop();
					a = this.s.pop();
					this.s.push(a ^ b);
					break;
				case 0133: // ([) shift left
					b = this.s.pop();
					a = this.s.pop();
					this.s.push(a << b);
					break;
				case 0135: // (]) shift right
					b = this.s.pop();
					a = this.s.pop();
					this.s.push(a >> b);
					break;
				case 075: // (=)
					b = this.s.pop();
					a = this.s.pop();
					this.s.push(a == b);
					break;
				case 074: // (<)
					b = this.s.pop();
					a = this.s.pop();
					this.s.push(a < b);
					break;
				case 076: // (>)
					b = this.s.pop();
					a = this.s.pop();
					this.s.push(a > b);
					break;
				case 0173: // ({)
					b = this.s.pop();
					a = this.s.pop();
					this.s.push(a <= b);
					break;
				case 0175: // ({)
					b = this.s.pop();
					a = this.s.pop();
					this.s.push(a >= b);
					break;
				case 042: // (")
					b = this.s.pop();
					a = this.s.pop();
					this.s.push(a != b);
					break;
				case 0111: // (I) increment
					b = this.s.pop();
					a = this.v.get(b) + 1;
					this.v.set(b, a);
					this.s.push(a);
					break;
				case 0104: // (D) decrement
					b = this.s.pop();
					a = this.v.get(b) - 1;
					this.v.set(b, a);
					this.s.push(a);
					break;

				case 0176: // (~) binary not
					a = this.s.pop();
					this.s.push(~a);
					break;
				case 041: // (!) boolean not
					a = this.s.pop();
					this.s.push(!a);
					break;

				//------------------------------------------------------------
				default:
					// window.stop();
					// throw 'boo';
					throw this.bc.charCodeAt(this.p);
			}
			this.p++;
		}
	}
	this.__read_i = function() {
		var a, b;
		this.p++;
		a = this.bc.charCodeAt(this.p); // length in bytes
		// document.write('length: ' + a + ' at ' + this.p + '\n');
		b = 0;
		while (a & 0x7f) {
			this.p++;
			a--;
			b += this.bc.charCodeAt(this.p) << ((a & 0x7f) * 8);
		}
		if (a & 0x80)
			b = -b;
		return b;
	}
}

function __vm_run(bc)
{
	var vm = new __vm();
	return vm.run(bc);
}
