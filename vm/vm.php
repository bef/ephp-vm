<?php

class __VM_Stack
{
	protected $s = array();
	public function push($v)
	{
		$this->s[] = $v;
	}
	public function pop()
	{
		return array_pop($this->s);
	}
}
class __VM_Scope
{
	protected $ss = array(); // scope stack
	public function __VM_Scope()
	{
		$this->ss[] = array(); // global scope
	}
	public function enter() // enter a new scope
	{
		$this->ss[] = array();
	}
	public function leave() // leave scope and destroy variables
	{
		array_pop($this->ss);
	}
	public function set($key, $value)
	{
		$s = &$this->ss[count($this->ss)-1];
		
		// check for php variable, but prefer local scope
		if (!array_key_exists($key, $s) and (array_key_exists($key, $GLOBALS) or $key[0] == '_')) {
			$GLOBALS[$key] = $value;
			return;
		}
		
		// create new variable in local scope
		$s[$key] = $value;
	}
	public function get($key)
	{
		// check for variable in any scope
		// for ($i = count($this->ss)-1; $i >= 0; $i--)
		// 	if (array_key_exists($key, $this->ss[$i]))
		// 		return $this->ss[$i][$key];
		
		// check for variable in local scope
		$s = &$this->ss[count($this->ss)-1];
		if (array_key_exists($key, $s))
			return $s[$key];
		
		// variable not found -> return php's global var
		return $GLOBALS[$key];
	}
}

function __read_i(&$bc, &$p)
{
	$a = ord($bc[++$p]); // length in bytes
	$b = 0;
	while ($a & 0x7f)
		$b += ord($bc[++$p]) << (((--$a) & 0x7f) * 8);
	if ($a & 0x80)
		$b = -$b;
	return $b;
}

function __vm_run($bc)
{
	$p = 0; // code pointer
	$s = new __VM_Stack(); // stack
	$funcs = array(); // functions
	$v = new __VM_Scope(); // variables
	while ($p < strlen($bc)) {
		// var_dump($s);
		// echo "p is " . $p . ", op is " . ord($bc[$p]) . "(" . $bc[$p] . ")\n";
		// readline("--step-debug---->");
		
		switch($bc[$p]) {
			case "\000": // nop
				break;
			
			//------------------------------------------------------------
			// internal functions
			//------------------------------------------------------------
			
			case "\001": // echo
				echo $s->pop();
				break;

			//------------------------------------------------------------
			// variables and stack operations
			//------------------------------------------------------------
		
			case 'a': // set/assign variable
				$a = $s->pop();
				$b = $s->pop();
				$v->set($a, $b);
				$s->push($b);
				break;
			case 'g': // get variable
				$s->push($v->get($s->pop()));
				break;
			case 'P': // pop stack and forget value
				$s->pop();
				break;
			case 'S': // swap / exchange last two stack values
				$a = $s->pop();
				$b = $s->pop();
				$s->push($a);
				$s->push($b);
				break;
			
			//------------------------------------------------------------
			// functions
			//------------------------------------------------------------
		
			case 'f': // function call
				$a = $s->pop(); // argc
				$b = array();
				while ($a--)
					$b[] = $s->pop();
				$b = array_reverse($b);
				$c = $s->pop(); // function name
				if (array_key_exists($c, $funcs)) {
					$v->enter(); // new variable scope
					
					// set args in new scope
					$v->set('__args__', $b);
					for ($i = 0; $i < count($funcs[$c]['args']); $i++) {
						if ($i < count($b))
							$v->set($funcs[$c]['args'][$i], $b[$i]);
						else
							$v->set($funcs[$c]['args'][$i], null);
					}
					
					$s->push($p); // store return address
					$p = $funcs[$c]['addr']; // set code pointer
				} elseif (is_callable($c)) {
					$a = call_user_func_array($c, $b);
					$s->push($a);
				} else {
					die("no such function " . $c);
				}
				break;
			case 'r': // return from function
				$v->leave();
				$p = $s->pop();
				break;
			case 'F': // function definition
				$a = $s->pop(); // argc
				$b = array(); // argnames
				while ($a--)
					$b[] = $s->pop($s);
				$b = array_reverse($b);
				$c = $s->pop(); // function name
				$funcs[$c] = array(
					'addr' => $p + 6 + 1, // 6 bytes jump addr, 1 byte jump op
					'args' => $b);
				break;
			
			//------------------------------------------------------------
			// conditions
			//------------------------------------------------------------
			
			case 'c': // conditional jump
				$a = $s->pop(); // jump target
				$b = $s->pop(); // condition
				if ($b)
					$p += $a;
				break;
			case 'C': // !conditional jump
				$a = $s->pop(); // jump target
				$b = $s->pop(); // condition
				if (!$b)
					$p += $a;
				break;
			case 'j': // jump
				$a = $s->pop();
				$p += $a;
				break;
			
			//------------------------------------------------------------
			// data types
			//------------------------------------------------------------
			
			case 'i': // integer
				$b = __read_i($bc, $p);
				$s->push($b);
				break;
			case 's': // string
				$a = __read_i($bc, $p);
				$b = "";
				while ($a-- > 0)
					$b .= $bc[++$p];
				$s->push($b);
				break;

			//------------------------------------------------------------
			// operators
			//------------------------------------------------------------

			case '+': // add
				$b = $s->pop();
				$a = $s->pop();
				$s->push($a + $b);
				break;
			case '-': // sub
				$b = $s->pop();
				$a = $s->pop();
				$s->push($a - $b);
				break;
			case '*':
				$b = $s->pop();
				$a = $s->pop();
				$s->push($a * $b);
				break;
			case '/':
				$b = $s->pop();
				$a = $s->pop();
				$s->push($a / $b);
				break;
			case '%':
				$b = $s->pop();
				$a = $s->pop();
				$s->push($a % $b);
				break;
			case '.': // concat
				$b = $s->pop();
				$a = $s->pop();
				$s->push($a . $b);
				break;
			case 'A': // logical and
				$a = $s->pop();
				$b = $s->pop();
				$s->push($a and $b);
				break;
			case 'O': // logical or
				$b = $s->pop();
				$a = $s->pop();
				$s->push($a or $b);
				break;
			case 'X': // logical xor
				$b = $s->pop();
				$a = $s->pop();
				$s->push($a xor $b);
				break;
			case '&':
				$b = $s->pop();
				$a = $s->pop();
				$s->push($a & $b);
				break;
			case '|':
				$b = $s->pop();
				$a = $s->pop();
				$s->push($a | $b);
				break;
			case '^':
				$b = $s->pop();
				$a = $s->pop();
				$s->push($a ^ $b);
				break;
			case '[':
				$b = $s->pop();
				$a = $s->pop();
				$s->push($a << $b);
				break;
			case ']':
				$b = $s->pop();
				$a = $s->pop();
				$s->push($a >> $b);
				break;
			case ',':
				$b = $s->pop();
				$a = $s->pop();
				$s->push($a && $b);
				break;
			case ';':
				$b = $s->pop();
				$a = $s->pop();
				$s->push($a || $b);
				break;
			case '=':
				$b = $s->pop();
				$a = $s->pop();
				$s->push($a == $b);
				break;
			case '<':
				$b = $s->pop();
				$a = $s->pop();
				$s->push($a < $b);
				break;
			case '>':
				$b = $s->pop();
				$a = $s->pop();
				$s->push($a > $b);
				break;
			case '{':
				$b = $s->pop();
				$a = $s->pop();
				$s->push($a <= $b);
				break;
			case '}':
				$b = $s->pop();
				$a = $s->pop();
				$s->push($a >= $b);
				break;
			// case '_':
			// 	$b = $s->pop();
			// 	$a = $s->pop();
			// 	$s->push($a === $b);
			// 	break;
			// case '@':
			// 	$b = $s->pop();
			// 	$a = $s->pop();
			// 	$s->push($a !== $b);
			// 	break;
			case '"':
				$b = $s->pop();
				$a = $s->pop();
				$s->push($a != $b);
				break;
			case 'I': // increment
				$b = $s->pop();
				$a = $v->get($b) + 1;
				$v->set($b, $a);
				$s->push($a);
				break;
			case 'D': // decrement
				$b = $s->pop();
				$a = $v->get($b) - 1;
				$v->set($b, $a);
				$s->push($a);
				break;
			case '~': // binary not
				$a = $s->pop();
				$s->push(~$a);
				break;
			case '!': // boolean not
				$a = $s->pop();
				$s->push(!$a);
				break;
			
			//------------------------------------------------------------
			default:
				die("boo");
		}

		$p++;
	}
}


if (array_key_exists('argv', $_SERVER) and count($_SERVER['argv']) >= 2) {
	// echo "VM\n";
	$input = base64_decode($_SERVER['argv'][1]);
	__vm_run($input);
}
	
