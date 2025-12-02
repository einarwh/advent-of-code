(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}




// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	/**_UNUSED/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**_UNUSED/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**/
	if (typeof x.$ === 'undefined')
	//*/
	/**_UNUSED/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0 = 0;
var _Utils_Tuple0_UNUSED = { $: '#0' };

function _Utils_Tuple2(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2_UNUSED(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3_UNUSED(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr(c) { return c; }
function _Utils_chr_UNUSED(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _List_Nil = { $: 0 };
var _List_Nil_UNUSED = { $: '[]' };

function _List_Cons(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons_UNUSED(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log = F2(function(tag, value)
{
	return value;
});

var _Debug_log_UNUSED = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString(value)
{
	return '<internals>';
}

function _Debug_toString_UNUSED(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File !== 'undefined' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[36m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash_UNUSED(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.R.I === region.W.I)
	{
		return 'on line ' + region.R.I;
	}
	return 'on lines ' + region.R.I + ' through ' + region.W.I;
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return !isNaN(word)
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**_UNUSED/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (Object.prototype.hasOwnProperty.call(value, key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap_UNUSED(value) { return { $: 0, a: value }; }
function _Json_unwrap_UNUSED(value) { return value.a; }

function _Json_wrap(value) { return value; }
function _Json_unwrap(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	var unwrapped = _Json_unwrap(value);
	if (!(key === 'toJSON' && typeof unwrapped === 'function'))
	{
		object[key] = unwrapped;
	}
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.aC,
		impl.aK,
		impl.aI,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**_UNUSED/, _Json_errorToString(result.a) /**/);
	var managers = {};
	var initPair = init(result.a);
	var model = initPair.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		var pair = A2(update, msg, model);
		stepper(model = pair.a, viewMetadata);
		_Platform_enqueueEffects(managers, pair.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, initPair.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/elm/core/issues/980
//   https://github.com/elm/core/pull/981
//   https://github.com/elm/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.


// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;


function _Platform_enqueueEffects(managers, cmdBag, subBag)
{
	_Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.p, fx.q, fx.r);
	}
	_Platform_effectsActive = false;
}


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				s: bag.n,
				t: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.t)
		{
			x = temp.s(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		u: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		u: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**/
	var node = args['node'];
	//*/
	/**_UNUSED/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS
//
// For some reason, tabs can appear in href protocols and it still works.
// So '\tjava\tSCRIPT:alert("!!!")' and 'javascript:alert("!!!")' are the same
// in practice. That is why _VirtualDom_RE_js and _VirtualDom_RE_js_html look
// so freaky.
//
// Pulling the regular expressions out to the top level gives a slight speed
// boost in small benchmarks (4-10%) but hoisting values to reduce allocation
// can be unpredictable in large programs where JIT may have a harder time with
// functions are not fully self-contained. The benefit is more that the js and
// js_html ones are so weird that I prefer to see them near each other.


var _VirtualDom_RE_script = /^script$/i;
var _VirtualDom_RE_on_formAction = /^(on|formAction$)/i;
var _VirtualDom_RE_js = /^\s*j\s*a\s*v\s*a\s*s\s*c\s*r\s*i\s*p\s*t\s*:/i;
var _VirtualDom_RE_js_html = /^\s*(j\s*a\s*v\s*a\s*s\s*c\s*r\s*i\s*p\s*t\s*:|d\s*a\s*t\s*a\s*:\s*t\s*e\s*x\s*t\s*\/\s*h\s*t\s*m\s*l\s*(,|;))/i;


function _VirtualDom_noScript(tag)
{
	return _VirtualDom_RE_script.test(tag) ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return _VirtualDom_RE_on_formAction.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'outerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return _VirtualDom_RE_js.test(value)
		? /**/''//*//**_UNUSED/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return _VirtualDom_RE_js_html.test(value)
		? /**/''//*//**_UNUSED/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlJson(value)
{
	return (
		(typeof _Json_unwrap(value) === 'string' && _VirtualDom_RE_js_html.test(_Json_unwrap(value)))
		||
		(Array.isArray(_Json_unwrap(value)) && _VirtualDom_RE_js_html.test(String(_Json_unwrap(value))))
	)
		? _Json_wrap(
			/**/''//*//**_UNUSED/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		) : value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2($elm$json$Json$Decode$map, func, handler.a)
				:
			A3($elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		q: func(record.q),
		S: record.S,
		P: record.P
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!$elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.q;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.S;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.P) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.aC,
		impl.aK,
		impl.aI,
		function(sendToApp, initialModel) {
			var view = impl.aL;
			/**/
			var domNode = args['node'];
			//*/
			/**_UNUSED/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.aC,
		impl.aK,
		impl.aI,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.Q && impl.Q(sendToApp)
			var view = impl.aL;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.av);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.aJ) && (_VirtualDom_doc.title = title = doc.aJ);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.aE;
	var onUrlRequest = impl.aF;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		Q: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = $elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.ai === next.ai
							&& curr._ === next._
							&& curr.af.a === next.af.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		aC: function(flags)
		{
			return A3(impl.aC, flags, _Browser_getUrl(), key);
		},
		aL: impl.aL,
		aK: impl.aK,
		aI: impl.aI
	});
}

function _Browser_getUrl()
{
	return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { aA: 'hidden', aw: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { aA: 'mozHidden', aw: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { aA: 'msHidden', aw: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { aA: 'webkitHidden', aw: 'webkitvisibilitychange' }
		: { aA: 'hidden', aw: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		am: _Browser_getScene(),
		ap: {
			ar: _Browser_window.pageXOffset,
			as: _Browser_window.pageYOffset,
			aq: _Browser_doc.documentElement.clientWidth,
			Z: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		aq: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		Z: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			am: {
				aq: node.scrollWidth,
				Z: node.scrollHeight
			},
			ap: {
				ar: node.scrollLeft,
				as: node.scrollTop,
				aq: node.clientWidth,
				Z: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			am: _Browser_getScene(),
			ap: {
				ar: x,
				as: y,
				aq: _Browser_doc.documentElement.clientWidth,
				Z: _Browser_doc.documentElement.clientHeight
			},
			ay: {
				ar: x + rect.left,
				as: y + rect.top,
				aq: rect.width,
				Z: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}



function _Time_now(millisToPosix)
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(millisToPosix(Date.now())));
	});
}

var _Time_setInterval = F2(function(interval, task)
{
	return _Scheduler_binding(function(callback)
	{
		var id = setInterval(function() { _Scheduler_rawSpawn(task); }, interval);
		return function() { clearInterval(id); };
	});
});

function _Time_here()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(
			A2($elm$time$Time$customZone, -(new Date().getTimezoneOffset()), _List_Nil)
		));
	});
}


function _Time_getZoneName()
{
	return _Scheduler_binding(function(callback)
	{
		try
		{
			var name = $elm$time$Time$Name(Intl.DateTimeFormat().resolvedOptions().timeZone);
		}
		catch (e)
		{
			var name = $elm$time$Time$Offset(new Date().getTimezoneOffset());
		}
		callback(_Scheduler_succeed(name));
	});
}
var $elm$core$Basics$EQ = 1;
var $elm$core$Basics$GT = 2;
var $elm$core$Basics$LT = 0;
var $elm$core$List$cons = _List_cons;
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === -2) {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (!node.$) {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Result$Err = function (a) {
	return {$: 1, a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 0, a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 2, a: a};
};
var $elm$core$Basics$False = 1;
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Maybe$Nothing = {$: 1};
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 0:
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 1) {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 1:
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 2:
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 0, a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 1, a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.a) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.c),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.c);
		} else {
			var treeLen = builder.a * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.d) : builder.d;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.a);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.c) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.c);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{d: nodeList, a: (len / $elm$core$Array$branchFactor) | 0, c: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = 0;
var $elm$core$Result$isOk = function (result) {
	if (!result.$) {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 0:
			return 0;
		case 1:
			return 1;
		case 2:
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Browser$External = function (a) {
	return {$: 1, a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$browser$Browser$Dom$NotFound = $elm$core$Basics$identity;
var $elm$url$Url$Http = 0;
var $elm$url$Url$Https = 1;
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {Y: fragment, _: host, ad: path, af: port_, ai: protocol, aj: query};
	});
var $elm$core$String$contains = _String_contains;
var $elm$core$String$length = _String_length;
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$indexes = _String_indexes;
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$toInt = _String_toInt;
var $elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, '@', str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, ':', str);
			if (!_v0.b) {
				return $elm$core$Maybe$Just(
					A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_v0.b.b) {
					var i = _v0.a;
					var _v1 = $elm$core$String$toInt(
						A2($elm$core$String$dropLeft, i + 1, str));
					if (_v1.$ === 1) {
						return $elm$core$Maybe$Nothing;
					} else {
						var port_ = _v1;
						return $elm$core$Maybe$Just(
							A6(
								$elm$url$Url$Url,
								protocol,
								A2($elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '/', str);
			if (!_v0.b) {
				return A5($elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _v0.a;
				return A5(
					$elm$url$Url$chompBeforePath,
					protocol,
					A2($elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '?', str);
			if (!_v0.b) {
				return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _v0.a;
				return A4(
					$elm$url$Url$chompBeforeQuery,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '#', str);
			if (!_v0.b) {
				return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
			} else {
				var i = _v0.a;
				return A3(
					$elm$url$Url$chompBeforeFragment,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$core$String$startsWith = _String_startsWith;
var $elm$url$Url$fromString = function (str) {
	return A2($elm$core$String$startsWith, 'http://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		0,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		1,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = $elm$core$Basics$identity;
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(0);
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return 0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0;
		return A2($elm$core$Task$map, tagger, task);
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			A2($elm$core$Task$map, toMessage, task));
	});
var $elm$browser$Browser$element = _Browser_element;
var $author$project$Main$Sample = 1;
var $author$project$Main$defaultTickInterval = 5;
var $author$project$Main$input = 'L37\nL16\nL20\nL18\nR30\nR11\nR47\nR38\nL49\nR22\nR23\nL43\nR47\nL40\nR20\nL42\nL17\nR38\nR30\nL36\nR11\nL8\nL42\nR45\nL34\nR46\nL16\nL30\nR48\nR38\nR26\nL20\nL9\nL28\nL29\nR16\nR31\nR23\nR38\nR43\nR47\nR49\nL19\nR29\nL50\nR14\nL44\nL24\nR33\nL99\nR99\nL26\nR25\nL46\nL75\nR14\nL85\nL3\nR44\nR83\nR94\nR80\nR35\nR67\nR71\nR6\nL5\nR60\nR98\nL59\nR64\nR36\nL34\nR43\nL94\nL65\nR50\nR31\nL31\nL20\nR20\nR85\nL13\nR96\nR38\nR75\nR65\nR24\nL69\nR99\nL4\nL45\nL37\nR2\nL69\nL47\nL72\nR45\nR13\nL86\nL24\nL76\nL19\nR14\nL53\nR20\nL9\nL95\nR77\nL69\nR44\nL10\nL90\nR90\nL19\nL4\nL77\nR305\nR56\nL58\nL3\nR95\nR5\nR164\nL40\nL860\nR36\nR45\nR76\nR8\nR44\nL73\nR66\nR18\nL40\nL529\nR68\nL183\nR36\nL96\nL47\nR97\nL80\nR890\nR96\nR4\nR5\nR64\nL72\nL97\nL93\nL286\nL9\nR88\nR78\nL878\nR32\nR93\nR175\nR76\nL13\nL63\nR780\nL43\nR827\nR84\nR71\nR83\nR80\nL668\nL872\nL742\nL79\nR89\nR62\nR828\nR712\nR88\nL98\nR198\nR148\nR49\nL1\nR4\nR412\nL15\nR40\nL28\nL509\nL16\nL784\nL64\nR64\nR8\nL618\nL90\nL815\nR15\nR40\nR29\nR35\nR596\nR76\nR73\nL49\nL96\nR96\nR25\nL725\nL17\nL83\nL25\nL75\nL68\nL81\nL14\nR938\nL913\nL62\nL20\nR20\nL385\nL92\nL12\nL53\nL958\nL23\nL77\nL81\nL80\nL739\nR81\nR719\nR71\nL86\nR260\nL45\nL87\nL90\nL8\nL68\nR53\nR82\nL18\nL64\nL93\nL12\nR44\nR58\nL59\nR28\nR34\nR181\nL77\nL4\nL402\nR2\nR57\nR43\nR141\nR159\nR476\nL828\nR71\nR49\nL68\nR846\nR54\nR505\nL94\nL11\nL49\nL98\nL79\nL76\nR2\nR52\nL10\nL13\nL30\nR50\nR51\nL44\nL484\nL72\nR850\nL804\nL625\nL21\nL84\nR78\nL2\nR35\nR97\nL36\nR74\nL913\nL25\nR76\nR560\nL76\nL384\nL14\nR28\nL6\nR892\nR651\nL895\nR244\nR48\nR34\nR55\nL37\nR91\nL68\nR53\nR24\nR43\nL84\nR41\nL35\nL635\nL30\nR63\nR67\nR74\nR65\nR59\nL88\nR68\nL83\nL625\nR72\nR62\nR36\nR30\nL34\nR79\nR21\nR34\nR74\nR48\nR21\nL43\nL41\nR67\nR5\nL31\nR29\nL27\nR275\nR23\nR1\nR8\nL8\nL67\nR71\nR86\nR928\nR73\nR80\nL41\nR5\nL982\nL50\nR295\nL99\nL33\nR5\nR29\nR56\nL62\nR281\nR61\nL36\nR68\nR889\nL21\nR29\nL57\nL291\nL18\nL9\nL73\nL99\nL75\nL856\nR907\nL56\nR66\nR95\nR45\nR54\nL99\nL45\nR21\nL25\nR57\nL76\nR68\nR36\nL36\nL6\nR6\nR8\nR40\nR2\nL71\nL68\nL27\nR315\nR82\nR19\nR79\nR55\nR16\nR81\nL64\nR33\nR32\nR70\nL2\nR93\nR93\nR80\nL5\nR11\nL445\nR373\nR34\nL39\nR5\nL48\nL14\nR62\nL54\nR58\nR356\nR39\nR19\nR82\nR453\nL53\nR895\nL295\nR6\nL106\nL22\nL10\nR32\nR249\nL40\nR38\nR65\nR88\nL68\nL87\nR555\nL12\nR39\nR29\nR15\nL271\nR3\nL15\nR696\nR55\nL927\nL69\nL343\nL3\nR59\nR84\nR21\nL61\nL1\nR58\nL583\nL75\nR53\nR48\nL780\nL20\nL26\nR476\nR75\nR875\nR81\nR77\nR891\nL90\nL59\nR17\nR146\nR920\nL83\nL98\nR98\nL793\nL7\nL87\nR87\nL72\nR4\nL58\nR83\nL45\nL93\nL19\nL53\nR353\nL83\nL17\nR43\nR820\nL63\nL307\nR7\nR58\nR42\nR53\nR96\nL96\nR26\nL54\nR729\nR82\nR53\nL83\nL83\nR424\nR46\nR7\nL33\nL433\nR51\nL81\nR570\nR7\nR87\nR885\nR649\nR906\nL81\nR887\nL61\nR47\nR35\nL701\nL34\nL37\nR37\nR95\nR4\nR98\nR63\nL60\nR207\nL42\nL11\nR25\nL62\nL802\nR47\nL43\nR81\nL88\nL84\nL687\nL41\nL212\nL88\nL747\nL563\nL90\nR568\nR7\nL56\nR32\nR12\nL21\nL42\nR13\nL113\nR62\nL62\nL59\nR43\nL1\nL65\nL18\nR16\nR57\nR236\nR91\nR37\nR304\nR13\nR569\nR77\nL50\nR88\nR62\nR821\nR10\nR527\nR776\nR588\nL21\nR99\nL49\nR20\nL826\nR64\nR293\nR98\nR25\nL27\nL78\nL20\nL97\nR97\nL41\nL459\nR595\nR55\nL1\nR25\nL74\nR63\nR61\nL13\nR98\nL22\nL187\nR24\nL65\nL52\nL93\nL65\nR95\nR11\nL55\nR39\nL39\nR9\nR59\nR32\nR772\nR34\nL6\nL78\nR78\nR77\nR47\nL34\nL197\nL493\nL2\nL224\nR80\nR33\nL80\nL22\nL30\nR913\nR58\nL680\nR54\nL90\nR32\nR43\nL86\nR1\nL1\nL219\nL87\nL73\nR92\nR24\nR64\nL74\nL30\nR710\nR474\nR20\nL68\nL78\nL154\nL33\nR33\nL972\nR72\nR62\nR138\nR409\nR35\nL44\nL350\nR50\nR659\nR46\nR19\nL514\nR92\nR87\nL82\nL2\nL284\nR28\nL49\nR294\nL35\nL59\nL44\nL99\nR77\nR57\nL57\nL58\nR24\nR5\nL5\nR90\nL90\nR32\nR88\nL354\nL36\nL631\nR24\nL21\nL11\nR9\nL417\nL12\nL609\nL62\nR589\nR7\nR13\nL47\nL46\nR32\nL48\nL59\nR67\nL54\nL177\nL61\nR99\nL81\nL44\nL90\nR746\nL644\nL648\nL41\nL13\nR21\nL84\nL37\nR11\nR589\nR313\nL13\nL8\nR983\nL875\nR20\nL19\nL1\nR631\nL31\nR39\nL21\nR58\nR49\nL6\nL25\nL857\nR63\nR36\nR45\nR19\nR468\nR80\nR510\nR10\nR86\nR26\nR90\nR54\nL140\nL32\nL2\nR50\nR98\nR24\nL85\nL62\nL75\nL22\nR1\nR921\nR94\nR68\nR88\nR588\nR18\nL477\nR63\nR44\nL86\nR90\nL90\nR330\nR670\nL11\nL89\nL18\nL19\nR16\nR21\nL50\nL705\nL45\nL21\nR621\nR29\nL55\nR53\nL85\nR6\nR48\nL46\nR50\nL98\nL877\nR75\nL51\nR48\nL33\nR99\nL6\nR16\nL12\nL61\nR773\nL15\nR261\nL81\nL43\nR23\nL318\nR57\nL57\nR36\nR48\nR81\nR385\nR50\nR363\nL22\nR2\nL13\nL16\nL52\nL62\nL58\nR58\nR54\nR3\nR95\nL83\nR29\nL102\nL85\nR28\nL670\nR40\nR91\nL28\nL88\nL92\nL15\nL72\nL852\nL31\nL22\nR36\nL36\nR61\nR39\nL21\nL579\nL7\nR135\nL54\nR73\nR53\nL331\nL1\nR241\nR14\nR55\nL66\nL812\nL19\nL94\nR389\nL20\nR44\nR847\nR91\nL38\nR81\nR19\nL35\nL89\nL3\nR27\nR79\nR21\nL39\nR73\nL34\nL47\nL499\nR397\nR649\nR42\nR60\nR23\nL62\nR18\nR519\nR25\nL25\nR51\nR98\nR151\nR72\nR7\nL79\nL30\nR79\nR41\nR779\nL69\nL9\nL64\nR73\nL74\nL391\nL35\nR34\nR90\nR76\nL17\nL52\nR24\nR97\nR48\nL28\nL20\nR910\nR538\nL36\nL27\nL19\nL405\nR19\nR3\nL35\nL12\nR238\nL26\nR95\nL595\nL70\nL30\nR34\nR34\nR99\nL67\nR15\nL898\nL961\nR44\nL29\nL21\nL50\nL45\nL4\nR77\nL24\nR86\nL590\nR562\nL17\nR255\nL698\nL865\nR47\nL95\nL89\nL78\nR57\nR21\nR74\nL974\nL36\nL13\nR149\nR82\nL30\nL80\nR35\nR97\nL11\nR55\nR2\nR98\nL48\nL16\nR16\nR36\nL222\nR74\nR66\nL54\nR373\nL373\nL88\nL62\nR50\nR68\nL39\nR471\nR241\nR3\nR56\nL122\nL176\nL63\nR140\nR77\nL56\nL39\nL61\nL27\nL76\nL97\nR70\nR74\nR52\nL74\nL46\nR24\nL85\nR693\nL58\nR86\nR64\nL714\nL35\nR749\nR38\nR6\nR73\nL19\nL98\nR10\nL10\nL41\nL97\nR38\nR80\nL782\nL74\nR276\nL76\nR34\nR30\nR61\nR861\nL10\nR6\nR84\nR10\nL436\nL64\nL27\nL30\nL47\nL96\nR471\nR72\nL91\nR99\nR27\nR22\nL2\nL98\nR1\nL1\nR78\nL78\nL520\nL80\nL12\nR112\nL57\nR67\nL24\nL26\nL58\nR3\nL85\nL220\nL77\nL51\nR90\nR3\nR635\nL42\nR85\nR557\nL22\nR81\nL59\nR546\nR56\nL9\nR7\nR68\nR403\nR97\nL30\nR62\nR61\nR61\nL58\nR29\nL96\nR45\nR603\nL45\nR56\nL12\nR656\nL3\nR988\nR665\nL36\nR122\nL349\nR13\nR44\nL12\nR39\nR29\nL52\nL48\nR89\nR70\nL59\nR97\nL93\nR96\nR43\nL43\nR63\nL63\nR838\nL99\nR61\nR314\nR60\nL878\nR93\nR22\nL11\nR43\nL364\nL750\nL774\nL99\nL91\nL342\nL95\nL171\nL816\nL54\nR13\nR95\nL795\nL2\nR16\nR927\nR83\nL524\nL57\nL43\nR57\nR479\nR148\nL32\nR48\nL57\nL43\nR51\nR179\nR270\nR82\nR18\nL80\nR29\nL49\nL91\nL9\nR83\nR51\nR66\nL95\nL24\nR15\nL61\nL35\nR75\nR65\nR760\nL78\nL22\nL1\nR701\nR57\nR43\nR318\nL99\nR3\nL22\nR91\nL91\nR5\nL58\nL347\nL837\nL72\nL91\nR11\nL11\nL90\nL37\nL73\nL25\nL3\nR80\nL95\nL92\nL65\nL1\nR139\nL69\nL69\nL67\nR20\nL53\nR759\nR41\nL92\nR1\nL9\nL55\nR70\nL56\nR47\nR694\nL37\nR477\nL40\nL55\nR355\nR84\nR31\nL291\nR21\nL45\nL583\nR83\nR554\nL74\nL80\nR92\nL83\nR1\nR98\nL26\nL77\nL5\nL91\nL9\nL19\nL9\nL72\nL1\nL62\nR99\nL17\nR81\nR40\nR81\nL21\nR87\nR829\nL1\nR83\nL98\nR531\nR28\nL19\nL81\nR41\nL98\nR65\nR19\nR90\nR24\nR97\nR8\nR8\nR87\nL22\nL78\nL64\nR21\nR43\nR820\nL52\nL32\nL11\nR5\nL9\nL9\nR80\nL2\nL90\nR50\nR74\nL19\nL318\nL87\nR99\nR67\nL66\nR610\nL510\nL77\nL23\nL15\nR515\nR63\nL63\nR71\nR24\nR984\nL79\nL73\nL11\nL16\nL11\nL89\nR81\nL81\nL31\nL14\nL36\nL428\nR480\nL20\nR92\nR57\nR85\nR55\nR60\nL55\nL45\nL41\nL37\nL55\nR62\nR71\nR13\nL413\nR570\nL70\nL47\nL78\nL75\nL87\nR39\nR48\nR608\nL8\nR49\nL57\nL160\nL65\nR14\nR28\nL9\nL89\nL84\nL27\nL60\nR25\nR123\nL88\nR33\nR67\nL32\nL293\nR215\nR82\nR35\nL7\nR39\nL39\nR51\nL51\nR9\nL9\nR660\nR40\nR164\nL64\nR88\nR12\nR292\nL92\nR22\nR9\nL571\nL960\nR88\nL87\nR99\nR81\nL81\nL5\nL55\nL3\nR63\nR1\nR29\nR574\nL4\nL67\nR167\nR255\nL55\nL5\nL71\nR76\nR70\nR37\nR28\nR65\nL68\nL26\nL529\nR676\nR31\nL14\nL70\nR2\nR98\nL54\nR81\nR37\nR398\nL62\nR45\nR555\nL39\nL961\nL48\nR48\nR23\nR77\nR52\nR73\nR75\nL67\nL84\nR51\nL44\nR32\nL88\nL312\nR58\nR44\nL52\nL32\nL6\nR163\nL157\nR94\nR69\nL80\nL89\nL85\nR80\nR5\nR22\nL922\nR46\nR276\nL83\nR461\nL17\nL83\nR54\nL62\nR91\nR17\nR49\nR272\nR4\nL25\nR17\nR83\nL76\nR692\nL63\nR123\nL76\nR790\nR56\nL89\nR43\nL57\nR5\nL78\nL59\nL68\nL43\nR28\nR17\nR25\nR91\nL62\nL99\nR312\nR671\nR8\nR48\nR24\nL63\nL468\nL765\nL35\nL32\nL76\nR348\nL88\nR11\nL290\nL705\nL13\nL87\nR67\nR33\nR20\nR58\nL25\nL876\nR58\nR65\nR280\nR7\nR13\nL11\nR11\nR16\nL16\nL94\nL41\nL65\nR44\nL26\nR82\nL99\nL1\nL90\nL85\nL25\nR59\nL75\nL84\nR4\nR96\nL59\nL32\nL389\nR32\nL465\nR13\nR94\nR6\nL48\nL83\nL53\nR53\nL84\nR50\nR49\nR16\nR28\nR224\nL72\nR7\nL87\nL85\nR14\nR60\nL33\nL91\nR90\nL55\nR35\nR8\nR57\nR78\nR76\nR51\nL5\nR41\nL41\nL20\nR4\nL84\nR68\nR32\nR1\nR99\nL15\nR1\nL65\nL66\nL47\nR92\nR72\nL52\nR53\nL993\nL470\nL66\nR61\nL5\nL57\nR322\nL36\nL87\nL958\nR845\nR71\nR75\nL75\nR16\nR44\nL695\nR61\nL26\nL16\nL84\nR4\nL77\nR73\nR166\nL77\nL89\nL88\nL24\nL88\nL81\nR81\nL43\nL5\nL20\nL952\nR27\nL33\nR10\nR16\nL33\nL68\nR69\nL23\nR55\nR689\nR31\nR5\nR75\nR26\nR5\nR69\nR82\nL40\nL42\nL28\nL62\nR90\nL73\nL51\nL291\nL63\nL147\nL75\nL47\nL174\nR53\nL75\nR43\nL82\nR93\nR91\nL39\nR58\nL47\nR81\nR84\nL39\nR45\nR8\nL842\nL11\nR52\nL52\nL75\nR53\nR37\nL15\nL69\nR22\nR269\nR441\nL67\nR27\nL74\nL13\nL19\nR339\nR44\nL11\nR699\nR352\nR76\nR84\nR7\nR993\nR23\nL23\nR863\nL63\nR49\nL49\nR59\nR24\nL83\nR13\nL295\nR75\nL213\nR20\nR86\nL70\nL16\nL343\nL56\nR99\nL166\nR21\nL61\nL17\nL77\nR21\nL21\nL97\nL30\nL73\nR70\nL3\nR44\nL35\nL67\nR191\nL377\nR451\nR126\nR90\nL90\nR74\nR7\nR30\nR389\nR64\nR137\nL51\nR50\nR5\nL43\nL62\nR83\nR58\nL93\nR52\nR11\nL58\nR247\nR34\nL34\nL53\nL4\nR69\nR81\nL41\nR23\nR25\nL92\nR61\nR31\nR327\nR82\nL9\nR87\nR217\nL793\nL97\nR18\nL86\nR54\nL76\nL24\nR615\nR19\nL34\nL64\nL737\nR1\nL30\nR78\nL87\nL73\nR45\nL40\nL83\nR61\nR29\nR13\nR74\nL32\nL82\nR32\nL3\nL33\nL38\nR47\nR22\nR83\nL931\nR271\nR49\nL17\nL20\nR65\nR55\nL12\nL43\nR38\nL80\nL58\nR61\nL90\nL271\nL91\nL4\nR74\nR21\nL20\nL56\nL47\nR68\nR55\nR249\nL24\nR612\nL54\nL768\nR335\nR85\nL364\nL12\nL59\nR68\nL268\nL90\nR90\nL31\nL25\nR32\nR24\nL39\nL51\nL10\nR88\nR13\nL1\nL15\nR88\nR27\nL923\nL331\nR94\nL526\nL14\nR550\nL914\nR93\nR71\nL146\nR67\nL36\nR76\nR839\nL95\nL66\nL15\nR76\nL50\nL859\nR51\nL74\nR60\nR72\nR24\nR956\nL571\nL43\nR47\nR87\nL60\nR29\nL69\nR989\nL98\nL591\nL40\nL814\nR90\nR371\nL107\nL54\nL43\nL45\nL97\nL61\nL832\nL843\nR14\nR9\nR52\nR48\nL81\nL81\nR48\nL34\nL305\nL62\nL533\nL93\nR795\nL39\nR90\nL60\nL58\nR65\nR935\nR65\nL70\nR62\nL403\nL89\nR66\nL65\nR17\nL66\nR503\nR94\nR650\nL88\nL517\nR106\nR85\nR8\nL72\nR70\nR4\nL139\nL56\nL623\nR27\nL61\nL43\nL873\nR673\nR63\nL62\nR63\nR36\nL275\nL625\nL927\nL63\nL77\nL9\nR96\nL124\nL32\nR43\nR68\nL29\nR54\nR24\nL48\nL7\nL39\nR12\nR58\nL96\nL56\nR252\nL5\nL16\nL24\nR45\nR450\nR71\nR24\nL57\nL467\nL87\nR66\nR670\nL33\nL68\nL57\nR29\nR176\nR9\nL26\nL88\nR56\nL68\nR24\nR820\nR489\nL33\nR41\nR66\nR93\nR686\nL69\nR82\nL879\nL635\nR94\nR21\nL68\nR68\nR58\nL58\nL24\nL76\nR94\nR6\nR2\nR266\nL521\nR64\nR9\nL33\nR63\nL13\nR74\nL64\nL98\nL49\nL542\nR42\nL57\nL96\nL52\nR5\nL91\nL59\nR113\nL63\nL71\nR87\nR64\nL80\nL81\nR19\nL69\nR28\nL97\nR890\nL90\nR404\nL32\nL54\nL255\nR237\nL3\nR423\nL120\nR9\nR356\nL375\nR10\nL71\nR24\nL546\nR93\nR5\nL5\nR47\nL376\nL84\nR13\nL30\nL70\nR878\nR86\nR21\nL85\nL394\nL71\nL465\nL623\nL20\nL17\nR78\nL836\nL45\nR93\nL68\nR78\nR30\nL140\nR24\nL24\nR643\nR9\nR48\nR95\nR94\nL31\nL58\nL16\nR17\nL746\nR844\nR31\nR70\nR84\nL84\nL879\nR61\nR470\nL48\nR287\nR5\nL796\nR59\nR71\nR70\nL58\nR740\nR18\nR98\nL1\nL97\nR7\nL7\nL901\nR420\nL80\nL9\nL76\nL97\nL59\nR15\nL98\nL45\nR18\nR12\nL36\nL64\nR25\nL25\nL91\nL94\nL95\nR80\nR305\nL90\nL27\nL82\nR77\nL83\nR544\nL70\nR26\nR37\nR31\nR133\nR21\nL93\nL29\nR499\nL499\nL269\nR69\nL675\nR25\nL50\nL25\nR214\nL85\nR96\nR2\nR28\nL30\nL18\nR83\nL65\nR12\nL358\nR2\nL56\nR36\nL3\nR167\nR44\nL43\nL501\nL85\nL82\nR67\nR290\nL31\nL7\nR48\nL24\nR130\nR423\nR20\nL15\nL95\nR61\nR81\nR19\nL98\nL536\nL66\nR69\nL42\nR52\nR8\nL687\nR76\nL8\nL176\nL44\nR52\nR6\nR94\nR86\nL12\nR178\nL99\nL53\nR933\nR67\nR6\nR44\nL50\nL25\nL28\nL42\nL18\nL666\nR74\nL98\nR3\nR2\nL33\nR654\nL73\nL65\nR71\nR44\nL25\nR61\nL98\nR62\nR80\nL7\nL23\nL50\nL609\nL43\nR52\nR96\nR81\nL77\nR55\nL529\nL94\nR26\nL40\nL18\nL32\nR33\nR4\nL87\nL18\nL29\nL74\nL283\nR46\nR40\nL787\nL113\nR473\nR71\nL99\nR58\nR78\nR19\nR1\nL18\nL65\nR251\nL69\nL90\nR90\nR77\nR11\nR12\nL591\nL9\nL76\nL48\nR24\nL96\nL766\nR44\nL837\nR574\nL6\nR87\nR870\nL615\nL355\nL64\nR5\nL22\nL21\nR59\nR18\nR525\nR24\nR76\nL510\nR135\nL25\nR911\nL72\nL239\nR81\nL45\nR42\nR49\nR773\nL72\nR448\nR50\nR21\nL47\nL16\nR16\nR207\nL7\nR797\nR10\nR75\nL64\nL18\nL7\nL93\nL656\nR693\nR11\nR35\nR17\nR16\nR84\nL835\nR35\nL64\nL64\nL95\nL13\nR81\nL180\nL571\nR38\nR68\nR688\nR789\nL41\nL396\nR73\nR66\nR8\nR51\nR71\nL9\nL793\nR695\nR94\nR83\nL79\nL171\nR271\nR947\nR53\nL57\nR57\nR18\nR894\nL212\nR481\nR619\nL29\nR81\nL33\nR56\nR63\nL38\nL98\nR98\nR3\nR41\nR856\nR72\nR85\nR93\nR719\nL198\nL62\nL609\nR59\nR841\nL58\nR671\nL313\nR12\nL69\nL79\nL54\nR90\nR266\nL966\nR590\nR10\nR76\nL73\nR97\nR627\nL14\nR987\nR714\nL97\nL89\nL28\nL71\nR601\nR70\nL868\nR19\nR144\nR631\nR71\nL997\nR82\nL92\nR29\nR281\nR708\nR69\nL933\nL63\nL187\nR34\nL728\nR41\nL81\nR794\nL54\nR297\nL42\nR67\nR678\nR32\nL98\nR92\nL26\nL4\nR4\nL90\nL73\nR48\nR85\nR80\nR31\nR319\nR10\nR90\nL49\nR6\nR79\nL94\nR59\nR407\nL89\nL19\nR102\nL2\nR36\nR976\nL75\nL16\nL203\nR89\nR454\nL84\nL50\nL83\nR44\nL12\nL676\nL19\nR63\nR746\nR144\nL88\nL46\nR85\nL597\nR89\nR8\nR230\nR93\nR472\nL75\nL98\nR5\nR88\nR86\nR18\nR96\nR65\nL65\nL13\nR33\nL20\nR687\nL87\nR38\nL38\nL82\nL42\nL28\nL73\nR754\nL81\nR52\nR786\nR80\nL27\nR2\nR89\nL43\nR992\nL79\nL77\nL83\nR15\nR66\nR279\nL808\nR12\nR96\nR45\nL80\nL14\nL670\nL16\nL170\nR5\nR332\nL955\nR9\nR83\nL85\nL84\nL270\nR42\nL472\nR31\nL66\nR98\nR137\nR32\nL395\nL46\nL246\nL59\nR15\nL1\nL10\nL90\nR638\nL405\nR77\nR99\nR97\nL401\nL5\nR70\nR30\nL80\nR81\nR90\nR12\nR742\nL7\nL77\nR739\nR359\nR34\nL84\nL64\nR74\nL17\nL94\nL8\nL625\nL40\nR65\nL95\nR70\nR16\nL45\nR54\nR998\nR524\nL22\nR27\nR52\nR121\nR49\nL9\nL80\nR80\nR712\nR731\nR17\nL48\nR417\nL50\nL19\nR29\nL829\nL772\nL28\nR77\nL77\nL88\nR296\nR907\nL66\nR337\nL748\nR362\nR49\nL70\nR936\nL85\nR770\nR55\nR62\nR37\nR382\nR25\nL55\nR94\nL2\nR702\nL47\nL971\nL94\nL88\nL96\nR94\nR43\nL49\nR15\nL55\nR98\nR35\nL386\nL699\nL10\nL590\nL95\nL67\nL39\nR939\nR31\nR31\nL2\nL83\nR85\nL98\nL34\nR221\nL790\nR145\nL38\nR94\nL72\nR72\nL517\nR64\nL47\nR10\nR39\nL89\nR840\nL82\nL50\nR59\nR673\nR59\nL859\nL22\nL46\nL22\nR7\nL17\nR53\nR58\nR13\nL24\nR25\nL79\nR40\nL86\nR74\nR61\nR54\nL91\nL5\nR117\nR10\nL339\nR75\nL238\nR69\nR36\nR977\nL348\nR450\nL181\nR55\nL76\nR16\nL18\nR181\nL81\nL398\nL20\nR80\nR40\nR95\nL61\nL908\nL702\nL85\nL99\nR226\nR134\nL30\nL90\nL87\nL14\nL86\nR7\nR63\nR715\nR48\nL95\nL31\nR37\nL816\nR59\nL80\nR61\nL61\nL10\nR21\nL11\nL54\nR712\nL258\nL99\nL2\nR37\nL23\nL13\nR335\nR65\nL32\nL668\nR98\nR12\nL58\nL16\nL38\nR3\nR92\nL96\nR3\nR81\nR72\nL62\nR762\nR8\nR91\nL60\nR67\nL29\nL49\nL183\nL98\nR84\nL84\nR89\nR11\nL829\nL40\nR657\nR27\nR41\nL56\nR27\nL59\nR32\nL717\nL69\nR86\nR24\nR76\nR616\nL9\nL11\nL512\nL984\nR29\nR471\nR38\nL94\nL32\nR93\nR95\nL43\nL3\nR89\nR357\nR17\nR3\nR80\nL83\nR13\nL19\nR946\nR93\nL47\nR68\nL86\nL85\nL83\nR70\nR12\nR186\nR78\nL6\nR13\nR48\nL18\nR952\nR48\nL10\nR10\nL48\nR272\nL39\nL751\nR21\nL87\nL68\nL53\nR409\nL56\nL33\nL83\nL84\nL95\nL1\nR41\nR976\nL21\nL437\nL63\nR551\nR9\nR15\nL23\nR69\nR679\nL21\nR12\nR4\nL895\nR687\nL95\nR674\nR91\nR43\nL72\nR672\nL32\nR29\nR295\nR16\nR637\nR755\nL85\nL82\nR76\nR36\nL78\nL56\nL11\nR19\nR61\nR48\nL628\nL45\nL52\nR97\nL98\nL702\nR54\nL854\nL43\nR43\nR41\nR59\nL27\nL23\nL4\nL46\nR31\nL11\nL820\nR20\nR32\nL12\nL40\nR849\nR47\nR69\nL65\nL222\nL78\nR67\nL49\nR184\nR97\nR701\nL86\nR980\nR6\nR44\nL759\nL49\nL50\nL10\nR980\nL56\nR45\nR62\nL15\nL66\nL543\nR17\nL96\nL14\nR67\nL585\nL95\nR23\nL46\nL196\nL70\nL89\nR99\nL72\nL806\nR16\nR92\nL20\nR392\nL98\nL19\nR47\nR38\nL68\nL34\nL66\nL591\nR86\nL95\nL43\nL2\nL90\nL65\nL16\nL284\nL59\nR47\nR70\nL64\nR30\nL16\nL744\nR64\nR72\nL57\nR37\nR747\nR610\nR81\nL74\nR608\nR48\nR816\nL7\nL90\nR81\nR27\nL89\nR54\nL7\nR2\nL187\nR10\nR5\nL15\nL19\nR88\nR231\nL17\nL335\nL58\nR89\nR21\nL64\nR43\nL58\nR79\nL62\nL22\nR4\nR80\nR75\nL79\nL96\nL30\nL70\nR887\nR13\nL6\nR432\nL76\nL50\nL19\nL360\nL28\nR7\nL38\nR938\nR62\nR86\nR52\nR18\nR53\nR782\nL953\nL121\nL79\nL28\nR28\nR6\nL276\nR70\nR71\nL873\nR25\nR29\nL1\nL51\nL27\nR70\nL27\nL69\nL89\nR14\nR28\nR27\nR573\nL4\nL66\nL889\nR3\nL54\nR51\nL58\nL819\nL64\nR37\nR763\nL58\nR65\nL83\nL224\nR73\nR69\nL42\nL5\nR216\nR39\nR114\nR36\nL79\nR43\nR36\nL74\nR74\nR202\nR70\nR22\nR47\nL15\nL53\nR27\nR65\nR235\nR46\nR64\nL10\nL276\nL149\nL4\nL71\nL71\nL37\nL29\nL63\nR757\nR412\nR95\nR936\nR63\nR90\nL53\nR128\nL28\nR529\nR7\nR22\nR642\nR324\nR976\nR64\nL64\nR42\nL49\nR79\nR18\nR6\nL239\nL57\nL41\nL11\nR89\nR32\nL9\nL76\nL184\nL72\nR81\nR11\nR80\nR85\nR15\nR571\nL25\nR95\nR71\nR452\nR132\nR604\nR63\nL663\nR20\nR34\nL50\nL4\nR401\nL501\nL56\nL944\nL135\nL627\nL46\nL91\nR86\nR76\nR84\nR5\nL49\nR88\nR85\nR524\nR51\nR58\nL66\nR34\nL39\nL2\nL38\nL98\nR990\nR630\nL820\nL20\nR86\nR687\nL90\nR38\nL1\nL29\nR81\nR25\nL819\nL24\nL33\nR240\nL63\nR65\nL76\nR42\nL9\nR1\nL171\nL30\nL22\nR32\nR90\nL40\nL11\nL1\nR52\nL42\nR14\nL17\nL825\nR70\nL912\nL40\nL48\nR49\nR67\nL862\nL62\nL29\nL27\nR83\nL19\nL13\nL187\nL61\nR36\nR13\nR63\nR92\nL294\nR545\nL94\nL82\nR20\nL38\nR60\nR31\nL77\nL14\nR86\nL99\nL87\nL4\nL596\nR6\nL18\nL4\nR35\nR54\nL49\nL87\nL84\nL42\nR48\nL7\nR448\nL36\nR36\nL53\nR53\nR92\nR182\nR81\nR85\nR321\nR75\nL62\nR80\nR94\nR97\nR55\nR63\nR246\nR791\nL44\nL56\nR387\nL26\nR21\nR18\nR29\nL88\nR84\nL25\nR27\nR73\nR42\nR18\nL60\nR20\nR388\nL708\nR89\nR11\nL852\nR86\nL31\nL43\nL60\nL60\nR60\nR4\nR484\nR29\nR3\nR80\nL3\nR78\nL75\nR173\nR551\nL32\nR6\nR802\nL32\nL72\nL47\nR89\nL838\nR9\nR91\nL47\nR38\nR49\nL53\nR213\nL17\nL90\nL49\nL92\nR112\nL64\nL85\nL56\nL78\nR847\nR79\nL28\nR15\nL3\nL86\nR95\nL18\nR21\nL31\nR617\nR11\nR57\nR743\nR12\nR91\nL28\nL75\nL53\nR211\nL587\nL19\nL52\nL56\nR25\nR56\nL25\nL430\nR30\nL5\nL195\nL6\nL79\nR85\nL76\nL524\nR20\nL33\nR62\nL10\nR44\nL56\nL27\nR39\nL39\nL866\nR60\nR393\nL74\nL60\nR36\nR522\nL918\nL358\nR77\nL18\nL94\nL10\nL6\nL24\nR540\nL28\nL18\nR451\nR515\nR37\nR30\nL87\nR91\nR6\nL41\nL80\nL41\nR865\nL49\nL712\nL210\nL78\nR70\nL21\nL56\nL44\nL807\nR7\nL27\nL60\nL78\nL3\nL30\nL89\nL55\nL58\nL928\nR23\nL60\nR65\nL73\nL35\nR8\nR72\nR28\nL6\nL51\nL531\nL855\nR43\nL64\nR64\nR73\nR35\nR989\nL49\nL48\nR98\nR2\nL40\nR41\nL1\nR89\nR23\nR88\nR32\nR93\nR75\nR442\nR58\nL18\nL94\nL71\nL17\nL26\nL23\nL31\nL31\nR76\nL492\nL6\nR33\nR87\nR13\nL38\nR21\nL21\nR13\nR49\nL647\nL77\nL753\nL1\nR64\nL10\nL51\nL1\nR2\nR50\nR86\nR59\nR55\nL1\nL99\nL158\nR24\nL366\nR60\nR40\nL35\nR335\nL84\nL16\nL98\nL885\nR87\nR96\nR51\nL27\nR60\nR16\nR37\nR18\nL777\nR27\nR95\nL88\nR68\nL67\nR26\nR361\nL70\nR770\nL316\nR22\nR47\nR31\nR497\nL68\nL18\nR70\nL3\nR138\nL92\nL8\nR43\nR57\nR47\nR153\nR7\nR680\nL10\nR28\nL205\nR767\nR98\nL765\nL31\nL698\nR29\nL55\nR55\nL7\nR21\nR71\nL1\nR16\nL12\nL88\nR815\nL225\nL69\nL33\nL88\nL72\nR64\nL87\nL405\nL30\nR82\nL52\nL901\nR7\nR1\nL437\nL52\nL818\nL12\nL87\nL90\nR89\nL84\nL47\nL69\nL34\nL819\nR53\nR617\nL347\nR57\nL27\nR790\nL60\nL30\nL77\nL23\nL97\nL3\nR16\nL89\nR659\nR87\nL21\nR748\nL55\nR55\nR668\nR98\nR88\nR46\nR91\nR55\nR43\nL5\nL184\nL85\nL48\nR35\nR78\nR20\nR91\nR509\nR18\nR66\nR75\nL97\nL48\nL14\nL297\nR97\nL63\nL89\nR81\nR71\nR51\nR449\nR171\nL79\nL957\nR4\nL909\nR70\nL10\nL90\nR65\nL65\nL54\nL46\nL89\nL11\nR27\nR27\nL357\nR11\nR92\nR87\nL771\nL95\nL26\nL95\nL14\nR329\nL130\nL42\nL43\nR53\nL8\nL45\nR76\nR285\nR539\nR40\nR98\nL38\nR52\nR228\nR837\nL65\nR85\nR96\nR41\nL174\nR596\nL536\nR71\nR69\nL5\nR96\nL91\nL290\nR90\nL458\nL93\nL49\nR26\nR24\nL30\nR280\nR74\nR26\nL518\nR70\nL23\nL75\nR70\nL404\nR39\nR45\nR8\nL418\nR41\nL34\nL54\nL17\nR8\nR61\nR1\nL52\nL48\nR34\nL49\nL23\nL8\nR157\nL23\nL688\nL74\nL60\nL66\nR26\nR5\nR36\nR33\nR41\nR56\nR80\nL75\nR698\nL26\nL2\nR28\nL764\nR64\nR31\nL16\nR89\nR52\nR43\nL440\nL59\nL90\nL36\nL38\nL37\nR1\nL489\nR89\nL92\nL22\nL86\nR394\nL94\nR18\nR845\nR637\nL63\nR44\nR19\nL83\nL74\nL50\nL567\nR83\nL45\nR84\nL215\nL133\nR58\nL414\nL860\nR60\nL65\nL79\nL73\nL64\nR476\nL846\nL86\nL7\nL75\nL78\nR53\nR95\nL35\nR40\nR33\nL45\nR12\nL94\nL55\nR49\nR37\nL941\nL96\nL91\nR376\nL22\nL8\nL55\nL26\nL174\nR899\nL99\nL537\nL53\nL233\nR980\nL57\nR48\nL95\nR47\nL37\nR53\nL68\nL48\nR86\nR10\nL538\nR742\nR38\nL38\nL7\nL452\nL790\nR5\nL656\nR34\nL68\nR607\nL51\nL986\nR64\nR91\nR95\nR95\nR526\nL66\nL16\nL21\nR96\nL947\nL24\nR71\nR43\nR430\nL91\nR618\nR19\nR81\nR4\nL4\nL31\nR51\nL120\nR49\nL49\nR24\nL24\nR44\nL206\nL65\nR18\nR496\nL22\nR266\nR41\nL72\nL19\nR719\nR228\nR80\nL645\nR10\nL34\nL39\nR44\nR756\nL23\nL66\nR6\nR51\nL68\nR12\nR88\nL68\nR68\nL463\nL887\nL85\nL6\nL935\nL24\nL57\nL43\nL86\nL699\nR85\nR31\nL48\nR17\nR829\nL29\nR73\nR104\nR473\nL50\nL26\nL55\nL19\nR92\nR8\nR8\nR6\nR86\nL81\nL906\nR51\nR95\nR541\nR13\nR41\nR895\nR61\nL10\nL23\nL12\nL65\nR2\nL2\nR5\nL56\nR25\nR19\nR49\nL77\nL65\nR87\nR90\nL353\nR3\nR73\nR47\nL94\nL953\nL11\nR11\nL13\nR35\nR978\nL12\nR90\nL97\nL781\nL17\nL24\nL783\nL87\nR11\nL55\nL93\nL27\nR128\nL745\nL608\nL76\nR76\nR13\nL113\nL44\nL37\nR84\nR39\nR68\nR744\nL54\nL615\nR363\nR27\nR72\nR85\nL32\nR963\nR37\nR41\nR59\nR33\nR67\nL671\nR43\nR863\nL35\nL66\nL29\nR53\nL76\nR118\nR78\nL84\nL382\nL12\nL41\nL59\nL61\nR67\nR94\nR69\nR31\nL312\nR57\nL45\nL75\nR56\nL95\nL4\nL82\nR59\nR617\nL76\nR95\nL11\nL43\nL41\nR476\nL72\nL67\nR390\nR30\nR43\nL46\nR729\nL89\nR79\nR33\nL42\nR38\nR83\nL2\nR97\nR20\nR429\nR64\nL47\nR29\nR25\nL599\nR83\nR976\nL66\nR64\nL422\nL936\nR85\nR15\nL6\nL18\nR43\nL94\nR42\nL31\nL2\nR76\nR35\nR12\nL57\nR88\nL908\nR891\nL70\nR299\nR49\nR15\nR247\nR89\nL49\nL12\nR61\nL291\nL78\nR53\nL538\nL82\nR74\nL38\nR620\nR15\nR65\nL59\nL241\nL82\nR827\nR55\nL73\nL48\nL56\nR77\nL462\nL68\nR8\nR19\nL523\nR26\nL898\nL63\nL39\nR76\nR12\nL88\nL8\nL918\nL311\nL63\nR77\nR91\nL68\nL71\nR48\nR523\nL89\nR96\nR893\nR92\nR8\nR73\nR12\nL51\nL444\nR64\nL79\nL61\nR714\nR69\nR7\nL430\nR19\nR88\nR836\nL17\nL29\nR29\nL68\nL39\nR75\nL27\nR74\nR41\nR66\nL56\nL83\nR29\nR80\nR97\nL89\nL87\nR287\nR41\nR59\nL12\nL88\nR42\nL42\nR84\nR71\nL87\nL50\nR82\nR37\nR86\nR14\nR263\nL399\nL7\nR22\nL61\nL16\nR61\nL29\nR29\nL180\nR22\nL142\nR43\nR248\nR33\nR76\nL26\nL17\nR58\nL9\nL6\nR52\nR59\nL97\nR55\nL56\nL90\nL823\nL37\nL63\nR56\nR13\nR82\nL54\nL31\nR734\nL268\nL504\nR72\nL75\nR75\nL16\nL45\nR22\nL61\nR515\nL15\nR32\nL72\nL60\nR59\nL92\nR29\nR204\nR6\nL969\nL11\nR74\nR66\nR48\nR86\nL50\nR94\nL44\nL32\nR91\nR49\nR92\nR52\nR48\nL36\nL61\nR97\nL7\nL86\nL7\nL87\nR87\nL99\nR46\nR46\nR90\nR92\nL75\nL83\nL17\nR4\nR75\nR59\nR78\nL73\nL10\nR67\nR49\nR51\nR87\nL25\nL62\nR5\nR95\nR47\nR24\nL20\nR84\nR65\nL14\nR31\nR83\nR66\nL66\nL19\nR10\nL12\nL32\nL12\nR2\nR29\nR16\nR42\nR1\nR39\nR32\nR5\nR19\nL30\nR31\nL19\nL13\nR37\nL9\nR20\nL50\nL41\nR24\nR2\nL12\nL45\nL5\nL1\nL40\nR46\nL32\nL18\nR26\nL29\nR13\nL27\nL25\nR23\nL41\nR17\nR22\nL30\nL43\nL35\nR26\nL5\nR6\nL15\nL44\nL11';
var $author$project$Main$Left = function (a) {
	return {$: 0, a: a};
};
var $author$project$Main$Right = function (a) {
	return {$: 1, a: a};
};
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $author$project$Main$parseMove = function (line) {
	var steps = A2(
		$elm$core$Maybe$withDefault,
		0,
		$elm$core$String$toInt(
			A2($elm$core$String$dropLeft, 1, line)));
	return A2($elm$core$String$startsWith, 'L', line) ? $author$project$Main$Left(steps) : $author$project$Main$Right(steps);
};
var $author$project$Main$sample = 'L68\nL30\nR48\nL5\nR60\nL55\nL1\nL99\nR14\nL82';
var $author$project$Main$initModel = function (dataSource) {
	var position = 50;
	var data = function () {
		if (dataSource === 1) {
			return $author$project$Main$sample;
		} else {
			return $author$project$Main$input;
		}
	}();
	var moves = A2(
		$elm$core$List$map,
		$author$project$Main$parseMove,
		A2($elm$core$String$split, '\n', data));
	return {u: 0, C: dataSource, v: '----', H: false, r: moves, m: true, n: position, z: $author$project$Main$defaultTickInterval, B: 0};
};
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $author$project$Main$init = function (_v0) {
	return _Utils_Tuple2(
		$author$project$Main$initModel(1),
		$elm$core$Platform$Cmd$none);
};
var $author$project$Main$Tick = {$: 0};
var $elm$time$Time$Every = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$time$Time$State = F2(
	function (taggers, processes) {
		return {ah: processes, ao: taggers};
	});
var $elm$core$Dict$RBEmpty_elm_builtin = {$: -2};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $elm$time$Time$init = $elm$core$Task$succeed(
	A2($elm$time$Time$State, $elm$core$Dict$empty, $elm$core$Dict$empty));
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === -2) {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1) {
					case 0:
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 1:
						return $elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var $elm$core$Dict$Black = 1;
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: -1, a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$Red = 0;
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === -1) && (!right.a)) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === -1) && (!left.a)) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					0,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, 1, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === -1) && (!left.a)) && (left.d.$ === -1)) && (!left.d.a)) {
				var _v5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _v6 = left.d;
				var _v7 = _v6.a;
				var llK = _v6.b;
				var llV = _v6.c;
				var llLeft = _v6.d;
				var llRight = _v6.e;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					0,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 1, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === -2) {
			return A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1) {
				case 0:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 1:
					return A5($elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3($elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var $elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
		if ((_v0.$ === -1) && (!_v0.a)) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, 1, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$time$Time$addMySub = F2(
	function (_v0, state) {
		var interval = _v0.a;
		var tagger = _v0.b;
		var _v1 = A2($elm$core$Dict$get, interval, state);
		if (_v1.$ === 1) {
			return A3(
				$elm$core$Dict$insert,
				interval,
				_List_fromArray(
					[tagger]),
				state);
		} else {
			var taggers = _v1.a;
			return A3(
				$elm$core$Dict$insert,
				interval,
				A2($elm$core$List$cons, tagger, taggers),
				state);
		}
	});
var $elm$core$Process$kill = _Scheduler_kill;
var $elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === -2) {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var $elm$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _v0) {
				stepState:
				while (true) {
					var list = _v0.a;
					var result = _v0.b;
					if (!list.b) {
						return _Utils_Tuple2(
							list,
							A3(rightStep, rKey, rValue, result));
					} else {
						var _v2 = list.a;
						var lKey = _v2.a;
						var lValue = _v2.b;
						var rest = list.b;
						if (_Utils_cmp(lKey, rKey) < 0) {
							var $temp$rKey = rKey,
								$temp$rValue = rValue,
								$temp$_v0 = _Utils_Tuple2(
								rest,
								A3(leftStep, lKey, lValue, result));
							rKey = $temp$rKey;
							rValue = $temp$rValue;
							_v0 = $temp$_v0;
							continue stepState;
						} else {
							if (_Utils_cmp(lKey, rKey) > 0) {
								return _Utils_Tuple2(
									list,
									A3(rightStep, rKey, rValue, result));
							} else {
								return _Utils_Tuple2(
									rest,
									A4(bothStep, lKey, lValue, rValue, result));
							}
						}
					}
				}
			});
		var _v3 = A3(
			$elm$core$Dict$foldl,
			stepState,
			_Utils_Tuple2(
				$elm$core$Dict$toList(leftDict),
				initialResult),
			rightDict);
		var leftovers = _v3.a;
		var intermediateResult = _v3.b;
		return A3(
			$elm$core$List$foldl,
			F2(
				function (_v4, result) {
					var k = _v4.a;
					var v = _v4.b;
					return A3(leftStep, k, v, result);
				}),
			intermediateResult,
			leftovers);
	});
var $elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var $elm$time$Time$Name = function (a) {
	return {$: 0, a: a};
};
var $elm$time$Time$Offset = function (a) {
	return {$: 1, a: a};
};
var $elm$time$Time$Zone = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$time$Time$customZone = $elm$time$Time$Zone;
var $elm$time$Time$setInterval = _Time_setInterval;
var $elm$core$Process$spawn = _Scheduler_spawn;
var $elm$time$Time$spawnHelp = F3(
	function (router, intervals, processes) {
		if (!intervals.b) {
			return $elm$core$Task$succeed(processes);
		} else {
			var interval = intervals.a;
			var rest = intervals.b;
			var spawnTimer = $elm$core$Process$spawn(
				A2(
					$elm$time$Time$setInterval,
					interval,
					A2($elm$core$Platform$sendToSelf, router, interval)));
			var spawnRest = function (id) {
				return A3(
					$elm$time$Time$spawnHelp,
					router,
					rest,
					A3($elm$core$Dict$insert, interval, id, processes));
			};
			return A2($elm$core$Task$andThen, spawnRest, spawnTimer);
		}
	});
var $elm$time$Time$onEffects = F3(
	function (router, subs, _v0) {
		var processes = _v0.ah;
		var rightStep = F3(
			function (_v6, id, _v7) {
				var spawns = _v7.a;
				var existing = _v7.b;
				var kills = _v7.c;
				return _Utils_Tuple3(
					spawns,
					existing,
					A2(
						$elm$core$Task$andThen,
						function (_v5) {
							return kills;
						},
						$elm$core$Process$kill(id)));
			});
		var newTaggers = A3($elm$core$List$foldl, $elm$time$Time$addMySub, $elm$core$Dict$empty, subs);
		var leftStep = F3(
			function (interval, taggers, _v4) {
				var spawns = _v4.a;
				var existing = _v4.b;
				var kills = _v4.c;
				return _Utils_Tuple3(
					A2($elm$core$List$cons, interval, spawns),
					existing,
					kills);
			});
		var bothStep = F4(
			function (interval, taggers, id, _v3) {
				var spawns = _v3.a;
				var existing = _v3.b;
				var kills = _v3.c;
				return _Utils_Tuple3(
					spawns,
					A3($elm$core$Dict$insert, interval, id, existing),
					kills);
			});
		var _v1 = A6(
			$elm$core$Dict$merge,
			leftStep,
			bothStep,
			rightStep,
			newTaggers,
			processes,
			_Utils_Tuple3(
				_List_Nil,
				$elm$core$Dict$empty,
				$elm$core$Task$succeed(0)));
		var spawnList = _v1.a;
		var existingDict = _v1.b;
		var killTask = _v1.c;
		return A2(
			$elm$core$Task$andThen,
			function (newProcesses) {
				return $elm$core$Task$succeed(
					A2($elm$time$Time$State, newTaggers, newProcesses));
			},
			A2(
				$elm$core$Task$andThen,
				function (_v2) {
					return A3($elm$time$Time$spawnHelp, router, spawnList, existingDict);
				},
				killTask));
	});
var $elm$time$Time$Posix = $elm$core$Basics$identity;
var $elm$time$Time$millisToPosix = $elm$core$Basics$identity;
var $elm$time$Time$now = _Time_now($elm$time$Time$millisToPosix);
var $elm$time$Time$onSelfMsg = F3(
	function (router, interval, state) {
		var _v0 = A2($elm$core$Dict$get, interval, state.ao);
		if (_v0.$ === 1) {
			return $elm$core$Task$succeed(state);
		} else {
			var taggers = _v0.a;
			var tellTaggers = function (time) {
				return $elm$core$Task$sequence(
					A2(
						$elm$core$List$map,
						function (tagger) {
							return A2(
								$elm$core$Platform$sendToApp,
								router,
								tagger(time));
						},
						taggers));
			};
			return A2(
				$elm$core$Task$andThen,
				function (_v1) {
					return $elm$core$Task$succeed(state);
				},
				A2($elm$core$Task$andThen, tellTaggers, $elm$time$Time$now));
		}
	});
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $elm$time$Time$subMap = F2(
	function (f, _v0) {
		var interval = _v0.a;
		var tagger = _v0.b;
		return A2(
			$elm$time$Time$Every,
			interval,
			A2($elm$core$Basics$composeL, f, tagger));
	});
_Platform_effectManagers['Time'] = _Platform_createManager($elm$time$Time$init, $elm$time$Time$onEffects, $elm$time$Time$onSelfMsg, 0, $elm$time$Time$subMap);
var $elm$time$Time$subscription = _Platform_leaf('Time');
var $elm$time$Time$every = F2(
	function (interval, tagger) {
		return $elm$time$Time$subscription(
			A2($elm$time$Time$Every, interval, tagger));
	});
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $elm$core$Platform$Sub$none = $elm$core$Platform$Sub$batch(_List_Nil);
var $author$project$Main$subscriptions = function (model) {
	var tickSub = (model.m || model.H) ? $elm$core$Platform$Sub$none : A2(
		$elm$time$Time$every,
		model.z,
		function (_v0) {
			return $author$project$Main$Tick;
		});
	return tickSub;
};
var $author$project$Main$updateReset = function (model) {
	return $author$project$Main$initModel(model.C);
};
var $author$project$Main$updateStep = function (model) {
	var position = model.n;
	var pause = model.m;
	var _v0 = model.r;
	if (!_v0.b) {
		return _Utils_update(
			model,
			{v: 'paused?', H: true, m: true, n: position});
	} else {
		var move = _v0.a;
		var rest = _v0.b;
		if (!move.$) {
			var steps = move.a;
			if (steps > 0) {
				var pos = ((position - 1) < 0) ? 99 : (position - 1);
				var allZeroes = model.u + ((!position) ? 1 : 0);
				return _Utils_update(
					model,
					{
						u: allZeroes,
						v: 'lefty',
						r: A2(
							$elm$core$List$cons,
							$author$project$Main$Left(steps - 1),
							rest),
						n: pos
					});
			} else {
				var zeroes = model.B + ((!position) ? 1 : 0);
				return _Utils_update(
					model,
					{v: 'L done', r: rest, n: position, B: zeroes});
			}
		} else {
			var steps = move.a;
			if (steps > 0) {
				var pos = ((position + 1) > 99) ? 0 : (position + 1);
				var allZeroes = model.u + ((!position) ? 1 : 0);
				return _Utils_update(
					model,
					{
						u: allZeroes,
						v: 'righty',
						r: A2(
							$elm$core$List$cons,
							$author$project$Main$Right(steps - 1),
							rest),
						n: pos
					});
			} else {
				var zeroes = model.B + ((!position) ? 1 : 0);
				return _Utils_update(
					model,
					{v: 'R done', r: rest, n: position, B: zeroes});
			}
		}
	}
};
var $elm$core$Basics$not = _Basics_not;
var $author$project$Main$updateTogglePlay = function (model) {
	if (model.H) {
		var m = $author$project$Main$initModel(model.C);
		return _Utils_update(
			m,
			{m: false});
	} else {
		return _Utils_update(
			model,
			{m: !model.m});
	}
};
var $author$project$Main$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 6:
				return _Utils_Tuple2(
					$author$project$Main$updateReset(model),
					$elm$core$Platform$Cmd$none);
			case 0:
				return _Utils_Tuple2(
					$author$project$Main$updateStep(model),
					$elm$core$Platform$Cmd$none);
			case 1:
				return _Utils_Tuple2(
					$author$project$Main$updateStep(model),
					$elm$core$Platform$Cmd$none);
			case 4:
				return _Utils_Tuple2(
					$author$project$Main$updateTogglePlay(model),
					$elm$core$Platform$Cmd$none);
			case 2:
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{z: model.z / 2}),
					$elm$core$Platform$Cmd$none);
			case 3:
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{z: model.z * 2}),
					$elm$core$Platform$Cmd$none);
			default:
				var dataSource = msg.a;
				return _Utils_Tuple2(
					$author$project$Main$initModel(dataSource),
					$elm$core$Platform$Cmd$none);
		}
	});
var $author$project$Main$Faster = {$: 2};
var $author$project$Main$Input = 0;
var $author$project$Main$Reset = {$: 6};
var $author$project$Main$Slower = {$: 3};
var $author$project$Main$Step = {$: 1};
var $author$project$Main$TogglePlay = {$: 4};
var $author$project$Main$UseDataSource = function (a) {
	return {$: 5, a: a};
};
var $elm$html$Html$a = _VirtualDom_node('a');
var $elm$json$Json$Encode$string = _Json_wrap;
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$align = $elm$html$Html$Attributes$stringProperty('align');
var $elm$html$Html$button = _VirtualDom_node('button');
var $elm$json$Json$Encode$bool = _Json_wrap;
var $elm$html$Html$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$bool(bool));
	});
var $elm$html$Html$Attributes$checked = $elm$html$Html$Attributes$boolProperty('checked');
var $elm$html$Html$div = _VirtualDom_node('div');
var $elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _v0 = f(mx);
		if (!_v0.$) {
			var x = _v0.a;
			return A2($elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var $elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			$elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var $elm$core$String$cons = _String_cons;
var $elm$core$String$fromChar = function (_char) {
	return A2($elm$core$String$cons, _char, '');
};
var $elm$html$Html$Attributes$href = function (url) {
	return A2(
		$elm$html$Html$Attributes$stringProperty,
		'href',
		_VirtualDom_noJavaScriptUri(url));
};
var $elm$html$Html$input = _VirtualDom_node('input');
var $elm$html$Html$label = _VirtualDom_node('label');
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 0, a: a};
};
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $elm$html$Html$Events$onClick = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'click',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var $elm$html$Html$Attributes$style = $elm$virtual_dom$VirtualDom$style;
var $elm$html$Html$table = _VirtualDom_node('table');
var $elm$html$Html$td = _VirtualDom_node('td');
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $elm$core$String$foldr = _String_foldr;
var $elm$core$String$toList = function (string) {
	return A3($elm$core$String$foldr, $elm$core$List$cons, _List_Nil, string);
};
var $elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3($elm$core$List$foldr, $elm$core$List$cons, ys, xs);
		}
	});
var $elm$core$List$concat = function (lists) {
	return A3($elm$core$List$foldr, $elm$core$List$append, _List_Nil, lists);
};
var $elm$svg$Svg$Attributes$height = _VirtualDom_attribute('height');
var $elm$svg$Svg$Attributes$style = _VirtualDom_attribute('style');
var $elm$svg$Svg$trustedNode = _VirtualDom_nodeNS('http://www.w3.org/2000/svg');
var $elm$svg$Svg$svg = $elm$svg$Svg$trustedNode('svg');
var $elm$svg$Svg$circle = $elm$svg$Svg$trustedNode('circle');
var $elm$svg$Svg$Attributes$cx = _VirtualDom_attribute('cx');
var $elm$svg$Svg$Attributes$cy = _VirtualDom_attribute('cy');
var $elm$svg$Svg$Attributes$fill = _VirtualDom_attribute('fill');
var $elm$core$String$fromFloat = _String_fromNumber;
var $elm$svg$Svg$Attributes$points = _VirtualDom_attribute('points');
var $elm$svg$Svg$polygon = $elm$svg$Svg$trustedNode('polygon');
var $elm$svg$Svg$Attributes$r = _VirtualDom_attribute('r');
var $elm$svg$Svg$Attributes$transform = _VirtualDom_attribute('transform');
var $author$project$Main$toDialElement = function (number) {
	var cc = A2(
		$elm$svg$Svg$circle,
		_List_fromArray(
			[
				$elm$svg$Svg$Attributes$cx('0'),
				$elm$svg$Svg$Attributes$cy('0'),
				$elm$svg$Svg$Attributes$r('140'),
				$elm$svg$Svg$Attributes$fill('lightgrey')
			]),
		_List_Nil);
	var angle = 3.6 * number;
	var angleStr = $elm$core$String$fromFloat(angle);
	var transformStr = 'rotate(' + (angleStr + ', 0, 0)');
	var arrow = A2(
		$elm$svg$Svg$polygon,
		_List_fromArray(
			[
				$elm$svg$Svg$Attributes$points('-10,-100 10,-100 0,-135'),
				$elm$svg$Svg$Attributes$fill('black'),
				$elm$svg$Svg$Attributes$transform(transformStr)
			]),
		_List_Nil);
	return _List_fromArray(
		[cc, arrow]);
};
var $elm$core$Basics$cos = _Basics_cos;
var $elm$core$Basics$pi = _Basics_pi;
var $elm$core$Basics$degrees = function (angleInDegrees) {
	return (angleInDegrees * $elm$core$Basics$pi) / 180;
};
var $elm$svg$Svg$line = $elm$svg$Svg$trustedNode('line');
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $elm$core$Basics$sin = _Basics_sin;
var $elm$svg$Svg$Attributes$stroke = _VirtualDom_attribute('stroke');
var $elm$svg$Svg$Attributes$x1 = _VirtualDom_attribute('x1');
var $elm$svg$Svg$Attributes$x2 = _VirtualDom_attribute('x2');
var $elm$svg$Svg$Attributes$y1 = _VirtualDom_attribute('y1');
var $elm$svg$Svg$Attributes$y2 = _VirtualDom_attribute('y2');
var $author$project$Main$toNumberElement = function (angle) {
	var yPos = (-1) * $elm$core$Basics$sin(
		$elm$core$Basics$degrees(90 - angle));
	var y2Str = $elm$core$String$fromFloat(146 * yPos);
	var y1Str = $elm$core$String$fromFloat(140 * yPos);
	var xPos = $elm$core$Basics$cos(
		$elm$core$Basics$degrees(90 - angle));
	var x2Str = $elm$core$String$fromFloat(146 * xPos);
	var x1Str = $elm$core$String$fromFloat(140 * xPos);
	var lineElement = A2(
		$elm$svg$Svg$line,
		_List_fromArray(
			[
				$elm$svg$Svg$Attributes$x1(x1Str),
				$elm$svg$Svg$Attributes$x2(x2Str),
				$elm$svg$Svg$Attributes$y1(y1Str),
				$elm$svg$Svg$Attributes$y2(y2Str),
				$elm$svg$Svg$Attributes$stroke('currentcolor')
			]),
		_List_Nil);
	return _List_fromArray(
		[lineElement]);
};
var $elm$svg$Svg$Attributes$viewBox = _VirtualDom_attribute('viewBox');
var $elm$svg$Svg$Attributes$width = _VirtualDom_attribute('width');
var $author$project$Main$toSvg = function (model) {
	var numberElements = $elm$core$List$concat(
		A2(
			$elm$core$List$map,
			function (n) {
				return $author$project$Main$toNumberElement(3.6 * n);
			},
			A2($elm$core$List$range, 0, 99)));
	var dialElement = $author$project$Main$toDialElement(model.n);
	var elements = _Utils_ap(dialElement, numberElements);
	return A2(
		$elm$svg$Svg$svg,
		_List_fromArray(
			[
				$elm$svg$Svg$Attributes$viewBox('-150 -150 300 300'),
				$elm$svg$Svg$Attributes$width('300'),
				$elm$svg$Svg$Attributes$height('300'),
				$elm$svg$Svg$Attributes$style('font-family:Source Code Pro,monospace')
			]),
		elements);
};
var $elm$html$Html$tr = _VirtualDom_node('tr');
var $elm$html$Html$Attributes$type_ = $elm$html$Html$Attributes$stringProperty('type');
var $author$project$Main$view = function (model) {
	var svgElement = $author$project$Main$toSvg(model);
	var playButtonText = 'Play';
	var dbgStr = '..';
	var cups = A2(
		$elm$core$List$filterMap,
		$elm$core$String$toInt,
		A2(
			$elm$core$List$map,
			$elm$core$String$fromChar,
			$elm$core$String$toList($author$project$Main$input)));
	return A2(
		$elm$html$Html$table,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$align('center'),
				A2($elm$html$Html$Attributes$style, 'width', '100%'),
				A2($elm$html$Html$Attributes$style, 'font-family', 'Courier New')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$tr,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$elm$html$Html$td,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$align('center'),
								A2($elm$html$Html$Attributes$style, 'font-family', 'Courier New'),
								A2($elm$html$Html$Attributes$style, 'font-size', '32px'),
								A2($elm$html$Html$Attributes$style, 'padding', '20px')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$div,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('Advent of Code 2025')
									])),
								A2(
								$elm$html$Html$div,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('Day 1: Secret Entrance')
									]))
							]))
					])),
				A2(
				$elm$html$Html$tr,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$elm$html$Html$td,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$align('center'),
								A2($elm$html$Html$Attributes$style, 'padding-bottom', '10px')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$a,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$href('https://adventofcode.com/2025/day/1')
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('https://adventofcode.com/2025/day/1')
									]))
							]))
					])),
				A2(
				$elm$html$Html$tr,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$elm$html$Html$td,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$align('center'),
								A2($elm$html$Html$Attributes$style, 'font-family', 'Courier New'),
								A2($elm$html$Html$Attributes$style, 'font-size', '16px')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$input,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$type_('radio'),
										$elm$html$Html$Events$onClick(
										$author$project$Main$UseDataSource(0)),
										$elm$html$Html$Attributes$checked(!model.C)
									]),
								_List_Nil),
								A2(
								$elm$html$Html$label,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('Input')
									])),
								A2(
								$elm$html$Html$input,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$type_('radio'),
										$elm$html$Html$Events$onClick(
										$author$project$Main$UseDataSource(1)),
										$elm$html$Html$Attributes$checked(model.C === 1)
									]),
								_List_Nil),
								A2(
								$elm$html$Html$label,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('Sample')
									]))
							]))
					])),
				A2(
				$elm$html$Html$tr,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$elm$html$Html$td,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$align('center'),
								A2($elm$html$Html$Attributes$style, 'padding', '10px')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$button,
								_List_fromArray(
									[
										A2($elm$html$Html$Attributes$style, 'width', '80px'),
										$elm$html$Html$Events$onClick($author$project$Main$Reset)
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('Reset')
									])),
								A2(
								$elm$html$Html$button,
								_List_fromArray(
									[
										A2($elm$html$Html$Attributes$style, 'width', '80px'),
										$elm$html$Html$Events$onClick($author$project$Main$Slower)
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('Slower')
									])),
								A2(
								$elm$html$Html$button,
								_List_fromArray(
									[
										A2($elm$html$Html$Attributes$style, 'width', '100px'),
										$elm$html$Html$Events$onClick($author$project$Main$TogglePlay)
									]),
								_List_fromArray(
									[
										model.m ? $elm$html$Html$text(playButtonText) : $elm$html$Html$text('Pause')
									])),
								A2(
								$elm$html$Html$button,
								_List_fromArray(
									[
										A2($elm$html$Html$Attributes$style, 'width', '80px'),
										$elm$html$Html$Events$onClick($author$project$Main$Faster)
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('Faster')
									])),
								A2(
								$elm$html$Html$button,
								_List_fromArray(
									[
										A2($elm$html$Html$Attributes$style, 'width', '80px'),
										$elm$html$Html$Events$onClick($author$project$Main$Step)
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('Step')
									]))
							]))
					])),
				A2(
				$elm$html$Html$tr,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$elm$html$Html$td,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$align('center'),
								A2($elm$html$Html$Attributes$style, 'font-family', 'Source Code Pro, monospace'),
								A2($elm$html$Html$Attributes$style, 'font-size', '24px'),
								A2($elm$html$Html$Attributes$style, 'padding', '0px')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$div,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text(
										$elm$core$String$fromInt(
											$elm$core$List$length(model.r)))
									]))
							]))
					])),
				A2(
				$elm$html$Html$tr,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$elm$html$Html$td,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$align('center'),
								A2($elm$html$Html$Attributes$style, 'font-family', 'Source Code Pro, monospace'),
								A2($elm$html$Html$Attributes$style, 'font-size', '20px'),
								A2($elm$html$Html$Attributes$style, 'padding', '10px')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$div,
								_List_Nil,
								_List_fromArray(
									[svgElement]))
							]))
					])),
				A2(
				$elm$html$Html$tr,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$elm$html$Html$td,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$align('center'),
								A2($elm$html$Html$Attributes$style, 'font-family', 'Source Code Pro, monospace'),
								A2($elm$html$Html$Attributes$style, 'font-size', '24px'),
								A2($elm$html$Html$Attributes$style, 'padding', '0px')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$div,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text(
										$elm$core$String$fromInt(model.B))
									])),
								A2(
								$elm$html$Html$div,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text(
										$elm$core$String$fromInt(model.u))
									]))
							]))
					]))
			]));
};
var $author$project$Main$main = $elm$browser$Browser$element(
	{aC: $author$project$Main$init, aI: $author$project$Main$subscriptions, aK: $author$project$Main$update, aL: $author$project$Main$view});
_Platform_export({'Main':{'init':$author$project$Main$main(
	$elm$json$Json$Decode$succeed(0))(0)}});}(this));