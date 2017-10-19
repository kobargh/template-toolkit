{-|
Module      : Text.TemplateToolkitLang
Description : Template Toolkit language manual for Haskell
Copyright   : (c) Dzianis Kabanau, 2017
Maintainer  : kobargh@gmail.com

Template Toolkit is a template processing system originally written in Perl by Andy Wardley.
Below is the complete documentation of features implemented in Haskell version of the system. Original documentation is available <http://www.template-toolkit.org/docs/index.html here>, but be warned that not all features are realized in Haskell version.

-}

module Text.TemplateToolkitLang (
    -- * Tags
    -- $tags
    
    -- * Variables
    -- $variables
    
    -- * Operators
    -- $operators
    
    -- * Directives
    -- $directives
    
    -- * Virtual methods
    -- $vmethods
    
    -- * Filters
    -- $filters
    
 ) where

{-$tags

Character sequences @__[% %]__@ are used to enclose directive tags within a template.

@
 Hello __[% user.name %]__!
@

If directive tag starts by @__[%-__@ then all spaces before the tag are stripped. With @__-%]__@ all spaces after the tag are removed.

Multiple directives may be enclosed within one tag. In this case they must be separated by @__;__@

Comments are started by @__#__@

@
 __[%__
   __FOREACH__ __x__ = [0,15,100];
     __x__;     /# Comments here/
   __END__;
 __%]__
@
-}

{- $variables
There are three types of variables implemented: SCALARS, ARRAYS and HASHES.

Dots are used to traverse hashes and arrays: @__[% my_hash.key1 %]__@, @__[% my_array.0 %]__@.

Arrays may be represented in range form: @__[% xs = [1..10] %]__@

Actually arrays and hashes are kind-of Perl's arrayrefs and hashrefs.
So, for example, if two variables refer to one hash, modifying one variable affects another too.
Assignment to undefined hashes and arrays __autovivificates__ needed structures.

@
 __[%__
   /# scalar/
   __int__ = 100;
   __flt__ = 0.2;
   __str__ = \"Hello \\\"guest\\\"\"; /# backslash is used to escape characters \" \' \\ $ and to type newlines \\n and tabs \\t./
   
   /# array/
   __arr__ = [1,2,\"three\"];
   __rng__ = [1..10];       /# [1,2,3,4,5,6,7,8,9,10]/
   
   /# hash/
   __foo__ = {
     k1 => 1,
     k2 => \"value 2\",
     k3 => [1,2,3],
     k4 => {
        subk1 => 10,
        subk2 => 20,
     }
   };
   
   __bar__ = __foo.k4;__
   __bar.subk1__ = \"this is ten\";
   __foo.k4.subk1;__        /# \'this is ten\'/
   
   __bazarray.10.k1__ = 1; /# autovivification of array \'bazarray\' and hash \'bazarray.10\'/
   
 __%]__
@

Variables may be __interpolated__ in double-quoted strings and in complex dotted variables (hashes or arrays).

__$__ character is used to precede interpolated variable: @"__$var__"@. Complex variables may be enclosed in curly braces: @"__${myhash.k1.0}__"@. 

Single-qouted strings do not interpolate variables.

@
 __[%__
   __i__ = 2;
   __kbar__ = \'bar\';
   __arr__ = [\'one\',\'two\',\'three\',\'four\',\'five\'];
   __hsh__ = {foo => 10, bar => 20};
   __arr.$i__; /# \'three\'/
   \"Foo is __${hsh.foo}__.0\";         /# \'Foo is 10.0\'/
   __hsh.$kbar__; /# 20/
 __%]__
@
-}

{-$operators
=== Arithmetic operators

@
 __[%__
   15 + 3.14;    /# 18.14/
   12 - 10;      /# 2/
   2 * 2;        /# 4/
   10 \/ 4;       /# 2.5/

   5 % 2;        /# 1       modulus (remainder) of division/
                 /#         \'mod\' - synonym of \'%\'/
   13 mod 5;     /# 3/

   +7;           /# 7       unary positive/
   -7;           /# -7      unary negative/
 __%]__
@

=== Logical operators
__0__ and empty strings are /false/ values. Everything else is /true/. Internal representation of /true\/false/ is __1__\/__0__ respectively.

If both operands of comparison are strings, then they are compared as strings. Otherwise string operand is parsed as number.

@
 __[%__
   100 __>__ 99;            /# 1        true/
   100 __>=__ 101;          /# 0        false/
   100 __<__ 100;           /# 0        false/
   100 __<=__ 100;          /# 1        true/
   100 __==__ 100;          /# 1        true/
   100 __!=__ 100;          /# 0        false/
   __!__0;                  /# 1        unary negation/
   
   \"100\" > 2;           /# 1        true - numeric comparison/
   \"100\" > \"2\";         /# 0        false - string comparison/
   
   0 __||__ 100;            /# 100/
                        /# \'or\' - synonym of \'||\'/
   30 __or__ \'\';            /# 30/
   
   \'fox\' __&&__ \'dog\';      /# \'dog\'/
                        /# \'and\' - synonym of \'&&\'/
   \'fox\' __and__ \"\";        /# 0        evaluates to false value/
   
   /# operations group with ( )/
   (\"string\" && 0) __?__ \"snow\" __:__ \"rain\";      /# \'rain\'/
   /# cond ? val_true : val_false          ternary condition operator/
   /# complex operands of ternary condition operator must be enclosed in parentheses:/
   (!loop.last) ? \'not last\' : \'last\' /# \'not last\' or \'last\'/
   !loop.last ? \'not last\' : \'last\' /# always evals to 0 - same as !(loop.last ? \'not last\' : \'last\')/
 __%]__
@

=== String concatenation
Underscore @__\___@ separated by spaces is used to concatenate strings.

@
 __[%__
   __name__ = \"Zork\";
   \"Hello,\" _ __name__;        /# \'Hello, Zork\'/
 __%]__
@
-}

{-$directives
=== Conditionals

__IF__ ... [ELSIF...] [ELSE...] __END__

@
__[% IF user.name__ == \'root\' __%]__

Hello, Admin!

__[% ELSIF user.status__ > 1 __%]__

Welcome, master __[% user.name %]__!

__[% ELSE %]__

Hi, __[% user.name %]__!

__[% END %]__
@

=== Loops

__FOREACH__ x = ... __END__

Special variable @__loop__@ is used to save information about foreach loop and current iteration.

@
__[%__ __FOREACH__ __i__ = [10..15] __%]__

 I is __[% i %]__

 __[%__
   __IF__ __i__ == 12;
     __loop.index;__ /# 2       current index/
     __loop.count;__ /# 3       iteration count - index+1/
     __loop.first;__ /# 10      first loop element/
     __loop.last;__  /# 15      last loop element/
     __loop.prev;__  /# 11      previous loop element/
     __loop.next;__  /# 13      next loop element/
     __loop.odd;__   /# 1       true if iteration count is odd/
     __loop.even;__  /# 0       true if iteration count is even/
   __END;__
 __%]__

__[% END %]__
@

__WHILE__ ... __END__

@
__[%__
  x = 1;
  __WHILE__ x <= 20;

    "| X$x ";
    x = x + 1; __IF__ x > 3; __LAST__; __END__;

    __FOREACH__ y = [1..10];
      __IF__ y > 2 && y < 9; __NEXT__; __END__;
      "y$y ";
    __END__;
    
  __END__;
  /# Output: | X1 y1 y2 y9 y10 | X2 y1 y2 y9 y10 | X3 y1 y2 y9 y10/
__%]__
@

__LAST__ is used to immediately break current loop. __NEXT__ - to skip to the next iteration.

=== Processing template files and blocks

__PROCESS__ /blockname_or_filename/ [var1 = ... var2 = ... varN = ...]

__BLOCK__ /blockname/ ... __END__

@
\<html>
  __[%__ __PROCESS__ header.tt __title__ = \'Homepage\' __descr__ = \'Homepage description\' __keyw__ = \'homepage,home,firstpage\' __%]__

  \<p>Hello, __[% user.name %]__!
  
  __[% BLOCK__ foobarbazzer __%]__
    \<p>Foo__[% word %]__barbaz
  __[% END %]__
  
  __[%__
    __FOREACH w__ = [\'fox\', \'dog\', \'cat\'];
      __PROCESS__ foobarbazzer __word__ = __w__; /# \<p>Foofoxbarbaz \<p>Foodogbarbaz \<p>Foocatbarbaz/
    __END__;
  __%]__
  
  __[%__ __PROCESS__ footer.tt __%]__
\</html>
@

Variable may be passed to __PROCESS__ or __WRAPPER__ directive as a name using __$__: @__[% PROCESS $template %]__@

=== Wrapper

__WRAPPER__ /wrapper_block_or_filename/ [var1 = ... var2 = ... varN = ...] ... __END__

The @__WRAPPER__@ block evaluates its content and passes the result in the variable @__content__@ to /wrapper_block_or_filename/. This is very useful for web templates with the same skeleton.

@
/FILE index.tt/
__[% WRAPPER html_wrap.tt title = \'Barbaz homepage!\' %]__
\<p>Welcome to Barbaz home!
__[% END %]__


/FILE html_wrap.tt/
\<html>
\<head>
 \<meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\">
 \<title>__[% title %]__\</title>
\</head>
\<body>
__[% content %]__
\</body>
\</html>
@

-}

{-$vmethods

Virtual methods are applied to the variables to get new values or to modify array or hash variable.

@__[% my_array.size %]__@ - Get the number of elements in @__my_array__@.

@__[% karr = my_hash.keys %]__@ - Create array @__karr__@. The elements are the keys of @__my_hash__@.

@__[% my_array.push(\"Baz\") %]__@ - Modify array @__my_array__@ appending scalar @__\"Baz\"__@ to the end of the array.

Methods that use regular expressions are based on __pcre__ patterns, e.g. @__[% bar.match(\'(?i)^baz\\\\d$\') %]__@.

Below is the list of implemented virtual methods. You can also take a look at the original documentation: <http://www.template-toolkit.org/docs/manual/VMethods.html Template Toolkit VMethods>.

===Scalar vmethods
[@.collapse@] Returns the string with trailing and leading spaces removed and internal multiple spaces converted to single space.

[@.defined@] Returns true if the value is defined.

[@.lcfirst@] Returns the string with first character converted to lower case.

[@.length@] Returns the length of the string.

[@.lower@] Returns the string converted to lower case.

[@.match(/pattern/, /global/)@] Returns the array of captured substrings from regexing target string with @/pattern/@. If pattern-match fails, returns @/false/@. Optional @/global/@ is used for global pattern-match to return all captured substrings.

[@.replace(/old/, /new/)@] Returns the string with all occurrences of @/old/@ replaced by @/new/@.

[@.split(/pattern/)@] Returns the array produced by splitting the string by @/pattern/@. 

[@.trim@] Returns the string with leading and trailing spaces removed.

[@.ucfirst@] Returns the string with first character converted to upper case.

[@.upper@] Returns the string converted to upper case.

===Array vmethods

[@.first@] Returns the first element of the array.

[@.grep(/pattern/)@] Returns the array of elements that match @/pattern/@.

[@.import(/arr1/, /arr2/, /arrN/)@] Imports to the target array elements of all passed arrays. Target array is modified.

[@.join(/separator/)@] Returns the string created by joining all elements of the array separated by @/separator/@.

[@.last@] Returns the last element of the array.

[@.nsort@] Returns the array sorted numerically.

[@.pop@] Removes the last element from the array. Target array is modified.

[@.push(/el1/, /el2/, /elN/)@] Appends element(s) to the end of the array. Target array is modified.

[@.reverse@] Returns reversed version of the array.

[@.shift@] Removes the first element from the array. Target array is modified.

[@.size@] Returns the number of elements in the array.

[@.slice(/from/, /to/)@] Returns the array of elements idexing in the range @/from/../to/@ of the original array. If @/to/@ is omitted, then to the last element.

[@.sort@] Returns the array sorted alphabetically.

[@.splice(/offset/,/length/,/insertarray/)@] Powerful array-modification method that behaves just like <https://perldoc.perl.org/functions/splice.html Perl's splice>. Removes from the array @/length/@ elements starting from @/offset/@ index, replacing them with @/insertarray/@. If @/insertarray/@ is omitted - just removes elements. If @/length/@ is omitted removes everything up to the end of the array. @/length/@ may be @0@ - no elements are removed. @/offset/@ may be negative - counts index from the end of the array, e.g. @-2@ - penultimate element's index. Target array is modified.

[@.unique@] Returns the array of only unique elements.

[@.unshift(/el1/, /el2/, /elN/)@] Prepends element(s) to the beginning of the array. Target array is modified.

===Hash vmethods

[@.delete(/key1/, /key2/, /keyN/)@] Deletes items identified by the keys from the hash. Target hash is modified.

[@.each@] Returns keys and values of the hash in one array - @[key1, val1, key2, val2, ... keyN, valN]@.

[@.keys@] Returns the array of the hash keys.

[@.import(/hash1/, /hash2/, /hashN/)@] Imports to the target hash items of all passed hashes. Existing items get new values, new items are added. Target hash is modified.

[@.item(/key/)@] Returns the value of hash item identified by /key/. This is useful to get values with keys that contain any characters. E.g. @__[% hash.item('complex $ key.') %]__@ or @__[% hash.item('12') %]__@.

[@.pairs@] Returns the array of key-value pairs of the hash. The pairs are hashes each with two keys: @__key__@ and @__value__@.

[@.size@] Returns the number of items in the hash.

[@.values@] Returns the array of the hash values.

Sometimes a hash may have items whith keys that conflict with the above method names. In this situation parentheses may be used to force method call.

@
__[%__
  __my_car__ = {
    \'keys\' => \'lost\',
    \'mileage\' => 150000,
  };
  __my_car.keys__; /# \'lost\'/
  __my_car.keys()__; /# [\'keys\', \'mileage\']/
__%]__
@

-}

{-$filters

Filters transform text of a block. Filter may be applied as a directive:

__FILTER__ /filter_name/ ... __END__

Or in piped form (multiple filters may be chained):

/expr/ __|__ /filter_name [| /filter_name]/

@
__[% FILTER collapse %]__
__[% PROCESS body.tt %]__
__[% END %]__

The same:

__[% PROCESS body.tt | collapse %]__
@

Below is the list of implemented filters.

[@collapse@] Removes leading and trailing spaces. Internal multiple spaces are converted to single space.

[@html@] Replaces all @__\<__@, @__>__@, @__&__@, @__\"__@ with @__&lt;__@, @__&gt;__@, @__&amp;__@, @__&quot;__@ respectively.

[@lcfirst@] Converts the first character to lower case.

[@lcfirst@] Converts all characters to lower case.

[@null@] Discards the text of block

[@replace(/old/, /new/)@] Replaces all occurrences of @/old/@ with @/new/@.

[@trim@] Removes leading and trailing spaces.

[@ucfirst@] Converts the first character to upper case.

[@upper@] Converts all characters to upper case.

[@uri@] URI-encodes text.

-}
