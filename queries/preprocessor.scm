(preprocessor_define (identifier) @define)

(preprocessor_undef (identifier) @undef)

(preprocessor_import (identifier) @import)

(preprocessor_include (filename) @include)

(preprocessor_once) @once

(
	(preprocessor_if condition: (preprocessor_expression) @condition)
    (_)* @body
	(
		(preprocessor_else)
		(_)* @alternative
	)?
	(preprocessor_endif)
)
