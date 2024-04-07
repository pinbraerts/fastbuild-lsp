(preprocessor_define (variable) @variable) @define

(preprocessor_undef (variable) @variable) @undef

(preprocessor_import (environment_variable) @variable) @import

(preprocessor_include (filename) @filename) @include

(preprocessor_once) @once

(
	(preprocessor_if condition: (preprocessor_expression) @condition)
	.
	(
		(preprocessor_else)
		.
	)? @else
	(preprocessor_endif)
) @if
