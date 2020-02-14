# AspectAG-Examples

Some [AspectAG](https://gitlab.fing.edu.uy/jpgarcia/AspectAG) examples and benchmarks.

[Informe](https://www.overleaf.com/read/swxqrbtqhqqn)

## /bechmark

Contains benchmarks for compilation times.

## /expr

* **/expr/ExprSyntax.lhs**: Expression syntax.
* **/expr/ExprEval.lhs**: Evaluation of expressions.
* **/expr/ExprOptimizer.lhs**: Optimization of expressions.
* **/expr/ExprPrettyPrint.lhs**: Printing of expresions.
* **/expr/ExprTypeChecker.lhs**: Checking if the expresions are valid or not.
* **/expr/TestsEval.hs**: Expression evaluation testing.
* **/expr/ExprCodeGenerator.lhs**: Generates the machine code from a expresion.

## /micro-pascal

* **/micro-pascal/MicroPascalSyntax.lhs**: MicroPascal syntax.
* **/micro-pascal/MicroPascalCodeGenerator.lhs**: Generates the machine code from a Micro Pascal program.
* **/micro-pascal/MicroPascalOptimizer.lhs**: Micro Pascal Code Optimization.
* **/micro-pascal/MicroPascalPrettyPrint.lhs**: Printing of MicroPascal Code.
* **/micro-pascal/MicroPascalTypeChecker.lhs**: Checks a MicroPascal program for type errors.
* **/micro-pascal/MicroPascalNameChecker.lhs**: Checks a MicroPascal program for errors in it's variable names.

## /machine-lang
* **/machine-lang/MachineLangSyntax.lhs**: Machine Language Syntax.
* **/machine-lang/MachineLangInterpreter.lhs**: An interpreter for the Machine Language Code.

### /machine-lang/benchmark
Contains execution time tests for the interpreter implemented with recursion and for the interpreter implemented with AspectAG.

## /machine-lang-alt
* **/machine-lang-alt/MachineLangAltSyntax.lhs**: Another Machine Language Syntax used by the alternative implementation of th interpreter.
* **/machine-lang-alt/MachineLangAltGenerator.lhs**: Generates the Alternative Machine Language "code" used by the alternative interpreter.
* **/machine-lang-alt/MachineLangAltInterpreter.lhs**: An alternative interpreter for the Machine Language Code.
