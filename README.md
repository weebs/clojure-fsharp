# h'what is this?
A work in progress (compiler|interpreter|?) for a subset of Clojure

# Why?
I've been wanting to learn more about interpreters, and rather than create my own language, I decided it'd be more useful to interpret an existing language and learn more lisp along the way :)

# Goals
- Implement enough of Clojure to be useful
- Have some fun
- ??
- Port parser and one mode of execution (likely AST interpretation) to be compatible with Fable
- Support generation of .NET types for Unity/Godot modding

# Future Work
- Compilation of Clojure into LambdaExpression's or another intermediate form
- Experiment with dynamic method generation or libgccjit/MIR
- LSP impelementation with a focus on accurate type tracking and potential solutions from inference (I have the memory of a fish and can't write dynamic code longer than my face okay?)
- Transpilation into Fable's AST for some profit :)

# Motivations Pt 2
- If I spend any more time fighting with Mono v462 and the FCS API I'm going to uninstall programming.so and stop flipping tables
