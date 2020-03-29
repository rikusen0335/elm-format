## upgrade-tool

- there's now usage info `elm-refactor --help`
- upgrade definitions are specified with `--upgrade`, and you can specify more than one
- specifying no upgrade definitions will normalize references
- you can now specify multiple files to transform
- you can now specify directories containing .elm files to transform
- types can be upgraded with `type alias Upgrade_SomeModule_OldType a = SomeModule.NewType a`
- variables and types that are referenced via `exposing (SomeType, someVar)` will now be upgraded
- variables and types that are references via `exposing (..)` will now be upgraded
- preferred `exposing` clauses in the imports of the upgrade definition will now be applied when possible


## alpha-59-g62e3772

- upgrade scripts now must fully qualify references to custom type variants from deprecated modules
- import aliases in the source modules are applied to the upgraded code
- substitutions using `<|` are now simplified the same as when using parens
- simplification of case expressions is smarter about knowing when it can simplify (for example, when there is a wildcard branch but earlier branches are guaranteed to never match)


## alpha-53-g77d3d70

- no longer misses certain replacements and simplifications in the body of let expressions
- lets you define upgrade definitions for constructor tags (`upgrade_Module_SomeCapitalizedTagName = ...`)


## alpha-51-g190ce73

- should more correctly simplify nested expressions
- fixes the problems where it wouldn't always find all the expressions it should upgrade


## alpha-50-g44bcffd

- first release
