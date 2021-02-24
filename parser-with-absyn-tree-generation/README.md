# Parser Documentation
Anshu Dwibhashi, Yuanrui Zhang, Clayton Shafer  
ECE 553 Spring 2021

## Shift-Reduce Conflicts
Our parser has a single shift-reduce conflict. The conflict arises due to the confusion between `lvalue : lvalue LBRACK exp RBRACK` and `exp : ID LBRACK exp RBRACK OF exp`. This situation is problematic because the parser does not know whether to shift the `ID` in anticipation of an array creation, or to reduce the `ID` to an `lvalue` in anticipation of an array access. We solved this issue by creating an additional grammar rule `lvalue : ID LBRACK exp RBRACK`. This allows the parser to wait until it sees an `OF` before making the decision between the two conflicting rules. Since default behavior is to shift instead of reducing, this approach will parse correctly.
